-- |
-- Copyright:   (c) 2016, AlphaSheets, Inc
-- Stability:   Experimental
-- Portability: Portable
--
-- A 'Matcher' lets you match 'SEXP' values against composable patterns, where
-- cascading cases would otherwise be necessary otherwise.
--
-- Example:
--
-- @
-- -- Check that input is an S3 object of class "matrix"
-- -- and return the value of the "dim" attribute.
-- isMatrix = matchOnly $ do
--    s3 ["matrix"]
--    dim
-- @

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.R.Matcher
  ( Matcher(..)
  , matchOnly
    -- * Matcher interface.
    -- $interface
  , somesexp
  , sexp
  , with
    -- * Type guards
    -- $guards
  , hexp
  , null
  , s4
  , s3
  , guardType
    -- * Queries
  , typeOf
  , getS3Class
    -- * Attributes
    -- $attributes
  , someAttribute
  , attribute
  , attributes
  , lookupAttribute
    -- * Attribute matchers
  , names
  , dim
  , dimnames
  , rownames
    -- * Derived matchers
  , factor
    -- * Helpers
  , charList
  , choice
  , list
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (guard, ap, liftM)
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Maybe (mapMaybe)
import Data.Semigroup as Sem
import Data.Singletons
import Data.Traversable
import Data.Typeable (Typeable)
import qualified Data.Vector.SEXP as SV
import Foreign hiding (void, with)
import Foreign.C.String
import qualified Foreign.R as R
import GHC.Generics (Generic)
import qualified H.Prelude as H
import H.Prelude hiding (typeOf, hexp)
import System.IO.Unsafe

import Prelude hiding (null)

-- | A composition of 'SEXP' destructors. A 'Matcher' is bound to the region
-- where 'SomeSEXP' is allocated, so extracted value will not leak out of the
-- region scope.
--
-- This matcher is a pure function, so if you need to allocate any object (for
-- example for comparison or lookup) you should do it before running matcher.
newtype Matcher s a = Matcher
  { runMatcher
      :: forall r.
         SomeSEXP s  -- expression to match
      -> (a -> r) -- continuation in case of success
      -> (MatcherError s -> r) -- continuation in case of failure
      -> r
  }

-- Continuation monad is used in order to make matching fast and and have an
-- equal cost for left and right combinations. Different continuations for
-- success and failure cases were chosen because otherwise we'd have to keep
-- result in 'Either' that would lead to more boxing. Though I have to admit
-- that benchmarks were not done, and this approach were chosen as initial one,
-- as it's not much more complex then others.

instance Monad (Matcher s) where
  return x = Matcher $ \_ f _ -> f x
  Matcher f >>= k = Matcher $ \s ok err -> f s (\o -> runMatcher (k o) s ok err) err
  fail s = Matcher $ \_ _ err -> err $ MatcherError s

instance Applicative (Matcher s) where
  pure = return
  (<*>) = ap

instance Functor (Matcher s) where
  fmap = liftM

instance Alternative (Matcher s) where
  empty = fail "empty"
  f <|> g = Matcher $ \s ok err ->
      runMatcher f s ok (\e' -> runMatcher g s ok (err . (mappend e')))

instance Sem.Semigroup (MatcherError s) where
  a <> MatcherError "empty" = a
  _ <> a = a

instance Monoid (MatcherError s) where
  mempty = MatcherError "empty"
  mappend = (<>)

-- | Exception during matching.
data MatcherError s
  = MatcherError String
    -- ^ Generic error.
  | TypeMissmatch (SomeSEXP s) R.SEXPTYPE R.SEXPTYPE
    -- ^ SEXP's type differ from requested one.
  | NoSuchAttribute (SomeSEXP s) String
    -- ^ Requested attribute does not exit.
  deriving (Typeable, Show, Generic)

instance NFData (MatcherError s)

-- | Match a 'SomeSEXP', returning a 'MatchError' if matching failed.
--
-- Result is always fully evaluated, since otherwise it wouldn't be possible to
-- guarantee that thunks in the return value will not escape the memory region.
matchOnly
  :: (MonadR m, NFData a)
  => Matcher s a
  -> SomeSEXP s
  -> m (Either (MatcherError s) a)
matchOnly p s =
  runMatcher p s (return . force . Right) (return . force . Left)

-- $interface
--
-- The main functions of the matcher provide a simple way of accessing
-- information about the current 'SomeSEXP'. Those functions are useful if you
-- use pure internal functions 'Foreign.R' functions to get information out of
-- the data structure.
--
-- Another scenario is to use them in submatchers together with 'with'
-- combinator, that allow you to inspect the structure deeper without exiting
-- the matcher.

-- | Returns current 'SomeSEXP'. Never fails.
somesexp :: Matcher s (SomeSEXP s)
somesexp = Matcher $ \s ok _ -> ok s

-- | Returns current 'SEXP' if it is of the requested type, fails otherwise,
-- returns @TypeMissmatch@ in that case.
sexp :: SSEXPTYPE ty -> Matcher s (SEXP s ty)
sexp p = Matcher $ \(SomeSEXP s) ok err ->
    if fromSing p == H.typeOf s
    then ok (R.unsafeCoerce s)
    else err $ TypeMissmatch (SomeSEXP s) (R.typeOf s) (fromSing p)

-- | Run a submatcher on another 'SomeSEXP'. All exceptions in the internal
-- matcher are propagated to the parent one. This combinator allows to inspect
-- nested structures without exiting the matcher, so it's possible to effectively
-- combine it with alternative function.
with :: SomeSEXP s -> Matcher s a -> Matcher s a
with s p = Matcher $ \_ ok err -> runMatcher p s ok err

-- $guards
--
-- Guards provides a handy way to check if we are expecting object of the type
-- we are interested in.

-- | Succeeds if current @SomeSEXP@ is 'R.Null'.
null :: Matcher s ()
null = void $ sexp SNil

-- | Succeeds if current @SomeSEXP@ is S4 object. This check is more accurate
-- then using @guardType S4@ as it uses internal R's function to check if the
-- object is S4.
s4 :: Matcher s ()
s4 = Matcher $ \(SomeSEXP s) ok err ->
    -- Manual check using 'sexp' or 'hexp' is not enough, as R is clever enough
    -- to make this check not obvious.
    if R.isS4 s
    then ok ()
    else err (TypeMissmatch (SomeSEXP s) (R.typeOf s) R.S4)

-- | Succeeds if 'SomeSEXP' is an S3 object of the given type. In general case
-- it's better to use 'getS3Class' because it will run same check, but also will
-- return the class(es) of the current expression.
--
-- This test is not expressible in terms of the 'guardType', because guardType
-- does not see additional information about S3 types. And any raw object can be
-- a class instance.
s3 :: [String] -> Matcher s ()
s3 ns = getS3Class >>= guard . (ns ==)

-- | Continue execution if SEXP have required type. This check tests basic types
-- of the expression like if it's integer, or real or character vector and such.
-- If you need to test object type use 's3' or 's4' directly.
guardType :: R.SEXPTYPE -> Matcher s ()
guardType s = typeOf >>= guard . (s ==)

-- $attributes
--
-- Attributes are additional data that can be attached to any R value.
-- Attributes may be seen as a @Map Text (SomeSEXP s0)@. Attributes may add
-- additional information to the data that may completely change it's meaning.
-- For example by adding 'dim' attribute matrix or array can be created out of
-- vector, or factors are presented as an interger vector with 'rownames'
-- attribute attached.

-- | Returns any attribute by its name if it exists. Fails with
-- @NoSuchAttribute@ otherwise.
someAttribute :: String -> Matcher s (SomeSEXP s)
someAttribute n = Matcher $ \(SomeSEXP s) ok err ->
    let result = unsafePerformIO $ do
          c <- withCString n R.install
          evaluate $ R.getAttribute s c
    in case R.typeOf result of
      R.Nil -> err (NoSuchAttribute (SomeSEXP s) n)
      _ -> ok (SomeSEXP result)

-- | Typed version of the 'someAttribute' call. In addition to retrieving value
-- it's dynamically type checked.
attribute :: SSEXPTYPE a -> String -> Matcher s (SEXP s a)
attribute p s = do
    (SomeSEXP z) <- someAttribute s
    if fromSing p == H.typeOf z
    then return $ R.unsafeCoerce z
    else empty

-- | Match all attributes, takes a matcher and applies it to the each attribute
-- exists, returns list of the attribute name, together with matcher result. If
-- matcher returns @Nothing@ - result is omitted..
attributes :: Matcher s (Maybe a) -> Matcher s [(String, a)]
attributes p = do
    SomeSEXP s <- somesexp
    let sa = unsafePerformIO $ SomeSEXP <$> R.getAttributes s
    with sa $ choice
      [ null *> pure []
      , do mns <- optional names
           case mns of
             Nothing -> return []
             Just ns -> do
               ps <- list (length ns) p
               return $ mapMaybe (\(x,y) -> fmap (x,) y) $ zip ns ps
      , pure []
      ]

-- | Find an attribute in attribute list if it exists.
lookupAttribute :: String -> Matcher s (Maybe (SomeSEXP s))
lookupAttribute s = (Just <$> someAttribute s) <|> pure Nothing

-- | 'Language.R.Hexp.hexp' lifted to Matcher, applies hexp to the current value
-- and allows you to run internal matcher on it. Is useful when you need to inspect
-- data using high level functions from @Language.R@.
hexp :: SSEXPTYPE ty -> (HExp s ty -> Matcher s a) -> Matcher s a
hexp ty f = f . H.hexp =<< sexp ty

-- | Returns type of the current SEXP. Can never fail.
typeOf :: Matcher s R.SEXPTYPE
typeOf = (\(SomeSEXP s) -> H.typeOf s) <$> somesexp

-- | Return the class of the S3 object, fails otherwise.
getS3Class :: Matcher s [String]
getS3Class = charList <$> attribute SString "class"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert String 'SEXP' to the list of 'String's.
charList :: SEXP s 'R.String -> [String]
charList (H.hexp -> String v) =
  map ((\(Char s) -> SV.toString s) . H.hexp) $ SV.toList v
charList _ = error "Impossible happened."

-- | Get 'dim' attribute.
dim :: Matcher s [Int]
dim = go <$> attribute SInt "dim"
  where
    go :: SEXP s 'R.Int -> [Int]
    go (H.hexp -> Int v) = fromIntegral <$> SV.toList v
    go _ = error "Impossible happened."

-- | Get 'dimnames' attribute.
dimnames :: Matcher s [[String]]
dimnames = do
    s <- attribute SVector "dimnames"
    case H.hexp s of
      Vector _ v -> for (SV.toList v) $ \x ->
        with (R.unsafeReleaseSome x) go
  where
    go = choice [charList <$> sexp SString, null *> pure []]

-- | Get 'names' attribute.
names :: Matcher s [String]
names = do
    s <- attribute SString "names"
    return $ charList s

-- | Get 'rownames' attribute.
rownames :: Matcher s [String]
rownames = do
    s <- attribute SString "row.names"
    return $ charList s

-- | Execute first matcher that will not fail.
choice :: [Matcher s a] -> Matcher s a
choice = asum

-- | Matches a @List@ object.
list
  :: Int -- ^ Upper bound on number of elements to match.
  -> Matcher s a -- ^ Matcher to apply to each element
  -> Matcher s [a]
list 0 _ = return []
list n p = choice
    [ null *> pure []
      -- TODO: quite possibly this method uses linear stack space. It should be
      -- verified and fixed if this is the case.
    , hexp SList $ \(List car cdr _) -> do
         v <- with (SomeSEXP car) p
         vs <- with (SomeSEXP cdr) $ list (n-1) p
         return (v:vs)
    ]

-- | Match a factor. Returns the levels of the factor.
factor :: Matcher s [String]
factor = do
    s3 ["factor"]
    levels <- charList <$> attribute SString "levels"
    hexp R.SInt $ \(Int v) ->
      return $! (\i -> levels !! (fromIntegral i - 1)) <$> SV.toList v
