-- |
-- Copyright:   (c) 2016, AlphaSheets, Inc
-- Stability:   Experimental
-- Portability: Portable
--
-- "Parser" module provides high-level interface that allows to parse
-- SEXP values into the complex Haskell data types.
-- This parser may be useful for application specific solutions
-- that need to convert different R types to the application specific ADT.
--
-- In addition this is the the simplest way to get additional attributes
-- that exists in the structure.
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.R.Parser
  ( Parser(..)
  , parseOnly
    -- * Parser interface.
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
    -- * Queries
  , typeOf
  , getS3Class
    -- * Attributes
    -- $attributes
  , someAttribute
  , attribute
    -- * Attribute parsers.
  , names
  , dim
  , dimnames
  , rownames
    -- * Helpers
  , charList
  , choice
  , list
  ) where

import Data.Singletons
import qualified Foreign.R as R

import           Control.DeepSeq
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except
import           Control.Monad (unless, guard)
import           Control.Exception (Exception, evaluate, throwIO)
import           Foreign hiding (void, with)
import           Foreign.C.String
import           H.Prelude hiding (typeOf, hexp)
import qualified H.Prelude as H

import           Data.Typeable (Typeable)
import           Data.Foldable (asum)
import           Data.Traversable
import           Data.Functor
import           Data.Maybe (mapMaybe)
import qualified Data.Vector.SEXP as SV
import           System.IO.Unsafe
import           GHC.Generics (Generic)

import Prelude hiding (null)

-- | Parser. That run inspection over 'SomeSEXP'.
-- Parser is bound to the region where 'SomeSEXP' is allocated,
-- so extracted value will not leak out of the region scope.
--
-- This parser is a pure function, so if you need to allocate
-- any object (for example for comparison or lookup) you should
-- do it before running parser.
newtype Parser s r a = Parser {runParser :: SomeSEXP s -> (a -> r) -> (ParserError s -> r) -> r}

instance Monad (Parser s r) where
  return x = Parser $ \_ f _ -> f x
  (Parser f) >>= k = Parser $ \s ok err ->
    f s (\o -> runParser (k o) s ok err) err
  fail s = Parser $ \_ _ err -> err $ ParserError s 
instance Applicative (Parser s r) where
  pure = return
  (<*>) = ap
instance Functor (Parser s r) where
  fmap = liftM
instance Alternative (Parser s r) where
  empty = fail "empty"
  f <|> g = Parser $ \s ok err ->
    runParser f s ok (const $ runParser g s ok err)

-- | Exception during parsing.
data ParserError s
  = ParserError String
    -- ^ Generic error.
  | TypeMissmatch (SomeSEXP s) R.SEXPTYPE
    -- ^ SEXP's type differ from requested one.
  | NoSuchAttribute (SomeSEXP s) String
    -- ^ Requested attribute does not exit.
  deriving (Typeable, Show, Generic)

instance NFData (ParserError s)

-- | Parse 'SomeSEXP' throwing 'ParseException' in case if parse failed.
-- 
-- Result is always forced to NF as otherwise it's not possible to
-- guarantee that value with thunks will not escape protection region.
parseOnly :: NFData a 
          => Parser s (R s (Either (ParserError s) a)) a
          -> SomeSEXP s
          -> R s (Either (ParserError s) a)
parseOnly p s = runParser p s (return . force . Right)
                              (return . force . Left)


-- $interface
-- The main functions of the parser provide a simple way of accessing
-- information about the current 'SomeSEXP'. Those functions are useful
-- if you use pure internal functions 'Foreign.R' functions to get
-- information out of the data structure.
-- 
-- Another scenario is to use them in subparsers together with 'with'
-- combinator, that allow you to inspect the structure deeper without
-- exiting the parser.

-- | Returns current 'SomeSEXP'. Never fails.
somesexp :: Parser s r (SomeSEXP s)
somesexp = Parser $ \s ok _ -> ok s

-- | Returns current 'SEXP' if it is of the requested type,
-- fails otherwise, returns @TypeMissmatch@ in that case.
sexp :: SSEXPTYPE ty
     -> Parser s r (SEXP s ty)
sexp p = Parser $ \(SomeSEXP s) ok err ->
  if fromSing p == H.typeOf s
  then ok (R.unsafeCoerce s)
  else err $ TypeMissmatch (SomeSEXP s) (fromSing p)

-- | Run a subparser on another 'SomeSEXP'. All exceptions in the
-- internal parser are propagated to the parent one.
-- This combinator allows to inspect nested structures without
-- exiting the parser, so it's possible to effectively combine it
-- with alternative function.
with :: SomeSEXP s -> Parser s r a -> Parser s r a
with s p = Parser $ \_ ok err -> runParser p s ok err

-- $guards
-- Guards provides a handy way to check if we are expecting
-- object of the type we are interesting in.

-- | Succeeds if current @SomeSEXP@ is 'R.Null'.
null :: Parser s r ()
null = void $ sexp SNil

-- | Succeeds if current @SomeSEXP@ is S4 object.
-- This check is more accurate then using @guardType S4@ as it uses
-- internal R's function to check if the object is S4.
s4 :: Parser s r ()
s4 = Parser $ \(SomeSEXP s) ok err ->
  -- Manual check using 'sexp' or 'hexp' is not enough, as R is
  -- clever enough to make this check not obvious.
  if R.isS4 s
  then ok ()
  else err (TypeMissmatch (SomeSEXP s) R.S4)
 
-- | Succeeds if 'SomeSEXP' is an S3 object of the given type.
-- In general case it's better to use 'getS3Class' because it
-- will run same check, but also will return the class(es) of
-- the current expression.
--
-- This test is not expressible in terms of the 'guardType',
-- becausee guardType does not see additional information about
-- S3 types. And any raw object can be a class instance.
s3 :: [String] -> Parser s r ()
s3 ns = getS3Class >>= guard . (ns ==)

-- | Continue execution if SEXP have required type.
-- This check tests basic types of the expression like if
-- it's integer, or real or character vector and such.
-- If you need to test object type use 's3' or 's4' directly.
guardType :: R.SEXPTYPE -> Parser s r ()
guardType s = typeOf >>= guard . (s ==)

-- $attributes
-- Attributes are additional data that can be attached to any
-- R value. Attributes may be seen as a @Map Text (SomeSEXP s0)@.
-- Attributes may add additional information to the
-- data that may completely change it's meaning. For example
-- by adding 'dim' attribute matrix or array can be created
-- out of vector, or factors are presented as an interger
-- vector with 'rownames' attribute attached.


-- | Returns any attribute by it's name if it exists.
-- Returns @NoSuchAttribute@ otherwise.
someAttribute :: String -> Parser s r (SomeSEXP s)
someAttribute n = Parser $ \(SomeSEXP s) ok err ->
  let result = unsafePerformIO $ do
        c <- withCString n R.install
        evaluate $ R.getAttribute s c
  in case R.typeOf result of
    R.Nil -> err (NoSuchAttribute (SomeSEXP s) n)
    _ -> ok (SomeSEXP result)

-- | Typed version of the 'someAttribute' call. In addition
-- to retrieving value it's dynamically type checked.
attribute :: SSEXPTYPE a -> String -> Parser s r (SEXP s a)
attribute p s = do
  (SomeSEXP z) <- someAttribute s
  if fromSing p == H.typeOf z
  then return $ R.unsafeCoerce z
  else empty

-- | Parse all attributes, takes a parser and applies it to the  
-- each attribute exists, returns list of the attribute name, together
-- with parser result.
-- If parser returns @Nothing@ - result is omitted..
attributes :: Parser s r (Maybe a) -> Parser s r [(String, a)]
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

-- | 'Language.R.Hexp.hexp' lifted to Parser, applies hexp to the current
-- value and allow to run internal parser on it. Is useful when you need
-- to inspect data using high level functions from @Language.R@.
hexp :: SSEXPTYPE ty -> (HExp s ty -> Parser s r a) -> Parser s r a
hexp ty f = f . H.hexp =<< sexp ty

-- | Returns type of the current SEXP. Can never fail.
typeOf :: Parser s r R.SEXPTYPE
typeOf = (\(SomeSEXP s) -> H.typeOf s) <$> somesexp

-- | Return the class of the S3 object, fails otherwise.
getS3Class :: Parser s r [String]
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
dim :: Parser s r [Int]
dim = go <$> attribute SInt "dim"
 where
   go :: SEXP s 'R.Int -> [Int]
   go (H.hexp -> Int v) = fromIntegral <$> SV.toList v
   go _ = error "Impossible happened."

-- | Get 'dimnames' attribute.
dimnames :: Parser s r [[String]]
dimnames = do
  s <- attribute SVector "dimnames"
  case H.hexp s of
    Vector _ v -> for (SV.toList v) (`with` go)
  where
    go = choice [ charList <$> sexp SString
                , null *> pure []
                ]

-- | Get 'names' attribute.
names :: Parser s r [String]
names = do
  s <- attribute SString "names"
  return $ charList s

-- | Get 'rownames' attribute.
rownames :: Parser s r [String]
rownames = do s <- attribute SString "row.names"
              return $ charList s

-- | Execute first parser that will not fail.
choice :: [Parser r s a] -> Parser r s a
choice = asum

-- | Parser that is applied to the R @List@ object and returns list
-- of the results.
list :: Int -- ^ Number of elements in a list.
     -> Parser s r (Maybe a) -- ^ Parser to apply to each element
     -> Parser s r [Maybe a]
list 0 _ = return []
list n p = choice
  [ hexp SList $ \(List car cdr _) -> do
       v  <- with (SomeSEXP car) p
       vs <- with (SomeSEXP cdr) $ list (n-1) p
       return (v:vs)
  , pure []
  ]
