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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Language.R.Parser
  ( Parser
  , runParse
  , parseOnly
  , die
    -- * Combinators
  , choice
  , with
    -- * SEXP
  , sexp
  , somesexp
    -- * HEXP
  , hexp
    -- * Attributes
  , attributes
  , someAttribute
  , attribute
  , dim
  , dimnames
  , names
  , rownames
    -- * Helpers
  , charList
    -- * SEXP predicates
  , null
  , s4
  , s3
  , isS3
  , typeOf
  , withType
  ) where

import Data.Singletons
import qualified Foreign.R as R

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad (unless, guard)
import Control.Exception (Exception, evaluate, throwIO)
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

import Prelude hiding (null)

-- | Parser. That run inspection over 'SomeSEXP'.
-- Parser is bound to the region where 'SomeSEXP' is allocated,
-- so extracted value will not leak out of the region scope.
newtype Parser s a = Parser (ReaderT (SomeSEXP s) (Except ParseException) a)
  deriving (Functor,Applicative,Alternative, Monad)

-- | Execute a parser.
runParse :: Parser s a -> SomeSEXP s -> R s (Either ParseException a)
runParse (Parser f) s = io $ evaluate $ runExcept (runReaderT f s)

-- | Parse 'SomeSEXP' throwing 'ParseException' in case if parse failed.
parseOnly :: Parser s a -> SomeSEXP s -> R s a
parseOnly p s = do
  result <- runParse p s
  case result of
    Left e -> io $ throwIO e
    Right x -> return x

-- | Fail parsing. Can be catched by alternative path.
die :: String -> Parser s a
die = Parser . ReaderT . const . throwE . ParseException

-- | Exception during parsing.
newtype ParseException = ParseException String
  deriving (Monoid, Typeable, Show)

instance Exception ParseException

-- | Check whether SEXP is 'R.Null'
null :: Parser s ()
null = void $ sexp SNil

-- | S4 object
s4 :: Parser s ()
s4 = Parser $ ReaderT $ \(SomeSEXP s) ->
  unless (R.isS4 s) $ throwE $ ParseException "Not an s4 object."

-- | Return the class of the S3 object, fails otherwise.
s3 :: Parser s [String]
s3 = charList <$> attribute SString "class"

-- | Check if 'SomeSEXP' is an S3 object of the given type.
isS3 :: [String] -> Parser s ()
isS3 ns = s3 >>= guard . (ns ==)

-- | Get any attribute of the current SEXP.
someAttribute :: String -> Parser s (SomeSEXP s)
someAttribute n = Parser $ ReaderT $ \(SomeSEXP s) -> do
  let result = unsafePerformIO $ do
        c <- withCString n R.install
        evaluate $ R.getAttribute s c
  case R.typeOf result of
    R.Nil -> throwE $ ParseException "no such attribute."
    _ -> return (SomeSEXP result)

-- | Parse attribyte of the required type.
attribute :: SSEXPTYPE a -> String -> Parser s (SEXP s a)
attribute p s = do
  (SomeSEXP z) <- someAttribute s
  if fromSing p == H.typeOf z
  then return $ R.unsafeCoerce z
  else empty

list :: Int -> Parser s (Maybe a) -> Parser s [Maybe a]
list 0 _ = return []
list n p = choice
  [ hexp SList $ \(List car cdr _) -> do
       v  <- with (SomeSEXP car) p
       vs <- with (SomeSEXP cdr) $ list (n-1) p
       return (v:vs)
  , pure []
  ]


-- | Parse all attributes.
attributes :: Parser s (Maybe a) -> Parser s [(String, a)]
attributes p = do
   SomeSEXP s <- somesexp
   let sa = unsafePerformIO $ SomeSEXP <$> R.getAttributes s
   with sa $ choice
     [ null *> pure []
     , do mns <- optional names -- XXX: better monad fail
          case mns of
            Nothing -> return []
            Just ns -> do
              ps <- list (length ns) p
              return $ mapMaybe (\(x,y) -> fmap (x,) y) $ zip ns ps
     , pure []
     ]

-- | Get current sexp. Analogue of the 'ask'.
somesexp :: Parser s (SomeSEXP s)
somesexp = Parser ask

-- | Get current sexp, and check it's type.
sexp :: SSEXPTYPE ty -> Parser s (SEXP s ty)
sexp p = somesexp >>= \(SomeSEXP s) -> if fromSing p == H.typeOf s then return (R.unsafeCoerce s) else empty

-- | Run 'Language.R.Hexp.hexp' on the current sexp.
hexp :: SSEXPTYPE ty -> (HExp s ty -> Parser s a) -> Parser s a -- XXX: rename to SEXP
hexp ty f = f . H.hexp =<< sexp ty

-- | Get type of the current SEXP.
typeOf :: Parser s R.SEXPTYPE
typeOf = (\(SomeSEXP s) -> H.typeOf s) <$> somesexp

-- | Continue execution if SEXP have required type.
withType :: R.SEXPTYPE -> Parser s ()
withType s = typeOf >>= guard . (s ==)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert String 'SEXP' to the list of 'String's.
charList :: SEXP s 'R.String -> [String]
charList (H.hexp -> String v) =
  map ((\(Char s) -> SV.toString s) . H.hexp) $ SV.toList v
charList _ = error "Impossible happened."

-- | Get 'dim' attribute.
dim :: Parser s [Int]
dim = go <$> attribute SInt "dim"
 where
   go :: SEXP s 'R.Int -> [Int]
   go (H.hexp -> Int v) = fromIntegral <$> SV.toList v
   go _ = error "Impossible happened."

-- | Get 'dimnames' attribute.
dimnames :: Parser s [[String]]
dimnames = do
  s <- attribute SVector "dimnames"
  case H.hexp s of
    Vector _ v -> for (SV.toList v) (`with` go)
  where
    go = choice [ charList <$> sexp SString
                , null *> pure []
                ]

-- | Get 'names' attribute.
names :: Parser s [String]
names = do
  s <- attribute SString "names"
  return $ charList s

-- | Get 'rownames' attribute.
rownames :: Parser s [String]
rownames = do s <- attribute SString "row.names"
              return $ charList s

-- | Execute first parser that will not fail.
choice :: [Parser s a] -> Parser s a
choice = asum

-- | Run subparser.
with :: SomeSEXP s -> Parser s a -> Parser s a
with s (Parser p) = Parser $ local (const s) p
