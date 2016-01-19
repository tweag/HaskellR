{-# Language FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# Language GADTs #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FunPtr
  ( tests )
  where

import Control.Memory.Region
import H.Prelude
import qualified Language.R.Internal.FunWrappers as R
import qualified Foreign.R as R
import qualified Foreign.R.Type as SingR
import qualified Language.R.Internal as R (r2)
import           Language.R.QQ

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString.Char8
import Foreign (FunPtr, castFunPtr)
import System.Mem.Weak
import System.Mem
import Prelude -- silence AMP warning

data HaveWeak a b = HaveWeak
       (R.SEXP0 -> IO R.SEXP0)
       (MVar (Weak (FunPtr (R.SEXP0 -> IO R.SEXP0))))

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr () -> IO (R.SEXP s 'R.Any)

instance Literal (HaveWeak a b) 'R.ExtPtr where
  mkSEXPIO (HaveWeak a box) = do
      z <- R.wrap1 a
      putMVar box =<< mkWeakPtr z Nothing
      fmap R.unsafeCoerce . funPtrToSEXP . castFunPtr $ z
  fromSEXP = error "not now"

tests :: TestTree
tests = testGroup "funptr"
  [ testCase "funptr is freed from R" $ do
      ((Nothing @=?) =<<) $ do
         hwr <- HaveWeak return <$> newEmptyMVar
         _ <- R.withProtected (mkSEXPIO hwr) $
           \sf -> R.withProtected (mkSEXPIO (2::Double)) $ \z ->
                     return $ R.r2 (Data.ByteString.Char8.pack ".Call") sf z
         replicateM_ 10 (R.allocVector SingR.SReal 1024 :: IO (R.SEXP V 'R.Real))
         replicateM_ 10 R.gc
         replicateM_ 10 performGC
         (\(HaveWeak _ x) -> takeMVar x >>= deRefWeak) hwr
  , testCase "funptr works in quasi-quotes" $
       (((2::Double) @=?) =<<) $ runRegion $ do
         let foo = (\x -> return $ x + 1) :: Double -> R s Double
         s <- [r| foo_hs(1) |]
         return $ dynSEXP s
  ]
