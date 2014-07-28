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
module Test.FunPtr
  ( tests )
  where

import Control.Memory.Region
import H.Prelude
import qualified Language.R.Internal.FunWrappers as R
import qualified Foreign.R as R hiding (withProtected)
import qualified Foreign.R.Type as SingR
import qualified Language.R as R (withProtected)

import Test.Missing

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Foreign (FunPtr, castFunPtr, peek)
import System.Mem.Weak
import System.Mem

data HaveWeak a b = HaveWeak
       (R.SEXP0 -> IO R.SEXP0)
       (MVar (Weak (FunPtr (R.SEXP0 -> IO R.SEXP0))))

foreign import ccall "missing_r.h funPtrToSEXP" funPtrToSEXP
    :: FunPtr () -> IO (R.SEXP s R.Any)

instance Literal (HaveWeak a b) R.ExtPtr where
  mkSEXPIO (HaveWeak a box) = do
      z <- R.wrap1 a
      putMVar box =<< mkWeakPtr z Nothing
      fmap R.unsafeCoerce . funPtrToSEXP . castFunPtr $ z
  fromSEXP = error "not now"

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "funptr is freed from R" $ do
      ((Nothing @=?) =<<) $ do
         hwr <- HaveWeak return <$> newEmptyMVar
         e <- peek R.globalEnv
         _ <- R.withProtected (mkSEXPIO hwr) $
           \sf -> return $ apply2 ".Call" sf (unsafeMkSEXP (2::Double))
         replicateM_ 10 (R.allocVector SingR.SReal 1024 :: IO (R.SEXP V R.Real))
         replicateM_ 10 R.gc
         replicateM_ 10 performGC
         (\(HaveWeak _ x) -> takeMVar x >>= deRefWeak) hwr
  ]
