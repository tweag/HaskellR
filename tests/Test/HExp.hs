{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Test.HExp ( tests ) where

import H.Constraints
import qualified Language.R.HExp as H
import           Language.R as R hiding (withProtected)
import qualified Foreign.R as R
import           Language.R.Literal

import Foreign.C

import Control.Monad.Trans
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import Test.SmallCheck.Series
import Test.Tasty.SmallCheck
import Unsafe.Coerce (unsafeCoerce)

-- Smallcheck stull
import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.SmallCheck.Drivers as SC
import Data.Typeable

import Data.IORef
import Text.Printf

instance Monad m => Serial m Int32 where
  series = fmap (fromIntegral :: Int -> Int32) series

serialChar :: Series (R s) (H.HExp s R.Char)
serialChar = localDepth (\x -> x - 1) $ do
  s <- cons1 id
  fmap H.hexp (lift $ acquire =<< io (withCString s R.mkChar))

serialInt :: Series (R s) (H.HExp s R.Int)
serialInt = do
  s <- localDepth (const 2) series
  fmap H.hexp (lift $ acquire =<< io (mkSEXPIO (s :: [Int32])))

serialReal :: Series (R s) (H.HExp s R.Real)
serialReal = do
  s <- localDepth (const 2) series
  fmap H.hexp (lift $ acquire =<< io (mkSEXPIO (s :: [Double])))

serialSymbol :: Series (R s) (H.HExp s R.Symbol)
serialSymbol = localDepth (\x -> x -3) $ do
  sa <- lift . H.unhexp =<< serialChar
  hb <- lift . H.unhexp =<< decDepth series
  c  <- cons0 Nothing
     \/ (lift . fmap Just . H.unhexp =<< decDepth notNull)
  return $ H.Symbol sa hb c

notNull :: Series (R s) (H.HExp s a)
notNull =  fmap unsafeCoerce serialChar
        \/ fmap unsafeCoerce serialSymbol
        \/ fmap unsafeCoerce serialList
        \/ fmap unsafeCoerce serialInt
        \/ fmap unsafeCoerce serialReal

serialList :: Series (R s) (H.HExp s R.List)
serialList = localDepth (\x -> x - 3) $ do
  carval <- lift . H.unhexp =<< decDepth series
  cdrval <- cons0 Nothing
         \/ (lift . fmap Just . H.unhexp =<< decDepth notNull)
  tagval <- cons0 Nothing
         \/ (lift . fmap Just . H.unhexp =<< decDepth serialSymbol)
  return $ H.List carval cdrval tagval

instance Serial (R s) (H.HExp s a) where
  series = cons0 (unsafeCoerce H.Nil)
         \/ fmap unsafeCoerce serialChar
         \/ fmap unsafeCoerce serialSymbol
         \/ fmap unsafeCoerce serialList
         \/ fmap unsafeCoerce serialInt
         \/ fmap unsafeCoerce serialReal

unhexpOnFields :: H.HExp s a -> R s [H.HExp s a]
unhexpOnFields x = case x of
  H.Nil -> return [H.Nil]
  H.Char{} -> fmap ((:[]).H.hexp) $ H.unhexp x
  H.Int{}  -> fmap ((:[]).H.hexp) $ H.unhexp x
  H.Real{} -> fmap ((:[]).H.hexp) $ H.unhexp x
  H.Symbol a b c -> do
      a' <- H.unhexp (H.hexp a)
      b' <- mapM H.unhexp =<< unhexpOnFields (H.hexp b)
      c' <- maybe (return Nothing) (fmap Just . H.unhexp . H.hexp) c
      return $ [ H.Symbol f g h | f <- [a,a'], g <- b:b', h <- [c,c']]
  H.List a b c -> do
      a' <- H.unhexp (H.hexp a)
      b' <- maybe (return Nothing) (fmap Just . H.unhexp . H.hexp) b
      c' <- maybe (return Nothing) (fmap Just . H.unhexp . H.hexp) c
      return $ [ H.List f g h | f <- [a,a'], g <- [b,b'], h <- [c,c']]
  _ -> return []


tests :: TestTree
tests = testGroup "hexp"
    [ testGroup "Cyclyc structures"
        [ testCase "naked-cyclic-structure" $
            R.withProtected (withCString "test" R.mkChar) $ \chr -> do
              R.withProtected (H.selfSymbol chr) $ \slf -> do
                assertBool "selfSymbol==selfSymbol" (H.hexp slf === H.hexp slf)
        ]
   , localOption d $ testGroup "small-check-properties"
      [ testPropertyM "hexp.unhexp " $ MonadicProperty2 $ \depth hook ->
          runRegion $ SC.smallCheckWithHook depth (liftIO . hook) $ \p -> monadic $
           fmap ((===) p) (fmap H.hexp $ H.unhexp p)
      , testPropertyM "unhexp-on-field" $ MonadicProperty2 $ \depth hook ->
          runRegion $ SC.smallCheckWithHook depth (liftIO . hook) $ \p -> monadic $ fmap (all ((===) p)) (unhexpOnFields p)
      , testPropertyM "uniq ===" $ MonadicProperty2 $ \depth hook ->
          runRegion $ SC.smallCheckWithHook depth (liftIO . hook) $ \(p :: H.HExp s a) -> prop2 p
      ]
  ]
  where d = 5 :: SmallCheckDepth

prop2 :: forall s a . H.HExp s a -> Property (R s)
prop2 p = existsUnique $ \v -> p === (v :: H.HExp s a)

testPropertyM :: TestName -> MonadicProperty2  -> TestTree
testPropertyM name prop = singleTest name prop

newtype MonadicProperty2 = MonadicProperty2 (Depth -> (SC.TestQuality -> IO ()) -> IO (Maybe SC.PropertyFailure)) deriving (Typeable)

instance IsTest MonadicProperty2 where
  testOptions = return [Option (Proxy :: Proxy SmallCheckDepth)]

  run opts (MonadicProperty2 run) yeildProgress = do
     let SmallCheckDepth depth = lookupOption opts

     counter <- newIORef (0 :: Int, 0 :: Int)

     let
       hook quality = do
         let
           inc (total, bad) =
             case quality of
               SC.GoodTest -> ((,) $! total +1) bad
               SC.BadTest  -> ((,) $! total +1) $! bad + 1
         count <- atomicModifyIORef' counter (\c -> let c' = inc c in (c', fst c'))
         yeildProgress $ Progress
           { progressText = show count
           , progressPercent = 0
           }

     scResult <- run depth hook

     (total, bad) <- readIORef counter
     let
       desc
         | bad == 0
           = printf "%d tests completed" total
         | otherwise
           = printf "%d tests completed (but %d did not meet the condition)" total bad

     return $
       case scResult of
         Nothing -> testPassed desc
         Just f  -> testFailed $ SC.ppFailure f
