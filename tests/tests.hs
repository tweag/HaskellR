-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Tests. Run H on a number of R programs of increasing size and complexity,
-- comparing the output of H with the output of R.

{-# LANGUAGE GADTs #-}
module Main where

import qualified Test.Constraints
import qualified Test.FunPtr
import qualified Test.RVal
import qualified Test.Regions
import           Test.Missing

import H.Prelude
import H.Constraints
import qualified Language.R.HExp as H
import qualified Foreign.R as R hiding (withProtected)
import qualified Language.R.Instance as R
    ( initialize
    , defaultConfig )
import qualified Language.R as R
    ( withProtected )

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden.Advanced
import Test.Tasty.Golden.Manage
import Test.Tasty.HUnit

import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T (readFile)

import Control.Monad (guard)
import Control.Monad.Trans
import Control.Applicative ((<$>))

import Foreign
import System.IO
import qualified System.IO.Strict as Strict (readFile)
import System.Process
import System.FilePath

invokeR :: FilePath -> ValueGetter r Text
invokeR fp = do
    inh <- liftIO $ openFile fp ReadMode
    (_, Just outh, _, _) <- liftIO $ createProcess $ (proc "R" ["--vanilla","--silent","--slave"])
      { std_out = CreatePipe
      , std_in = UseHandle inh
      }
    liftIO $ T.pack <$> hGetContents outh


invokeH :: FilePath -> ValueGetter r Text
invokeH fp = do
    -- Logic:
    --
    --    1. Run translation process that will output translation result to the
    --    pipe.
    --
    --    XXX: in general case when multifile translation will be enabled we
    --    will have to use files that were generated by H
    --
    --    2. Save file to the temporary module
    --
    --    3. Call ghci on resulting file
    --
    (_, Just outh1, _, _) <- liftIO $ createProcess $ (proc "./dist/build/H/H" ["--ghci",fp])
      { std_out = CreatePipe }
    (_, Just outh2, _, _) <- liftIO $ createProcess $ (proc "sh" ["tests/ghciH.sh","-v0","-ghci-script","H.ghci"])
      { std_out = CreatePipe
      , std_in = UseHandle outh1 }
    liftIO $ T.pack <$> hGetContents outh2

invokeGHCi :: FilePath -> ValueGetter r Text
invokeGHCi fp = liftIO $ fmap T.pack $
    Strict.readFile fp >>= readProcess "sh" ["tests/ghciH.sh","-v0","-ghci-script","H.ghci"]

scriptCase :: TestName
           -> FilePath
           -> TestTree
scriptCase name scriptPath =
    goldenTest
      name
      (invokeR scriptPath)
      (invokeH scriptPath)
      (\outputR outputH -> return $ do
         let a = T.lines outputR
             b = T.lines outputH
         -- Continue only if values don't match. If they do, then there's
         -- 'Nothing' to do...
         guard $ not $ and (zipWith compareValues a b) && Prelude.length a == Prelude.length b
         return $ unlines ["Outputs don't match."
                          , "R: "
                          , T.unpack outputR
                          , "H: "
                          , T.unpack outputH
                          ])
      (const $ return ())
  where
    -- Compare Haskell and R outputs:
    -- This function assumes that output is a vector string
    --    INFO Value1 Value2 .. ValueN
    -- where INFO is [OFFSET]. For decimals we are checking if
    -- they are equal with epsilon 1e-6, this is done because R
    -- output is not very predictable it can output up to 6
    -- characters or round them.
    compareValues :: Text -> Text -> Bool
    compareValues r h =
      let (r': rs') = T.words r
          (h': hs') = T.words h
      in (r' == h') && (all eqEpsilon $ zip (map (read . T.unpack) rs' :: [Double]) (map (read . T.unpack) hs' :: [Double]))
    eqEpsilon :: (Double, Double) -> Bool
    eqEpsilon (a, b) = (a - b < 1e-6) && (a - b > (-1e-6))

ghciSession :: TestName -> FilePath -> TestTree
ghciSession name scriptPath =
    goldenTest
      name
      (liftIO $ T.readFile $ scriptPath ++ ".golden.output")
      (invokeGHCi scriptPath)
      (\goldenOutput outputH ->
         let a = T.replace "\r\n" "\n" goldenOutput
             b = T.replace "\r\n" "\n" outputH
         in if T.words a == T.words b
            then return Nothing
            else return $ Just $
              unlines ["Outputs don't match."
                      , "expected: "
                      , T.unpack a
                      , ""
                      , Prelude.show $ T.unpack a
                      , ""
                      , "H: "
                      , T.unpack b
                      , ""
                      , Prelude.show $ T.unpack b
                      ])
      (const $ return ())

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "fromSEXP . mkSEXP" $ unsafeRunInRThread $
      (2 :: Double) @=? fromSEXP (unsafeMkSEXP (2 :: Double))
  , testCase "HEq HExp" $ unsafeRunInRThread $ do
      -- XXX ideally randomly generate input.
      let x = 2 :: Double
      assertBool "reflexive" $
          let s = H.hexp $ unsafeMkSEXP x in s === s
      assertBool "symmetric" $
          let s1 = H.hexp $ unsafeMkSEXP x
              s2 = H.hexp $ unsafeMkSEXP x
          in s1 === s2 && s2 === s1
      assertBool "transitive" $
          let s1 = H.hexp $ unsafeMkSEXP x
              s2 = H.hexp $ unsafeMkSEXP x
              s3 = H.hexp $ unsafeMkSEXP x
          in s1 === s2 && s2 === s3 && s1 === s3
  , testCase "Haskell function from R" $ unsafeRunInRThread $ do
--      (("[1] 3.0" @=?) =<<) $
--        fmap ((\s -> trace s s).  show . toHVal) $ alloca $ \p -> do
      (((3::Double) @=?) =<<) $ fmap fromSEXP $
          alloca $ \p -> do
            e <- peek R.globalEnv
            R.withProtected (return $ unsafeMkSEXP $ \x -> return $ x + 1 :: R s Double) $
              \sf -> apply2 ".Call"
                          sf
                          (unsafeMkSEXP (2::Double))
                     >>= \(R.SomeSEXP s) -> R.cast R.Real <$> R.tryEval s (R.release e) p
  , testCase "Weak Ptr test" $ unsafeRunInRThread $ runRegion $ do
      key  <- mkSEXP (return 4 :: R s Int32)
      val  <- mkSEXP (return 5 :: R s Int32)
      True <- return $ R.typeOf val == R.ExtPtr
      n    <- io $ H.unhexpIO H.Nil
      rf   <- io $ R.mkWeakRef key val n True
      True <- case H.hexp rf of
                H.WeakRef a b c _ -> do
                  True <- return $ (R.unsexp a) == (R.unsexp key)
                  True <- return $ (R.unsexp b) == (R.unsexp val)
                  return $ (R.unsexp c) == (R.unsexp n)
                _ -> error "unexpected type"
      return ()
  , Test.Constraints.tests
  , Test.FunPtr.tests
  , Test.RVal.tests
  , Test.Regions.tests
  , testCase "sanity check " $ unsafeRunInRThread $ return ()
  ]

integrationTests :: TestTree
integrationTests = testGroup "Integration tests"
  [ ghciSession "qq.ghci" $
       "tests" </> "ghci" </> "qq.ghci"
  , ghciSession "qq-stderr.ghci" $
       "tests" </> "ghci" </> "qq-stderr.ghci"
  -- , scriptCase "Functions - factorial" $
  --     "tests" </> "R" </> "fact.R"
  -- , scriptCase "Functions - Fibonacci sequence" $
  --     "tests" </> "R" </> "fib.R"
  ]

tests :: TestTree
tests = testGroup "Tests" [unitTests, integrationTests]

main :: IO ()
main = do
    _ <- R.initialize R.defaultConfig
    defaultMain tests
