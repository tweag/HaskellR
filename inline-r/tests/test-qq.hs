-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Run H on a number of R programs of increasing size and complexity, comparing
-- the output of H with the output of R.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--prune-unsorted" @-}
module Main where

import qualified Foreign.R as R
import qualified Foreign.R.Internal as R
import H.Prelude as H
import qualified Data.Vector.SEXP as SVector
import qualified Data.Vector.SEXP.Mutable as SMVector
import Control.Memory.Region
import Data.Text (Text)

import qualified Control.Monad.Reader
import qualified Data.IORef
import qualified Foreign.C.String
import           GHC.ForeignPtr -- Needed to help LH name resolution
import qualified GHC.ST

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Int
import Data.Singletons (sing)
import Test.Tasty.HUnit hiding ((@=?))
import Prelude -- Silence AMP warning

{-@ assume hFib :: SEXP s -> R s (TSEXP s R.SInt) @-}
{-@ lazy hFib @-}
hFib :: SEXP s -> R s (SEXP s)
hFib (H.fromSEXP -> 0 :: Int32) = [r| 0L |]
hFib (H.fromSEXP -> 1 :: Int32) = [r| 1L |]
hFib n = [r| hFib_hs(n_hs - 1L) + hFib_hs(n_hs - 2L) |]

-- | Version of '(@=?)' that works in the R monad.
(@=?) :: Literal a => String -> a -> R s ()
expected @=? actual = liftIO $ do
    let actualstr = R.checkSEXPTYPE R.SString [rsafe| actual_hs |]
    putStrLn $ "8:" ++ expected
    putStrLn $ "9:" ++ seq actualstr "ok"
    assertEqual "" expected (fromSEXP actualstr)

main :: IO ()
main = H.withEmbeddedR H.defaultConfig $ H.runRegion $ do

    -- Placing it before enabling gctorture2 for speed.
    -- XXX: how to prove that the output of mkSEXP fits
    -- the type expected by hFib if is made to expect an
    -- R.SInt.
--    ("4181L" @=?) =<< hFib =<< H.mkSEXP (19 :: Int32)

    liftIO $ putStrLn "hello1"
    _ <- [r| gctorture2(1,0,TRUE) |]

    liftIO $ putStrLn "hello2"
    ("1" @=?) =<< [r| "1" |]
{-
    ("1" @=?) =<< return [rsafe| 1 |]

    ("3" @=?) =<< [r| 1 + 2 |]

    ("3" @=?) =<< return [rsafe| base::`+`(1, 2) |]

    ("c(\"1\", \"2\", \"3\")" @=?) =<< [r| c(1,2,"3") |]

    ("2" @=?) =<< [r| x <<- 2 |]

    ("3" @=?) =<< [r| x+1 |]

    let y = (5::Double)
    ("6" @=?) =<< [r| y_hs + 1 |]

    _ <- [r| z <<- function(y) y_hs + y |]
    ("8" @=?) =<< [r| z(3) |]

    ("1:10" @=?) =<< [r| y <<- c(1:10) |]

    let foo1 = (\x -> (return $ x+1 :: R s Double))
    let foo2 = (\x -> (return $ map (+1) x :: R s [Int32]))

    ("3" @=?) =<< [r| mapply(foo1_hs, 2) |]

    ("2:11" @=?) =<< [r| mapply(foo2_hs, y) |]

    ("43" @=?) =<< [r| x <<- 42 ; x + 1 |]

    let xs = [1,2,3]::[Double]
    ("c(1, 2, 3)" @=?) =<< [r| xs_hs |]

    ("8" @=?) =<< [r| foo1_hs(7) |]

    ("NULL" @=?) H.nilValue

    let foo3 = (\n -> fmap fromSEXP [r| n_hs |]) :: Int32 -> R s Int32
    ("3L" @=?) =<< [r| foo3_hs(3L) |]

    let foo4 = (\n m -> return $ n + m) :: Double -> Double -> R s Double
    ("99" @=?) =<< [r| foo4_hs(33, 66) |]

    ("120L" @=?) =<< [r| fact_hs(5L) |]

    let foo5  = \(n :: Int32) -> return (n+1) :: R s Int32
    let apply :: R.SEXP s -> Int32 -> R s (R.SEXP s)
        apply n m = [r| n_hs(m_hs) |]
    ("29L" @=?) =<< [r| apply_hs(foo5_hs, 28L ) |]

    -- test Vector literal instance
    v1 <- do
      x <- SMVector.new R.SInt 3 :: R s (SMVector.MVector s Int32)
      SMVector.unsafeWrite x 0 1
      SMVector.unsafeWrite x 1 2
      SMVector.unsafeWrite x 2 3
      return x
    let v2 = SMVector.release v1 :: SMVector.MVector V Int32
    ("c(7, 2, 3)" @=?) =<< [r| v = v2_hs; v[1] <- 7; v |]
    io . assertEqual "" "fromList [1,2,3]" . Prelude.show =<< SVector.unsafeFreeze v1

    let utf8string = "abcd çéõßø" :: String
    s0 <- R.checkSEXPTYPE R.SString <$> [r| utf8string_hs |]
    io $ assertEqual "" utf8string $ fromSEXP s0

    let utf8string1 = "abcd çéõßø" :: Text
    s1 <- R.checkSEXPTYPE R.SString <$> [r| utf8string1_hs |]
    io $ assertEqual "" utf8string1 $ fromSEXP s1

    -- Disable gctorture, otherwise test takes too long to execute.
    _ <- [r| gctorture2(0) |]
    let x = ([1] :: [Double])
    ("3" @=?) =<< [r| v <- x_hs + 1
                      v <- v + 1
                      v |]
-}
    return ()

{-@ assume fact :: Int32 -> R s Int32 @-}
{-@ lazy fact @-}
fact :: Int32 -> R s Int32
fact (0 :: Int32) = return 1 :: R s Int32
fact n = fmap dynSEXP [r| n_hs * fact_hs(n_hs - 1L) |]

