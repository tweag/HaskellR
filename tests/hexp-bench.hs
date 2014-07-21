-- A benchmark comparing hexp with integer.
--
-- To get the lowest results:
--
--  * define integer as an unsafe foreign call
--
--  * replace 'System.IO.Unsafe.unsafePerformIO' with
--    'Control.Monad.Primitive.unsafeInlineIO' in the definition
--    of 'hexp' and 'Foreign.R.typeOf'.
--
--  * Add an INLINE pragma for peekHExp
--
-- >    {-# INLINE peekHExp #-}
--
--  * redefine hexp as
--
-- >    hexp :: SEXP a -> HExp a
-- >    hexp = unsafeInlineIO . peekHExp
-- >    {-# INLINE hexp #-}
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

import Foreign.R (integer, SEXP, SomeSEXP(..))
import qualified Foreign.R as R (SSEXPTYPE, SEXPTYPE(Int), typeOf, cast)
import H.Prelude (runR, defaultConfig, io)
import Language.R.Literal (mkSEXP)
import Language.R.HExp (hexp, HExp(..))
import Data.Singletons (sing)

import Control.Monad.Primitive
import Criterion.Main
import Data.Int
import Data.Vector.Generic (basicUnsafeIndexM)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main = runR defaultConfig $ io $ do
    let x = mkSEXP (1 :: Int32)
    defaultMain
      [ bgroup "vector access"
          [ bench "typeof>integer"   $ whnfIO $ benchInteger x
          , bench "hexp>unsafeIndex" $ whnf benchHExp x
          , bench "unsafe-integer"   $ whnfIO $ benchUncheckedInteger x
          , bench "hexp-cast"        $ whnf benchCast (SomeSEXP x)

--          , bench "unsafePerformIO" $ whnf unsafePerformIO $ return x
--          , bench "unsafeInlineIO" $ whnf unsafeInlineIO $ return x
--          , bench "(+)" $ whnf (\i -> i + 1) (1 :: Int)
          , bench "unsafePerformIO" $ whnf unsafePerformIO $ return x
          , bench "unsafeInlineIO" $ whnf unsafeInlineIO $ return x
          , bench "(+)" $ whnf (\i -> i + 1) (1 :: Int)
          ]
      ]

benchInteger :: SEXP s R.Int -> IO Int32
benchInteger x = do
    case R.typeOf x of
      R.Int -> integer x >>= (peek :: Ptr Int32 -> IO Int32)
      _ -> error "unexpected SEXP"

benchHExp :: SEXP s a -> Int32
benchHExp x =
    case hexp x of
      Int s -> unsafeInlineIO $ basicUnsafeIndexM s 0
      _ -> error "unexpected SEXP"

benchUncheckedInteger :: SEXP s R.Int -> IO Int32
benchUncheckedInteger x = integer x >>= (peek :: Ptr Int32 -> IO Int32)

benchCast :: SomeSEXP s -> Int32
benchCast x@(SomeSEXP z) =
 let y = R.cast (sing :: R.SSEXPTYPE R.Int) x
 in case hexp y of
      Bytecode -> error $ "hello! ^_^" ++ show (R.typeOf z, R.typeOf y)
      Int s -> unsafeInlineIO $ basicUnsafeIndexM s 0
