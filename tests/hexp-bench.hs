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

import Foreign.R.Internal (integer, SEXP)
import qualified Foreign.R.Internal as R (SEXPTYPE(Int), typeOf)
import H.Prelude (runR, defaultConfig, io)
import Language.R.Literal.Unsafe (mkSEXP)
import Language.R.HExp.Unsafe (hexp, HExp(..))

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
          [ bench "integer" $ whnfIO $ benchInteger x
          , bench "hexp" $ whnf benchHExp x
          , bench "unsafePerformIO" $ whnf unsafePerformIO $ return x
          , bench "unsafeInlineIO" $ whnf unsafeInlineIO $ return x
          , bench "(+)" $ whnf (\i -> i + 1) (1 :: Int)
          ]
      ]

benchInteger :: SEXP R.Int -> IO Int32
benchInteger x = do
    case R.typeOf x of
      R.Int -> integer x >>= (peek :: Ptr Int32 -> IO Int32)
      _ -> error "unexpected SEXP"

benchHExp :: Foreign.R.Internal.SEXP a -> Int32
benchHExp x =
    case hexp x of
      Int s -> unsafeInlineIO $ basicUnsafeIndexM s 0
      _ -> error "unexpected SEXP"
