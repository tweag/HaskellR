{-# LANGUAGE DataKinds #-}
{-# Language ViewPatterns #-}

module Language.R.Internal (r1, r2, installIO) where

import           Control.Memory.Region
import           Control.Monad.R.Class
import qualified Foreign.R as R
import           Foreign.R (SEXP, SomeSEXP)
import           Language.R

import Data.ByteString as B
import Foreign.C.String ( withCString )

-- | Helper
inVoid :: R V z -> R V z
inVoid = id
{-# INLINE inVoid #-}

-- | Call a pure unary R function of the given name in the global environment.
r1 :: ByteString -> SEXP s a -> IO (SomeSEXP V)
r1 fn a =
    useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      R.withProtected (R.lang2 f (R.release a)) (unsafeToIO . inVoid . eval)

-- | Call a pure binary R function. See 'r1' for additional comments.
r2 :: ByteString -> SEXP s a -> SEXP s b -> IO (SomeSEXP V)
r2 fn a b =
    useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      R.withProtected (R.lang3 f (R.release a) (R.release b)) (unsafeToIO . inVoid . eval)

-- | Internalize a symbol name.
installIO :: String -> IO (SEXP V 'R.Symbol)
installIO str = withCString str R.install
