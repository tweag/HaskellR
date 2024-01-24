{-# LANGUAGE DataKinds #-}
{-# Language ViewPatterns #-}

module Language.R.Internal
  ( r1
  , r2
  , installIO
  ) where

import           Control.Memory.Region
import qualified Foreign.R as R
import           Language.R

import Data.ByteString as B
import Foreign.C.String ( withCString )

inVoid :: R V z -> R V z
inVoid = id
{-# INLINE inVoid #-}

-- | Call a pure unary R function of the given name in the global environment.
r1 :: ByteString -> SEXP s -> IO (SEXP V)
r1 fn a =
    useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      R.withProtected (R.lang2 f (R.release a)) (unsafeRunRegion . inVoid . eval)

-- | Call a pure binary R function. See 'r1' for additional comments.
r2 :: ByteString -> SEXP s -> SEXP s -> IO (SEXP V)
r2 fn a b =
    useAsCString fn $ \cfn -> R.install cfn >>= \f ->
      R.withProtected (R.lang3 f (R.release a) (R.release b)) (unsafeRunRegion . inVoid . eval)

-- | Internalize a symbol name.
{-@ installIO :: String -> IO (TSEXP V Foreign.R.Type.Symbol) @-}
installIO :: String -> IO (SEXP V)
installIO str = withCString str R.install
