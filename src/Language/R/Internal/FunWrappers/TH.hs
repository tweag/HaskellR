-- |
-- Copyright: (C) 2013 Amgen, Inc.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Language.R.Internal.FunWrappers.TH
  ( thWrappers
  , thWrapper
  , thWrapperLiteral
  , thWrapperLiterals
  ) where

import H.Internal.Error
import qualified Foreign.R.Type as R

import Control.Monad (replicateM)
import Foreign (FunPtr)
import Language.Haskell.TH

-- XXX: If we build quotes that mention names imported from Foreign.R, then
-- GHC panics because it fails to link in all adequate object files to
-- resolve all R symbols. So instead we build the symbol names
-- programmatically, using mkName...
nSEXP0 :: Q Type
nSEXP0 = conT (mkName "SEXP0")

-- | Generate wrappers from n to m.
thWrappers :: Int -> Int -> Q [Dec]
thWrappers n m = mapM thWrapper [n..m]

-- | Generate wrapper.
--
-- Example for input 5:
--
-- @
-- foreign import ccall \"wrapper\" wrap5
--    :: (  SEXP a -> SEXP b -> SEXP c
--       -> SEXP d -> SEXP e -> IO (SEXP f)
--       )
--    -> IO (FunPtr (  SEXP a -> SEXP b -> SEXP c
--                  -> SEXP d -> SEXP e -> IO (SEXP f)
--                  )
--          )
-- @
thWrapper :: Int -> Q Dec
thWrapper n = do
    let vars = map (mkName . return) $ take (n + 1) ['a'..]
        ty = go (map varT vars)
    forImpD cCall safe "wrapper" (mkName $ "wrap" ++ show n) $
      [t| $ty -> IO (FunPtr $ty) |]
  where
    go :: [Q Type] -> Q Type
    go [] = impossible "thWrapper"
    go [_] = [t| IO $nSEXP0 |]
    go (_:xs) = [t| $nSEXP0 -> $(go xs) |]

thWrapperLiterals :: Int -> Int -> Q [Dec]
thWrapperLiterals n m = mapM thWrapperLiteral [n..m]

-- | Generate Literal Instance for wrapper.
--
-- Example for input 6:
-- @
-- instance ( Literal a a0, Literal b b0, Literal c c0, Literal d d0, Literal e e0
--         , Literal f f0, Literal g g0
--         )
--         => Literal (a -> b -> c -> d -> e -> f -> IO g) R.ExtPtr where
--    mkSEXP = funToSEXP wrap6
--    fromSEXP = error \"Unimplemented.\"
-- @
thWrapperLiteral :: Int -> Q Dec
thWrapperLiteral n = do
    let s = varT =<< newName "s"
    names1 <- replicateM (n + 1) $ newName "a"
    names2 <- replicateM (n + 1) $ newName "i"
    let mkTy []     = impossible "thWrapperLiteral"
        mkTy [x]    = [t| $nR $s $x |]
        mkTy (x:xs) = [t| $x -> $(mkTy xs) |]
        ctx = cxt (zipWith f (map varT names1) (map varT names2))
          where
#if MIN_VERSION_template_haskell(2,10,0)
            f tv1 tv2 = foldl AppT (ConT (mkName "Literal")) <$> sequence [tv1, tv2]
#else
            f tv1 tv2 = classP (mkName "Literal") [tv1, tv2]
#endif
        -- XXX: Ideally would import these names from their defining module, but
        -- see GHC bug #1012. Using 'mkName' is a workaround.
        nR = conT $ mkName "R"
        nwrapn = varE $ mkName $ "wrap" ++ show n
        nfunToSEXP = varE $ mkName "Language.R.Literal.funToSEXP"
        nLiteral = conT $ mkName "Literal"
    instanceD ctx [t| $nLiteral $(mkTy $ map varT names1) 'R.ExtPtr |]
      [ funD (mkName "mkSEXPIO")
          [ clause [] (normalB [| $nfunToSEXP $nwrapn |]) [] ]
      , funD (mkName "fromSEXP")
          [ clause [] (normalB [| unimplemented "thWrapperLiteral fromSEXP" |]) [] ]
      ]
