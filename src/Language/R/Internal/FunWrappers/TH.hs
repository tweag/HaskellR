-- |
-- Copyright: (C) 2013 Amgen, Inc.
module Language.R.Internal.FunWrappers.TH
  ( thWrappers
  , thWrapper
  , thWrapperLiteral
  , thWrapperLiterals
  ) where

import H.Internal.Error
import Language.Haskell.TH


-- | Generate wrappers from n to m.
thWrappers :: Int -> Int -> DecsQ
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
thWrapper :: Int -> DecQ
thWrapper n = do
    forImpD cCall safe "wrapper" (mkName $ "wrap"++show n) $
      forallT (map PlainTV vars) (cxt []) $
        (appT arrowT (go vars))  `appT`
        ( conT (mkName "IO") `appT` (conT (mkName "FunPtr") `appT` (go vars)))
  where
    vars :: [Name]
    vars = take (n+1) $ map (mkName.return) ['a'..]
    go :: [Name] -> TypeQ
    go [] = impossible "thWrapper"
    go [x] =
      conT (mkName "IO") `appT` (conT (mkName "SEXP") `appT` (varT x))
    go (x:xs) =
      (appT arrowT (conT (mkName "SEXP") `appT` (varT x)))
        `appT` (go xs)

thWrapperLiterals :: Int -> Int -> DecsQ
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
thWrapperLiteral :: Int -> DecQ
thWrapperLiteral n = instanceD ctx typ funs
  where
    vars :: [Name]
    vars = take (n+1) $ map (mkName.return) ['a'..]
    vars0 :: [Name]
    vars0 = take (n+1) $ map (\i -> mkName [i,'0']) ['a'..]
    tps []  = impossible "thWrapperLiteral"
    tps [x] = conT (mkName "R") `appT` (varT x)
    tps (x:xs) = (appT arrowT (varT x)) `appT` tps xs
    -- context
    ctx = cxt (map go (zip vars vars0))
      where
        go (k, k0) = classP (mkName "Literal") [varT k, varT k0]
    -- type
    typ = (conT (mkName "Literal") `appT` (tps vars) `appT` (conT (mkName "R.ExtPtr")))
    -- funs
    funs = [ mk, from ]
    mk = funD (mkName "mkSEXP") [clause [] (normalB $ appE  (varE (mkName "H.Internal.Literal.funToSEXP")) (varE (mkName ("wrap"++show n)))) []]
    from = funD (mkName "fromSEXP") [clause [] (normalB $ appE (varE (mkName "unimplemented")) (litE (stringL "thWrapperLiteral fromSEXP"))) []]
