-- | A number of helpers that are used in tests
module Test.Missing
  ( apply2
  ) where

import Control.Memory.Region
import Foreign.R as R
import Foreign.C (withCString)
import Language.R (evalIO)



-- | Call a pure binary R function. See 'r1' for additional comments.
apply2 :: String -> SEXP s a -> SEXP s b -> IO (SomeSEXP V)
apply2 fn a b =
    withCString fn $ \cfn -> R.install cfn >>= \f ->
      withProtected (R.lang3 f (R.release a) (R.release b)) evalIO
