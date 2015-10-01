---
id: differences-repl-source
---

# Differences when using H from compiled Haskell modules

There are two ways to use `inline-r`. The simplest is at an
interactive prompt, such as H or IHaskell, for interacting with R in
the small. But we support equally well writing full blown programs
that interact with R in elaborate and intricate ways, possibly even
multiple instances of R, using the `inline-r` library.

For simplicity, at the H interactive prompt, every function in the
R library is either pure or lifted to the `IO` monad. This is because
the prompt itself is an instance of the `IO` monad. However, in large
projects, one would like to enforce static guarantees about the
R interpreter being properly initialized before attempting to invoke
any of its internal functions. Hence, in `.hs` source files `inline-r`
instead lifts all functions to the `R` monad, which provides stronger
static guarantees than the `IO` monad. This parametricity over the
underlying monad is achieved by introducing the `MonadR` class, as
explained in previous sections.

To avoid having multiple instances of `MonadR` lying around, it is
important NOT to import `Language.R.Instance.Interactive` in compiled
code - that module should only be loaded in an interactive session.

## "Hello World!" from source

Another major difference between interactive sessions and compiled
programs linked against `inline-r` directly is that one needs to
handle R initialization and configuration explicitly in compiled
programs, while `H --interactive` takes care of this for us. Here is
a template small program using the `inline-r` library:

```Haskell
module Main where

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

hello :: String -> R s ()
hello name = do
    [r| print(s_hs) |]
    return ()
  where
    s = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Name?"
    name <- getLine
    R.withEmbeddedR R.defaultConfig $ R.runRegion $ hello name
```
