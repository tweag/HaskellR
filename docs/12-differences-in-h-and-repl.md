---
id: differences-in-h-and-repl
permalink: differences-in-h-and-repl.html
---

Differences when using H in GHCi and in compiled Haskell modules
================================================================

There are two ways to use the H library and facilities. The simplest
is at the H interactive prompt, for interacting with R in the small.
But H supports equally well writing full blown programs that interact
with R in elaborate and intricate ways, possibly even multiple
instances of R.

For simplicity, at the H interactive prompt, every function in the
R library is either pure or lifted to the IO monad. This is because
the prompt itself is an instance of the IO monad. However, in large
projects, one would like to enforce static guarantees about the
R interpreter being properly initialized before attempting to invoke
any of its internal functions. Hence, in `.hs` source files H instead
lifts all functions to the `R` monad, which provides stronger static
guarantees than the `IO` monad. This parametricity over the underlying
monad is achieved by introducing the `MonadR` class, as explained
above.

To avoid having multiple instances of `MonadR` lying around, it is
important NOT to import `Language.R.Instance.Interactive` in compiled
code - that module should only be loaded in a GHCi session.

## "Hello World!" from source

Another major difference between interactive sessions and compiled
programs using H is that one needs to handle R initialization and
configuration explicitly in compiled programs, while `H --interactive`
takes care of this for us. Here is a template small program using the
H library:

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
    R.withEmbeddedR R.defaultConfig (hello name)
```
