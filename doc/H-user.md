% H user's guide
% Amgen, Inc
%

<!-- fill-column: 70 -->

Building and Installing H
=========================

For build and installation instruction please see the `README.md` file
included with the H distribution.

Using H
=======

H is a library, as well as a command. The library can be used from
Haskell source files as well as from GHCi. The command is a wrapper
script that fires up a GHCi session set up just the right way for
interacting with R.

Evaluating R expressions in GHCi
================================

Using H, the most convenient way to interact with R is through
quasiquotation. `r` is a quasiquoter that constructs an R expression,
ships it off to R and has the expression evaluated by R, yielding
a value.

    H> [r| 1 |]
    0x00007f355520ab38

We do not normally manipulate R values directly, instead leaving them
alone for R to manage. In Haskell, we manipulate *handles* to
R values, rather than the values themselves directly. Handles have
types of the form `SEXP a`, which is actually a type synonym for
a pointer type, so values of type `SEXP a` are not terribly
interesting in their own right: they are just memory addresses, as
seen above. However, one can use `H.print` to ask R to show us the
value pointed to by a handle:

    H> H.printQuote [r| 1 + 1 |]
    [1] 2
    H> H.printQuote [r| x <- 1; x + 2 |]
    [1] 3
    H> H.printQuote [r| x |]
    [1] 1

In the following, we will loosely identify *handles to R values* and
*R values*, since the distinction between the two is seldom relevant.

The `r` quasiquoter hides much of the heavy lifting of building
expressions ourselves, allowing us to conveniently use R syntax to
denote R expressions. The next sections document some advanced uses of
quasiquotes. But for now, note that `r` is not the only quasiquoter
and one is free to implement new quasiquoters[[1]](#mainland-quasiquotes)
if needed. One such alternative quasiquoter is `rexp`, also defined in
H, which acts in much the same way as `r`, except that it returns
R expressions unevaluated:

    H> H.printQuote [rexp| 1 + 1 |]
    expression(1+1)

Because quasiquoters ask R itself to parse expressions, at quasiquote
expansion time, H itself need not implement its own R parser. This
means that the entirety of R’s syntax is supported, and that it is
possible to take advantage of any future additions to the R syntax for
free in H, without any extra effort.

> **Note:** quasiquotes are also available in compiled modules, not just
> GHCi. However, unfortunately, quasiquotes are not available in the
> H library itself. That is, one cannot use quasiquotes provided by
> H to implement part of H.

## Splicing Haskell values in R

Haskell values can be used in R code given to quasiquoters. When
a Haskell value is bound to a name in the lexical scope surrounding
a quasi-quote, the quasi-quote may suffix the name with `_hs` in order
to splice the Haskell value.

    H> let x = 2 :: Double
    H> H.printQuote [r| x_hs + x_hs |]
    [1] 4

    H> let f x = return (x + 1) :: R Double
    H> H.printQuote [r| f_hs(1) |]
    [1] 2

    H> x <- [r| 1 + 1 |]
    H> H.printQuote [r| 1 + x_hs |]
    [1] 3

## Defining spliceable types

Not all values can be spliced --- only values of certain types. The
set of spliceable types is not fixed and new types can be added as
needed. To splice a value, its type needs to be an instance of the
`H.Literal` class which defines conversion functions between Haskell
and R values.

```Haskell
class Literal a b | a -> b where
  mkSEXP   ::      a -> SEXP b
  fromSEXP :: SEXP c ->      a
```

Some predefined instances are:

```Haskell
instance Literal      Double (R.Vector Double)
instance Literal    [Double] (R.Vector Double)
instance Literal       Int32  (R.Vector Int32)
instance Literal     [Int32]  (R.Vector Int32)
instance Literal    (SEXP a)                 b
instance Literal      String        (R.String)

-- several instances of the form:
instance ( Literal a_0 a_0’, ..., Literal a_n a_n’)
      => Literal (a_0 -> ... -> a_(n-1) -> IO a_n) R.ExtPtr
```

`mkSEXP` and `fromSEXP` can be defined so that either the values on
both sides share memory or the data is copied. When memory is shared,
special care is needed to prevent garbage collection on either Haskell
or R sides to invalidate values pointed by the other side. See
[Managing memory].

Note that as a general rule, in H we avoid any conversion to and from
R values. The reason is that such conversions have runtime costs, thus
incurring a performance overhead when interoperating with R. The
`Literal` type class is only a convenience for expressing R values
using Haskell literals. Contrary to arbitrary values, literals are
typically small, and some of the conversion work can be inlined and
executed at compile time, ahead of runtime.

The R monad
===========

All expressions like

```Haskell
[r| ... |]   :: MonadR m => m (SEXP b)
H.print sexp :: MonadR m => m ()
H.eval sexp  :: MonadR m => m (SEXP b)
```

are computations in a monad instantiating `MonadR`.

```Haskell
class (Applicative m, MonadIO m) => MonadR m where
  io :: IO a -> m a
```

These monads ensure that:

 1. the R interpreter is initialized, and
 2. the computations run in a special OS thread reserved for R calls
    (so called the R thread).

There are two instances of `MonadR`, which are `IO` and `R`.

The `R` monad is intended to be used in compiled code. Functions are
provided to initialize R and to run `R` computations in the `IO`
monad.

```Haskell
runR         :: Config -> R a -> IO a
io           :: IO a -> R a
unsafeRToIO  :: R a -> IO a
```

The `IO` monad is used instead in GHCi, as it allows evaluating
expressions without the need to wrap every command at the prompt with
the function `runR`. The `IO` monad is not as safe as the `R` monad,
because it does not guarantee that R has been properly initialized,
but in the context of an interactive session this is superfluous as
the `H --interactive` command takes care of this at startup.

The `io` method of `MonadR` is used in both monads to bring
computations to the R thread.

Additionally, callback functions passed from Haskell to R are expected
to produce computations in the `R` monad, as in the example shown in
the previous section, where the type given to `f` is `Double ->
R Double`.

How to analyze R values in Haskell
==================================

## Pattern matching on views

In order to avoid unnecessary copying or other overhead every time
code crosses the boundary to/from R and Haskell, H opts for the
strategy of representing R values exactly as R itself does. However,
R values cannot be pattern matched upon directly, since they do not
share the same representation as values of algebraic datatypes in
Haskell. Regardless, R values can still be deconstructed and pattern
matched, provided a *view function* constructing a *view* of any given
R value as an algebraic datatype. H provides one such view function:

```Haskell
hexp :: SEXP a -> HExp a
```

The `HExp` datatype is a *view type* for `SEXP`. Matching on a value
of type `HExp a` is an alternative to using accessor functions to gain
access to each field. The `HExp` datatype offers a view that exposes
exactly what accessor functions would: it is a one-level unfolding of
`SEXP`. See the "H internals" document for further justifications for
using one-level unfoldings and their relevance for performance.

We have for example that:

```Haskell
hexp H.nilValue == Nil
hexp (mkSEXP ([ 2, 3 ] :: [Double])) == Real (fromList ([ 2.0, 3.0 ] :: [Double]))
```

Where `H.nilValue` is the result of evaluating `[r| NULL |]`

Using a language extension known as `ViewPatterns` one could use
`hexp` to examine an expression to any depth in a rather compact form.
For instance:

```Haskell
f (hexp -> Real xs) = …
f (hexp -> Lang rator (hexp -> List rand1 (hexp -> List rand2 _ _) _) = …
f (hexp -> Closure args body@(hexp -> Lang _ _) env) = ...
```

The fact that `hexp` is defined as a pure function introduces a caveat.
The user needs to make sure that the `SEXP` given to `hexp` has not been
released at the time hexp is evaluated. The following program wouldn't
work:

```Haskell
main = do
    () <- runR defaultConfig $ return
            $ (\(Int s) -> ())
            $ hexp
            $ mkSEXP (1 :: Int32)
    return ()
```

This program would fail because hexp is evaluated when pattern matching the
result of `runR`. And in turn, this can happen only after `runR` has
terminated the R interpreter thus invalidating the argument of `hexp`.

Similarly, the user needs to consider what the value of the `SEXP` will be
when evaluating `hexp`. Consider the following program:

```Haskell
f :: SEXP a -> (R s SomeSEXP,HExp a)
f x = let h = hexp x
         in ([r| x_hs <- 'hello' |], h)
```

The value of the expression `snd (f x)` depends on whether it is evaluated
before or after evaluating the monadic computation `fst (f x)`.


### Physical and structural equality on R values

The standard library defines an `Eq` instance for `Ptr`, which simply
compares the addresses of the pointers. Because `SEXP a` is a `Ptr` in
disguise, `s1 == s2` where `s1`, `s2` are `SEXP`s tests for *physical
equality*, namely whether `s1` and `s2` are one and the same object.

Often this is not what we want. In fact most `Eq` instances in Haskell
test for *structural equality*, a broader notion of equality. Under
physical equality, `Char x /= Char y` even if `x == y`, whereas under
structural equality, `x == y` implies `Char x == Char y`.

However, we need a notion broader still. Equality on two values of
uniform type is too restrictive for `HExp` which is a GADT. Consider
symbols, which can be viewed using the `Symbol` constructor:

```Haskell
Symbol :: SEXP (R.Vector Word8)
       -> SEXP a
       -> Maybe (SEXP b)
       -> HExp R.Symbol
```

The second field in a symbol is its "value", which can be of any form,
*a priori* unknown. Therefore, when comparing two symbols recursively,
one cannot compare the two values at the same type. While type
refinement occurring *after* pattern matching on the values themselves
will always unify both types if the values are indeed equal, this is
not known to be the case *before* pattern matching. In any case, we
want to be also be able to compare values that are not in fact equal.
For this, we need a slightly generalized notion of equality, called
*heterogeneous equality*:

```Haskell
class HEq t where
  (===) :: t a -> t b -> Bool
```

`(===)` is a generalization of `(==)` where the types of the arguments
can be indexed by arbitrary types.

If the type checker knows statically how to unify the types of two
values you want to compare, then `(==)` suffices. This is the case
most of the time. But if not, then you can use `(===)` to compare the
values.

The `Eq` instance for `HExp` is defined in terms of the `HEq`
instance. See `H.HExp` for its definition. It compares values for
structural equality.

## Vectors

Most data items in R are vectors, e.g. integers, reals, characters,
etc. H supports constructing and manipulating R vectors entirely in
Haskell, without invoking the R interpreter, and using the same API as
the de facto standard
[vector](http://hackage.haskell.org/package/vector) package.
Conversely, any data that is stored as an R vector rather than some
other vector type can be fed to R functions without any prior
conversion or copying. Considering that the memory layout of an
R vector is practically as efficient as any other unboxed
representation, programs that interact with the R interpreter
frequently should consider using R vectors as a representation by
default.

Please refer to the Haddock generated documentation of the
`Data.Vector.SEXP` and `Data.Vector.SEXP.Mutable` modules for a full
reference on the vector API supported by H.

## Casts and coercions

Type indexing `SEXP`s makes it possible to precisely characterize the
set of values that a function can accept as argument or return as
a result, but this only works well when the forms of R values are
known *a priori*, which is not always the case. In particular, the
type of the result of a call to the `r` quasiquoter is always
`SomeSEXP`, meaning that the form of the result is not statically
known. If the result needs to be passed to a function with a precise
signature, say `SEXP R.Real -> SEXP R.Logical`, then one needs to
either discover the form of the result, by first performing pattern
matching on the result before passing it to the function:

```Haskell
f :: SEXP R.Real -> SEXP R.Logical

g = do SomeSEXP x <- [r| 1 + 1 |]
       case x of
         (hexp -> R.Int v) -> return (f x)
         _ -> error "Not an int."
```

But pattern matching in this manner can be verbose, and sometimes the
user knows more than the type checker does. In the example above, we
know that `[r| 1 + 1 |]` will always return a real. We can use *casts*
or *coercions* to inform the type checker of this:

```Haskell
f :: SEXP R.Real -> SEXP R.Logical

g = do x <- [r| 1 + 1 |]
       return $ f (R.Int `R.cast` x)
```

A *cast* introduces a dynamic form check at runtime to verify that the
form of the result was indeed of the specified type. This dynamic type
check was a (very small) cost. If the user is extra sure about the
form, she may use *coercions* to avoid even the dynamic check, when
the situation warrants it (say in tight loops). This is done with

```Haskell
unsafeCoerce :: SEXP a -> SEXP b
```

This function is highly unsafe - it is H's equivalent of Haskell's
`System.IO.Unsafe.unsafeCoerce`. It is a trapdoor that can break type
safety: if the form of the argument happens to not match the expected
form at runtime then a segfault may result, or worse, silent memory
corruption.

Managing memory
===============

One tricky aspect of bridging two languages with automatic memory
management such as R and Haskell is that we must be careful that the
garbage collectors (GC) of both languages see eye-to-eye. The embedded
R instance manages objects in its own heap, separate from the heap
that the GHC runtime manages. However, objects from one heap can
reference objects in the other heap and the other way around. This can
make garbage collection unsafe because neither GC's have a global view
of the object graph, only a partial view corresponding to the objects
in the heaps of each GC.

Fortunately, R provides a mechanism to "protect" objects from garbage
collection until they are unprotected. We can use this mechanism to
prevent R's GC from deallocating objects that are still referenced by
at least one object in the Haskell heap.

One particular difficulty with protection is that one must not forget
to unprotect objects that have been protected, in order to avoid
memory leaks. H provides the following facility for pinning an object
in memory and guaranteeing unprotection when a function returns:

```Haskell
withProtected :: IO (SEXP a)      -- ^ Action to acquire resource
              -> (SEXP a -> IO b) -- ^ Action
              -> IO b
```

An example of bad memory usage
------------------------------

```Haskell
do x <- [rexp| 2 |]
   y <- [rexp| 3 |]
   [rexp| x_hs + y_hs |]
```

Since the R garbage collector may kick-in before trying to allocate
more objects, there is the potential for `x` to be collected when `y`
is allocated. Moreover, both `x` and `y` may be collected when [r| x +
y |] allocates and evaluates the expression `x_hs + y_hs`.

A way around this is to use `withProtected`.

```Haskell
withProtected [rexp| 2 |] >>= \x ->
  withProtected [rexp| 3 |] >>= \y ->
    [rexp| x_hs + y_hs |]
```

The `withProtect` pattern for memory protection works well for simple
scenarios when the lifetime of an object follows lexical scope. For
more complex scenarios, H provides a mechanism to wrap `SEXP`s as
foreign pointers, called `RVal`s. This mechanism piggybacks Haskell's
GC to notify R's GC when it is safe to deallocate. The downside of
`RVal`s is that one must frequently unwrap them when passing them as
arguments to R primitives and other functions.

Continuing with our example, we could use `RVal`s as follows:

```Haskell
do rx <- newSomeRVal =<< [r| 2 |]
   ry <- newSomeRVal =<< [r| 3 |]
   withSomeRVal rx $ \x ->
     withSomeRVal ry $ \y ->
       newSomeRVal =<< [r| x_hs + y_hs |]
```

A good way to stress test whether R values are being protected
adequately is to turn on `gctorture`:

```Haskell
do [r| gctorture2(1, 0, TRUE) |]
   ...   
```

This instructs R to run a GC sweep at every allocation, hence making
it much more likely to detect inadequately protected objects. It is
recommended to use a version of R that has been compiled with
`--enable-strict-barrier`.

See The Haddock generated documentation for the `Language.R.GC` module
for further details.

Catching runtime errors
=======================

Evaluating R expressions may result in runtime errors. All errors are
wrapped in the `Foreign.R.Error.RError` exception that carries the
error message.

    H> (H.printQuote [r| plot() |]) 
         `catch` (\(H.RError msg) -> putStrLn msg)
    Error in xy.coords(x, y, xlabel, ylabel, log) : 
      argument "x" is missing, with no default

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

hello :: String -> R ()
hello name = do
    [r| print(s_hs) |]
    return ()
  where
    s = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Name?"
    name <- getLine
    R.runR R.defaultConfig (hello name)
```

References
==========

[1]
   <a name=mainland-quasiquotes></a>
   Geoffrey B. Mainland. _Why it’s nice to be quoted: Quasiquoting for Haskell_.
   Proceedings of the ACM SIGPLAN workshop on Haskell workshop, Pages 73-82, ACM New York, NY, 2007.
