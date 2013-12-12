H user’s guide
==============

Building and Installing H
-------------------------

For build and installation instruction please see README.md file
included with the H distribution.

Using H
-------

H is a library, as well as a command. The library can be used from
Haskell source files as well as from GHCi. The command is a wrapper
script that fires up a GHCi session set up just the right way for
interacting with R.

Evaluating R expressions
------------------------

Using H, the most convenient way to interact with R is through
quasiquotation. `r` is a quasiquoter that constructs an R expression,
ships it off to R and has the expression evaluated by R, yielding
a value. `H.Prelude.print`.

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

    H> H.print =<< [r| 1 + 1 |]
    [1] 2
    H> H.print =<< [r| x <- 1; x + 2 |]
    [1] 3
    H> H.print =<< [r| x |]
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

    H> H.print =<< [rexp| 1 + 1 |]
    expression(1+1)

Because quasiquoters ask R itself to parse expressions, at quasiquote
expansion time, H itself need not implement its own R parser. This
means that the entirety of R’s syntax is supported, and that it is
possible to take advantage of any future additions to the R syntax for
free in H, without any extra effort.

### Splicing Haskell values in R

Haskell values can be used in R code given to quasiquoters. When
a Haskell value is bound to a name in the lexical scope surrounding
a quasi-quote, the quasi-quote may suffix the name with `_hs` in order
to splice the Haskell value.

    H> let x = 2 :: Double
    H> H.print [r| x_hs + x_hs |]
    [1] 4

    H> let f = (\x -> return (x + 1)) :: Double -> R Double
    H> H.print =<< [r| f_hs(1) |]
    [1] 2

    H> x <- [r| 1 + 1 |]
    H> H.print =<< [r| 1 + x_hs |]
    [1] 3

### Defining spliceable types

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
[Constructing R expressions with explicit calls].

How to analyze R values in Haskell
----------------------------------

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
`SEXP`. See the “H internals” document for further justifications for
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
f (hexp -> Lang rand (hexp -> List x0 (hexp -> List x1 _ _) _) = …
f (hexp -> Closure args body@(hexp -> Lang _ _) env) = ...
```

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

The second field in a symbol is its “value”, which can be of any form,
*a priori* unknown. Therefore, when comparing two symbols recursively,
one cannot compare the two values at the same type. While type
refinement occurring *after* pattern matching on the values themselves
will always unify both types if the values are indeed equal, this is
not known to be the case *before* pattern matching. In any case, we
want to be also be able to compare values that are not in fact equal.
For this, we need a slightly generalized notion of equality, called
*heterogeneous equality*: ```Haskell class HEq t where (===) :: t a ->
t b -> Bool ``` `(===)` is a generalization of `(==)` where the types
of the arguments can be indexed by arbitrary types.

If the type checker knows statically how to unify the types of two
values you want to compare, then `(==)` suffices. This is the case
most of the time. But if not, then you can use `(===)` to compare the
values.

The `Eq` instance for `HExp` is defined in terms of the `HEq`
instance. See `H.HExp` for its definition. It compares values for
structural equality.

Catching runtime errors
-----------------------

Evaluating R expressions may result in runtime errors. All errors are
wrapped in the Foreign.R.Error.RError exception that carries the error
message.

    H> (H.print =<< [r| plot() |]) 
         `catch` (\(H.RError msg) -> putStrLn msg)
    Error in xy.coords(x, y, xlabel, ylabel, log) : 
      argument "x" is missing, with no default

Differences when using H in GHCi and in Haskell modules
-------------------------------------------------------

TODO when the differences are clear.

We need to comment that functions like H.print and H.eval use IO
instead of R monad.

Are there any exposed functions that we want the user to run in the
R thread only?

The R monad
-----------

TODO

Constructing R expressions with explicit calls
----------------------------------------------

TODO - probably remove

    H> runInRThread $ withProtected (return $ mkSEXP (1 :: Double)) $ \x0 ->
         withProtected (return $ mkSEXP (2 :: Double)) $ \x1 ->
            r2 (Data.ByteString.Char8.pack “+”) x0 x1


Calling R functions from Haskell
--------------------------------

To be implemented.

References
----------

[1]
   <a name=mainland-quasiquotes></a>
   Geoffrey B. Mainland. _Why it’s nice to be quoted: Quasiquoting for Haskell_.
   Proceedings of the ACM SIGPLAN workshop on Haskell workshop, Pages 73-82, ACM New York, NY, 2007.
