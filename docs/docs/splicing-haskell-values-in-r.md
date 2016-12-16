---
id: splicing-haskell-values-in-r
---
## Splicing Haskell values in R

Haskell values can be used in R code given to quasiquoters. When
a Haskell value is bound to a name in the lexical scope surrounding
a quasi-quote, the quasi-quote may suffix the name with `_hs` in order
to splice the Haskell value.

    H> let x = 2 :: Double
    H> p [r| x_hs + x_hs |]
    [1] 4

    H> let f x = return (x + 1) :: R s Double
    H> p [r| f_hs(1) |]
    [1] 2

    H> x <- [r| 1 + 1 |]
    H> p [r| 1 + x_hs |]
    [1] 3

## Defining spliceable types

Not all values can be spliced --- only values of certain types. The
set of spliceable types is not fixed and new types can be added as
needed. To splice a value, its type needs to be an instance of the
`H.Literal` class which defines conversion functions between Haskell
and R values.

```Haskell
class Literal a b | a -> b where
  mkSEXP   ::      a -> SEXP s b
  fromSEXP :: SEXP s c ->      a
```

See the [Haddock API documentation][stackage-inline-r] of the
`Language.R.Literal` for a list of predefined instances.

`mkSEXP` and `fromSEXP` can be defined so that either the values on
both sides share memory or the data is copied. When memory is shared,
special care is needed to prevent garbage collection on either Haskell
or R sides to invalidate values pointed by the other side. See
[Managing memory](managing-memory.html).

Note that as a general rule, in `inline-r` we avoid any conversion to
and from R values. The reason is that such conversions have runtime
costs, thus incurring a performance overhead when interoperating with
R. The `Literal` type class is only a convenience for expressing
R values using Haskell literals. Contrary to arbitrary values,
literals are typically small, and some of the conversion work can be
inlined and executed at compile time, ahead of runtime.

[stackage-inline-r]: http://www.stackage.org/package/inline-r
