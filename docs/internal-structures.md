---
id: internal-structures
---

Internal Structures
===================

A native view of expressions
----------------------------

By default, and in order to avoid having to pay
marshalling/unmarshalling costs for each argument every time one
invokes an internal R function, we represent R values in exactly the
same way R does, as a pointer to a `SEXPREC` structure (defined in
`R/Rinternals.h`). This choice has a downside, however: Haskell's
pattern matching facilities are not immediately available, since only
algebraic datatypes can be pattern matched.

`HExp` is R's `SEXP` (or `*SEXPREC`) structure represented as
a (generalized) algebraic datatype. A simplified definition of `HExp`
would go along the lines of:

```Haskell
data HExp
  = Nil                                           -- NILSXP
  | Symbol { ... }                                -- SYMSXP
  | Real { ... }                                  -- REALSXP
  | ...
```

We define one constructor for each value of the `SEXPTYPE` enumeration
in `<RInternals.h>`.

For the sake of efficiency, we do *not* use `HExp` as the basic
datatype that all H generated code expects. That is, we do not use
`HExp` as the universe of R expressions, merely as a *view*. We
introduce the following *view function* to *locally* convert to
a `HExp`, given a `SEXP` from R.

```Haskell
hexp :: SEXP s -> HExp
```

The fact that this conversion is local is crucial for good performance
of the translated code. It means that conversion happens at each use
site, and happens against values with a statically known form. Thus we
expect that the view function can usually be inlined, and the
short-lived `HExp` values that it creates compiled away by code
simplification rules applied by GHC. In this manner, we get the
convenience of pattern matching that comes with a *bona fide*
algebraic datatype, but without paying the penalty of allocating
long-lived data structures that need to be converted to and from
R internals every time we invoke internal R functions or C extension
functions.

Using an algebraic datatype for viewing R internal functions further
has the advantage that invariants about these structures can readily
be checked and enforced, including invariants that R itself does not
check for (e.g. that types that are special forms of the list type
really do have the right number of elements). The algebraic type
statically guarantees that no ill-formed type will ever be constructed
on the Haskell side and passed to R.

We also define an inverse of the view function:

```Haskell
unhexp :: HExp -> SEXP
```

A form indexed native view of expresions
----------------------------------------

In reality, H defines `HExp` in a slightly more elaborate way. Most
R functions expect their inputs to have certain predetermined forms.
For example, the `+` function expects that its arguments be of some
numeric type. A runtime error will occur when this is not the case.
Likewise, `append` expects its first argument to be a vector, and its
last argument to be a subscript. These form restrictions are
documented in a systematic way in each function's manual page. While
R itself, nor its implementation, make any attempt to enforce these
restrictions statically, Haskell's type system is rich enough to allow
us to do so.

For this reason, H allows the `SEXP` and `HExp` types to be indexed by
the form of the expression. For example, a value which is known to be
a real number can be given the type `SEXP s R.Real`. In general, one
does not always know *a priori* the form of an R expression, but
pattern matching on an algebraic view of the expression allows us to
"discover" the form at runtime. In H, we define the `HExp` algebraic
view type as a [generalized algebraic
datatype](http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt)
(GADT). In this way, the body of each branch can be typed under the
assumption that the scrutinee matches the pattern in the left hand
side of the branch. For example, in the body of a branch with pattern
`Real x`, the type checker can refine the type of the scrutinee to
`SEXP s R.Real`. In H, `HExp` is defined as follows:

```Haskell
data HExp s (a :: SEXPTYPE) where
  Nil       :: HExp R.Nil
  -- Fields: pname, value, internal.
  Symbol    :: SEXP s R.Char
            -> SEXP s a
            -> Maybe (SEXP s b)
            -> HExp R.Symbol
  Int       :: {-# UNPACK #-} !(Vector.Vector R.Int Int32)
            -> HExp R.Int
  Real      :: {-# UNPACK #-} !(Vector.Vector R.Real Double)
  ...
```

See the Haddock generated documentation for the `Language.R.HExp`
module for the full definition.

In the above, notice that the `Symbol` constructor produces a value of
type `HExp R.Symbol`, while the `Real` constructor produces a value of
type `HExp R.Real`. In other words, the type index *reflects* the
constructor of each variant, which itself is a function of the form of
a `SEXP`. For safety and clarity, we preclude indexing `SEXP` and
`HExp` with any Haskell type (which are all usually of kind `*`). We
use GHC's `DataKinds` extension to introduce a new kind of types,
named `SEXPTYPE`, and limit the possible type indexes to types that
have kind `SEXPTYPE`. Version 7.4 of GHC and later feature the
`DataKinds` extension to permit defining `SEXPTYPE` as a regular
algebraic datatype and then allowing `SEXPTYPE` to be considered as
a kind and the constructors of this type to be considered types of the
`SEXPTYPE` kind, depending on context. See the [relevant
section](http://www.haskell.org/ghc/docs/latest/html/users_guide/promotion.html)
in GHC user's guide for more information.
