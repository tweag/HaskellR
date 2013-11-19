% H internals
% Amgen, Inc
%

<!-- fill-column: 70 -->

\newcommand{\trans}[2]{[\![#1]\!]\;#2}
\newcommand{\fn}[1]{\mathsf{#1}\;}
\newcommand{\infix}[1]{\;`\mathsf{#1}`\;}
\newcommand{\kw}[1]{\mathbf{#1}}
\newcommand{\function}[1]{\kw{function}(#1)\;}
\newcommand{\hsabs}[1]{\backslash #1\to\;}
\newcommand{\refv}[1]{#1^{\mathsf{ref}}}

Introduction
============

This is a guide to the internal structures and inner workings of H,
documenting the design rationale and possible variations.

TODO Documentation license

Architectural overview
======================

R source code is organized as a set of *scripts*. In its simplest
form, H is a script-to-script translator.

In its most general form, H is a desgurer for quasiquotations.
A quasi-quotation is a partial R script --- that is, a script with
holes in it that stand in for as of yet undetermined portions. An
example quasiquote in Haskell of an R snippet is:

```Haskell
[r| function(x) x + 1 ]
```

This quasiquote is *ground*, in that does not contain any holes
(called *antiquotes*), unlike the below quasiquote:

```Haskell
let y = mkSEXP 1
in [r| function(x) x + y_hs ]
```

Unlike all other symbols, any symbol with a `_hs` suffix is by
convention interpreted as a reference to a Haskell variable defined
somewhere in the ambient source code. Given any quasiquote, it is
possible to obtain a full R script, with no holes in it, by *splicing*
the value of the Haskell variables into the quasiquote, in place of
the antiquotes.

Given this general view of H as a desugarer for R quasiquoted
snippets, translating an entire R script stored in a file into
Haskell code is a mere special case: an R script file can be seen as
one giant quasiquote that contains no antiquotes.

Rather than fed directly to R, the result of a quasiquote can
optionally be *translated*. In this case, R functions are translated
to Haskell functions, and the symbols they are bound to in the
embedded R instance are rebound to wrappers that invoke the Haskell
versions of the functions.

Rationale
---------

### Rebinding and dynamic code construction

R is a very dynamic language, allowing many code modifications during
runtime, such as rebinding of top-level definitions, super assignment
(modifying bindings in parent scopes), (quasi-)quotation and
evaluation of expressions constructed dynamically. The R programming
language is also so-called "latently typed" - types are checked during
execution of the code, not ahead of time. Many of these features are
not compiler friendly.

Haskell, by contrast, is a language with principled dynamic
programming features, making it much easier to compile. This means
that not all R constructs and primitives can be readily mapped to
statically generated Haskell code with decent performance.

TODO footnote about cardinality.

Much of the dynamic flavour of R likely stems from the fact that it is
a scripting language. The content of a script is meant to be evaluated
in sequential order in an interactive R prompt. The effect of loading
multiple R scripts at the prompt is in general different depending on
the order of the scripts.

Central to the design of Haskell, by contrast, is the notion of
*separately compilable* units of code, called *modules*. Modules can
be compiled separately and in any order (provided some amount of
metadata about dependencies). Contrary to R scripts, the order in
which modules are loaded into memory is likewise non-deterministic.

There exists, however, an interactive prompt for Haskell, that behaves
much in the same way as R, called GHCi. This prompt reads statements
and expressions from standard input one by one, evaluates them and
prints the values of expressions, just as in R. The value of an
expression, or indeed whether it is well scoped, depends on the
statements that were fed to the interactive prompt before it.

### Dynamic typing

Haskell is also statically typed. However, this apparent mismatch does
not cause any particular problem in practice. This is because the
distinction between "statically typed" languages and "dynamically
typed" languages is [largely artificial][harper-dynamic-static],
stemming from the conflation of two distinct concepts: that of
a *class* and that of a *type*. From the above link:

> We all recognize that it is often very useful to have multiple
> *classes* of values of the same *type*. The prototypical example is
> provided by the complex numbers. There is one *type* of complex
> numbers that represent points in two-dimensional space. In school we
> encounter two *classes* of complex numbers, the rectangular and the
> polar. That is, there are two ways to present a complex number using
> two different systems of coordinates. They are, of course,
> interchangeable, because they represent values of the same *type*.
> But the form of presentation differs, and some forms are more
> convenient than others. [...]
>
> Crucially, the distinction between the two *classes* of complex
> number is *dynamic* in that a given computation may result in
> a number of either class, according to convenience or convention.
> A program may *test* whether a complex number is in polar or
> rectangular form, and we can form data structures such as sets of
> complex numbers in which individual elements can be of either form.

Hence what R calls "types" are better thought of as "classes" in the
above sense. They correspond to *variants* (or *constructors*) of
a single type in the Haskell sense. R is really a unityped language.

We call the type of all the classes that exist in R the *universe*
(See [Internal Structures](#internal-structures)).

Because "class" is already an overloaded term in both R and in
Haskell, in the following we use the term *shape* to refer to what the
above calls a "class".

[harper-dynamic-static]:
http://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/

Internal Structures
===================

A native view of expresions
---------------------------

`HExp` is R's `SEXP` (or `*SEXPREC`) structure represented as
a Haskell datatype. We make `HExp` an instance of the `Storable` type
class to convert to/from R's internal representation as needed.

```Haskell
data HExp
  = Nil						  -- NILSXP
  | Symbol { ... }				  -- SYMSXP
  | Real { ... }				  -- REALSXP
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
hexp :: SEXP -> HExp
```

The fact that this conversion is local is crucial for good performance
of the translated code. It means that conversion happens at each use
site, and happens against values with a statically known shape. Thus
we expect that the view function can usually be inlined, and the
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
sexp :: HExp -> SEXP
```

The universe of values
----------------------

For the purposes of the translation below, we introduce the following
datatype:

```Haskell
data HVal
  = SEXP SEXP
  | Lam    (SEXP -> HVal)
  | Lam1   (HVal -> HVal)
  | ...
  | Lam<ARGMAX> (HVal -> ... -> HVal -> HVal)
```

Where `ARGMAX` is some small number (e.g. 6). The `Lam*` family of
constructors is for functions of known arity. Variadic functions or
functions whose arity exceeds that of the available constructors are
encoded using the `Lam` constructor, whose function expects an
argument pairlist, in the style of `.External` foreign functions.

Translation from R to Haskell
=============================

We express translations in terms of a simplified grammar of
R expressions. This grammar corresponds to [R's internal
representation][R-ints] of all expressions (i.e. `SEXP`), so we do not
attempt to define it explicitly here. However, for readability, we
reuse elements of R's surface syntax to denote `SEXP`'s.

There is only one translation function in H. This translation is often
applied to the value of a quasiquote, so we describe it first.

Naming conventions
------------------

$$
\begin{align*}
   i,j,k &::= \mbox{literals}
\\ x,y,z &::= \mbox{(local) variables}
\\ f,g,h &::= \mbox{(function) variables}
\\ M, N  &::= \mbox{R expressions}
\\ cargs  &::= \mbox{A non simple expression list (e.g. with names and dots)}
\end{align*}
$$

We give translation schemas below, where we use an ellipsis ($\ldots$)
for repetition. But `...` is also a syntactic construct within R. Mind
the difference in font to disambiguate between the two.

`SEXP` construction
-------------------

Given an R expression, represented as a `SEXP`, a quasiquote expands
to the programmatic construction of a SEXP.

Parsing is delegated to a compile-time instance of R. One might wonder
why the string of a quasiquote is not passed to the runtime instance
of R, rather than some compile-time instance. The reason is that doing
so would preclude the ability to do any splicing.

But doing parsing at compile-time means that the AST produced by R's
parser, being allocated in the compile-time instance of R, is not
available at runtime. Therefore, a quasiquote expands to Haskell code
that builds an equivalent AST at runtime, using the view and inverse
view function defined above.

`HVal` translation
------------------

In general R computations have arbitrary side effects. These include
I/O, but also mutation of variables. We cannot just assume
computations to be pure. In Haskell, effectful computations are
modelled using monads. The translation below targets some abstract
monad allowing at least all of the same effects as the `IO` monad. In
practice, we target a concrete monad which we call the `R` monad.

$$
\begin{align*}
\\ \trans x \rho &= \fn{readIORef} \refv x
     &\mbox{if $x \in \rho$}
\\ \trans x \rho &= x
     &\mbox{otherwise}
\\ \trans i \rho &= \fn{return} \fn{SEXP} (\fn{mkSEXP} i)
\\ \trans{\function{x_1, \ldots, x_n} M} \rho &=
     \fn{return} \mathsf{Lam}_n\; (\hsabs{x_1 \;\ldots\; x_n} \kw{do}
     &\hskip{-2em}\mbox{if $n \leq \mathsf{ARG_{max}}$}
\\&\qquad \refv{x_1} \leftarrow \fn{newIORef}{x_1}
\\&\qquad \qquad\vdots
\\&\qquad \refv{x_n} \leftarrow \fn{newIORef}{x_n}
\\&\qquad \trans M (\rho \cup \{x_1,\ldots,x_n\}))
% \\ \trans{\function{cargs} M} &=
%      \fn{return} \fn{Lam} (\hsabs{args} \trans M) & \mbox{where $args$ fresh}
\\ \trans{M(N_1, \ldots, N_n)} \rho &= \kw{do}
     &\hskip{-7em}\mbox{where $\forall i.\;rator,rand_i$ fresh}
\\&\qquad rator \leftarrow \trans M \rho
\\&\qquad rand_1 \leftarrow \trans{N_1} \rho
\\&\qquad \qquad\vdots
\\&\qquad rand_n \leftarrow \trans{N_n} \rho
\\&\qquad \mathsf{apply}_n \; rator \; rand_1 \;\ldots\; rand_n
\end{align*}
$$

Where we have that:

```Haskell
class Literal a where
  mkSEXP :: a -> SEXP

toSEXP :: HVal -> SEXP
toSEXP (SEXP s) = s
toSEXP _ = error "Bad argument."

apply :: HVal -> HVal -> HVal
apply (SEXP s)
      (mkPromise -> p) = [r| s_hs(p_hs) |]
apply (Lam f)  x = f x

apply1 :: HVal -> HVal -> HVal
apply1 (SEXP s)
       (mkPromise -> p) = [r| s_hs(p_hs) |]
apply1 (Lam1 f) x = f x
apply1 (Lam2 f) x = f x R.undefinedValue
...

apply2 :: HVal -> HVal -> HVal -> HVal
apply2 (SEXP s)
       (mkPromise -> p1)
       (mkPromise -> p2) = [r| s_hs(p1_hs, p2_hs) |]
apply2 (Lam1 f) x1 x2 = error "Too many arguments."
apply2 (Lam2 f) x1 x2 = f x1 x2
apply2 (Lam3 f) x1 x2 = f x1 x2 R.undefinedValue
...
```

### `.Internal()` and `.Primitive()` calls

### `.External()` calls


Optimizations
=============

TODO Explain eval/apply optimization.

For all practical purposes, we can typically limit the number of
function constructors to some small number, say 6, and encode all
higher arity functions in terms of functions variable arity.

Note: GHC uses [pointer
tagging](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/PointerTagging)
for certain optimizations. In particular, when a pointer points to
a constructor, the tag bits in the pointer say precisely which
constructor is being pointed to, which avoids having to scrutinize the
info table to get the same information. On 64-bit platforms, pointer
tagging only works for datatypes of 8 constructors or less, so it is
important not to add too many constructors to the universe for the
purposes of the eval/apply optimization.

Pointer tagging is also used to indicate the arity of a function.
Doing so allows to optimize calls to unknown functions when the arity
of the function matches exactly that of the number of actual
arguments. Notice that this optimization is still useful given the
universe above: the various `apply` operators can jump directly into
the code for the functions given as the first argument, without having
to go through the steps of a [generic
apply](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls#Genericapply).

H implementation naming conventions
===================================

[R-ints]: http://cran.r-project.org/doc/manuals/R-ints.html
