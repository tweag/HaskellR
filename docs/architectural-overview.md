---
id: architectural-overview
---

Architectural overview
======================

R source code is organized as a set of *scripts*, which are loaded one
by one into the R interpreter. Each statement in a each script is
evaluated in-order and affect the global environment mapping symbols
to values maintained by the R interpreter. In its simplest form, H is
an interactive environment much like R, with a global environment
altered by the in-order evaluation of statements.

H offers a number of facilities for interoperating with R. The central
and most general mechanism by which this is done is /quasiquotation/.
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

At a high-level, H is a desugarer for quasiquotes. It defines how to
translate a quasiquotation into a Haskell expression. Hence the
H interactive environment is an interpreter for sequences of
quasiquotes, containing R code snippets, and other Haskell
snippets.

H is structured as a library. The H interactive environment is
a simple launcher for GHCi that loads the H library into the session
and sets a number of parameters.

The library itself is structured as two layers: a bottom-half binding
to low-level internal functions of the R interpreter, using the
`Foreign.R.*` namespace, and a top-half building higher-level
functionality upon the bottom-half, using the `Language.R.*`
namespace.

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

Much of the dynamic flavour of R likely stems from the fact that it is
a scripting language. The content of a script is meant to be evaluated
in sequential order in an interactive R prompt. The effect of loading
multiple R scripts at the prompt is in general different depending on
the order of the scripts.

Central to the design of Haskell, by contrast, is the notion of
*separately compilable* units of code, called *modules*. Modules can
be compiled separately and in any order (provided some amount of
metadata about dependencies). Contrary to R scripts, the order in
which modules are loaded into memory is non-deterministic.

For this reason, in keeping to a simple solution to interoperating
with R, we choose to devolve as much processing of R code as possible
to an embedded instance of the R interpreter and retain the notion of
global environment that R provides. This global environment can
readily be manipulated from an interactive environment such as GHCi.
In compiled modules, access to the environment as well as
encapsulation of any effects can be mediated through a custom monad,
which we call the `R` monad.

### Dynamic typing

Haskell is also statically typed. However, this apparent mismatch does
not cause any particular problem in practice. This is because the
distinction between "statically typed" languages and "dynamically
typed" languages is [largely artificial](http://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/),
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
(See [Internal Structures](/H/docs/internal-structures.html)).

Because "class" is already an overloaded term in both R and in
Haskell, in the following we use the term *form* to refer to what the
above calls a "class".

