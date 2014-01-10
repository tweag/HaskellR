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

R source code is organized as a set of *scripts*, which are loaded one
by one into the R interpreter. Each statement in a each script is
evaluated in order and affect the global environment mapping symbols
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
which modules are loaded into memory is likewise non-deterministic.

For this reason, in keeping to a simple solution to interoperating
with R, we choose to devolve as much processing of R code to an
embedded instance of the R interpreter and retain the notion of global
environment that R provides. This global environment can readily be
manipulated from an interactive environment such as GHCi. In compiled
modules, access to the environment can be mediated as well as
encapsulation of any effects can be mediated through a custom monad,
which we call the `R` monad.

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
Haskell, in the following we use the term *form* to refer to what the
above calls a "class".

[harper-dynamic-static]:
http://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/

Internal Structures
===================

A native view of expresions
---------------------------

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
hexp :: SEXP -> HExp
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
a real number can be given the type `SEXP R.Real`. In general, one
does not always know *a priori* the form of an R expression, but
pattern matching on an algebraic view of the expression allows us to
"discover" the form at runtime. In H, we define the `HExp` algebraic
view type as a GADT. In this way, the body of each branch can be typed
under the assumption that the scrutinee matches the pattern in the
left hand side of the branch. For example, in the body of a branch
with pattern `Real x`, the type checker can refine the type of the
scrutinee to `SEXP R.Real`. In H, `HExp` is defined as follows:

```Haskell
data HExp (a :: SEXPTYPE) where
  Nil       :: HExp R.Nil
  -- Fields: pname, value, internal.
  Symbol    :: SEXP R.Char
            -> SEXP a
            -> Maybe (SEXP b)
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
have kind `SEXPTYPE`. Version 7.6 of GHC and later feature the
`DataKinds` extension to permit defining `SEXPTYPE` as a regular
algebraic datatype and then allowing `SEXPTYPE` to be considered as
a kind and the constructors of this type to be considered types of the
`SEXPTYPE` kind, depending on context. See the GHC user's guide for
more information.

Implementation of quasiquoters
==============================

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

Threads in H
============

When H is used in GHCi the following threads materialize:

 The R thread
  : The only thread that can execute calls of the R API.

 The GHCi thread
  : A thread that reads commands from the GHCi prompt.

 The GHCi command threads
  : Ephemeral GHCi threads, with each being spawned to evaluate one
    GHCi command.

 The GUI timer thread
  : A thread that periodically produces requests to handle GUI events
    in the R thread.

When the user types a command in GHCi, the GHCi thread spawns a GHCi
command thread that evaluates the command. This command may require in
turn calls to the R API which will be posted to the R thread. Usually
the GHCi command thread would block until the R thread completes
execution of one or more calls and sends back a reply.

The R thread receives requests through a channel. Each request is
represented as an action of type `IO ()`. If the request is supposed
to produce a reply, the sender will block while waiting for the reply.

The interface to the R thread:

    postToRThread :: IO ()    -> IO ()  -- Non-blocking
    runInRThread  :: IO a     -> IO a   -- Blocking
    startRThread  :: ThreadId -> IO ()  -- Takes the id of the GUI timer thread.
    stopRThread   :: IO ()              -- Blocking

If the action of a request throws an exception, the R thread would
continue to evaluate subsequent requests.

Memory allocation
=================

H allocates memory in two separate heaps, managed by two separate
garbage collectors: the R garbage collector and the one that is part
of the GHC runtime. Either heap can in general contain pointers to
values in the other heap, yet neither garbage collector is aware of
what is going on in the heap that it does not manage. As a result, one
must be very careful when allocating memory to ensure that one GC does
not cut the grass under the feet of the other, e.g. by deallocating
a value pointed to from the other heap. H provides a wide list of
features and mechanisms for safe and easy memory management.

Description of garbage collection systems
-----------------------------------------

### Haskell garbage collection system

Haskell has a concurrent, stop-the-world, copying generational GC.
This means that all evaluation will be stopped while GC is performed.
And Haskell data structures can`t be used transparently in foreign
code, since the GC may move them. However, Haskell does support pinned
memory, which is not relocatable.

Another option for using Haskell structures in foreign runtime is
through the use of the
[StablePtr](http://hackage.haskell.org/package/base-4.6.0.1/docs/Foreign-StablePtr.html).
A stable pointer is a reference to a Haskell expression that is
guaranteed not to be affected by garbage collection, i.e., it will
neither be deallocated nor will the value of the stable pointer itself
change during GC (ordinary references may be relocated during garbage
collection).

To check objects being used, the Haskell GC traverses objects starting
from the top level (root) objects and relocates usable objects, so no
additional protection is required. For protection of foreign memory
Haskell uses Foreign.Ptr that contains a finalizer callback that is
invoked when the object is no longer used in Haskell.

### R garbage collection system.

R uses a single threaded, stop-the-world, non copying generational GC.

Newly created structures in C that are not yet connected to the object
graph can be protected from the GC using the following mechanisms:

  * `PROTECT`, and `PROTECT_PTR` macros for protection
  * `REPROTECT` for protection of the updated object
  * `UNPROTECT` and `UNPROTECT_PTR` for removing protection.

Every object that is reachable from a protected variable is also
protected.

Basic strategy of H
-------------------

To keep objects safe from garbage collection, H implements the
following strategy. Every object that may be reachable from R is
allocated in the R heap so that R can perform deallocation and
protection of values. For the programmer, H provides additional
functionality that can help to keep values safe.

### Low level functions

Low level functions may be used inside the IO or R monad to protect
variables in H.

  * `protect` -- synonym for PROTECT macros

  * `unprotect` -- synonym for UNPROTECT_PTR macros

  * `withProtected` -- bracket function that protects variable and
      unprotects it at the end. Usage example:

    ```haskell
    H.eval =<< withProtected (mkString "H.Home") R.lang1
    ```

    Here String SEXP "H.Home" is protected until it will be a part of
    LangSEXP, created by `R.lang1`. Then `R.lang1` becomes
    unprotected, which is fine as it immediately enters evaluation
    where it can't be freed until end of the execution. If resulting
    SEXP doesn't enter evaluation you'll need to protect it in the
    toplevel (see [Toplevel expressions](#toplevel-expressions)).

### Allocating SEXP

SEXP allocation can be done in the following ways:

  1. Using the low-level `alloc*` functions. Such variables are not
  protected.

  2. With `unhexp`. See [SEXP introspection and
  construction](#sexp-introspectio-and-construction). Such variables
  require explicit protection.

  3. Using quasi-quotation. Such variables are automatically
  protected, since the quasiquoter generates code that uses
  `withProtected`, so only toplevel expression should be protected.

### Function parameters

Most parameter types cannot cross memory boundaries or are explicitly
copied when they are passed to and from functions. However it possible
to coerce some types with zero-copying:

  * `SEXP a` -> `SEXP b`
  * `SEXP (Vector Real)` -> Vector.Storable Double
  * `SEXP (Vector Int)`  -> Vector.Storable Int
  * `SEXP (Vector Logical)` -> Vector.Storable Bool
  * `SEXP (Vector Char)`    -> Vector.Storable Word8
  * `SEXP (Vector Char)`    -> Vector.Storable ByteString

### Toplevel expressions

Toplevel R expressions should be protected by hiding them in `RVal`

```haskell
newtype RVal a = RVal { unRVal :: ForeignPtr (SEXP a) }
```

With following API:

  * `newRVal` - create new wrapper
  * `withRVal` - run continuation with rvalue.
  * `peekRVal` - noop function that may be used to
        protect from early collection of protection variable.

Toplevel haskell expressions are protected by using `ExtPtr` in R. For
example this is done automatically for function wrappers that are used
internally to call haskell code.

To protect Haskell variables from the GC, a global registry is used,
which is a global mutable variable that maintains a list of variables
that are used by R code.

[R-ints]: http://cran.r-project.org/doc/manuals/R-ints.html
