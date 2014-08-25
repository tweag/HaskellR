\documentclass[preprint,authoryear]{sigplanconf}

\usepackage{amsmath}

\include{definitions}

%include lambda.fmt
%include polycode.fmt
%subst conid a = "\mathsf{" a "}"
%subst varid a = "\mathsf{" a "}"
%format a = "\mathit{a}"
%format b = "\mathit{b}"
%format s = "\mathit{s}"
%format x = "\mathit{x}"
%format y = "\mathit{y}"
% UGLY HACK: we abuse string literals to denote quasiquotes.
%subst string txt = "\llbracket \mathsf{r}|\texttt{\;" txt "\;}\rrbracket"

\begin{document}

\conferenceinfo{IFL'14}{October 1--3, 2014, Boston, MA, USA}
\copyrightyear{2014}

\exclusivelicense

\title{Project H: Programming R in Haskell}
\subtitle{(Extended Abstract)}

\authorinfo{Name1}
           {Affiliation1}
           {Email1}
\authorinfo{Name2\and Name3}
           {Affiliation2/3}
           {Email2/3}

\maketitle

\begin{abstract}
  A standard method for augmenting the ``native'' set of libraries
  available within any given programming environment is to extend this
  set via a foreign function interface provided by the programming
  language. In this way, by exporting the functionality of external
  libraries via {\em binding modules}, one is able to reuse libraries
  without having to reimplement them in the language {\em du jour}.

  However, {\em a priori} bindings of entire system libraries is
  a tedious process that quickly creates an unbearable maintenance
  burden. We demonstrate an alternative to monolithic and imposing
  binding modules, be it to make use of libraries implemented in
  a special-purpose, dynamically typed, interpreted language. As
  a case study, we present \SysH, an R-to-Haskell interoperability
  solution making it possible to program all of R, including all
  library packages on CRAN, from Haskell, a general-purpose,
  statically typed, compiled language. We demonstrate how to do so
  efficiently, without marshalling costs when crossing language
  boundaries and with static guarantees of well-formation of
  expressions and safe acquisition of foreign language resources.
\end{abstract}

%\category{CR-number}{subcategory}{third-level}

\keywords R, Haskell, foreign function interface, quasiquotation,
language embedding, memory regions

\section{Introduction}
  
The success or failure in the industry of a programming language
within a particular problem domain is often predicated upon the
availability of a sufficiently plethoric set of good quality libraries
relevant to the domain. Libraries enable code reuse, which ultimately
leads to shorter development cycles. Yet business and regulatory
constraints may impose orthogonal requirements that not all
programming languages are able to satisfy.

Case in point: at Amgen, we operate within a stringent regulatory
environment that requires us to establish high confidence as to the
correctness of the software that we create. In life sciences, it is
crucial that we aggressively minimize the risk that any bug in our
code, which could lead to numerical, logical or modelling errors with
tragic consequences, goes undetected.

TODO more about the needs of Amgen and why it uses Haskell.

TODO more about the needs of Amgen and why it needs R.

%% R is a free software environment for statistical computing and
%% graphics. It includes a full blown programming language for
%% exploratory programming and statistics.

We present a method to make available any foreign library without the
overheads typically associated with more traditional approaches. Our
goal is to allow for the seamless integration of R with Haskell ---
invoking R functions on Haskell data and {\em vice versa}.

\paragraph{Foreign Function Interfaces} The complexity of modern
software environments makes it all but essential to interoperate
software components implemented in different programming languages.
Most high-level programming languages today include a {\em foreign
  function interface (FFI)}, which allows interfacing with lower-level
programming languages to get access to existing system and/or
purpose-specific libraries (TODO refs). An FFI allows the programmer
to give enough information to the compiler of the host language to
figure out how to {\em invoke} a foreign function included as part of
a foreign library, and how to {\em marshal} arguments to the function
in a form that the foreign function expects. This information is
typically given as a set of bindings, one for each function, as in the
example below:
%% newtype ClockId = ClockId Int32
%%
%% instance Storable TimeSpec where
%%   sizeOf _ = 12
%%   alignment _ = 4
%%   peek ptr = do
%%       ss <- peekByteOff ptr 0
%%       ns <- peekByteOff ptr 8
%%       return $ TimeSpec ss ns
%%   poke ptr (TimeSpec ss ns) = do
%%       pokeByteOff ptr 0 ss
%%       pokeByteOff ptr 0 ns

%format INCLUDE_TIME_H = "\texttt{\#include <time.h>}"
%format CLOCK_GETTIME = "\char34" clock_gettime "\char34"
%format GETTIME = "\char34" getTime "\char34"
%format cid = "\mathit{cid}"
%format ts = "\mathit{ts}"
%format foreign = "\mathbf{foreign}"
%format ccall = "\mathbf{ccall}"
\begin{code}
{-# LANGUAGE ForeignFunctionInterface #-}
module Example1 (getTime) where
import Foreign
import Foreign.C

INCLUDE_TIME_H

data TimeSpec = TimeSpec
  { seconds      :: Int64
  , nanoseconds  :: Int32
  }

foreign import ccall CLOCK_GETTIME
  c_clock_gettime :: ClockId -> Ptr TimeSpec -> IO CInt

getTime :: ClockId -> IO TimeSpec
getTime cid = alloca $ \ts -> do
    throwErrnoIfMinus1_ GETTIME $
      c_clock_gettime cid ts
    peek ts
\end{code}
In the above, |c_clock_gettime| is a binding to the
\verb|clock_gettime()| C function. The API conventions of C functions
are often quite different from that of the host language, so that it
is convenient to export the wrapper function |getTime| rather than the
binding directly. The wrapper function takes care of converting from
C representations of arguments to values of user defined data types
(performed by the |peek| function, not shown), as well as mapping any
foreign language error condition to a host language exception.

\paragraph{Binding generators} These bindings are tedious and error prone to write, verbose, hard to
read and a pain to maintain as the API of the underlying library
shifts over time. To ease the pain, over the years, {\em binding
  generators} have appeared (TODO ref), in the form of pre-processors
that can parse C header files and automate the construction of binding
wrapper functions and argument marshalling. However, these tools:
\begin{enumerate}
\item do not alleviate the programmer from the need to repeat in the
  host language the type of the foreign function;
\item add yet more complexity to the compilation pipeline;
\item being textual pre-processors, generate code that is hard to
  debug;
\item are necessarily limited in terms of the fragments of the source
  language they understand and the types they can handle, or repeat
  the complexity of the compiler to parse the source code.
\end{enumerate}
Point (1) above is particularly problematic, because function
signatures in many foreign libraries have a knack for evolving over
time, meaning that bindings invariably lag behind the upstream foreign
libraries in terms of both the versions they support, and the number
of functions they bind to.

Moreover, such binding generators are language specific, since they
rely on intimate knowledge of the foreign language in which the
foreign functions are available. In our case, the foreign language is
R, which none of the existing binding generators support. We would
have to implement our own binding generator to alleviate some of the
burden of working with an FFI. But even with such a tool in hand, the
tedium of writing bindings for all standard library functions of R,
let alone all functions in all CRAN packages, is but a mildly exciting
prospect. One would need to define a monolithic set of bindings
(i.e.\ a {\em binding module}), for {\em each} R package. Because we
cannot anticipate exactly which functions a user will need, we would
have little recourse but to make these bindings as exhaustive as
possible.

Rather than {\em bind} all of R, the alternative is to {\em embed} all
of R. Noting that GHC flavoured Haskell is a capable meta-programming
environment, the idea is to define code generators which, at each call
site, generates code to invoke the right R function and pass arguments
to it using the calling convention that it expects. In this way, there
is no need for {\em a priori} bindings to all functions. Instead, it
is the code generator that produces code spelling out to the compiler
exactly how to perform the R function call -- no binding necessary.

It just so happens that the source language for these code generators
is R itself. In this way, users of H may express invocation of an
R function using the full set of syntactical conveniences that
R provides (named arguments, variadic functions, {\em etc.}), or
indeed write arbitrary R expressions. R has its own equivalent to
\verb|clock_gettime()|, called \verb|Sys.time()|. With an embedding of
R in this fashion, calling it is as simple as:
%format GREETING = "\texttt{\char34 The current time is:\;\char34}"
%format now = "\mathit{now}"
\begin{code}
printCurrentTime = do
    now <- "Sys.time()"
    putStrLn (GREETING ++ fromSEXP now)
\end{code}
The key syntactical device here is {\em quasiquotes} (TODO ref), which
allow mixing code fragments with different syntax in the same source
file --- anything within an |"..."| pair of brackets is to be
understood as R syntax.

\paragraph{Contributions} In this paper, we advocate for a novel
approach to programming with foreign libraries, and illustrate this
approach with the first complete, high-performance tool to access all
of R from a statically typed, compiled language. We highlight the
difficulties of mixing and matching two garbage collected languages
that know nothing about each other, and how to solve them by bringing
together existing techniques in the literature for safe memory
management (TODO ref). Finally, we show how to allow optionally
ascribing precise types to R functions, as a form of compiler-checked
documentation and to offer better safety guarantees.

\paragraph{Outline} The paper is organized as follows. We will first
walk through typical uses of H, before presenting its overall
architecture (Section \ref{sec:architecture}). We delve into a number
of special topics in later sections, covering how to represent foreign
values efficiently in a way that still allows for pattern matching
(Section \ref{sec:hexp}), optional static typing of dynamically typed
foreign values (Section \ref{sec:types}), creating R values from
Haskell (Section \ref{sec:vectors}) and efficient memory management in
the presence of two separately managed heaps with objects pointing to
arbitrary other objects in either heaps (Section \ref{sec:regions}).
We conclude with a discussion of the overheads of cross language
communication (Section \ref{sec:benchmarks}) and an overview of
related work (Section \ref{sec:related-work}).

\section{Overall architecture}
\label{sec:architecture}

\subsection{Interoperating scripting languages}

R source code is organized as a set of {\em scripts}, which are loaded
one by one into the R interpreter. Each statement in a each script is
evaluated in-order and affects the global environment maintained by
the R interpreter, which maps symbols to values. In its simplest form,
H is an interactive environment much like R, with a global environment
altered by the in-order evaluation of statements.

The central and most general mechanism by which H allows
interoperating with R is quasiquotation. A {\em quasiquote} is
a partial R script --- that is, a script with holes in it that stand
in for as of yet undetermined portions. An example quasiquote in
Haskell of an R snippet is:
\begin{code}
"function(x) x + 1"
\end{code}
This quasiquote is {\em ground}, in that it does not contain any holes
(called {\em antiquotes}), unlike the below quasiquote:
\begin{code}
let y = mkSEXP 1
in "function(x) x + y_hs"
\end{code}
By convention, any symbol with a @_hs@ suffix is treated specially. It
is interpreted as a reference to a Haskell variable defined somewhere
in the ambient source code. Given any quasiquote, it is possible to
obtain a full R script, with no holes in it, by {\em splicing} the
value of the Haskell variables into the quasiquote, in place of the
antiquotes.

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
|Foreign.R.*| namespace, and a top-half building higher-level
functionality upon the bottom-half, using the |Language.R.*|
namespace.

\subsection{Rationale}

\paragraph{Rebinding and dynamic code construction}

R is a very dynamic language, allowing many code modifications during
runtime, such as rebinding of top-level definitions, super assignment
(modifying bindings in parent scopes), (quasi-)quotation and
evaluation of expressions constructed dynamically. The R programming
language is also so-called "latently typed" - types are checked during
execution of the code, not ahead of time. Many of these features are
not compiler friendly.

Haskell, by contrast, is designed to be much easier to compile. This
means that not all R constructs and primitives can be readily mapped
to statically generated Haskell code with decent performance. In
particular, top-level definitions in Haskell are never dynamically
rebound at runtime: a known function call is hence often a direct
jump, rather than incurring a dynamic lookup in a symbol table (the
environment).

Much of the dynamic flavour of R likely stems from the fact that it is
a scripting language. The content of a script is meant to be evaluated
in sequential order in an interactive R prompt. The effect of loading
multiple R scripts at the prompt is in general different depending on
the order of the scripts.

Central to the design of Haskell, by contrast, is the notion of
separately compilable units of code, called {\em modules}. Modules can
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
which we call the |R| monad.

\paragraph{Dynamic typing}

Haskell is also statically typed. However, this apparent mismatch does
not cause any particular problem in practice. This is because the
distinction between "statically typed" languages and "dynamically
typed" languages is largely artificial, stemming from the conflation
of two distinct concepts: that of a *class* and that of a *type* (TODO
ref Harper's book).

The prototypical example of a type with multiple classes of values is
that of complex numbers. There is {\em one} type of complex numbers,
but {\em two} (interchangeable) classes of complex numbers: those in
rectangular coordinates and those in polar coordinates. Both classes
represent values of the same type. Harper further points out:

\begin{quotation}
  Crucially, the distinction between the two classes of complex number
  is dynamic in that a given computation may result in a number of
  either class, according to convenience or convention. A program may
  test whether a complex number is in polar or rectangular form, and
  we can form data structures such as sets of complex numbers in which
  individual elements can be of either form.
\end{quotation}

Hence what R calls "types" are better thought of as "classes" in the
above sense. They correspond to *variants* (or *constructors*) of
a single type in the Haskell sense. R is really a unityped language.

We call the type of all the classes that exist in R the *universe*
(See Section~\ref{sec:hexp}).

Because "class" is already an overloaded term in both R and in
Haskell, in the following we use the term *form* to refer to what the
above calls a "class".

\section{Special topics}

\subsection{A native view of foreign values}
\label{sec:hexp}

By default, and in order to avoid having to pay marshalling and
unmarshalling costs for each argument every time one invokes an
internal R function, we represent R values in exactly the same way
R does, as a pointer to a |SEXPREC| structure (defined in
@R/Rinternals.h@). This choice has a downside, however: Haskell's
pattern matching facilities are not immediately available, since only
algebraic datatypes can be pattern matched.

|HExp| is R's |SEXP| (or @*SEXPREC@) structure represented as
a (generalized) algebraic datatype. A simplified definition of |HExp|
would go along the lines of:
\begin{code}
  data HExp
  = Nil                                           -- NILSXP
  | Symbol { ... }                                -- SYMSXP
  | Real { ... }                                  -- REALSXP
  | ...
\end{code}
We define one constructor for each value of the |SEXPTYPE| enumeration
in @<RInternals.h>@.

For the sake of efficiency, we do not use |HExp| as the basic datatype
that all H generated code expects. That is, we do not use |HExp| as
the universe of R expressions, merely as a {\em view}. We introduce
the following {\em view function} to locally convert to a |HExp|,
given a |SEXP| from R.
\begin{code}
hexp :: SEXP s -> HExp
\end{code}
The fact that this conversion is local is crucial for good performance
of the translated code. It means that conversion happens at each use
site, and happens against values with a statically known form. Thus we
expect that the view function can usually be inlined, and the
short-lived |HExp| values that it creates compiled away by code
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
\begin{code}
unhexp :: HExp -> SEXP
\end{code}

\subsection{Types for R}
\label{sec:types}

\subsection{R values are (usually) vectors}
\label{sec:vectors}

TODO

\subsection{Memory management}
\label{sec:regions}

One tricky aspect of bridging two languages with automatic memory
management such as R and Haskell is that we must be careful that the
garbage collectors (GC) of both languages see eye-to-eye. The embedded
R instance manages objects in its own heap, separate from the heap
that the GHC runtime manages. However, objects from one heap can
reference objects in the other heap and the other way around. This can
make garbage collection unsafe because neither GC's have a global view
of the object graph, only a partial view corresponding to the objects
in the heaps of each GC.

\subsubsection{Memory protection}

Fortunately, R provides a mechanism to "protect" objects from garbage
collection until they are unprotected. We can use this mechanism to
prevent R's GC from deallocating objects that are still referenced by
at least one object in the Haskell heap.

One particular difficulty with protection is that one must not forget
to unprotect objects that have been protected, in order to avoid
memory leaks. H uses "regions" for pinning an object in memory and
guaranteeing unprotection when the control flow exits a region.

\subsubsection{Memory regions}

There is currently one global region for R values, but in the future
H will have support for multiple (nested) regions. A region is opened
with the |runRegion| action, which creates a new region and executes
the given action in the scope of that region. All allocation of
R values during the course of the execution of the given action will
happen within this new region. All such values will remain protected
(i.e. pinned in memory) within the region. Once the action returns,
all allocated R values are marked as deallocatable garbage all at
once.

\begin{code}
runRegion :: (forall s . R s a) -> IO a
\end{code}

\subsubsection{Automatic memory management}

Nested regions work well as a memory management discipline for simple
scenarios when the lifetime of an object can easily be made to fit
within nested scopes. For more complex scenarios, it is often much
easier to let memory be managed completely automatically, though at
the cost of some memory overhead and performance penalty. H provides
a mechanism to attach finalizers to R values. This mechanism
piggybacks Haskell's GC to notify R's GC when it is safe to deallocate
a value.
\begin{code}
automatic :: MonadR m => R.SEXP s a -> m (R.SEXP G a)
\end{code}
In this way, values may be deallocated far earlier than reaching the
end of a region: As soon as Haskell's GC recognizes a value to no
longer be reachable, and if the R GC agrees, the value is prone to be
deallocated. Because automatic values have a lifetime independent of
the scope of the current region, they are tagged with the global
region |G| (a type synonym for |GlobalRegion|).

For example:
\begin{code}
do  x <- "1:1000"
    y <- "2"
    return $ automatic "x_hs * y_hs"
\end{code}
Automatic values can be mixed freely with other values.

\section{Benchmarks}
\label{sec:benchmarks}

\section{Related Work}
\label{sec:related-work}

\section{Conclusion}
\label{sec:conclusion}

%\acks

\bibliographystyle{abbrvnat}
\bibliography{references}

\end{document}
