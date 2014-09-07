\documentclass[preprint,authoryear]{sigplanconf}

\usepackage[utf8]{inputenc}
\usepackage{amsmath}
\usepackage{graphicx}

\include{definitions}

%include lambda.fmt
%include polycode.fmt
%options ghci -fglasgow-exts

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
\subtitle{PRELIMINARY DRAFT}

\authorinfo{Mathieu Boespflug \and Facundo Domínguez \and Alexander Vershilov}
           {Tweag I/O}
           {}
%           {@{m,facundo.dominguez,alexander.vershilov}@@tweag.io@}
\authorinfo{Allen Brown}
           {Amgen}
           {}
%           {@allbrown@@amgen.com@}

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

%% TODO more about the needs of Amgen and why it uses Haskell.
%%
%% TODO more about the needs of Amgen and why it needs R.

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
purpose-specific libraries \cite{chakravarty:haskell-ffi,milner:sml}.
An FFI allows the programmer to give enough information to the
compiler of the host language to figure out how to {\em invoke}
a foreign function included as part of a foreign library, and how to
{\em marshal} arguments to the function in a form that the foreign
function expects. This information is typically given as a set of
bindings, one for each function, as in the example below:
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
  generators} have appeared \cite{chakravarty:c2hs}, in the form of
pre-processors that can parse C header files and automate the
construction of binding wrapper functions and argument marshalling.
However, these tools:
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
The key syntactical device here is {\em quasiquotes}
\cite{mainland:quasiquotes}, which allow mixing code fragments with
different syntax in the same source file --- anything within an
|"..."| pair of brackets is to be understood as R syntax.

\paragraph{Contributions} In this paper, we advocate for a novel
approach to programming with foreign libraries, and illustrate this
approach with the first complete, high-performance tool to access all
of R from a statically typed, compiled language. We highlight the
difficulties of mixing and matching two garbage collected languages
that know nothing about each other, and how to solve them by bringing
together existing techniques in the literature for safe memory
management \cite{kiselyov:regions}. Finally, we show how to allow
optionally ascribing precise types to R functions, as a form of
compiler-checked documentation and to offer better safety guarantees.

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

\subsection{Foreign values}
\label{sec:foreign-values}

Foreign functions act on {\em values}, for which presumably these
foreign functions know the representation in order to compute with
them. In the specific case of R, {\em all} values share a common
structure. Internally, R represents every entity that it manipulates,
be they scalars, vectors, uninterpreted term expressions, functions or
external resources such as sockets, as pointers to a @SEXPREC@
structure, defined in C as follows:
\begin{verbatim}
typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
        struct primsxp_struct primsxp;
        struct symsxp_struct symsxp;
        struct listsxp_struct listsxp;
        struct envsxp_struct envsxp;
        struct closxp_struct closxp;
        struct promsxp_struct promsxp;
    } u;
} SEXPREC, *SEXP;
\end{verbatim}
Each variant of the union struct corresponds to a different form of
value. However, no matter the form, all values at least share the same
header (called @SEXPREC_HEADER@). The type of pointers to @SEXPREC@s
is abbreviated as @SEXP@. In order to invoke functions defined in
R then, we simply need a way to construct the right @SEXPREC@
representing that invocation, and then have R interpret that
invocation. We will cover how to do so in the next sections, but for
now we do need to define in Haskell what a @SEXP@ is:
\begin{code}
data SEXPREC
type SEXP = Ptr SEXPREC
\end{code}

\subsection{Interoperating scripting languages}

R source code is organized as a set of {\em scripts}, which are loaded
one by one into the R interpreter. Each statement in a each script is
evaluated in-order and affects the global environment maintained by
the R interpreter, which maps symbols to values. In its simplest form,
H is an {\em interactive environment} much like R, with a global
environment altered by the in-order evaluation of statements.

The central and most general mechanism by which H allows
interoperating with R is quasiquotation. A {\em quasiquote} is
a partial R script --- that is, a script with holes in it that stand
in for as of yet undetermined portions. An example quasiquote in
Haskell of an R snippet is:
\begin{code}
"function(x) x + 1"
\end{code}
This quasiquote is {\em ground}, in that it does not contain any holes
(called {\em antiquotes}), but one can also antiquote inside
a quasiquote:
\begin{code}
let y = mkSEXP 1
in "function(x) x + y_hs"
\end{code}
By convention, any symbol with a @_hs@ suffix is treated specially. It
is interpreted as a reference to a Haskell variable defined somewhere
in the ambient source code. That is, any occurrence of a symbol of the
form @x_hs@ does denote a variable of the object language --- it is an
antiquote referring to variable |x| in the host language. Given any
quasiquote, it is possible to obtain a full R script, with no holes in
it, by {\em splicing} the value of the Haskell variables into the
quasiquote, in place of the antiquotes.

At a high-level, H is a desugarer for quasiquotes implemented on top
of a Haskell interactive environment, such as GHCi \cite{ghc}. It
defines how to translate a quasiquotation into a Haskell expression.
Just as R includes an interactive environment, H includes an
interactive environment, where the input is a sequence of Haskell
expressions including quasiquoted R code snippets, such as in the
following session, where we plot part of the quadratic function,
directly from the Haskell interactive prompt:
%format H_PROMPT = "\mathtt{H\rangle}\;"
%format x__2 = "x^2"
%format xs = "\mathit{xs}"
%format ys = "\mathit{ys}"
%format R_OUTPUT1 = "\texttt{[1] \char34 1\char34\; \char34 4\char34\; \char34 9\char34\; \char34 16\char34\; \char34 25\char34\; \char34 36\char34\; \char34 49\char34\; \char34 64\char34\;\ldots}"
%format R_OUTPUT2 = "\langle \textrm{graphic output (See Figure~\ref{fig:quadratic})} \rangle"
\begin{code}
H_PROMPT let xs = [1..10] :: [Double]
H_PROMPT let ys = [ x__2 | x <- xs ]
H_PROMPT result <- "as.character(ys_hs)"
H_PROMPT H.print result
R_OUTPUT1
H_PROMPT "plot(xs_hs, ys_hs)"
R_OUTPUT2
\end{code}

\begin{figure}
  \centering
  \includegraphics[width=5.5cm]{r-quadratic.pdf}
  \caption{Output of |"plot(xs_hs, ys_hs)"|. The data is generated from Haskell.
R draws the plot.}

  \label{fig:quadratic}
\end{figure}

Now say that we are given a set of random points, roughly fitted by
some non-linear model. For the sake of example, we can use points
generated at random along non-linear curve by the following Haskell
function:
\begin{code}
import System.Random.MWC
import System.Random.MWC.Distributions

generate :: Int32 -> IO Double
generate ix =
  withSystemRandom . asGenIO $ \gen ->
    let r =  (x-10)*(x-20)*(x-40)*(x-70)
             + 28*x*(log x)
    in do v <- standard gen
    return $ r * (1 + 0.15 * v)
    where x = fromIntegral ix
\end{code}
As before, take a set of coordinates:
\begin{code}
H_PROMPT "xs <- c(1:100)"
H_PROMPT "ys <- mapply(generate_hsfun, xs)"
\end{code}
@generate_hsfun@ is a {\em function splice} --- just like any other
splice, except that the spliced value is higher-order, i.e.
a function. R's @mapply()@ applies the Haskell function to each
element of @xs@, yielding the list @ys@.

Our goal is to ask R to compute estimates of the parameters of
polynomial models of increasing degree model, with models of higher
degree having a higher chance of fitting our dataset well. The
R standard library provides the @nls()@ function to compute the
non-linear least-squares estimate of the parameters of the model. For
example, we can try to fit a model expressing the relation between
@ys@ to @xs@ as a polynomial of degree 3:
\begin{code}
H_PROMPT "P3 <- ys ~ a3*xs**3 + a2*xs**2 + a1*xs + a0"
H_PROMPT "initialPoints <- list(a0=1,a1=1,a2=1,a3=1)"
H_PROMPT "model3 <- nls(P3, start=initialPoints)"
\end{code}
As the degree of the model increases, the residual sum-of-squares
decreases, to the point where in the end we can find a polynomial that
fits the dataset rather well, as depicted in Figure~\ref{fig:nls},
produced with the following code:
\begin{code}
"plot(xs,ys)"
"lines(xs,predict(model2), col = 2)"
"lines(xs,predict(model3), col = 3)"
"lines(xs,predict(model4), col = 4)"
\end{code}

\begin{figure}
  \centering
  \includegraphics[width=6cm]{r-nls.pdf}
  \caption{Fitting polynomial models of increasing degree ($n =
    \{2,3,4\}$) to a set of points in Haskell. R fits the models.}

  \label{fig:nls}
\end{figure}

\subsection{Scripting from compiled modules}

While an interactive prompt is extremely useful for exploratory
programming, writing a program as a sequence of inputs for a prompt is
a very imperative style of programming with limited abstraction
facilities. Fortuntaly, H is also a library. Importing the library
brings in scope the necessary definitions in order to embed
quasiquotes such as the above in modules of a compiled program.

Behind the scenes, the H library nurtures an {\em embedded instance}
of the R interpreter, available at runtime. As in the interactive
environment, this embedded instance is stateful. It is possible to
mutate the global environment maintained by the interpreter, say by
introducing a new top-level definition. Therefore, interaction with
this embedded instance must be sequential. In order to enforce
sequential access to the interpreter, we introduce the |R| monad and
make all code that ultimately calls into the interpreter {\em actions}
of the R monad. As a first approximation, the |R| monad is a simple
wrapper around the |IO| monad (but see
Section~\ref{sec:regions})\footnote{This definition does not guarantee
  that |R| actions will only be executed when the associated
  R interpreter instance is extant. See Section~\ref{sec:regions} for
  a fix.}.
\begin{code}
newtype R a = R (IO a)

withEmbeddedR :: R a -> IO a
\end{code}
|withEmbeddedR| first spawns an embedded instance, runs the provided
action, then finalizes the embedded instance. There is no other way to
run the |R| monad.

%% The library itself is structured as two layers: a bottom-half binding
%% to low-level internal functions of the R interpreter, using the
%% |Foreign.R.*| namespace, and a top-half building higher-level
%% functionality upon the bottom-half, using the |Language.R.*|
%% namespace.

\subsection{Rebinding and dynamic code construction}

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
readily be manipulated from an interactive environment such as GHCi
\cite{ghc}. In compiled modules, access to the environment as well as
encapsulation of any effects can be mediated through a custom monad,
which we call the |R| monad.

\section{Special topics}

\subsection{A native view of foreign values}
\label{sec:hexp}

\begin{figure}
\begin{description}
\item[NILSXP]
  There is only one object of type @NILSXP@, @R_NilValue@, with no data.

\item[SYMSXP] Pointers to the @PRINTNAME@ (a @CHARSXP@), @SYMVALUE@
  and @INTERNAL@. (If the symbol’s value is a @.Internal@ function,
  the last is a pointer to the appropriate @SEXPREC@.) Many symbols
  have the symbol value set to @R_UnboundValue@.

\item[LISTSXP] Pointers to the @CAR@, @CDR@ (usually a @LISTSXP@ or
  @NILSXP@) and @TAG@ (a @SYMSXP@ or @NILSXP@).

\item[CHARSXP] @LENGTH@, @TRUELENGTH@ followed by a block of bytes
  (allowing for the nul terminator).

\item[REALSXP] @LENGTH@, @TRUELENGTH@ followed by a block of
  C doubles.

\item[\ldots]
\end{description}
  \caption{Extract from the R documentation enumerating all the
    different forms that values can take.}
\label{fig:r-type-desc}
\end{figure}

Programming across two languages typically involves a tradeoff: one
can try shipping off an entire dataset and invoking a foreign function
that does all the processing in one go, or keep as much of the logic
in the host language and only call into foreign functions punctually.
For example, mapping an R function @frobnicate()@ over a list of
elements might be done entirely in R, on the whole list at once,
\begin{code}
H_PROMPT ys <- "mapply(frobnicate, xs_hs)"
\end{code}
or elementwise, driven from Haskell,
\begin{code}
H_PROMPT ys <- mapM (\x -> "frobnicate(x_hs)") xs
\end{code}
The latter style is often desirable --- the more code can be kept in
Haskell the safer, because more code can be type checked statically.

The bane of language interoperability is the perceived cost of
crossing the border between one language to another during execution.
Any significant overhead incurred in passing arguments to a foreign
function and transferring control to it discourages tightly integrated
programs where foreign functions are called frequently, such as in the
last line above. Much of this cost is due to marshalling values from
the native representation of data, to the foreign representation, and
back.

By default, and in order to avoid having to pay marshalling and
unmarshalling costs for each argument every time one invokes an
internal R function, we represent R values in exactly the same way
R does, as a pointer to a |SEXPREC| structure (defined in
@R/Rinternals.h@). This choice has a downside, however: Haskell's
pattern matching facilities are not immediately available, since only
algebraic datatypes can be pattern matched.

|HExp| is R's |SEXP| (or @*SEXPREC@) structure represented as
a (generalized) algebraic datatype. Each @SEXPREC@ comes with
a ``type'' tag the uniquely identifies the layout (one of
@primsxp_struct@, @symsxp_struct@, {\em etc.}~as seen in
Section~\ref{sec:foreign-values}). See Figure~\ref{fig:r-type-desc}
for an excerpt of the R documentation enumerating all possible type
tags\footnote{In R 3.1.0, there are 23 possible tags.}. A simplified
definition of |HExp| would go along the lines of
Figure~\ref{fig:untyped-hexp}. Notice that for each tag in
Figure~\ref{fig:r-type-desc}, there is a corresponding constructor in
Figure~\ref{fig:untyped-hexp}.

\begin{figure}
\begin{code}
data HExp
  = Nil                                           -- NILSXP
  | Symbol SEXP SEXP SEXP                         -- SYMSXP
  | List SEXP SEXP SEXP                           -- LISTSXP
  | Char Int32 (Vector Word8)                     -- CHARSXP
  | Real Int32 (Vector Double)                    -- REALSXP
  | ...
\end{code}
\caption{Untyped |HExp| view.}
\label{fig:untyped-hexp}
\end{figure}
For the sake of efficiency, we do not use |HExp| as the basic datatype
that all H generated code expects. That is, we do not use |HExp| as
the universe of R expressions, merely as a {\em view}. We introduce
the following {\em view function} to locally convert to a |HExp|,
given a |SEXP| from R.
\begin{code}
  hexp :: SEXP -> HExp
\end{code}
The fact that this conversion is local is crucial for good performance
of the translated code. It means that conversion happens at each use
site, and happens against values with a statically known form. Thus we
expect that the view function can usually be inlined, and the
short-lived |HExp| values that it creates is compiled away by code
simplification rules applied by GHC. Notice how |HExp| as defined in
Figure~\ref{fig:untyped-hexp} is a {\em shallow view} --- the fields
of each constructor are untranslated |SEXP|'s, not |HExp|'s. In other
words, a |HExp| value corresponds to the one-level unfolding of
a |SEXP| as an algebraic datatype. The fact that |HExp| is not
a recursive datatype is crucial for performance. It means that the
|hexp| view function can be defined non-recursively, and hence is
a candidate for inlining\footnote{The GHC optimizer never inlines
  recursive functions.}.

In this manner, we get the convenience of pattern matching that comes
with a {\em bona fide} algebraic datatype, but without paying the
penalty of allocating long-lived data structures that need to be
converted to and from R internals every time we invoke internal
R functions or C extension functions.

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

\subsection{``Types'' for R}
\label{sec:types}

\subsubsection{Of types, classes or forms}

Haskell is a statically typed language, whereas R is a dynamically
typed language. However, this apparent mismatch does not cause any
particular problem in practice. This is because the distinction
between "statically typed" languages and "dynamically typed" languages
is largely artificial, stemming from the conflation of two distinct
concepts: that of a {\em class} and that of a {\em type}
\cite{harper:pfpl}.

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
above sense. They correspond to {\em variants} (or {\em constructors})
of a single type in the Haskell sense. R is really a unityped
language.

We call the type of all the classes that exist in R the {\em universe}
(See Section~\ref{sec:hexp}). Each variant of the union field in the
@SEXPREC@ structure defined in Section~\ref{sec:foreign-values}
corresponds to a class in the above sense. The @SEXPREC@ structure
{\em is} the universe.

Because "class" is already an overloaded term in both R and in
Haskell, in the following we use the term {\em form} to refer to what
the above calls a "class".

Some R functions expect a large number of arguments. It is not always
clear what the usage of those functions is. It is all too easy to pass
a value of the wrong form as an argument, or provide too many
arguments, or too little. R itself cannot detect such conditions until
runtime, nor is it practical to create a static analysis for R to
detect them earlier, given the permissive semantics of the language.
However, some information about the expected forms for arguments is
given in R's documentation for practically every function in the
standard library. It is often useful to encode that information from
the documentation in machine checkable form, in such a way that the
Haskell compiler can bring to bear its own existing static analyses to
check for mismatches between formal parameters and actual arguments.

\subsubsection{Form indexed values}

To this end, in H we {\em form index} |SEXP|s. The actual definition
of a |SEXP| in H is:
\begin{code}
newtype SEXP s a = SEXP (PTR SEXPREC)
\end{code}
The |a| parameter refers to the form of a |SEXP| (See
Section~\ref{sec:regions} for the meaning of the |s| type parameter).
In this way, a |SEXP| of form @REALSXP@ (meaning a vector of reals),
can be ascribed the type |SEXP s R.Real|, distinct from |SEXP
s R.Closure|, the type of closures. When some given functionis used
frequently throughout the code, it is sometimes useful to introduce
a wrapper for it in Haskell, ascribing to it a particular type. For
example, the function that parses source files can be written as:
\begin{code}
import qualified Foreign.R.Type as R

parse  ::  SEXP s R.String       -- Filename of source
       ->  SEXP s R.Int          -- Number of expressions to parse
       ->  SEXP s R.String       -- Source text
       ->  R s (SEXP s R.Expr)
parse file n txt = "parse(file_hs, n_hs, txt_hs)"
\end{code}
Now that we have a Haskell function for parsing R source files, with
a Haskell type signature, the Haskell compiler can check that all
calls to @parse()@ are well-formed. We found this feature immensely
useful to document in the source code itself how to call various
R functions, without having to constantly look up this information in
the R documentation.

Of course, while form indexing |SEXP| can in practice be a useful
enough surrogate for a real type system, it does not replace a real
type system. A reasonable property for any adequate type system is
{\em type preservation}, also called {\em subject reduction}. That is,
we ought to have that:
\[
\mbox{If }\Gamma \vdash M : T \mbox{ and } M \Downarrow
V \mbox{ then } \Gamma \vdash V : T
\]
where $M$ is an expression, $T$ is a type and $V$ is the value of $M$.
The crude form indexing presented here does not enjoy this property.
In particular, given some arbitrary expression, in general the form of
the value of this expression is unknown. We have the following type of
|SEXP|'s of unknown form:
\begin{code}
data SomeSEXP s = forall a. SomeSEXP (SEXP s a)
\end{code}
Because the form of a value is in general unknown, the type of |eval|
is:
\begin{code}
eval :: SEXP s a -> R s (SomeSEXP s)
\end{code}
That is, for any |SEXP| of any form |a|, the result is a |SEXP| of
some (unknown) form.

\subsubsection{Casts and coercions}

|SEXP|'s of unknown form aren't terribly useful. For example, they
cannot be passed as-is to the successor function on integers, defined
as:
\begin{code}
succ :: SEXP s Int -> R s SomeSEXP
succ x = "x_hs + 1"
\end{code}
Therefore, H provides {\em casting functions}, which introduce
a dynamic form check. The user is allowed to {\em coerce} the type in
Haskell of a |SEXP| given that the dynamic check passes. |cast| is
defined as:
\begin{code}
cast :: SSEXPTYPE a -> SomeSEXP s -> SEXP s a
cast ty s = unsafeCast (fromSing ty) s
\end{code}
where
%format DYNAMIC_CAST_FAILED = "\texttt{\char34 cast: Dynamic type cast failed.\char34}"
\begin{code}
cast :: SSEXPTYPE a -> SomeSEXP s -> SEXP s a
cast ty s
    | fromSing ty == R.typeOf s = unsafeCoerce s
    | otherwise = error DYNAMIC_CAST_FAILED
\end{code}
Now, |"1 + 1"| stands for the {\em value} of the R expression ``@1 +
1@''. That is,
\begin{code}
two = "1 + 1" :: SomeSEXP s
\end{code}
In order to compute the successor of |two|, we need to cast the result:
\begin{code}
three :: R s SomeSEXP
three = succ (two `cast` R.Int)
\end{code}

\subsection{R values are (usually) vectors}
\label{sec:vectors}

An idiosyncratic feature of R is that scalars and vectors are treated
uniformly, and in fact {\em represented} uniformly. This means that
provided an interface to manipulate vectors alone, we can handle all
scalars as well as all vectors. H exports a library of vector
manipulation routines, that mirrors the API of the standard @vector@
package. The advantage of keeping data represented as R vectors
throughout a program is that no marshalling or unmarshalling need be
incurred when passing the data to an R function. Because we provide
the exact same API as for any other (non-R) vector representations, it
is just as easy to manipulate R vectors instead, throughout.

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

We implemented benchmarks to compute recursively Fibonacci numbers,
and the sum of all integers stored in the nodes of binary trees.

An implementation in R of these functions is compared against implementations in
Haskell using {\em views} to inspect both binary trees and Fibonacci indexes
produced with R. The source code can be found at \cite{Hrepo?}.

\begin{center}
\begin{tabular}{ ||l||r||r|| }
\hline
& {\bf R} & {\bf views} \\
\hline
{\tt binarytrees} & 1130 ms & 687 ms \\
\hline
{\tt fib}         &    8 ms & 307 ms \\
\hline
\end{tabular}
\end{center}

When computing the sum over binary trees, we can see that the Haskell program
using {\em views} is faster than the R program. This is due to Haskell being
a compiled language, while the R function computing the sum is interepreted.

\section{Related Work}
\label{sec:related-work}

\section{Conclusion}
\label{sec:conclusion}

%\acks

\bibliographystyle{abbrvnat}
\bibliography{references}

\end{document}
