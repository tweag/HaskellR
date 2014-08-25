\documentclass[preprint,authoryear]{sigplanconf}

\usepackage{amsmath}

\include{definitions}

%include lambda.fmt
%include polycode.fmt
%subst conid a = "\mathsf{" a "}"
%subst varid a = "\mathsf{" a "}"
% UGLY HACK: we abuse string literals to denote quasiquotes.
%subst string txt = "\llbracket r|\texttt{\;" txt "\;}\rrbracket"

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

The complexity of modern software environments makes it all but
essential to interoperate software components implemented in different
programming languages. Most high-level programming languages today
include a {\em foreign function interface (FFI)}, which allows
interfacing with lower-level programming languages to get access to
existing system and/or purpose-specific libraries (TODO refs). An FFI
allows the programmer to give enough information to the compiler of
the host language to figure out how to {\em invoke} a foreign function
included as part of a foreign library, and how to {\em marshal}
arguments to the function in a form that the foreign function expects.
This information is typically given as a set of bindings, one for each
function, as in the example below:
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
%format cid = "\textit{cid}"
%format ts = "\textit{ts}"
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

These bindings are tedious and error prone to write, verbose, hard to
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
R provides (named arguments, variadic functions, {\em etc.}). R has
its own equivalent to \verb|clock_gettime()|, called
\verb|Sys.time()|. With an embedding of R in this fashion, calling it
is as simple as:
%format GREETING = "\texttt{\char34 The current time is:\;\char34}"
\begin{code}
main = do
    now <- "Sys.time()"
    putStrLn (GREETING ++ fromSEXP now)
\end{code}

\section{Overall architecture}

\section{Special topics}

\subsection{The hexp view function}

\subsection{Types for R}

\subsection{R values are (usually) vectors}

\subsection{Memory management}

\section{Examples}

\section{Benchmarks}

\section{Related Work}

\section{Conclusion}

\acks

\bibliographystyle{abbrvnat}
\bibliography{references}

\end{document}
