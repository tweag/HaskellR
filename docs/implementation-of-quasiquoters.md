---
id: implementation-of-quasiquoters
---
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
