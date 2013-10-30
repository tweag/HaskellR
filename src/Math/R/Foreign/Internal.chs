-- | 
-- Module: Math.R.Foreign.Internal
-- Copyright: (C) 2013, Amgen, Inc.
--
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#include <R.h>
#include <Rinternals.h>
-- | TODO: group functions as in header
-- | TODO: add some way of typechecking
module Math.R.Foreign.Internal
  ( SEXPTYPE(..)
  , SEXP
  ) where

import Foreign
import Foreign.C

#c
typedef enum SEXPTYPE{
   NilXSP = NILSXP,               /* 0 - nil = NULL */
   SymSXP  = SYMSXP,              /* 1 - symbols */
   //   LISTSXP = 2,              /* lists of dotted pairs */
   //   CLOSXP  = 3,              /* closures */
   //   ENVSXP  = 4,              /* environments */
   //   PROMSXP = 5,              /* promises: [un]evaluated closure arguments */
   LangSXP = LANGSXP,             /* 6 - language constructs (special lists) */
   //   SPECIALSXP  = 7,          /* special forms */
   //   BUILTINSXP  = 8,          /* builtin non-special forms */
   //   CHARSXP = 9,              /* "scalar" string type (internal only)*/
   //   LGLSXP  = 10,             /* logical vectors */
   IntSXP  = INTSXP,              /* 11 - integer vectors */
   //   REALSXP = 14,             /* real variables */
   //   CPLXSXP = 15,             /* complex variables */
   //   STRSXP  = 16,             /* string vectors */
   //   DOTSXP  = 17,             /* dot-dot-dot object */
   //   ANYSXP  = 18,             /* make "any" args work */
   //   VECSXP  = 19,             /* generic vectors */
   ExpSXP = EXPRSXP,              /* 20 - expressions vectors */
   //   BCODESXP  = 21,           /* byte code */
   //   EXTPTRSXP = 22,           /* external pointer */
   //   WEAKREFSXP  = 23,         /* weak reference */
   //   RAWSXP  = 24,             /* raw bytes */
   //   S4SXP = 25,               /* S4 non-vector */
   //   NEWSXP      = 30,         /* fresh node creaed in new page */
   //   FREESXP     = 31,         /* node released by GC */
   FunSXP  = FUNSXP               /* 32 - Closure or Builtin */
};
#endc

-- | SEXP type representation
{# enum SEXPTYPE {} deriving (Eq,Show) #}
{# pointer *SEXPREC as SEXP #}
{# pointer *Rboolean as Rboolean #}




