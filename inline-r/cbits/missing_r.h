// Copyright: (C) 2013 Amgen, Inc.

#ifndef MISSING_R__H
#define MISSING_R__H

#include "HsFFI.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define GCGEN(x) ((x)->sxpinfo.gcgen)
#define GCCLS(x) ((x)->sxpinfo.gccls)
#define SET_GCGEN(x, v) (((x)->sxpinfo.gcgen)=(v))
#define SET_GCCLS(x, v) (((x)->sxpinfo.gccls)=(v))
#define SET_MARK(x, v) (((x)->sxpinfo.mark)=(v))


/* Create a variadic R function given any function pointer. */
SEXP funPtrToSEXP(DL_FUNC pf);

/* Indicates whether R has been initialized. */
extern int isRInitialized;

/* R global variables for GHCi. */
extern HsStablePtr rVariables;

#endif
