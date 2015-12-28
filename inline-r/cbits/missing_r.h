// Copyright: (C) 2013 Amgen, Inc.

#ifndef MISSING_R__H
#define MISSING_R__H

#include "HsFFI.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP funPtrToSEXP(DL_FUNC pf);

/* Indicates whether R has been initialized. */
extern int isRInitialized;

/* R global variables for GHCi. */
extern HsStablePtr rVariables;

#endif
