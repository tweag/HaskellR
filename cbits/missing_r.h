// Copyright: (C) 2013 Amgen, Inc.
#ifndef MISSING_R__H
#define MISSING_R__H

#include "HsFFI.h"
#include <Rinternals.h>

SEXP * INNER_VECTOR(SEXP);

#include <R_ext/Rdynload.h>

SEXP funPtrToSEXP(DL_FUNC pf);

#ifdef H_ARCH_UNIX
#include <R_ext/eventloop.h>
void processGUIEventsUnix(InputHandler** inputHandlers);
#endif

// Indicates whether R has been initialized.
extern int isRInitialized;

// R global variables for GHCi.
extern HsStablePtr rVariables;

// Pointer to the channel used for communication with the R thread.
extern HsStablePtr interpreterChan;

#endif
