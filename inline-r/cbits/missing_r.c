// Copyright: (C) 2013 Amgen, Inc.

#include "missing_r.h"
#include <R.h>
#include <R_ext/Rdynload.h>

void freeHsSEXP(SEXP extPtr) {
    hs_free_fun_ptr(R_ExternalPtrAddr(extPtr));
}

SEXP funPtrToSEXP(DL_FUNC pf) {
    SEXP value;
    PROTECT(value = R_MakeExternalPtr(pf, install("native symbol"), R_NilValue));
    R_RegisterCFinalizerEx(value, freeHsSEXP, TRUE);
    UNPROTECT(1);
    return value;
}

// Initializing isRInitialized to 0 here causes GHCi to fail with
// a linking error in Windows x64. But initializing to 2 poses no problem!
int isRInitialized = 2;

HsStablePtr rVariables;
