// Copyright: (C) 2013 Amgen, Inc.

#define USE_RINTERNALS

#include "missing_r.h"
#include <R.h>
#include <R_ext/Rdynload.h>

// Not declared in public headers but exposed in the R library.
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);

/**
 * Taken from src/main/Rdynload.c, and then commented the code
 * because it would cause a crash when trying to set the class of
 * the SEXP.
 * */
static inline SEXP Rf_MakeNativeSymbolRef(DL_FUNC f)
{
    //SEXP ref, klass;
    //
    //PROTECT(ref = R_MakeExternalPtrFn(f, install("native symbol"), R_NilValue));
    //PROTECT(klass = mkString("NativeSymbol"));
    //setAttrib(ref, R_ClassSymbol, klass);
    //UNPROTECT(2);

    // TODO: instead of passing NULL, pass R_NilValue but workaround linking bug in ghci.
    return R_MakeExternalPtrFn(f, install("native symbol"), NULL);
}

void freeHsSEXP(SEXP extPtr) {
    hs_free_fun_ptr(R_ExternalPtrAddr(extPtr));
}

SEXP funPtrToSEXP(DL_FUNC pf) {
    SEXP value;
    PROTECT(value = Rf_MakeNativeSymbolRef(pf));
    R_RegisterCFinalizerEx(value, freeHsSEXP, 1);
    UNPROTECT(1);
    return value;
};

// Initializing isRInitialized to 0 here causes GHCi to fail with
// a linking error in Windows x64. But initializing to 2 poses no problem!
int isRInitialized = 2;

HsStablePtr rVariables;

#undef USE_RINTERNALS
