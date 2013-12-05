// Copyright: (C) 2013 Amgen, Inc.
#define USE_RINTERNALS
#include "missing_r.h"
#include <R.h>

SEXP * INNER_VECTOR(SEXP x) {
    return VECTOR_PTR(x);
}

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


SEXP funPtrToSEXP(DL_FUNC pf) {
    return Rf_MakeNativeSymbolRef(pf);
};

#ifdef H_ARCH_UNIX
#include <R_ext/eventloop.h>

void processGUIEventsUnix(InputHandler** inputHandlers) {
  if (*inputHandlers == NULL)
      initStdinHandler();
  R_runHandlers(*inputHandlers, R_checkActivityEx(1000, 0, NULL));
}
#endif

// Initializing isRInitialized to 0 here causes GHCi to fail with
// a linking error in Windows x64. Therefore, initialization is done
// in H.ghci.
int isRInitialized;

HsStablePtr rVariables;
HsStablePtr interpreterChan;

#undef USE_RINTERNALS
