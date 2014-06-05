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

#ifdef H_ARCH_UNIX
#include <R_ext/eventloop.h>

void processGUIEventsUnix(InputHandler** inputHandlers) {
  if (*inputHandlers == NULL)
      initStdinHandler();
  R_runHandlers(*inputHandlers, R_checkActivityEx(1000, 0, NULL));
}
#endif

// Initializing isRInitialized to 0 here causes GHCi to fail with
// a linking error in Windows x64. But initializing to 2 poses no problem!
int isRInitialized = 2;

HsStablePtr rVariables;
HsStablePtr interpreterChan;

#undef USE_RINTERNALS


// The following macros and definitions are used to initialize R with
// stack limit checking disabled.

// NOTE: On Windows, R does not ship with Rinterface.h or Defn.h, so we
// declare them ourselves.
#ifdef H_ARCH_WINDOWS
extern uintptr_t R_CStackLimit;	/* C stack limit */
extern uintptr_t R_CStackStart;	/* Initial stack address */
#else
#define CSTACK_DEFNS
#include <Rinterface.h>
#endif

#include <Rembedded.h>

// NOTE: It is crucial to set the two R stack globals after calling
// Rf_initialize_R() and before calling setup_Rmainloop().
// H_initUnlimitedEmbeddedR() is based on Rf_initEmbeddedR(), found in
// R 3.1.0 source code at src/gnuwin32/embeddedR.c:127.
int H_initUnlimitedEmbeddedR(int argc, char **argv)
{
    Rf_initialize_R(argc, argv);
    R_CStackLimit = -1;
    R_CStackStart = -1;
    setup_Rmainloop();
    return(1);
}
