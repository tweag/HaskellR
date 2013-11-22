#include <R.h>

#define USE_RINTERNALS
#include <Rinternals.h>

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

    return R_MakeExternalPtrFn(f, install("native symbol"), R_NilValue);
}


SEXP funPtrToSEXP(DL_FUNC pf) {
    return Rf_MakeNativeSymbolRef(pf);
};

#undef USE_RINTERNALS
