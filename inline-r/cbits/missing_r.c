// Copyright: (C) 2013 Amgen, Inc.

#include "missing_r.h"
#include <R.h>
#include <R_ext/Rdynload.h>

static void freeHsSEXP(SEXP extPtr)
{
	hs_free_fun_ptr(R_ExternalPtrAddr(extPtr));
}

SEXP funPtrToSEXP(DL_FUNC pf)
{
	static SEXP callsym, functionsym, nativesym;
	if(!callsym) callsym = install(".Call");
	if(!functionsym) functionsym = install("function");
	if(!nativesym) nativesym = install("native symbol");
	SEXP value, formals;

	PROTECT(value = R_MakeExternalPtr(pf, nativesym, R_NilValue));
	R_RegisterCFinalizerEx(value, freeHsSEXP, TRUE);
	PROTECT(value = lang3(callsym, value, R_DotsSymbol));
	PROTECT(formals = CONS(R_MissingArg, R_NilValue));
	SET_TAG(formals, R_DotsSymbol);
	PROTECT(value = lang4(functionsym, formals, value, R_NilValue));
	UNPROTECT(4);
	return value;
}

// XXX Initializing isRInitialized to 0 here causes GHCi to fail with
// a linking error in Windows x64. But initializing to 2 poses no
// problem!
int isRInitialized = 2;

HsStablePtr rVariables;
