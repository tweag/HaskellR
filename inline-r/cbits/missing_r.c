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


// List the prototypes of functions and variables that inline-r
// uses from R. The purpose of this is to catch changes in the
// C interface when upgrading R.
#include <Rembedded.h>
int Rf_initEmbeddedR(int, char**);
void Rf_endEmbeddedR(int);

#ifndef mingw32_HOST_OS
#include <R_ext/eventloop.h>
static void test_R_PolledEvents(){ void (*a)(void) = R_PolledEvents; };
static void test_R_wait_usec(){ int *a = &R_wait_usec; };

static void test_R_InputHandlers(){ InputHandler *a = R_InputHandlers; };
fd_set *R_checkActivity(int usec, int ignore_stdin);
void R_runHandlers(InputHandler *handlers, fd_set *mask);
InputHandler *addInputHandler(InputHandler *handlers, int fd, InputHandlerProc handler, int activity);
int removeInputHandler(InputHandler **handlers, InputHandler *it);

#include <Rinterface.h>
static void test_R_Interactive(){
    Rboolean *a = &R_Interactive;
    int i=0;
    *a=i;
};
static void test_R_SignalHandlers(){ int *a = &R_SignalHandlers; };
#endif

int TYPEOF(SEXP x);
static void test_R_NilValue(){ SEXP *a = &R_NilValue; };
static void test_R_UnboundValue(){ SEXP *a = &R_UnboundValue; };
static void test_R_MissingArg(){ SEXP *a = &R_MissingArg; };
static void test_R_BaseEnv(){ SEXP *a = &R_BaseEnv; };
static void test_R_EmptyEnv(){ SEXP *a = &R_EmptyEnv; };
static void test_R_GlobalEnv(){ SEXP *a = &R_GlobalEnv; };

#include <R_ext/GraphicsEngine.h>
static void test_R_interrupts_pending(){ int *a = &R_interrupts_pending; };
int OBJECT(SEXP x);
int NAMED(SEXP x);
int LEVELS(SEXP x);
int RDEBUG(SEXP x);
SEXP ATTRIB(SEXP x);
void SET_ATTRIB(SEXP, SEXP);
SEXP Rf_getAttrib(SEXP, SEXP);
Rboolean Rf_isS4(SEXP x);

#include <R_ext/Parse.h>
static void test_ParseStatus() { ParseStatus a = (int)0; };
SEXP R_ParseVector(SEXP, int, ParseStatus*, SEXP);

// These variables are not in header files!
extern void (*Rg_PolledEvents)(void);
static void test_Rg_PolledEvents(){ void (*a)(void) = Rg_PolledEvents; };
extern int Rg_wait_usec;
static void test_Rg_wait_usec(){ int *a = &Rg_wait_usec; };
extern int R_PPStackTop;
static void test_R_PPStackTop(){ int *a = &R_PPStackTop; };
