#ifndef MISSING_R__H
#define MISSING_R__H

SEXP * INNER_VECTOR(SEXP);

#include <R_ext/Rdynload.h>

SEXP funPtrToSEXP(DL_FUNC pf);

#endif
