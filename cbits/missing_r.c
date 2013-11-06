#include <R.h>

#define USE_RINTERNALS
#include <Rinternals.h>

SEXP * INNER_VECTOR(SEXP x) {
    return VECTOR_PTR(x);
}
#undef USE_RINTERNALS
