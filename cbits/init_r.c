// Copyright: (C) 2013-2014 Amgen, Inc.

// NOTE: On Windows, R does not ship with Rinterface.h.
#ifdef H_ARCH_WINDOWS
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
typedef unsigned long uintptr_t;
#endif
extern uintptr_t R_CStackLimit;	/* C stack limit */
extern uintptr_t R_CStackStart;	/* Initial stack address */
#else
#define CSTACK_DEFNS
#include <Rinterface.h>
#endif

#include <Rembedded.h>

// NOTE: H_initUnlimitedEmbeddedR is based on Rf_initEmbeddedR(), found in
// R 3.1.0 source code at src/gnuwin32/embeddedR.c:127.
int H_initUnlimitedEmbeddedR(int argc, char **argv)
{
    Rf_initialize_R(argc, argv);
    R_CStackLimit = -1;
    R_CStackStart = -1;
    setup_Rmainloop();
    return(1);
}
