/**
 * This header files contains definitions from
 * src/include/Defn.h of the R source distribution.
 *
 * These are the definitions used by H.
 *
 * */
#include <stdint.h>


#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_visible __attribute__ ((visibility ("default")))
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_visible
# define attribute_hidden
#endif

#ifdef __MAIN__
# define extern0 attribute_hidden
#else
# define extern0 extern
#endif



#include <R_ext/libextern.h>

#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 attribute_hidden
#else
# define INI_as(v)
#define extern0 extern
#endif



extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
extern0 int	R_CStackDir	INI_as(1);	/* C stack direction */
