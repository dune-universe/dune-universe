#ifndef CHECKSEUM_PTRDIFF_T
#define CHECKSEUM_PTRDIFF_T

#if defined(_STDDEF_H)
#  include <stddef.h>
#elif defined(WIN32)
#  include <CRTDEFS.H>
#else
typedef long ptrdiff_t;
/* XXX(dinosaure): I guess... */
#endif

#endif
