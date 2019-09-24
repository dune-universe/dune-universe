#ifndef CHECKSEUM_SIZE_T
#define CHECKSEUM_SIZE_T

#if defined(NO_SIZE_T)
typedef unsigned NO_SIZE_T size_t;
#elif defined(_STDDEF_H)
#  include <stddef.h>
#elif defined(WIN32)
#  include <BaseTsd.h> /* XXX(dinosaure): see checkseum#7 */
typedef SIZE_T size_t;
#else
typedef unsigned long size_t;
#endif

#endif
