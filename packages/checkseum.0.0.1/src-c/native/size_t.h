#ifndef CHECKSEUM_SIZE_T
#define CHECKSEUM_SIZE_T

#if defined(NO_SIZE_T)
typedef unsigned NO_SIZE_T size_t;
#elif defined(STDC)
#  include <stddef.h>
#else
typedef unsigned long size_t;
#endif

#endif
