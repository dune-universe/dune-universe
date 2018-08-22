
#ifndef __ORDMA_DEBUG_H
#define __ORDMA_DEBUG_H

//#define ORDMA_DEBUG 1

#if ORDMA_DEBUG

#include <stdio.h>
#include <sys/time.h>

/*
static inline double ordma_timestamp() {
  struct timeval tp;
  double r = (double) tp.tv_sec + (double) tp.tv_usec / 1e6;
  return r;
}
*/

#define ORDMA_STREAM stderr 
#define ORDMA_LOG(...) {                            \
    /*fprintf(ORDMA_STREAM, "%f ", ordma_timestamp());*/        \
    fprintf(ORDMA_STREAM, __VA_ARGS__);             \
    fputc('\n', ORDMA_STREAM);                      \
    fflush(ORDMA_STREAM);                           \
  }
#else
#define ORDMA_LOG(...)
#endif

#endif /* __ORDMA_DEBUG_H */
