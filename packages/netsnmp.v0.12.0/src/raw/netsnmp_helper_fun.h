#ifndef __NETSNMP_HELPER_FUN_H
#define __NETSNMP_HELPER_FUN_H
#include <caml/mlvalues.h>

#define p_to_ml_value(p,mlv) \
  do { mlv = caml_alloc_string(sizeof (void *)); \
  memmove(Bytes_val(mlv), (char *)&p, sizeof (void *)); } while(0)

#define ml_value_to_p(mlv,p) \
  do { memmove((char *)&p, String_val(mlv), sizeof (void *)); } while(0)

void oom_error(void);
void netsnmp_raise_ocaml_exception(const char *exn, value msg);
void netsnmp_raise_ocaml_exception_system_error(const char *msg);
#endif /* __NETSNMP_HELPER_FUN_H */
