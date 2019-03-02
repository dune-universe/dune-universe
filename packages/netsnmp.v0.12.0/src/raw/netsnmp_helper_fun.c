#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

void oom_error(void)
{
  caml_raise_constant(*caml_named_value("Netsnmp_out_of_memory"));
}

void netsnmp_raise_ocaml_exception(const char *exn, value msg)
{
  caml_raise_with_arg(*caml_named_value(exn), msg);
}

void netsnmp_raise_ocaml_exception_system_error(const char *msg)
{
  CAMLparam0();
  CAMLlocal1(exn_msg);
  int msglen = strlen(msg);
  int buflen = 256;
  char *buf = (char *)malloc(buflen + msglen + 1);

  if (buf == NULL) oom_error();
  strncpy(buf, msg, buflen + msglen); *(buf + msglen + buflen) = '\0';

  while (strerror_r(errno, buf + msglen, buflen) == -1 && errno == ERANGE)
  {
    buflen *= 2;
    buf = (char *)realloc(buf, buflen + msglen + 1);
    if (buf == NULL) oom_error();
  }

  exn_msg = caml_copy_string(buf);  free(buf);
  netsnmp_raise_ocaml_exception("Netsnmp_error_system", exn_msg);
}
