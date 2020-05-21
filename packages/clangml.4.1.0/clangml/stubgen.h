#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <stdarg.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdcompat.h>
#include <stdio.h> /* vsnprintf */

static void *
xmalloc(size_t size)
{
    void *ptr = malloc(size);
    if (!ptr) {
        failwith("Virtual memory exhausted");
    }
    return ptr;
}

void
failwith_fmt(const char* format, ...)
{
    va_list argptr;
    ssize_t length = 255;
    char buffer[256];
    va_start(argptr, format);
/*
    ssize_t length;
    length = vsnprintf(NULL, 0, format, argptr);
    if (length < 0) {
      failwith("Unable to measure error format");
    }
    buffer = xmalloc(length + 1);
*/
    if (vsnprintf(buffer, length + 1, format, argptr) < 0) {
      failwith("Unable to format error");
    }
    va_end(argptr);
    failwith(buffer);
}

#define DECLARE_OPAQUE_EX(C_TYPE, OCAML_TYPE, C_OF_OCAML, OCAML_OF_C,   \
    DESTRUCTOR, COMPARE, HASH) \
  struct custom_operations OCAML_TYPE##_ops = {                         \
    (char *) #C_TYPE,                                                   \
    DESTRUCTOR,                                                         \
    COMPARE,                                                            \
    HASH,                                                               \
    custom_serialize_default,                                           \
    custom_deserialize_default,                                         \
    custom_compare_ext_default,                                         \
  };                                                                    \
                                                                        \
  value __attribute__((unused))                                  \
  OCAML_OF_C(C_TYPE v)                                                  \
  {                                                                     \
    CAMLparam0();                                                       \
    CAMLlocal1(ocaml);                                                  \
    ocaml = caml_alloc_custom(                                          \
      &OCAML_TYPE##_ops, sizeof(C_TYPE), 100, 10000);                   \
    *((C_TYPE *) Data_custom_val(ocaml)) = v;                           \
    CAMLreturn(ocaml);                                                  \
  }                                                                     \
                                                                        \
  C_TYPE __attribute__((unused))                                 \
  C_OF_OCAML(value ocaml)                                               \
  {                                                                     \
    CAMLparam1(ocaml);                                                  \
    CAMLreturnT(C_TYPE, *((C_TYPE *) Data_custom_val(ocaml)));          \
  }

#define DECLARE_OPAQUE(C_TYPE, OCAML_TYPE, C_OF_OCAML, OCAML_OF_C, DESTRUCTOR) \
  DECLARE_OPAQUE_EX(C_TYPE, OCAML_TYPE, C_OF_OCAML, OCAML_OF_C, DESTRUCTOR, \
    custom_compare_default, custom_hash_default)

#define Not_bool_val(X) (!Bool_val(X))

#define Val_not_bool(X) (Val_bool(!(X)))

static inline value
 __attribute__((unused))
safe_field(value V, mlsize_t I)
{
  if (Is_block(V) && I < Wosize_val(V)) {
    return Field(V, I);
  }
  else {
    return Val_int(0);
  }
}

static inline const char *
 __attribute__((unused))
safe_string(const char *s)
{
  if (s == NULL) {
    return "";
  }
  return s;
}

static value
 __attribute__((unused))
Val_option(value x)
{
  CAMLparam1(x);
  value tgt = caml_alloc(1, 0);
  Store_field(tgt, 0, x);
  CAMLreturn(tgt);
}

static value
 __attribute__((unused))
Val_string_option(const char *s)
{
  CAMLparam0();
  if (s == NULL) {
    return Val_int(0);
  }
  CAMLreturn(Val_option(caml_copy_string(s)));
}
