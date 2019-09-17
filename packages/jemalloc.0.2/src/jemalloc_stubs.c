#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>

#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <jemalloc/jemalloc.h>

static void invalid_property(value message)
{
  CAMLparam1(message);
  static value* exn = NULL;
  if (NULL == exn)
  {
    exn = caml_named_value("Jemalloc_ctl.Invalid_property");
    assert(NULL != exn);
  }
  caml_raise_with_arg(*exn, message);
  CAMLnoreturn;
}

#define boolean_val(foo) Bool_val(foo)
#define Val_boolean(foo) Val_bool(foo)
#define int_val(foo) Int_val(foo)
#define string_val(foo) String_val(foo)

#define make_mallctl_stub(type, ctype, get_len, val_type) \
CAMLprim value ml_je_mallctl_##type(value name, value new_value)\
{\
   CAMLparam2(name, new_value);\
   size_t old_length = sizeof(ctype);\
   ctype old_data;\
   int ret;\
   if (Is_block(new_value)) {\
           ctype data = type##_val(Field(new_value, 0));\
           size_t length = get_len(data);\
           ret = mallctl(String_val(name), (void *)&old_data, &old_length, (void *)&data, length);\
   } else {\
           ret = mallctl(String_val(name), (void *)&old_data, &old_length, NULL, 0);\
   }\
   if (ret != 0) invalid_property(name); \
   CAMLreturn(val_type(old_data));\
}

#define make_mallctl_simple(type, ctype) make_mallctl_stub(type, ctype, sizeof, Val_##type)

make_mallctl_simple(boolean, bool)
make_mallctl_simple(int, size_t)
make_mallctl_stub(string, const char *, strlen, caml_copy_string)

CAMLprim value ml_je_all_arena()
{
  CAMLparam0();
#ifdef MALLCTL_ARENAS_ALL
  CAMLreturn(Val_int(MALLCTL_ARENAS_ALL));
#else
  size_t size = sizeof(unsigned);
  unsigned narenas;
  int ret = mallctl("arenas.narenas", &narenas, &size, NULL, 0);
  if (ret != 0) invalid_property(caml_copy_string("arenas.narenas"));
  CAMLreturn(Val_int(narenas));
#endif
}

CAMLprim value ml_je_mallctl_unit(value name)
{
   CAMLparam1(name);
   int ret = mallctl(String_val(name), NULL, 0, NULL, 0);
   if (ret != 0) invalid_property(name);
   CAMLreturn(Val_unit);
}
