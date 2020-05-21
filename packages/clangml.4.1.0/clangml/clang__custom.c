#include "clang__custom.h"
#include <clang-c/Index.h>
#include <caml/memory.h>

extern CXCursor
Cxcursor_val(value);

int
clang_ext_compare_cursor(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CXCursor c1 = Cxcursor_val(Field(v1, 0));
  CXCursor c2 = Cxcursor_val(Field(v2, 0));

  /* Hack from CIndex.cpp:clang_equalCursors */
  if (clang_isDeclaration(c1.kind)) {
    c1.data[1] = NULL;
  }
  if (clang_isDeclaration(c2.kind)) {
    c2.data[1] = NULL;
  }

  if (c1.kind < c2.kind) {
    CAMLreturn(-1);
  }
  if (c1.kind > c2.kind) {
    CAMLreturn(1);
  }

  if (c1.xdata < c2.xdata) {
    CAMLreturn(-1);
  }
  if (c1.xdata > c2.xdata) {
    CAMLreturn(1);
  }

  int i;
  for (i = 0; i < 3; i++) {
    if (c1.data[i] < c2.data[i]) {
      CAMLreturn(-1);
    }
    if (c1.data[i] > c2.data[i]) {
      CAMLreturn(1);
    }
  }

  CAMLreturn(0);
}

value
clang_ext_compare_cursor_boxed(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(clang_ext_compare_cursor(v1, v2));
}

intnat
clang_ext_hash_cursor(value v)
{
  CXCursor c = Cxcursor_val(Field(v, 0));
  return clang_hashCursor(c);
}
