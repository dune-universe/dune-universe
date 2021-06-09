#ifndef UNIF01_STUBS_H
#define UNIF01_STUBS_H 1

#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include <unif01.h>

#define IGNORE(x) (void)(x)

#define unif01_Gen_box(gen, bgen, ops) ({bgen = caml_alloc_custom(&ops, sizeof(unif01_Gen*), 0, 1); memcpy(Data_custom_val(bgen), &gen, sizeof(unif01_Gen*));})

#define unif01_Gen_unbox(bgen) (* (unif01_Gen**) Data_custom_val(bgen))

static struct custom_operations unif01_Gen_boxed;

value caml_unif01_CreateExternGenBits(value name, value bits);

value caml_unif01_CreateExternGenInt32(value name, value bits);

value caml_unif01_CreateExternGen01(value name, value bits);

#endif
