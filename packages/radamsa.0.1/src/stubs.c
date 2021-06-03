// gcc -I`ocamlc -where` -I. -c stub.c 

#define CAML_NAME_SPACE

#include <inttypes.h>
#include <stddef.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/misc.h>
#include <caml/intext.h>
#include <caml/custom.h>

#include "stubs.h"

void radamsa_init();
size_t radamsa(uint8_t *ptr, size_t len, uint8_t *target, size_t max, unsigned int seed);

static uint8_t *gbuf = NULL;

value caml__init(value unit) {
    CAMLparam1(unit);
    radamsa_init();
    gbuf = malloc(1024*1024);
    CAMLreturn (Val_unit);
}

value caml__radamsa(value input, value seed)
{
    CAMLparam2 (input, seed);
    CAMLlocal1 (result);

    int input_length = caml_string_length(input);
    unsigned int iseed = Int_val(seed);
    radamsa_init();
    size_t res = radamsa(Bytes_val(input), input_length, gbuf, 1024*1024, iseed);
    result = caml_alloc_string(res);
    memcpy(Bytes_val(result), gbuf, res);

    CAMLreturn(result);
}
