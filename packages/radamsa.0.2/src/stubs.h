#ifndef MLRADAMSA_STUBS_H
#define MLRADAMSA_STUBS_H

#include <caml/mlvalues.h>
#include <caml/memory.h>

value caml__init(value unit);
value caml__radamsa(value input, value seed);

#endif

