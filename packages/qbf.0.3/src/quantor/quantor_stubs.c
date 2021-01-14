
/*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "caml/mlvalues.h"
#include <caml/memory.h>
#include <caml/alloc.h>
#include "caml/fail.h"
#include "quantor.h"

CAMLprim value quantor_stub_create(value unit)
{
  CAMLparam0();
  Quantor* q = quantor_new();
  CAMLreturn ((value) q);
}

CAMLprim value quantor_stub_delete(value raw)
{
  CAMLparam0();
  Quantor* q = (Quantor*) raw;
  quantor_delete(q);
  CAMLreturn (Val_unit);
}

CAMLprim value quantor_stub_sat(value raw)
{
  CAMLparam0();
  Quantor* q = (Quantor*) raw;

  int c = quantor_sat(q);
  CAMLreturn (Val_int(c));
}

CAMLprim value quantor_stub_exists(value raw)
{
  CAMLparam0();
  Quantor* q = (Quantor*) raw;

  const char *err = quantor_scope(q, QUANTOR_EXISTENTIAL_VARIABLE_TYPE);

  if (err != 0)
  {
    caml_failwith(err);
  }

  CAMLreturn (Val_unit);
}

CAMLprim value quantor_stub_forall(value raw)
{
  CAMLparam0();
  Quantor* q = (Quantor*) raw;

  const char *err = quantor_scope(q, QUANTOR_UNIVERSAL_VARIABLE_TYPE);

  if (err != 0)
  {
    caml_failwith(err);
  }

  CAMLreturn (Val_unit);
}

CAMLprim value quantor_stub_add(value raw, value i)
{
  CAMLparam0();
  Quantor* q = (Quantor*) raw;
  int j = Int_val(i);
  const char* err = quantor_add(q, j);

  if (err != 0)
  {
    caml_failwith(err);
  }

  CAMLreturn (Val_unit);
}

CAMLprim value quantor_stub_deref(value raw, value i)
{
  CAMLparam0();
  Quantor* q = (Quantor*) raw;
  int j = Int_val(i);

  int res = quantor_deref(q, j);

  CAMLreturn (Val_int(res));
}
