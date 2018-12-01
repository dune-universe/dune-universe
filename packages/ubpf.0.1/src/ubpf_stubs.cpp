
extern "C" {
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
}

#include "cxx_wrapped.h"
#include "bpf.h"

typedef wrapped_ptr<ubpf::bpf> ml_bpf;

template<>
char const* ml_name<ml_bpf::type>() { return "ubpf"; }

extern "C" {

value ml_bpf_load(value v_s, value v_is_elf)
{
  CAMLparam2(v_s,v_is_elf);

  ubpf::bpf* p = nullptr;

  try
  {
    p = new ubpf::bpf(String_val(v_s), caml_string_length(v_s), Bool_val(v_is_elf));
  }
  catch (std::exception const& e)
  {
    caml_failwith(e.what());
  }

  CAMLreturn(ml_bpf::alloc(p));
}

value ml_bpf_exec(value v, value v_s)
{
  CAMLparam2(v,v_s);
  uint64_t r = 0;
  try
  {
    r = ml_bpf::get(v)->exec(String_val(v_s), caml_string_length(v_s));
  }
  catch (std::exception const& e)
  {
    caml_failwith(e.what());
  }
  CAMLreturn(Val_long(r)); // truncated
}

value ml_bpf_compile(value v)
{
  CAMLparam1(v);
  try
  {
     ml_bpf::get(v)->compile();
  }
  catch (std::exception const& e)
  {
    caml_failwith(e.what());
  }
  CAMLreturn(Val_unit);
}

value ml_bpf_close(value v)
{
  CAMLparam1(v);
  try
  {
    ml_bpf::release(v);
  }
  catch (std::exception const& e)
  {
    caml_failwith(e.what());
  }
  CAMLreturn(Val_unit);
}

} // extern "C"
