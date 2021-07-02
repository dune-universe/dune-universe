#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/bigarray.h>

#include "xoshiro256plusplus_bindings.h"
#include "xoshiro256plusplus_bindings_stubs.h"

/* ************************************************************************** */
/* ************************ [ Low-level Interface ] ************************* */
/* ************************************************************************** */

CAMLprim value caml_x256pp_next(value bstate) {
  CAMLparam1(bstate);
  uint64_t *state = Caml_ba_data_val(bstate);
  uint64_t result = x256pp_next(state);
  CAMLreturn(caml_copy_int64(result));
}

CAMLprim value caml_x256pp_jump (value bstate) {
  CAMLparam1(bstate);
  uint64_t *state = Caml_ba_data_val(bstate);
  x256pp_jump(state);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_x256pp_long_jump (value bstate) {
  CAMLparam1(bstate);
  uint64_t *state = Caml_ba_data_val(bstate);
  x256pp_long_jump(state);
  CAMLreturn(Val_unit);
}

/* ************************************************************************** */
/* ************************* [ OCaml-y Interface ] ************************** */
/* ************************************************************************** */

/* ******************************** [ bits ] ******************************** */

static int x256pp_u30mask = (1 << 30) - 1;

unsigned int x256pp_bits (x256pp_state_t state) {
  if (*state.second > 0) {
    int result = *state.second;
    *state.second = -1;
    return result;
  }
  else {
    uint64_t result = x256pp_next(state.b_state);
    *state.second = result & x256pp_u30mask;
    return ((result >> 34) & x256pp_u30mask);
  }
}

CAMLprim value caml_x256pp_bits(value bstate) {
  CAMLparam1(bstate);
  x256pp_state_t state = unbox_x256pp_state(bstate);
  int result = x256pp_bits(state);
  CAMLreturn(Val_int(result));
}

/* ***************************** [ new_state ] ****************************** */

x256pp_state_t x256pp_new_state () {
  x256pp_state_t state;
  state.b_state = malloc(4 * sizeof(uint64_t));
  state.second = malloc(sizeof(int));
  *state.second = -1;
  return state;
}

CAMLprim value caml_x256pp_new_state(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(bstate);
  x256pp_state_t state = x256pp_new_state();
  box_x256pp_state(state, bstate);
  CAMLreturn(bstate);
}

/* ******************************* [ assign ] ******************************* */

void x256pp_assign(x256pp_state_t state1, x256pp_state_t state2) {
  for (int i = 0; i < 4; i++)
    state1.b_state[i] = state2.b_state[i];
  *state1.second = *state2.second;
}

CAMLprim value caml_x256pp_assign(value bstate1, value bstate2) {
  CAMLparam2(bstate1, bstate2);
  x256pp_state_t state1 = unbox_x256pp_state(bstate1);
  x256pp_state_t state2 = unbox_x256pp_state(bstate2);
  x256pp_assign (state1, state2);
  CAMLreturn(Val_unit);
}

/* ******************************** [ init ] ******************************** */

void x256pp_init(x256pp_state_t state, uint64_t* seed) {
  x256pp_state_t state2;
  state2.b_state = seed;
  state2.second = malloc(sizeof(int));
  *state2.second = -1;
  x256pp_assign(state, state2);
}

CAMLprim value caml_x256pp_init(value bstate, value bseed) {
  CAMLparam2(bstate, bseed);
  x256pp_state_t state = unbox_x256pp_state(bstate);

  uint64_t *seed = malloc(4 * sizeof(uint64_t));
  seed[0] = Int64_val(Field(bseed, 0));
  seed[1] = Int64_val(Field(bseed, 1));
  seed[2] = Int64_val(Field(bseed, 2));
  seed[3] = Int64_val(Field(bseed, 3));

  x256pp_init (state, seed);
  CAMLreturn(Val_unit);
}
