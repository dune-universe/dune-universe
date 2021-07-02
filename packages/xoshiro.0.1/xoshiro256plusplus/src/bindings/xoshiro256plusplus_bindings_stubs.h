#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>

struct x256pp_state {
  uint64_t *b_state;
  int *second;
};

#define x256pp_state_t struct x256pp_state

#define unbox_x256pp_state(bstate) (* (x256pp_state_t*) Data_custom_val(bstate))

void finalize_x256pp_state(value bstate) {
  x256pp_state_t state = unbox_x256pp_state(bstate);
  free(state.b_state);
  free(state.second);
}

static struct custom_operations x256pp_state_ops = {
  .identifier = "fr.boloss.xoshiro.bindings.256++.state",
  .finalize = finalize_x256pp_state,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default
};

#define box_x256pp_state(state, bstate) ({                              \
      bstate = caml_alloc_custom(&x256pp_state_ops, sizeof(x256pp_state_t), 0, 1); \
      memcpy(Data_custom_val(bstate), &state, sizeof(x256pp_state_t));  \
    })

CAMLprim value caml_x256pp_next(value barray);
CAMLprim value caml_x256pp_jump(value barray);
CAMLprim value caml_x256pp_long_jump(value barray);

CAMLprim value caml_x256pp_bits(value bstate);
CAMLprim value caml_x256pp_new_state(value unit);
CAMLprim value caml_x256pp_assign(value bstate1, value bstate2);
CAMLprim value caml_x256pp_init(value bstate, value bseed);
