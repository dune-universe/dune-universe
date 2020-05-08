//Provides: caml_ba_heap
var caml_ba_heap = (-1);

//Provides: overlap_bytecode_caml_ba_ptr
//Requires: caml_ba_heap
function overlap_bytecode_caml_ba_ptr(buf) {
  caml_ba_heap += 1;
  return caml_ba_heap;
}

//Provides: overlap_native_caml_ba_ptr
//Requires: caml_ba_heap
function overlap_native_caml_ba_ptr(buf) {
  caml_ba_heap += 1;
  return caml_ba_heap;
}
