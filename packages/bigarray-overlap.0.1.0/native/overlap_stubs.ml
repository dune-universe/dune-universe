open Bigarray_compat

external ptr : ('a, 'b, 'c) Genarray.t -> (nativeint[@unboxed]) = "overlap_bytecode_caml_ba_ptr" "overlap_native_caml_ba_ptr" [@@noalloc]
