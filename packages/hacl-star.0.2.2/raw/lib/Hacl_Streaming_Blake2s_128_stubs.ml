module CI = Cstubs_internals

external _1_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_create_in
  : unit -> CI.voidp
  = "_1_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_create_in" 

external _2_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_2_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_update" 

external _3_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_3_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_finish" 

external _4_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_free
  : _ CI.fatptr -> unit
  = "_4_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_free" 

external _5_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_create_in
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_5_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_create_in" 

external _6_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_update
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_6_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_update" 

external _7_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_finish
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_7_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_finish" 

external _8_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_free
  : Unsigned.uint32 -> _ CI.fatptr -> unit
  = "_8_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_free" 

type 'a result = 'a
type 'a return = 'a
type 'a fn =
 | Returns  : 'a CI.typ   -> 'a return fn
 | Function : 'a CI.typ * 'b fn  -> ('a -> 'b) fn
let map_result f x = f x
let returning t = Returns t
let (@->) f p = Function (f, p)
let foreign : type a b. string -> (a -> b) fn -> (a -> b) =
  fun name t -> match t, name with
| Function
    (CI.Primitive CI.Uint32_t, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Streaming_Blake2s_128_blake2s_128_with_key_free" ->
  (fun x1 x2 ->
    let CI.CPointer x3 = x2 in
    _8_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_free x1 x3)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Streaming_Blake2s_128_blake2s_128_with_key_finish" ->
  (fun x4 x5 x7 ->
    let CI.CPointer x6 = x5 in
    _7_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_finish x4 x6 x7)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Streaming_Blake2s_128_blake2s_128_with_key_update" ->
  (fun x8 x9 x11 x12 ->
    let CI.CPointer x10 = x9 in
    _6_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_update x8 x10 x11 x12)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x15))),
  "Hacl_Streaming_Blake2s_128_blake2s_128_with_key_create_in" ->
  (fun x13 x14 ->
    CI.make_ptr x15
      (_5_Hacl_Streaming_Blake2s_128_blake2s_128_with_key_create_in x13 x14))
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2s_128_blake2s_128_no_key_free" ->
  (fun x16 ->
    let CI.CPointer x17 = x16 in
    _4_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_free x17)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Streaming_Blake2s_128_blake2s_128_no_key_finish" ->
  (fun x18 x20 ->
    let CI.CPointer x19 = x18 in
    _3_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_finish x19 x20)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Streaming_Blake2s_128_blake2s_128_no_key_update" ->
  (fun x21 x23 x24 ->
    let CI.CPointer x22 = x21 in
    _2_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_update x22 x23 x24)
| Function (CI.Void, Returns (CI.Pointer x26)),
  "Hacl_Streaming_Blake2s_128_blake2s_128_no_key_create_in" ->
  (fun x25 ->
    CI.make_ptr x26
      (_1_Hacl_Streaming_Blake2s_128_blake2s_128_no_key_create_in x25))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
