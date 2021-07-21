module CI = Cstubs_internals

external _1_Hacl_Blake2s_32_blake2s_init
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> unit = "_1_Hacl_Blake2s_32_blake2s_init" 

external _2_Hacl_Blake2s_32_blake2s_update_multi
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64 ->
    bytes CI.ocaml -> Unsigned.uint32 -> unit
  =
  "_2_Hacl_Blake2s_32_blake2s_update_multi_byte6" "_2_Hacl_Blake2s_32_blake2s_update_multi"
  

external _3_Hacl_Blake2s_32_blake2s_update_last
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64 ->
    Unsigned.uint32 -> bytes CI.ocaml -> unit
  =
  "_3_Hacl_Blake2s_32_blake2s_update_last_byte6" "_3_Hacl_Blake2s_32_blake2s_update_last"
  

external _4_Hacl_Blake2s_32_blake2s_finish
  : Unsigned.uint32 -> bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_4_Hacl_Blake2s_32_blake2s_finish" 

external _5_Hacl_Blake2s_32_blake2s
  : Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_5_Hacl_Blake2s_32_blake2s_byte6" "_5_Hacl_Blake2s_32_blake2s" 

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
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function (CI.OCaml CI.Bytes, Returns CI.Void)))))),
  "Hacl_Blake2s_32_blake2s" -> _5_Hacl_Blake2s_32_blake2s
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Blake2s_32_blake2s_finish" ->
  (fun x7 x8 x9 ->
    let CI.CPointer x10 = x9 in _4_Hacl_Blake2s_32_blake2s_finish x7 x8 x10)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function
             (CI.Primitive CI.Uint64_t,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function (CI.OCaml CI.Bytes, Returns CI.Void)))))),
  "Hacl_Blake2s_32_blake2s_update_last" ->
  (fun x11 x12 x14 x16 x17 x18 ->
    let CI.CPointer x15 = x14 in
    let CI.CPointer x13 = x12 in
    _3_Hacl_Blake2s_32_blake2s_update_last x11 x13 x15 x16 x17 x18)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function
             (CI.Primitive CI.Uint64_t,
              Function
                (CI.OCaml CI.Bytes,
                 Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))))),
  "Hacl_Blake2s_32_blake2s_update_multi" ->
  (fun x19 x20 x22 x24 x25 x26 ->
    let CI.CPointer x23 = x22 in
    let CI.CPointer x21 = x20 in
    _2_Hacl_Blake2s_32_blake2s_update_multi x19 x21 x23 x24 x25 x26)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.Primitive CI.Uint32_t, Returns CI.Void))))),
  "Hacl_Blake2s_32_blake2s_init" ->
  (fun x27 x29 x31 x32 x33 ->
    let CI.CPointer x30 = x29 in
    let CI.CPointer x28 = x27 in
    _1_Hacl_Blake2s_32_blake2s_init x28 x30 x31 x32 x33)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
