module CI = Cstubs_internals

external _1_Hacl_Blake2b_32_blake2b_init
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> unit = "_1_Hacl_Blake2b_32_blake2b_init" 

external _2_Hacl_Blake2b_32_blake2b_finish
  : Unsigned.uint32 -> bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_2_Hacl_Blake2b_32_blake2b_finish" 

external _3_Hacl_Blake2b_32_blake2b
  : Unsigned.uint32 -> bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml ->
    Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_3_Hacl_Blake2b_32_blake2b_byte6" "_3_Hacl_Blake2b_32_blake2b" 

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
  "Hacl_Blake2b_32_blake2b" -> _3_Hacl_Blake2b_32_blake2b
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Blake2b_32_blake2b_finish" ->
  (fun x7 x8 x9 ->
    let CI.CPointer x10 = x9 in _2_Hacl_Blake2b_32_blake2b_finish x7 x8 x10)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function (CI.Primitive CI.Uint32_t, Returns CI.Void))))),
  "Hacl_Blake2b_32_blake2b_init" ->
  (fun x11 x13 x15 x16 x17 ->
    let CI.CPointer x14 = x13 in
    let CI.CPointer x12 = x11 in
    _1_Hacl_Blake2b_32_blake2b_init x12 x14 x15 x16 x17)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
