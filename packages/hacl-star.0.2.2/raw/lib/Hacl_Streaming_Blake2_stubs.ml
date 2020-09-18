module CI = Cstubs_internals

external _1_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in
  : unit -> CI.voidp = "_1_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in" 

external _2_Hacl_Streaming_Blake2_blake2s_32_no_key_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_2_Hacl_Streaming_Blake2_blake2s_32_no_key_update" 

external _3_Hacl_Streaming_Blake2_blake2s_32_no_key_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_3_Hacl_Streaming_Blake2_blake2s_32_no_key_finish" 

external _4_Hacl_Streaming_Blake2_blake2s_32_no_key_free
  : _ CI.fatptr -> unit = "_4_Hacl_Streaming_Blake2_blake2s_32_no_key_free" 

external _5_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in
  : unit -> CI.voidp = "_5_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in" 

external _6_Hacl_Streaming_Blake2_blake2b_32_no_key_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_6_Hacl_Streaming_Blake2_blake2b_32_no_key_update" 

external _7_Hacl_Streaming_Blake2_blake2b_32_no_key_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_7_Hacl_Streaming_Blake2_blake2b_32_no_key_finish" 

external _8_Hacl_Streaming_Blake2_blake2b_32_no_key_free
  : _ CI.fatptr -> unit = "_8_Hacl_Streaming_Blake2_blake2b_32_no_key_free" 

external _9_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_9_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in" 

external _10_Hacl_Streaming_Blake2_blake2s_32_with_key_update
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_10_Hacl_Streaming_Blake2_blake2s_32_with_key_update" 

external _11_Hacl_Streaming_Blake2_blake2s_32_with_key_finish
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_11_Hacl_Streaming_Blake2_blake2s_32_with_key_finish" 

external _12_Hacl_Streaming_Blake2_blake2s_32_with_key_free
  : Unsigned.uint32 -> _ CI.fatptr -> unit
  = "_12_Hacl_Streaming_Blake2_blake2s_32_with_key_free" 

external _13_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_13_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in" 

external _14_Hacl_Streaming_Blake2_blake2b_32_with_key_update
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_14_Hacl_Streaming_Blake2_blake2b_32_with_key_update" 

external _15_Hacl_Streaming_Blake2_blake2b_32_with_key_finish
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_15_Hacl_Streaming_Blake2_blake2b_32_with_key_finish" 

external _16_Hacl_Streaming_Blake2_blake2b_32_with_key_free
  : Unsigned.uint32 -> _ CI.fatptr -> unit
  = "_16_Hacl_Streaming_Blake2_blake2b_32_with_key_free" 

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
  "Hacl_Streaming_Blake2_blake2b_32_with_key_free" ->
  (fun x1 x2 ->
    let CI.CPointer x3 = x2 in
    _16_Hacl_Streaming_Blake2_blake2b_32_with_key_free x1 x3)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2b_32_with_key_finish" ->
  (fun x4 x5 x7 ->
    let CI.CPointer x6 = x5 in
    _15_Hacl_Streaming_Blake2_blake2b_32_with_key_finish x4 x6 x7)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Streaming_Blake2_blake2b_32_with_key_update" ->
  (fun x8 x9 x11 x12 ->
    let CI.CPointer x10 = x9 in
    _14_Hacl_Streaming_Blake2_blake2b_32_with_key_update x8 x10 x11 x12)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x15))),
  "Hacl_Streaming_Blake2_blake2b_32_with_key_create_in" ->
  (fun x13 x14 ->
    CI.make_ptr x15
      (_13_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in x13 x14))
| Function
    (CI.Primitive CI.Uint32_t, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_free" ->
  (fun x16 x17 ->
    let CI.CPointer x18 = x17 in
    _12_Hacl_Streaming_Blake2_blake2s_32_with_key_free x16 x18)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_finish" ->
  (fun x19 x20 x22 ->
    let CI.CPointer x21 = x20 in
    _11_Hacl_Streaming_Blake2_blake2s_32_with_key_finish x19 x21 x22)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_update" ->
  (fun x23 x24 x26 x27 ->
    let CI.CPointer x25 = x24 in
    _10_Hacl_Streaming_Blake2_blake2s_32_with_key_update x23 x25 x26 x27)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x30))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_create_in" ->
  (fun x28 x29 ->
    CI.make_ptr x30
      (_9_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in x28 x29))
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_free" ->
  (fun x31 ->
    let CI.CPointer x32 = x31 in
    _8_Hacl_Streaming_Blake2_blake2b_32_no_key_free x32)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_finish" ->
  (fun x33 x35 ->
    let CI.CPointer x34 = x33 in
    _7_Hacl_Streaming_Blake2_blake2b_32_no_key_finish x34 x35)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_update" ->
  (fun x36 x38 x39 ->
    let CI.CPointer x37 = x36 in
    _6_Hacl_Streaming_Blake2_blake2b_32_no_key_update x37 x38 x39)
| Function (CI.Void, Returns (CI.Pointer x41)),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_create_in" ->
  (fun x40 ->
    CI.make_ptr x41
      (_5_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in x40))
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_free" ->
  (fun x42 ->
    let CI.CPointer x43 = x42 in
    _4_Hacl_Streaming_Blake2_blake2s_32_no_key_free x43)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_finish" ->
  (fun x44 x46 ->
    let CI.CPointer x45 = x44 in
    _3_Hacl_Streaming_Blake2_blake2s_32_no_key_finish x45 x46)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_update" ->
  (fun x47 x49 x50 ->
    let CI.CPointer x48 = x47 in
    _2_Hacl_Streaming_Blake2_blake2s_32_no_key_update x48 x49 x50)
| Function (CI.Void, Returns (CI.Pointer x52)),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_create_in" ->
  (fun x51 ->
    CI.make_ptr x52
      (_1_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in x51))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
