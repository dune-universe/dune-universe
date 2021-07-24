module CI = Cstubs_internals

external _1_Hacl_Streaming_Blake2_blocks_state_len
  : Unsigned.uint8 -> Unsigned.uint8 -> Unsigned.uint32
  = "_1_Hacl_Streaming_Blake2_blocks_state_len" 

external _2_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in
  : unit -> CI.voidp = "_2_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in" 

external _3_Hacl_Streaming_Blake2_blake2s_32_no_key_init
  : _ CI.fatptr -> unit = "_3_Hacl_Streaming_Blake2_blake2s_32_no_key_init" 

external _4_Hacl_Streaming_Blake2_blake2s_32_no_key_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_4_Hacl_Streaming_Blake2_blake2s_32_no_key_update" 

external _5_Hacl_Streaming_Blake2_blake2s_32_no_key_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_5_Hacl_Streaming_Blake2_blake2s_32_no_key_finish" 

external _6_Hacl_Streaming_Blake2_blake2s_32_no_key_free
  : _ CI.fatptr -> unit = "_6_Hacl_Streaming_Blake2_blake2s_32_no_key_free" 

external _7_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in
  : unit -> CI.voidp = "_7_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in" 

external _8_Hacl_Streaming_Blake2_blake2b_32_no_key_init
  : _ CI.fatptr -> unit = "_8_Hacl_Streaming_Blake2_blake2b_32_no_key_init" 

external _9_Hacl_Streaming_Blake2_blake2b_32_no_key_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_9_Hacl_Streaming_Blake2_blake2b_32_no_key_update" 

external _10_Hacl_Streaming_Blake2_blake2b_32_no_key_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_10_Hacl_Streaming_Blake2_blake2b_32_no_key_finish" 

external _11_Hacl_Streaming_Blake2_blake2b_32_no_key_free
  : _ CI.fatptr -> unit = "_11_Hacl_Streaming_Blake2_blake2b_32_no_key_free" 

external _12_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_12_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in" 

external _13_Hacl_Streaming_Blake2_blake2s_32_with_key_init
  : Unsigned.uint32 -> bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_13_Hacl_Streaming_Blake2_blake2s_32_with_key_init" 

external _14_Hacl_Streaming_Blake2_blake2s_32_with_key_update
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_14_Hacl_Streaming_Blake2_blake2s_32_with_key_update" 

external _15_Hacl_Streaming_Blake2_blake2s_32_with_key_finish
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_15_Hacl_Streaming_Blake2_blake2s_32_with_key_finish" 

external _16_Hacl_Streaming_Blake2_blake2s_32_with_key_free
  : Unsigned.uint32 -> _ CI.fatptr -> unit
  = "_16_Hacl_Streaming_Blake2_blake2s_32_with_key_free" 

external _17_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_17_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in" 

external _18_Hacl_Streaming_Blake2_blake2b_32_with_key_init
  : Unsigned.uint32 -> bytes CI.ocaml -> _ CI.fatptr -> unit
  = "_18_Hacl_Streaming_Blake2_blake2b_32_with_key_init" 

external _19_Hacl_Streaming_Blake2_blake2b_32_with_key_update
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_19_Hacl_Streaming_Blake2_blake2b_32_with_key_update" 

external _20_Hacl_Streaming_Blake2_blake2b_32_with_key_finish
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_20_Hacl_Streaming_Blake2_blake2b_32_with_key_finish" 

external _21_Hacl_Streaming_Blake2_blake2b_32_with_key_free
  : Unsigned.uint32 -> _ CI.fatptr -> unit
  = "_21_Hacl_Streaming_Blake2_blake2b_32_with_key_free" 

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
    _21_Hacl_Streaming_Blake2_blake2b_32_with_key_free x1 x3)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2b_32_with_key_finish" ->
  (fun x4 x5 x7 ->
    let CI.CPointer x6 = x5 in
    _20_Hacl_Streaming_Blake2_blake2b_32_with_key_finish x4 x6 x7)
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
    _19_Hacl_Streaming_Blake2_blake2b_32_with_key_update x8 x10 x11 x12)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2b_32_with_key_init" ->
  (fun x13 x14 x15 ->
    let CI.CPointer x16 = x15 in
    _18_Hacl_Streaming_Blake2_blake2b_32_with_key_init x13 x14 x16)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x19))),
  "Hacl_Streaming_Blake2_blake2b_32_with_key_create_in" ->
  (fun x17 x18 ->
    CI.make_ptr x19
      (_17_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in x17 x18))
| Function
    (CI.Primitive CI.Uint32_t, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_free" ->
  (fun x20 x21 ->
    let CI.CPointer x22 = x21 in
    _16_Hacl_Streaming_Blake2_blake2s_32_with_key_free x20 x22)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_finish" ->
  (fun x23 x24 x26 ->
    let CI.CPointer x25 = x24 in
    _15_Hacl_Streaming_Blake2_blake2s_32_with_key_finish x23 x25 x26)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_update" ->
  (fun x27 x28 x30 x31 ->
    let CI.CPointer x29 = x28 in
    _14_Hacl_Streaming_Blake2_blake2s_32_with_key_update x27 x29 x30 x31)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_init" ->
  (fun x32 x33 x34 ->
    let CI.CPointer x35 = x34 in
    _13_Hacl_Streaming_Blake2_blake2s_32_with_key_init x32 x33 x35)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x38))),
  "Hacl_Streaming_Blake2_blake2s_32_with_key_create_in" ->
  (fun x36 x37 ->
    CI.make_ptr x38
      (_12_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in x36 x37))
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_free" ->
  (fun x39 ->
    let CI.CPointer x40 = x39 in
    _11_Hacl_Streaming_Blake2_blake2b_32_no_key_free x40)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_finish" ->
  (fun x41 x43 ->
    let CI.CPointer x42 = x41 in
    _10_Hacl_Streaming_Blake2_blake2b_32_no_key_finish x42 x43)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_update" ->
  (fun x44 x46 x47 ->
    let CI.CPointer x45 = x44 in
    _9_Hacl_Streaming_Blake2_blake2b_32_no_key_update x45 x46 x47)
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_init" ->
  (fun x48 ->
    let CI.CPointer x49 = x48 in
    _8_Hacl_Streaming_Blake2_blake2b_32_no_key_init x49)
| Function (CI.Void, Returns (CI.Pointer x51)),
  "Hacl_Streaming_Blake2_blake2b_32_no_key_create_in" ->
  (fun x50 ->
    CI.make_ptr x51
      (_7_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in x50))
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_free" ->
  (fun x52 ->
    let CI.CPointer x53 = x52 in
    _6_Hacl_Streaming_Blake2_blake2s_32_no_key_free x53)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_finish" ->
  (fun x54 x56 ->
    let CI.CPointer x55 = x54 in
    _5_Hacl_Streaming_Blake2_blake2s_32_no_key_finish x55 x56)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_update" ->
  (fun x57 x59 x60 ->
    let CI.CPointer x58 = x57 in
    _4_Hacl_Streaming_Blake2_blake2s_32_no_key_update x58 x59 x60)
| Function (CI.Pointer _, Returns CI.Void),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_init" ->
  (fun x61 ->
    let CI.CPointer x62 = x61 in
    _3_Hacl_Streaming_Blake2_blake2s_32_no_key_init x62)
| Function (CI.Void, Returns (CI.Pointer x64)),
  "Hacl_Streaming_Blake2_blake2s_32_no_key_create_in" ->
  (fun x63 ->
    CI.make_ptr x64
      (_2_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in x63))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x66; _},
     Function
       (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x69; _},
        Returns (CI.Primitive CI.Uint32_t))),
  "Hacl_Streaming_Blake2_blocks_state_len" ->
  (fun x65 x68 ->
    let x67 = x66 x65 in
    let x70 = x69 x68 in _1_Hacl_Streaming_Blake2_blocks_state_len x67 x70)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
