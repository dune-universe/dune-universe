module CI = Cstubs_internals

external _1_EverCrypt_Hash_alg_of_state : _ CI.fatptr -> Unsigned.uint8
  = "_1_EverCrypt_Hash_alg_of_state" 

external _2_EverCrypt_Hash_create_in : Unsigned.uint8 -> CI.voidp
  = "_2_EverCrypt_Hash_create_in" 

external _3_EverCrypt_Hash_create : Unsigned.uint8 -> CI.voidp
  = "_3_EverCrypt_Hash_create" 

external _4_EverCrypt_Hash_init : _ CI.fatptr -> unit
  = "_4_EverCrypt_Hash_init" 

external _5_EverCrypt_Hash_update_multi_256
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_5_EverCrypt_Hash_update_multi_256" 

external _6_EverCrypt_Hash_update : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_6_EverCrypt_Hash_update" 

external _7_EverCrypt_Hash_update_multi
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_7_EverCrypt_Hash_update_multi" 

external _8_EverCrypt_Hash_update_last_256
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_8_EverCrypt_Hash_update_last_256" 

external _9_EverCrypt_Hash_update_last
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint64 -> unit
  = "_9_EverCrypt_Hash_update_last" 

external _10_EverCrypt_Hash_finish : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_10_EverCrypt_Hash_finish" 

external _11_EverCrypt_Hash_free : _ CI.fatptr -> unit
  = "_11_EverCrypt_Hash_free" 

external _12_EverCrypt_Hash_copy : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_12_EverCrypt_Hash_copy" 

external _13_EverCrypt_Hash_hash_256
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_13_EverCrypt_Hash_hash_256" 

external _14_EverCrypt_Hash_hash_224
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_14_EverCrypt_Hash_hash_224" 

external _15_EverCrypt_Hash_hash
  : Unsigned.uint8 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_15_EverCrypt_Hash_hash" 

external _16_EverCrypt_Hash_Incremental_create_in
  : Unsigned.uint8 -> CI.voidp = "_16_EverCrypt_Hash_Incremental_create_in" 

external _17_EverCrypt_Hash_Incremental_init : _ CI.fatptr -> unit
  = "_17_EverCrypt_Hash_Incremental_init" 

external _18_EverCrypt_Hash_Incremental_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_18_EverCrypt_Hash_Incremental_update" 

external _19_EverCrypt_Hash_Incremental_finish_md5
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_19_EverCrypt_Hash_Incremental_finish_md5" 

external _20_EverCrypt_Hash_Incremental_finish_sha1
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_20_EverCrypt_Hash_Incremental_finish_sha1" 

external _21_EverCrypt_Hash_Incremental_finish_sha224
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_21_EverCrypt_Hash_Incremental_finish_sha224" 

external _22_EverCrypt_Hash_Incremental_finish_sha256
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_22_EverCrypt_Hash_Incremental_finish_sha256" 

external _23_EverCrypt_Hash_Incremental_finish_sha384
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_23_EverCrypt_Hash_Incremental_finish_sha384" 

external _24_EverCrypt_Hash_Incremental_finish_sha512
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_24_EverCrypt_Hash_Incremental_finish_sha512" 

external _25_EverCrypt_Hash_Incremental_alg_of_state
  : _ CI.fatptr -> Unsigned.uint8
  = "_25_EverCrypt_Hash_Incremental_alg_of_state" 

external _26_EverCrypt_Hash_Incremental_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_26_EverCrypt_Hash_Incremental_finish" 

external _27_EverCrypt_Hash_Incremental_free : _ CI.fatptr -> unit
  = "_27_EverCrypt_Hash_Incremental_free" 

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
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_Incremental_free" ->
  (fun x1 ->
    let CI.CPointer x2 = x1 in _27_EverCrypt_Hash_Incremental_free x2)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish" ->
  (fun x3 x5 ->
    let CI.CPointer x4 = x3 in _26_EverCrypt_Hash_Incremental_finish x4 x5)
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x8; _})),
  "EverCrypt_Hash_Incremental_alg_of_state" ->
  (fun x6 ->
    let CI.CPointer x7 = x6 in
    x8 (_25_EverCrypt_Hash_Incremental_alg_of_state x7))
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha512" ->
  (fun x9 x11 ->
    let CI.CPointer x10 = x9 in
    _24_EverCrypt_Hash_Incremental_finish_sha512 x10 x11)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha384" ->
  (fun x12 x14 ->
    let CI.CPointer x13 = x12 in
    _23_EverCrypt_Hash_Incremental_finish_sha384 x13 x14)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha256" ->
  (fun x15 x17 ->
    let CI.CPointer x16 = x15 in
    _22_EverCrypt_Hash_Incremental_finish_sha256 x16 x17)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha224" ->
  (fun x18 x20 ->
    let CI.CPointer x19 = x18 in
    _21_EverCrypt_Hash_Incremental_finish_sha224 x19 x20)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha1" ->
  (fun x21 x23 ->
    let CI.CPointer x22 = x21 in
    _20_EverCrypt_Hash_Incremental_finish_sha1 x22 x23)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_md5" ->
  (fun x24 x26 ->
    let CI.CPointer x25 = x24 in
    _19_EverCrypt_Hash_Incremental_finish_md5 x25 x26)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_Incremental_update" ->
  (fun x27 x29 x30 ->
    let CI.CPointer x28 = x27 in
    _18_EverCrypt_Hash_Incremental_update x28 x29 x30)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_Incremental_init" ->
  (fun x31 ->
    let CI.CPointer x32 = x31 in _17_EverCrypt_Hash_Incremental_init x32)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x34; _},
     Returns (CI.Pointer x36)),
  "EverCrypt_Hash_Incremental_create_in" ->
  (fun x33 ->
    let x35 = x34 x33 in
    CI.make_ptr x36 (_16_EverCrypt_Hash_Incremental_create_in x35))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x38; _},
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_hash" ->
  (fun x37 x40 x41 x42 ->
    let x39 = x38 x37 in _15_EverCrypt_Hash_hash x39 x40 x41 x42)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_Hash_hash_224" -> _14_EverCrypt_Hash_hash_224
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_Hash_hash_256" -> _13_EverCrypt_Hash_hash_256
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "EverCrypt_Hash_copy" ->
  (fun x49 x51 ->
    let CI.CPointer x52 = x51 in
    let CI.CPointer x50 = x49 in _12_EverCrypt_Hash_copy x50 x52)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_free" ->
  (fun x53 -> let CI.CPointer x54 = x53 in _11_EverCrypt_Hash_free x54)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_finish" ->
  (fun x55 x57 ->
    let CI.CPointer x56 = x55 in _10_EverCrypt_Hash_finish x56 x57)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint64_t, Returns CI.Void))),
  "EverCrypt_Hash_update_last" ->
  (fun x58 x60 x61 ->
    let CI.CPointer x59 = x58 in _9_EverCrypt_Hash_update_last x59 x60 x61)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_update_last_256" ->
  (fun x62 x64 x65 x66 ->
    let CI.CPointer x63 = x62 in
    _8_EverCrypt_Hash_update_last_256 x63 x64 x65 x66)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_update_multi" ->
  (fun x67 x69 x70 ->
    let CI.CPointer x68 = x67 in _7_EverCrypt_Hash_update_multi x68 x69 x70)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_update" ->
  (fun x71 x73 ->
    let CI.CPointer x72 = x71 in _6_EverCrypt_Hash_update x72 x73)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_update_multi_256" ->
  (fun x74 x76 x77 ->
    let CI.CPointer x75 = x74 in
    _5_EverCrypt_Hash_update_multi_256 x75 x76 x77)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_init" ->
  (fun x78 -> let CI.CPointer x79 = x78 in _4_EverCrypt_Hash_init x79)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x81; _},
     Returns (CI.Pointer x83)),
  "EverCrypt_Hash_create" ->
  (fun x80 ->
    let x82 = x81 x80 in CI.make_ptr x83 (_3_EverCrypt_Hash_create x82))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x85; _},
     Returns (CI.Pointer x87)),
  "EverCrypt_Hash_create_in" ->
  (fun x84 ->
    let x86 = x85 x84 in CI.make_ptr x87 (_2_EverCrypt_Hash_create_in x86))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x90; _})),
  "EverCrypt_Hash_alg_of_state" ->
  (fun x88 ->
    let CI.CPointer x89 = x88 in x90 (_1_EverCrypt_Hash_alg_of_state x89))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
