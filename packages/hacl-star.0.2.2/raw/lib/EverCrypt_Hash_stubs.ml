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

external _6_EverCrypt_Hash_update2
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> unit
  = "_6_EverCrypt_Hash_update2" 

external _7_EverCrypt_Hash_update : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_7_EverCrypt_Hash_update" 

external _8_EverCrypt_Hash_update_multi2
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_8_EverCrypt_Hash_update_multi2" 

external _9_EverCrypt_Hash_update_multi
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_9_EverCrypt_Hash_update_multi" 

external _10_EverCrypt_Hash_update_last_256
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_10_EverCrypt_Hash_update_last_256" 

external _11_EverCrypt_Hash_update_last2
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_11_EverCrypt_Hash_update_last2" 

external _12_EverCrypt_Hash_update_last
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint64 -> unit
  = "_12_EverCrypt_Hash_update_last" 

external _13_EverCrypt_Hash_finish : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_13_EverCrypt_Hash_finish" 

external _14_EverCrypt_Hash_free : _ CI.fatptr -> unit
  = "_14_EverCrypt_Hash_free" 

external _15_EverCrypt_Hash_copy : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_15_EverCrypt_Hash_copy" 

external _16_EverCrypt_Hash_hash_256
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_16_EverCrypt_Hash_hash_256" 

external _17_EverCrypt_Hash_hash_224
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_17_EverCrypt_Hash_hash_224" 

external _18_EverCrypt_Hash_hash
  : Unsigned.uint8 -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_18_EverCrypt_Hash_hash" 

external _19_EverCrypt_Hash_Incremental_create_in
  : Unsigned.uint8 -> CI.voidp = "_19_EverCrypt_Hash_Incremental_create_in" 

external _20_EverCrypt_Hash_Incremental_init : _ CI.fatptr -> unit
  = "_20_EverCrypt_Hash_Incremental_init" 

external _21_EverCrypt_Hash_Incremental_update
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_21_EverCrypt_Hash_Incremental_update" 

external _22_EverCrypt_Hash_Incremental_finish_md5
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_22_EverCrypt_Hash_Incremental_finish_md5" 

external _23_EverCrypt_Hash_Incremental_finish_sha1
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_23_EverCrypt_Hash_Incremental_finish_sha1" 

external _24_EverCrypt_Hash_Incremental_finish_sha224
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_24_EverCrypt_Hash_Incremental_finish_sha224" 

external _25_EverCrypt_Hash_Incremental_finish_sha256
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_25_EverCrypt_Hash_Incremental_finish_sha256" 

external _26_EverCrypt_Hash_Incremental_finish_sha384
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_26_EverCrypt_Hash_Incremental_finish_sha384" 

external _27_EverCrypt_Hash_Incremental_finish_sha512
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_27_EverCrypt_Hash_Incremental_finish_sha512" 

external _28_EverCrypt_Hash_Incremental_finish_blake2s
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_28_EverCrypt_Hash_Incremental_finish_blake2s" 

external _29_EverCrypt_Hash_Incremental_finish_blake2b
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_29_EverCrypt_Hash_Incremental_finish_blake2b" 

external _30_EverCrypt_Hash_Incremental_alg_of_state
  : _ CI.fatptr -> Unsigned.uint8
  = "_30_EverCrypt_Hash_Incremental_alg_of_state" 

external _31_EverCrypt_Hash_Incremental_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_31_EverCrypt_Hash_Incremental_finish" 

external _32_EverCrypt_Hash_Incremental_free : _ CI.fatptr -> unit
  = "_32_EverCrypt_Hash_Incremental_free" 

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
    let CI.CPointer x2 = x1 in _32_EverCrypt_Hash_Incremental_free x2)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish" ->
  (fun x3 x5 ->
    let CI.CPointer x4 = x3 in _31_EverCrypt_Hash_Incremental_finish x4 x5)
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x8; _})),
  "EverCrypt_Hash_Incremental_alg_of_state" ->
  (fun x6 ->
    let CI.CPointer x7 = x6 in
    x8 (_30_EverCrypt_Hash_Incremental_alg_of_state x7))
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_blake2b" ->
  (fun x9 x11 ->
    let CI.CPointer x10 = x9 in
    _29_EverCrypt_Hash_Incremental_finish_blake2b x10 x11)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_blake2s" ->
  (fun x12 x14 ->
    let CI.CPointer x13 = x12 in
    _28_EverCrypt_Hash_Incremental_finish_blake2s x13 x14)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha512" ->
  (fun x15 x17 ->
    let CI.CPointer x16 = x15 in
    _27_EverCrypt_Hash_Incremental_finish_sha512 x16 x17)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha384" ->
  (fun x18 x20 ->
    let CI.CPointer x19 = x18 in
    _26_EverCrypt_Hash_Incremental_finish_sha384 x19 x20)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha256" ->
  (fun x21 x23 ->
    let CI.CPointer x22 = x21 in
    _25_EverCrypt_Hash_Incremental_finish_sha256 x22 x23)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha224" ->
  (fun x24 x26 ->
    let CI.CPointer x25 = x24 in
    _24_EverCrypt_Hash_Incremental_finish_sha224 x25 x26)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha1" ->
  (fun x27 x29 ->
    let CI.CPointer x28 = x27 in
    _23_EverCrypt_Hash_Incremental_finish_sha1 x28 x29)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_md5" ->
  (fun x30 x32 ->
    let CI.CPointer x31 = x30 in
    _22_EverCrypt_Hash_Incremental_finish_md5 x31 x32)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_Incremental_update" ->
  (fun x33 x35 x36 ->
    let CI.CPointer x34 = x33 in
    _21_EverCrypt_Hash_Incremental_update x34 x35 x36)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_Incremental_init" ->
  (fun x37 ->
    let CI.CPointer x38 = x37 in _20_EverCrypt_Hash_Incremental_init x38)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x40; _},
     Returns (CI.Pointer x42)),
  "EverCrypt_Hash_Incremental_create_in" ->
  (fun x39 ->
    let x41 = x40 x39 in
    CI.make_ptr x42 (_19_EverCrypt_Hash_Incremental_create_in x41))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x44; _},
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_hash" ->
  (fun x43 x46 x47 x48 ->
    let x45 = x44 x43 in _18_EverCrypt_Hash_hash x45 x46 x47 x48)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_Hash_hash_224" -> _17_EverCrypt_Hash_hash_224
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_Hash_hash_256" -> _16_EverCrypt_Hash_hash_256
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "EverCrypt_Hash_copy" ->
  (fun x55 x57 ->
    let CI.CPointer x58 = x57 in
    let CI.CPointer x56 = x55 in _15_EverCrypt_Hash_copy x56 x58)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_free" ->
  (fun x59 -> let CI.CPointer x60 = x59 in _14_EverCrypt_Hash_free x60)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_finish" ->
  (fun x61 x63 ->
    let CI.CPointer x62 = x61 in _13_EverCrypt_Hash_finish x62 x63)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint64_t, Returns CI.Void))),
  "EverCrypt_Hash_update_last" ->
  (fun x64 x66 x67 ->
    let CI.CPointer x65 = x64 in _12_EverCrypt_Hash_update_last x65 x66 x67)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_update_last2" ->
  (fun x68 x70 x71 x72 ->
    let CI.CPointer x69 = x68 in
    _11_EverCrypt_Hash_update_last2 x69 x70 x71 x72)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_update_last_256" ->
  (fun x73 x75 x76 x77 ->
    let CI.CPointer x74 = x73 in
    _10_EverCrypt_Hash_update_last_256 x74 x75 x76 x77)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_update_multi" ->
  (fun x78 x80 x81 ->
    let CI.CPointer x79 = x78 in _9_EverCrypt_Hash_update_multi x79 x80 x81)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_update_multi2" ->
  (fun x82 x84 x85 x86 ->
    let CI.CPointer x83 = x82 in
    _8_EverCrypt_Hash_update_multi2 x83 x84 x85 x86)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_update" ->
  (fun x87 x89 ->
    let CI.CPointer x88 = x87 in _7_EverCrypt_Hash_update x88 x89)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_Hash_update2" ->
  (fun x90 x92 x93 ->
    let CI.CPointer x91 = x90 in _6_EverCrypt_Hash_update2 x91 x92 x93)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_update_multi_256" ->
  (fun x94 x96 x97 ->
    let CI.CPointer x95 = x94 in
    _5_EverCrypt_Hash_update_multi_256 x95 x96 x97)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_init" ->
  (fun x98 -> let CI.CPointer x99 = x98 in _4_EverCrypt_Hash_init x99)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x101; _},
     Returns (CI.Pointer x103)),
  "EverCrypt_Hash_create" ->
  (fun x100 ->
    let x102 = x101 x100 in CI.make_ptr x103 (_3_EverCrypt_Hash_create x102))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x105; _},
     Returns (CI.Pointer x107)),
  "EverCrypt_Hash_create_in" ->
  (fun x104 ->
    let x106 = x105 x104 in
    CI.make_ptr x107 (_2_EverCrypt_Hash_create_in x106))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x110; _})),
  "EverCrypt_Hash_alg_of_state" ->
  (fun x108 ->
    let CI.CPointer x109 = x108 in x110 (_1_EverCrypt_Hash_alg_of_state x109))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
