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
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_5_EverCrypt_Hash_update_multi_256" 

external _6_EverCrypt_Hash_update : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_6_EverCrypt_Hash_update" 

external _7_EverCrypt_Hash_update_multi
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_7_EverCrypt_Hash_update_multi" 

external _8_EverCrypt_Hash_update_last_256
  : _ CI.fatptr -> Unsigned.uint64 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    unit = "_8_EverCrypt_Hash_update_last_256" 

external _9_EverCrypt_Hash_update_last
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint64 -> unit
  = "_9_EverCrypt_Hash_update_last" 

external _10_EverCrypt_Hash_finish : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_10_EverCrypt_Hash_finish" 

external _11_EverCrypt_Hash_free : _ CI.fatptr -> unit
  = "_11_EverCrypt_Hash_free" 

external _12_EverCrypt_Hash_copy : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_12_EverCrypt_Hash_copy" 

external _13_EverCrypt_Hash_hash_256
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_13_EverCrypt_Hash_hash_256" 

external _14_EverCrypt_Hash_hash_224
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_14_EverCrypt_Hash_hash_224" 

external _15_EverCrypt_Hash_hash
  : Unsigned.uint8 -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> unit = "_15_EverCrypt_Hash_hash" 

external _16_EverCrypt_Hash_Incremental_create_in
  : Unsigned.uint8 -> CI.voidp = "_16_EverCrypt_Hash_Incremental_create_in" 

external _17_EverCrypt_Hash_Incremental_init : _ CI.fatptr -> unit
  = "_17_EverCrypt_Hash_Incremental_init" 

external _18_EverCrypt_Hash_Incremental_update
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_18_EverCrypt_Hash_Incremental_update" 

external _19_EverCrypt_Hash_Incremental_finish_md5
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_19_EverCrypt_Hash_Incremental_finish_md5" 

external _20_EverCrypt_Hash_Incremental_finish_sha1
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_20_EverCrypt_Hash_Incremental_finish_sha1" 

external _21_EverCrypt_Hash_Incremental_finish_sha224
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_21_EverCrypt_Hash_Incremental_finish_sha224" 

external _22_EverCrypt_Hash_Incremental_finish_sha256
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_22_EverCrypt_Hash_Incremental_finish_sha256" 

external _23_EverCrypt_Hash_Incremental_finish_sha384
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_23_EverCrypt_Hash_Incremental_finish_sha384" 

external _24_EverCrypt_Hash_Incremental_finish_sha512
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_24_EverCrypt_Hash_Incremental_finish_sha512" 

external _25_EverCrypt_Hash_Incremental_alg_of_state
  : _ CI.fatptr -> Unsigned.uint8
  = "_25_EverCrypt_Hash_Incremental_alg_of_state" 

external _26_EverCrypt_Hash_Incremental_finish
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
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
  (fun x1 -> _27_EverCrypt_Hash_Incremental_free (CI.cptr x1))
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish" ->
  (fun x2 x3 -> _26_EverCrypt_Hash_Incremental_finish (CI.cptr x2) x3)
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x5; _})),
  "EverCrypt_Hash_Incremental_alg_of_state" ->
  (fun x4 -> x5 (_25_EverCrypt_Hash_Incremental_alg_of_state (CI.cptr x4)))
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha512" ->
  (fun x6 x7 -> _24_EverCrypt_Hash_Incremental_finish_sha512 (CI.cptr x6) x7)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha384" ->
  (fun x8 x9 -> _23_EverCrypt_Hash_Incremental_finish_sha384 (CI.cptr x8) x9)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha256" ->
  (fun x10 x11 ->
    _22_EverCrypt_Hash_Incremental_finish_sha256 (CI.cptr x10) x11)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha224" ->
  (fun x12 x13 ->
    _21_EverCrypt_Hash_Incremental_finish_sha224 (CI.cptr x12) x13)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_sha1" ->
  (fun x14 x15 ->
    _20_EverCrypt_Hash_Incremental_finish_sha1 (CI.cptr x14) x15)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_Incremental_finish_md5" ->
  (fun x16 x17 ->
    _19_EverCrypt_Hash_Incremental_finish_md5 (CI.cptr x16) x17)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_Incremental_update" ->
  (fun x18 x19 x20 ->
    _18_EverCrypt_Hash_Incremental_update (CI.cptr x18) x19 x20)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_Incremental_init" ->
  (fun x21 -> _17_EverCrypt_Hash_Incremental_init (CI.cptr x21))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x23; _},
     Returns (CI.Pointer x25)),
  "EverCrypt_Hash_Incremental_create_in" ->
  (fun x22 ->
    let x24 = x23 x22 in
    CI.make_ptr x25 (_16_EverCrypt_Hash_Incremental_create_in x24))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x27; _},
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_hash" ->
  (fun x26 x29 x30 x31 ->
    let x28 = x27 x26 in _15_EverCrypt_Hash_hash x28 x29 x30 x31)
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
  (fun x38 x39 -> _12_EverCrypt_Hash_copy (CI.cptr x38) (CI.cptr x39))
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_free" ->
  (fun x40 -> _11_EverCrypt_Hash_free (CI.cptr x40))
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_finish" ->
  (fun x41 x42 -> _10_EverCrypt_Hash_finish (CI.cptr x41) x42)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint64_t, Returns CI.Void))),
  "EverCrypt_Hash_update_last" ->
  (fun x43 x44 x45 -> _9_EverCrypt_Hash_update_last (CI.cptr x43) x44 x45)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "EverCrypt_Hash_update_last_256" ->
  (fun x46 x47 x48 x49 ->
    _8_EverCrypt_Hash_update_last_256 (CI.cptr x46) x47 x48 x49)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_update_multi" ->
  (fun x50 x51 x52 -> _7_EverCrypt_Hash_update_multi (CI.cptr x50) x51 x52)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "EverCrypt_Hash_update" ->
  (fun x53 x54 -> _6_EverCrypt_Hash_update (CI.cptr x53) x54)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "EverCrypt_Hash_update_multi_256" ->
  (fun x55 x56 x57 ->
    _5_EverCrypt_Hash_update_multi_256 (CI.cptr x55) x56 x57)
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_Hash_init" ->
  (fun x58 -> _4_EverCrypt_Hash_init (CI.cptr x58))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x60; _},
     Returns (CI.Pointer x62)),
  "EverCrypt_Hash_create" ->
  (fun x59 ->
    let x61 = x60 x59 in CI.make_ptr x62 (_3_EverCrypt_Hash_create x61))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x64; _},
     Returns (CI.Pointer x66)),
  "EverCrypt_Hash_create_in" ->
  (fun x63 ->
    let x65 = x64 x63 in CI.make_ptr x66 (_2_EverCrypt_Hash_create_in x65))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x68; _})),
  "EverCrypt_Hash_alg_of_state" ->
  (fun x67 -> x68 (_1_EverCrypt_Hash_alg_of_state (CI.cptr x67)))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
