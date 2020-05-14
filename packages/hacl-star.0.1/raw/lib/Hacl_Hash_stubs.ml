module CI = Cstubs_internals

external _1_Hacl_Hash_MD5_legacy_update_multi
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_1_Hacl_Hash_MD5_legacy_update_multi" 

external _2_Hacl_Hash_MD5_legacy_update_last
  : _ CI.fatptr -> Unsigned.uint64 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    unit = "_2_Hacl_Hash_MD5_legacy_update_last" 

external _3_Hacl_Hash_MD5_legacy_hash
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_3_Hacl_Hash_MD5_legacy_hash" 

external _4_Hacl_Hash_Core_MD5_legacy_init : _ CI.fatptr -> unit
  = "_4_Hacl_Hash_Core_MD5_legacy_init" 

external _5_Hacl_Hash_Core_MD5_legacy_update
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_5_Hacl_Hash_Core_MD5_legacy_update" 

external _6_Hacl_Hash_Core_MD5_legacy_pad
  : Unsigned.uint64 -> Bytes.t CI.ocaml -> unit
  = "_6_Hacl_Hash_Core_MD5_legacy_pad" 

external _7_Hacl_Hash_Core_MD5_legacy_finish
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_7_Hacl_Hash_Core_MD5_legacy_finish" 

external _8_Hacl_Hash_SHA1_legacy_update_multi
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_8_Hacl_Hash_SHA1_legacy_update_multi" 

external _9_Hacl_Hash_SHA1_legacy_update_last
  : _ CI.fatptr -> Unsigned.uint64 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    unit = "_9_Hacl_Hash_SHA1_legacy_update_last" 

external _10_Hacl_Hash_SHA1_legacy_hash
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_10_Hacl_Hash_SHA1_legacy_hash" 

external _11_Hacl_Hash_Core_SHA1_legacy_init : _ CI.fatptr -> unit
  = "_11_Hacl_Hash_Core_SHA1_legacy_init" 

external _12_Hacl_Hash_Core_SHA1_legacy_update
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_12_Hacl_Hash_Core_SHA1_legacy_update" 

external _13_Hacl_Hash_Core_SHA1_legacy_pad
  : Unsigned.uint64 -> Bytes.t CI.ocaml -> unit
  = "_13_Hacl_Hash_Core_SHA1_legacy_pad" 

external _14_Hacl_Hash_Core_SHA1_legacy_finish
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_14_Hacl_Hash_Core_SHA1_legacy_finish" 

external _15_Hacl_Hash_SHA2_update_multi_224
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_15_Hacl_Hash_SHA2_update_multi_224" 

external _16_Hacl_Hash_SHA2_update_multi_256
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_16_Hacl_Hash_SHA2_update_multi_256" 

external _17_Hacl_Hash_SHA2_update_multi_384
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_17_Hacl_Hash_SHA2_update_multi_384" 

external _18_Hacl_Hash_SHA2_update_multi_512
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_18_Hacl_Hash_SHA2_update_multi_512" 

external _19_Hacl_Hash_SHA2_update_last_224
  : _ CI.fatptr -> Unsigned.uint64 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    unit = "_19_Hacl_Hash_SHA2_update_last_224" 

external _20_Hacl_Hash_SHA2_update_last_256
  : _ CI.fatptr -> Unsigned.uint64 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    unit = "_20_Hacl_Hash_SHA2_update_last_256" 

external _21_Hacl_Hash_SHA2_hash_224
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_21_Hacl_Hash_SHA2_hash_224" 

external _22_Hacl_Hash_SHA2_hash_256
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_22_Hacl_Hash_SHA2_hash_256" 

external _23_Hacl_Hash_SHA2_hash_384
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_23_Hacl_Hash_SHA2_hash_384" 

external _24_Hacl_Hash_SHA2_hash_512
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_24_Hacl_Hash_SHA2_hash_512" 

external _25_Hacl_Hash_Core_SHA2_init_224 : _ CI.fatptr -> unit
  = "_25_Hacl_Hash_Core_SHA2_init_224" 

external _26_Hacl_Hash_Core_SHA2_init_256 : _ CI.fatptr -> unit
  = "_26_Hacl_Hash_Core_SHA2_init_256" 

external _27_Hacl_Hash_Core_SHA2_init_384 : _ CI.fatptr -> unit
  = "_27_Hacl_Hash_Core_SHA2_init_384" 

external _28_Hacl_Hash_Core_SHA2_init_512 : _ CI.fatptr -> unit
  = "_28_Hacl_Hash_Core_SHA2_init_512" 

external _29_Hacl_Hash_Core_SHA2_update_224
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_29_Hacl_Hash_Core_SHA2_update_224" 

external _30_Hacl_Hash_Core_SHA2_update_256
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_30_Hacl_Hash_Core_SHA2_update_256" 

external _31_Hacl_Hash_Core_SHA2_update_384
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_31_Hacl_Hash_Core_SHA2_update_384" 

external _32_Hacl_Hash_Core_SHA2_update_512
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_32_Hacl_Hash_Core_SHA2_update_512" 

external _33_Hacl_Hash_Core_SHA2_pad_224
  : Unsigned.uint64 -> Bytes.t CI.ocaml -> unit
  = "_33_Hacl_Hash_Core_SHA2_pad_224" 

external _34_Hacl_Hash_Core_SHA2_pad_256
  : Unsigned.uint64 -> Bytes.t CI.ocaml -> unit
  = "_34_Hacl_Hash_Core_SHA2_pad_256" 

external _35_Hacl_Hash_Core_SHA2_finish_224
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_35_Hacl_Hash_Core_SHA2_finish_224" 

external _36_Hacl_Hash_Core_SHA2_finish_256
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_36_Hacl_Hash_Core_SHA2_finish_256" 

external _37_Hacl_Hash_Core_SHA2_finish_384
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_37_Hacl_Hash_Core_SHA2_finish_384" 

external _38_Hacl_Hash_Core_SHA2_finish_512
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_38_Hacl_Hash_Core_SHA2_finish_512" 

external _39_Hacl_Hash_Definitions_word_len
  : Unsigned.uint8 -> Unsigned.uint32 = "_39_Hacl_Hash_Definitions_word_len" 

external _40_Hacl_Hash_Definitions_block_len
  : Unsigned.uint8 -> Unsigned.uint32 = "_40_Hacl_Hash_Definitions_block_len" 

external _41_Hacl_Hash_Definitions_hash_word_len
  : Unsigned.uint8 -> Unsigned.uint32
  = "_41_Hacl_Hash_Definitions_hash_word_len" 

external _42_Hacl_Hash_Definitions_hash_len
  : Unsigned.uint8 -> Unsigned.uint32 = "_42_Hacl_Hash_Definitions_hash_len" 

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
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x2; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_hash_len" ->
  (fun x1 -> let x3 = x2 x1 in _42_Hacl_Hash_Definitions_hash_len x3)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x5; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_hash_word_len" ->
  (fun x4 -> let x6 = x5 x4 in _41_Hacl_Hash_Definitions_hash_word_len x6)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x8; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_block_len" ->
  (fun x7 -> let x9 = x8 x7 in _40_Hacl_Hash_Definitions_block_len x9)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x11; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_word_len" ->
  (fun x10 -> let x12 = x11 x10 in _39_Hacl_Hash_Definitions_word_len x12)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_512" ->
  (fun x13 x14 -> _38_Hacl_Hash_Core_SHA2_finish_512 (CI.cptr x13) x14)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_384" ->
  (fun x15 x16 -> _37_Hacl_Hash_Core_SHA2_finish_384 (CI.cptr x15) x16)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_256" ->
  (fun x17 x18 -> _36_Hacl_Hash_Core_SHA2_finish_256 (CI.cptr x17) x18)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_224" ->
  (fun x19 x20 -> _35_Hacl_Hash_Core_SHA2_finish_224 (CI.cptr x19) x20)
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_pad_256" -> _34_Hacl_Hash_Core_SHA2_pad_256
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_pad_224" -> _33_Hacl_Hash_Core_SHA2_pad_224
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_512" ->
  (fun x25 x26 -> _32_Hacl_Hash_Core_SHA2_update_512 (CI.cptr x25) x26)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_384" ->
  (fun x27 x28 -> _31_Hacl_Hash_Core_SHA2_update_384 (CI.cptr x27) x28)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_256" ->
  (fun x29 x30 -> _30_Hacl_Hash_Core_SHA2_update_256 (CI.cptr x29) x30)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_224" ->
  (fun x31 x32 -> _29_Hacl_Hash_Core_SHA2_update_224 (CI.cptr x31) x32)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_512" ->
  (fun x33 -> _28_Hacl_Hash_Core_SHA2_init_512 (CI.cptr x33))
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_384" ->
  (fun x34 -> _27_Hacl_Hash_Core_SHA2_init_384 (CI.cptr x34))
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_256" ->
  (fun x35 -> _26_Hacl_Hash_Core_SHA2_init_256 (CI.cptr x35))
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_224" ->
  (fun x36 -> _25_Hacl_Hash_Core_SHA2_init_224 (CI.cptr x36))
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_512" -> _24_Hacl_Hash_SHA2_hash_512
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_384" -> _23_Hacl_Hash_SHA2_hash_384
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_256" -> _22_Hacl_Hash_SHA2_hash_256
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_224" -> _21_Hacl_Hash_SHA2_hash_224
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_SHA2_update_last_256" ->
  (fun x49 x50 x51 x52 ->
    _20_Hacl_Hash_SHA2_update_last_256 (CI.cptr x49) x50 x51 x52)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_SHA2_update_last_224" ->
  (fun x53 x54 x55 x56 ->
    _19_Hacl_Hash_SHA2_update_last_224 (CI.cptr x53) x54 x55 x56)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_512" ->
  (fun x57 x58 x59 ->
    _18_Hacl_Hash_SHA2_update_multi_512 (CI.cptr x57) x58 x59)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_384" ->
  (fun x60 x61 x62 ->
    _17_Hacl_Hash_SHA2_update_multi_384 (CI.cptr x60) x61 x62)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_256" ->
  (fun x63 x64 x65 ->
    _16_Hacl_Hash_SHA2_update_multi_256 (CI.cptr x63) x64 x65)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_224" ->
  (fun x66 x67 x68 ->
    _15_Hacl_Hash_SHA2_update_multi_224 (CI.cptr x66) x67 x68)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA1_legacy_finish" ->
  (fun x69 x70 -> _14_Hacl_Hash_Core_SHA1_legacy_finish (CI.cptr x69) x70)
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA1_legacy_pad" -> _13_Hacl_Hash_Core_SHA1_legacy_pad
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA1_legacy_update" ->
  (fun x73 x74 -> _12_Hacl_Hash_Core_SHA1_legacy_update (CI.cptr x73) x74)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA1_legacy_init" ->
  (fun x75 -> _11_Hacl_Hash_Core_SHA1_legacy_init (CI.cptr x75))
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA1_legacy_hash" -> _10_Hacl_Hash_SHA1_legacy_hash
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_SHA1_legacy_update_last" ->
  (fun x79 x80 x81 x82 ->
    _9_Hacl_Hash_SHA1_legacy_update_last (CI.cptr x79) x80 x81 x82)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA1_legacy_update_multi" ->
  (fun x83 x84 x85 ->
    _8_Hacl_Hash_SHA1_legacy_update_multi (CI.cptr x83) x84 x85)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_MD5_legacy_finish" ->
  (fun x86 x87 -> _7_Hacl_Hash_Core_MD5_legacy_finish (CI.cptr x86) x87)
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_MD5_legacy_pad" -> _6_Hacl_Hash_Core_MD5_legacy_pad
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_MD5_legacy_update" ->
  (fun x90 x91 -> _5_Hacl_Hash_Core_MD5_legacy_update (CI.cptr x90) x91)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_MD5_legacy_init" ->
  (fun x92 -> _4_Hacl_Hash_Core_MD5_legacy_init (CI.cptr x92))
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_MD5_legacy_hash" -> _3_Hacl_Hash_MD5_legacy_hash
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_MD5_legacy_update_last" ->
  (fun x96 x97 x98 x99 ->
    _2_Hacl_Hash_MD5_legacy_update_last (CI.cptr x96) x97 x98 x99)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_MD5_legacy_update_multi" ->
  (fun x100 x101 x102 ->
    _1_Hacl_Hash_MD5_legacy_update_multi (CI.cptr x100) x101 x102)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
