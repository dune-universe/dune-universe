module CI = Cstubs_internals

external _1_Hacl_Hash_Core_Blake2_update_blake2s_32
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint64
  = "_1_Hacl_Hash_Core_Blake2_update_blake2s_32" 

external _2_Hacl_Hash_Core_Blake2_finish_blake2s_32
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> unit
  = "_2_Hacl_Hash_Core_Blake2_finish_blake2s_32" 

external _3_Hacl_Hash_Blake2_update_multi_blake2s_32
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    Unsigned.uint64 = "_3_Hacl_Hash_Blake2_update_multi_blake2s_32" 

external _4_Hacl_Hash_Blake2_update_last_blake2s_32
  : _ CI.fatptr -> Unsigned.uint64 -> Unsigned.uint64 -> bytes CI.ocaml ->
    Unsigned.uint32 -> Unsigned.uint64
  = "_4_Hacl_Hash_Blake2_update_last_blake2s_32" 

external _5_Hacl_Hash_Blake2_hash_blake2s_32
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_5_Hacl_Hash_Blake2_hash_blake2s_32" 

external _6_Hacl_Hash_Blake2_hash_blake2b_32
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_6_Hacl_Hash_Blake2_hash_blake2b_32" 

external _7_Hacl_Hash_MD5_legacy_update_multi
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_7_Hacl_Hash_MD5_legacy_update_multi" 

external _8_Hacl_Hash_MD5_legacy_update_last
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_8_Hacl_Hash_MD5_legacy_update_last" 

external _9_Hacl_Hash_MD5_legacy_hash
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_9_Hacl_Hash_MD5_legacy_hash" 

external _10_Hacl_Hash_Core_MD5_legacy_init : _ CI.fatptr -> unit
  = "_10_Hacl_Hash_Core_MD5_legacy_init" 

external _11_Hacl_Hash_Core_MD5_legacy_update
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_11_Hacl_Hash_Core_MD5_legacy_update" 

external _12_Hacl_Hash_Core_MD5_legacy_pad
  : Unsigned.uint64 -> bytes CI.ocaml -> unit
  = "_12_Hacl_Hash_Core_MD5_legacy_pad" 

external _13_Hacl_Hash_Core_MD5_legacy_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_13_Hacl_Hash_Core_MD5_legacy_finish" 

external _14_Hacl_Hash_SHA1_legacy_update_multi
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_14_Hacl_Hash_SHA1_legacy_update_multi" 

external _15_Hacl_Hash_SHA1_legacy_update_last
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_15_Hacl_Hash_SHA1_legacy_update_last" 

external _16_Hacl_Hash_SHA1_legacy_hash
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_16_Hacl_Hash_SHA1_legacy_hash" 

external _17_Hacl_Hash_Core_SHA1_legacy_init : _ CI.fatptr -> unit
  = "_17_Hacl_Hash_Core_SHA1_legacy_init" 

external _18_Hacl_Hash_Core_SHA1_legacy_update
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_18_Hacl_Hash_Core_SHA1_legacy_update" 

external _19_Hacl_Hash_Core_SHA1_legacy_pad
  : Unsigned.uint64 -> bytes CI.ocaml -> unit
  = "_19_Hacl_Hash_Core_SHA1_legacy_pad" 

external _20_Hacl_Hash_Core_SHA1_legacy_finish
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_20_Hacl_Hash_Core_SHA1_legacy_finish" 

external _21_Hacl_Hash_SHA2_update_multi_224
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_21_Hacl_Hash_SHA2_update_multi_224" 

external _22_Hacl_Hash_SHA2_update_multi_256
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_22_Hacl_Hash_SHA2_update_multi_256" 

external _23_Hacl_Hash_SHA2_update_multi_384
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_23_Hacl_Hash_SHA2_update_multi_384" 

external _24_Hacl_Hash_SHA2_update_multi_512
  : _ CI.fatptr -> bytes CI.ocaml -> Unsigned.uint32 -> unit
  = "_24_Hacl_Hash_SHA2_update_multi_512" 

external _25_Hacl_Hash_SHA2_update_last_224
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_25_Hacl_Hash_SHA2_update_last_224" 

external _26_Hacl_Hash_SHA2_update_last_256
  : _ CI.fatptr -> Unsigned.uint64 -> bytes CI.ocaml -> Unsigned.uint32 ->
    unit = "_26_Hacl_Hash_SHA2_update_last_256" 

external _27_Hacl_Hash_SHA2_hash_224
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_27_Hacl_Hash_SHA2_hash_224" 

external _28_Hacl_Hash_SHA2_hash_256
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_28_Hacl_Hash_SHA2_hash_256" 

external _29_Hacl_Hash_SHA2_hash_384
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_29_Hacl_Hash_SHA2_hash_384" 

external _30_Hacl_Hash_SHA2_hash_512
  : bytes CI.ocaml -> Unsigned.uint32 -> bytes CI.ocaml -> unit
  = "_30_Hacl_Hash_SHA2_hash_512" 

external _31_Hacl_Hash_Core_SHA2_init_224 : _ CI.fatptr -> unit
  = "_31_Hacl_Hash_Core_SHA2_init_224" 

external _32_Hacl_Hash_Core_SHA2_init_256 : _ CI.fatptr -> unit
  = "_32_Hacl_Hash_Core_SHA2_init_256" 

external _33_Hacl_Hash_Core_SHA2_init_384 : _ CI.fatptr -> unit
  = "_33_Hacl_Hash_Core_SHA2_init_384" 

external _34_Hacl_Hash_Core_SHA2_init_512 : _ CI.fatptr -> unit
  = "_34_Hacl_Hash_Core_SHA2_init_512" 

external _35_Hacl_Hash_Core_SHA2_update_224
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_35_Hacl_Hash_Core_SHA2_update_224" 

external _36_Hacl_Hash_Core_SHA2_update_256
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_36_Hacl_Hash_Core_SHA2_update_256" 

external _37_Hacl_Hash_Core_SHA2_update_384
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_37_Hacl_Hash_Core_SHA2_update_384" 

external _38_Hacl_Hash_Core_SHA2_update_512
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_38_Hacl_Hash_Core_SHA2_update_512" 

external _39_Hacl_Hash_Core_SHA2_pad_224
  : Unsigned.uint64 -> bytes CI.ocaml -> unit
  = "_39_Hacl_Hash_Core_SHA2_pad_224" 

external _40_Hacl_Hash_Core_SHA2_pad_256
  : Unsigned.uint64 -> bytes CI.ocaml -> unit
  = "_40_Hacl_Hash_Core_SHA2_pad_256" 

external _41_Hacl_Hash_Core_SHA2_finish_224
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_41_Hacl_Hash_Core_SHA2_finish_224" 

external _42_Hacl_Hash_Core_SHA2_finish_256
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_42_Hacl_Hash_Core_SHA2_finish_256" 

external _43_Hacl_Hash_Core_SHA2_finish_384
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_43_Hacl_Hash_Core_SHA2_finish_384" 

external _44_Hacl_Hash_Core_SHA2_finish_512
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_44_Hacl_Hash_Core_SHA2_finish_512" 

external _45_Hacl_Hash_Definitions_word_len
  : Unsigned.uint8 -> Unsigned.uint32 = "_45_Hacl_Hash_Definitions_word_len" 

external _46_Hacl_Hash_Definitions_block_len
  : Unsigned.uint8 -> Unsigned.uint32 = "_46_Hacl_Hash_Definitions_block_len" 

external _47_Hacl_Hash_Definitions_hash_word_len
  : Unsigned.uint8 -> Unsigned.uint32
  = "_47_Hacl_Hash_Definitions_hash_word_len" 

external _48_Hacl_Hash_Definitions_hash_len
  : Unsigned.uint8 -> Unsigned.uint32 = "_48_Hacl_Hash_Definitions_hash_len" 

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
  (fun x1 -> let x3 = x2 x1 in _48_Hacl_Hash_Definitions_hash_len x3)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x5; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_hash_word_len" ->
  (fun x4 -> let x6 = x5 x4 in _47_Hacl_Hash_Definitions_hash_word_len x6)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x8; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_block_len" ->
  (fun x7 -> let x9 = x8 x7 in _46_Hacl_Hash_Definitions_block_len x9)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x11; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_Hash_Definitions_word_len" ->
  (fun x10 -> let x12 = x11 x10 in _45_Hacl_Hash_Definitions_word_len x12)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_512" ->
  (fun x13 x15 ->
    let CI.CPointer x14 = x13 in _44_Hacl_Hash_Core_SHA2_finish_512 x14 x15)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_384" ->
  (fun x16 x18 ->
    let CI.CPointer x17 = x16 in _43_Hacl_Hash_Core_SHA2_finish_384 x17 x18)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_256" ->
  (fun x19 x21 ->
    let CI.CPointer x20 = x19 in _42_Hacl_Hash_Core_SHA2_finish_256 x20 x21)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_finish_224" ->
  (fun x22 x24 ->
    let CI.CPointer x23 = x22 in _41_Hacl_Hash_Core_SHA2_finish_224 x23 x24)
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_pad_256" -> _40_Hacl_Hash_Core_SHA2_pad_256
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_pad_224" -> _39_Hacl_Hash_Core_SHA2_pad_224
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_512" ->
  (fun x29 x31 ->
    let CI.CPointer x30 = x29 in _38_Hacl_Hash_Core_SHA2_update_512 x30 x31)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_384" ->
  (fun x32 x34 ->
    let CI.CPointer x33 = x32 in _37_Hacl_Hash_Core_SHA2_update_384 x33 x34)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_256" ->
  (fun x35 x37 ->
    let CI.CPointer x36 = x35 in _36_Hacl_Hash_Core_SHA2_update_256 x36 x37)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA2_update_224" ->
  (fun x38 x40 ->
    let CI.CPointer x39 = x38 in _35_Hacl_Hash_Core_SHA2_update_224 x39 x40)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_512" ->
  (fun x41 ->
    let CI.CPointer x42 = x41 in _34_Hacl_Hash_Core_SHA2_init_512 x42)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_384" ->
  (fun x43 ->
    let CI.CPointer x44 = x43 in _33_Hacl_Hash_Core_SHA2_init_384 x44)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_256" ->
  (fun x45 ->
    let CI.CPointer x46 = x45 in _32_Hacl_Hash_Core_SHA2_init_256 x46)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA2_init_224" ->
  (fun x47 ->
    let CI.CPointer x48 = x47 in _31_Hacl_Hash_Core_SHA2_init_224 x48)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_512" -> _30_Hacl_Hash_SHA2_hash_512
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_384" -> _29_Hacl_Hash_SHA2_hash_384
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_256" -> _28_Hacl_Hash_SHA2_hash_256
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA2_hash_224" -> _27_Hacl_Hash_SHA2_hash_224
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_SHA2_update_last_256" ->
  (fun x61 x63 x64 x65 ->
    let CI.CPointer x62 = x61 in
    _26_Hacl_Hash_SHA2_update_last_256 x62 x63 x64 x65)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_SHA2_update_last_224" ->
  (fun x66 x68 x69 x70 ->
    let CI.CPointer x67 = x66 in
    _25_Hacl_Hash_SHA2_update_last_224 x67 x68 x69 x70)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_512" ->
  (fun x71 x73 x74 ->
    let CI.CPointer x72 = x71 in
    _24_Hacl_Hash_SHA2_update_multi_512 x72 x73 x74)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_384" ->
  (fun x75 x77 x78 ->
    let CI.CPointer x76 = x75 in
    _23_Hacl_Hash_SHA2_update_multi_384 x76 x77 x78)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_256" ->
  (fun x79 x81 x82 ->
    let CI.CPointer x80 = x79 in
    _22_Hacl_Hash_SHA2_update_multi_256 x80 x81 x82)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA2_update_multi_224" ->
  (fun x83 x85 x86 ->
    let CI.CPointer x84 = x83 in
    _21_Hacl_Hash_SHA2_update_multi_224 x84 x85 x86)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA1_legacy_finish" ->
  (fun x87 x89 ->
    let CI.CPointer x88 = x87 in
    _20_Hacl_Hash_Core_SHA1_legacy_finish x88 x89)
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA1_legacy_pad" -> _19_Hacl_Hash_Core_SHA1_legacy_pad
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_SHA1_legacy_update" ->
  (fun x92 x94 ->
    let CI.CPointer x93 = x92 in
    _18_Hacl_Hash_Core_SHA1_legacy_update x93 x94)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_SHA1_legacy_init" ->
  (fun x95 ->
    let CI.CPointer x96 = x95 in _17_Hacl_Hash_Core_SHA1_legacy_init x96)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_SHA1_legacy_hash" -> _16_Hacl_Hash_SHA1_legacy_hash
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_SHA1_legacy_update_last" ->
  (fun x100 x102 x103 x104 ->
    let CI.CPointer x101 = x100 in
    _15_Hacl_Hash_SHA1_legacy_update_last x101 x102 x103 x104)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_SHA1_legacy_update_multi" ->
  (fun x105 x107 x108 ->
    let CI.CPointer x106 = x105 in
    _14_Hacl_Hash_SHA1_legacy_update_multi x106 x107 x108)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_MD5_legacy_finish" ->
  (fun x109 x111 ->
    let CI.CPointer x110 = x109 in
    _13_Hacl_Hash_Core_MD5_legacy_finish x110 x111)
| Function
    (CI.Primitive CI.Uint64_t, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_MD5_legacy_pad" -> _12_Hacl_Hash_Core_MD5_legacy_pad
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Hash_Core_MD5_legacy_update" ->
  (fun x114 x116 ->
    let CI.CPointer x115 = x114 in
    _11_Hacl_Hash_Core_MD5_legacy_update x115 x116)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Hash_Core_MD5_legacy_init" ->
  (fun x117 ->
    let CI.CPointer x118 = x117 in _10_Hacl_Hash_Core_MD5_legacy_init x118)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_MD5_legacy_hash" -> _9_Hacl_Hash_MD5_legacy_hash
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Hash_MD5_legacy_update_last" ->
  (fun x122 x124 x125 x126 ->
    let CI.CPointer x123 = x122 in
    _8_Hacl_Hash_MD5_legacy_update_last x123 x124 x125 x126)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Hash_MD5_legacy_update_multi" ->
  (fun x127 x129 x130 ->
    let CI.CPointer x128 = x127 in
    _7_Hacl_Hash_MD5_legacy_update_multi x128 x129 x130)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_Blake2_hash_blake2b_32" -> _6_Hacl_Hash_Blake2_hash_blake2b_32
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_Blake2_hash_blake2s_32" -> _5_Hacl_Hash_Blake2_hash_blake2s_32
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.Primitive CI.Uint64_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Returns (CI.Primitive CI.Uint64_t)))))),
  "Hacl_Hash_Blake2_update_last_blake2s_32" ->
  (fun x137 x139 x140 x141 x142 ->
    let CI.CPointer x138 = x137 in
    _4_Hacl_Hash_Blake2_update_last_blake2s_32 x138 x139 x140 x141 x142)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Uint64_t))))),
  "Hacl_Hash_Blake2_update_multi_blake2s_32" ->
  (fun x143 x145 x146 x147 ->
    let CI.CPointer x144 = x143 in
    _3_Hacl_Hash_Blake2_update_multi_blake2s_32 x144 x145 x146 x147)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Hash_Core_Blake2_finish_blake2s_32" ->
  (fun x148 x150 x151 ->
    let CI.CPointer x149 = x148 in
    _2_Hacl_Hash_Core_Blake2_finish_blake2s_32 x149 x150 x151)
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint64_t,
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_Hash_Core_Blake2_update_blake2s_32" ->
  (fun x152 x154 x155 ->
    let CI.CPointer x153 = x152 in
    _1_Hacl_Hash_Core_Blake2_update_blake2s_32 x153 x154 x155)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
