module CI = Cstubs_internals

external _1_Hacl_Bignum64_add
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr ->
    Unsigned.uint64 = "_1_Hacl_Bignum64_add" 

external _2_Hacl_Bignum64_sub
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr ->
    Unsigned.uint64 = "_2_Hacl_Bignum64_sub" 

external _3_Hacl_Bignum64_mul
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_3_Hacl_Bignum64_mul" 

external _4_Hacl_Bignum64_sqr
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_4_Hacl_Bignum64_sqr" 

external _5_Hacl_Bignum64_mod
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> bool
  = "_5_Hacl_Bignum64_mod" 

external _6_Hacl_Bignum64_mod_exp_vartime
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 ->
    _ CI.fatptr -> _ CI.fatptr -> bool
  =
  "_6_Hacl_Bignum64_mod_exp_vartime_byte6" "_6_Hacl_Bignum64_mod_exp_vartime" 

external _7_Hacl_Bignum64_mod_exp_consttime
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 ->
    _ CI.fatptr -> _ CI.fatptr -> bool
  =
  "_7_Hacl_Bignum64_mod_exp_consttime_byte6" "_7_Hacl_Bignum64_mod_exp_consttime"
  

external _8_Hacl_Bignum64_mod_inv_prime_vartime
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> bool
  = "_8_Hacl_Bignum64_mod_inv_prime_vartime" 

external _9_Hacl_Bignum64_mont_ctx_init
  : Unsigned.uint32 -> _ CI.fatptr -> CI.voidp
  = "_9_Hacl_Bignum64_mont_ctx_init" 

external _10_Hacl_Bignum64_mont_ctx_free : _ CI.fatptr -> unit
  = "_10_Hacl_Bignum64_mont_ctx_free" 

external _11_Hacl_Bignum64_mod_precomp
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_11_Hacl_Bignum64_mod_precomp" 

external _12_Hacl_Bignum64_mod_exp_vartime_precomp
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> unit = "_12_Hacl_Bignum64_mod_exp_vartime_precomp" 

external _13_Hacl_Bignum64_mod_exp_consttime_precomp
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> unit = "_13_Hacl_Bignum64_mod_exp_consttime_precomp" 

external _14_Hacl_Bignum64_mod_inv_prime_vartime_precomp
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_14_Hacl_Bignum64_mod_inv_prime_vartime_precomp" 

external _15_Hacl_Bignum64_new_bn_from_bytes_be
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_15_Hacl_Bignum64_new_bn_from_bytes_be" 

external _16_Hacl_Bignum64_new_bn_from_bytes_le
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_16_Hacl_Bignum64_new_bn_from_bytes_le" 

external _17_Hacl_Bignum64_bn_to_bytes_be
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_17_Hacl_Bignum64_bn_to_bytes_be" 

external _18_Hacl_Bignum64_bn_to_bytes_le
  : Unsigned.uint32 -> _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_18_Hacl_Bignum64_bn_to_bytes_le" 

external _19_Hacl_Bignum64_lt_mask
  : Unsigned.uint32 -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64
  = "_19_Hacl_Bignum64_lt_mask" 

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
       (CI.Pointer _,
        Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_Bignum64_lt_mask" ->
  (fun x1 x2 x4 ->
    let CI.CPointer x5 = x4 in
    let CI.CPointer x3 = x2 in _19_Hacl_Bignum64_lt_mask x1 x3 x5)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Bignum64_bn_to_bytes_le" ->
  (fun x6 x7 x9 ->
    let CI.CPointer x8 = x7 in _18_Hacl_Bignum64_bn_to_bytes_le x6 x8 x9)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Bignum64_bn_to_bytes_be" ->
  (fun x10 x11 x13 ->
    let CI.CPointer x12 = x11 in _17_Hacl_Bignum64_bn_to_bytes_be x10 x12 x13)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x16))),
  "Hacl_Bignum64_new_bn_from_bytes_le" ->
  (fun x14 x15 ->
    CI.make_ptr x16 (_16_Hacl_Bignum64_new_bn_from_bytes_le x14 x15))
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x19))),
  "Hacl_Bignum64_new_bn_from_bytes_be" ->
  (fun x17 x18 ->
    CI.make_ptr x19 (_15_Hacl_Bignum64_new_bn_from_bytes_be x17 x18))
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Bignum64_mod_inv_prime_vartime_precomp" ->
  (fun x20 x22 x24 ->
    let CI.CPointer x25 = x24 in
    let CI.CPointer x23 = x22 in
    let CI.CPointer x21 = x20 in
    _14_Hacl_Bignum64_mod_inv_prime_vartime_precomp x21 x23 x25)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))))),
  "Hacl_Bignum64_mod_exp_consttime_precomp" ->
  (fun x26 x28 x30 x31 x33 ->
    let CI.CPointer x34 = x33 in
    let CI.CPointer x32 = x31 in
    let CI.CPointer x29 = x28 in
    let CI.CPointer x27 = x26 in
    _13_Hacl_Bignum64_mod_exp_consttime_precomp x27 x29 x30 x32 x34)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))))),
  "Hacl_Bignum64_mod_exp_vartime_precomp" ->
  (fun x35 x37 x39 x40 x42 ->
    let CI.CPointer x43 = x42 in
    let CI.CPointer x41 = x40 in
    let CI.CPointer x38 = x37 in
    let CI.CPointer x36 = x35 in
    _12_Hacl_Bignum64_mod_exp_vartime_precomp x36 x38 x39 x41 x43)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Bignum64_mod_precomp" ->
  (fun x44 x46 x48 ->
    let CI.CPointer x49 = x48 in
    let CI.CPointer x47 = x46 in
    let CI.CPointer x45 = x44 in _11_Hacl_Bignum64_mod_precomp x45 x47 x49)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Bignum64_mont_ctx_free" ->
  (fun x50 ->
    let CI.CPointer x51 = x50 in _10_Hacl_Bignum64_mont_ctx_free x51)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Returns (CI.Pointer x55))),
  "Hacl_Bignum64_mont_ctx_init" ->
  (fun x52 x53 ->
    let CI.CPointer x54 = x53 in
    CI.make_ptr x55 (_9_Hacl_Bignum64_mont_ctx_init x52 x54))
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))))),
  "Hacl_Bignum64_mod_inv_prime_vartime" ->
  (fun x56 x57 x59 x61 ->
    let CI.CPointer x62 = x61 in
    let CI.CPointer x60 = x59 in
    let CI.CPointer x58 = x57 in
    _8_Hacl_Bignum64_mod_inv_prime_vartime x56 x58 x60 x62)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function
             (CI.Primitive CI.Uint32_t,
              Function
                (CI.Pointer _,
                 Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))))))),
  "Hacl_Bignum64_mod_exp_consttime" ->
  (fun x63 x64 x66 x68 x69 x71 ->
    let CI.CPointer x72 = x71 in
    let CI.CPointer x70 = x69 in
    let CI.CPointer x67 = x66 in
    let CI.CPointer x65 = x64 in
    _7_Hacl_Bignum64_mod_exp_consttime x63 x65 x67 x68 x70 x72)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function
             (CI.Primitive CI.Uint32_t,
              Function
                (CI.Pointer _,
                 Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))))))),
  "Hacl_Bignum64_mod_exp_vartime" ->
  (fun x73 x74 x76 x78 x79 x81 ->
    let CI.CPointer x82 = x81 in
    let CI.CPointer x80 = x79 in
    let CI.CPointer x77 = x76 in
    let CI.CPointer x75 = x74 in
    _6_Hacl_Bignum64_mod_exp_vartime x73 x75 x77 x78 x80 x82)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))))),
  "Hacl_Bignum64_mod" ->
  (fun x83 x84 x86 x88 ->
    let CI.CPointer x89 = x88 in
    let CI.CPointer x87 = x86 in
    let CI.CPointer x85 = x84 in _5_Hacl_Bignum64_mod x83 x85 x87 x89)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Bignum64_sqr" ->
  (fun x90 x91 x93 ->
    let CI.CPointer x94 = x93 in
    let CI.CPointer x92 = x91 in _4_Hacl_Bignum64_sqr x90 x92 x94)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)))),
  "Hacl_Bignum64_mul" ->
  (fun x95 x96 x98 x100 ->
    let CI.CPointer x101 = x100 in
    let CI.CPointer x99 = x98 in
    let CI.CPointer x97 = x96 in _3_Hacl_Bignum64_mul x95 x97 x99 x101)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t))))),
  "Hacl_Bignum64_sub" ->
  (fun x102 x103 x105 x107 ->
    let CI.CPointer x108 = x107 in
    let CI.CPointer x106 = x105 in
    let CI.CPointer x104 = x103 in _2_Hacl_Bignum64_sub x102 x104 x106 x108)
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.Pointer _,
        Function
          (CI.Pointer _,
           Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t))))),
  "Hacl_Bignum64_add" ->
  (fun x109 x110 x112 x114 ->
    let CI.CPointer x115 = x114 in
    let CI.CPointer x113 = x112 in
    let CI.CPointer x111 = x110 in _1_Hacl_Bignum64_add x109 x111 x113 x115)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
