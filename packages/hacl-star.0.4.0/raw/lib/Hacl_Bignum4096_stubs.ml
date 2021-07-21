module CI = Cstubs_internals

external _1_Hacl_Bignum4096_add
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64
  = "_1_Hacl_Bignum4096_add" 

external _2_Hacl_Bignum4096_sub
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64
  = "_2_Hacl_Bignum4096_sub" 

external _3_Hacl_Bignum4096_mul
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_3_Hacl_Bignum4096_mul" 

external _4_Hacl_Bignum4096_sqr : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_4_Hacl_Bignum4096_sqr" 

external _5_Hacl_Bignum4096_mod
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> bool
  = "_5_Hacl_Bignum4096_mod" 

external _6_Hacl_Bignum4096_mod_exp_vartime
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> bool = "_6_Hacl_Bignum4096_mod_exp_vartime" 

external _7_Hacl_Bignum4096_mod_exp_consttime
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> bool = "_7_Hacl_Bignum4096_mod_exp_consttime" 

external _8_Hacl_Bignum4096_mod_inv_prime_vartime
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> bool
  = "_8_Hacl_Bignum4096_mod_inv_prime_vartime" 

external _9_Hacl_Bignum4096_mont_ctx_init : _ CI.fatptr -> CI.voidp
  = "_9_Hacl_Bignum4096_mont_ctx_init" 

external _10_Hacl_Bignum4096_mont_ctx_free : _ CI.fatptr -> unit
  = "_10_Hacl_Bignum4096_mont_ctx_free" 

external _11_Hacl_Bignum4096_mod_precomp
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_11_Hacl_Bignum4096_mod_precomp" 

external _12_Hacl_Bignum4096_mod_exp_vartime_precomp
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> unit = "_12_Hacl_Bignum4096_mod_exp_vartime_precomp" 

external _13_Hacl_Bignum4096_mod_exp_consttime_precomp
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint32 -> _ CI.fatptr ->
    _ CI.fatptr -> unit = "_13_Hacl_Bignum4096_mod_exp_consttime_precomp" 

external _14_Hacl_Bignum4096_mod_inv_prime_vartime_precomp
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_14_Hacl_Bignum4096_mod_inv_prime_vartime_precomp" 

external _15_Hacl_Bignum4096_new_bn_from_bytes_be
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_15_Hacl_Bignum4096_new_bn_from_bytes_be" 

external _16_Hacl_Bignum4096_new_bn_from_bytes_le
  : Unsigned.uint32 -> bytes CI.ocaml -> CI.voidp
  = "_16_Hacl_Bignum4096_new_bn_from_bytes_le" 

external _17_Hacl_Bignum4096_bn_to_bytes_be
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_17_Hacl_Bignum4096_bn_to_bytes_be" 

external _18_Hacl_Bignum4096_bn_to_bytes_le
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_18_Hacl_Bignum4096_bn_to_bytes_le" 

external _19_Hacl_Bignum4096_lt_mask
  : _ CI.fatptr -> _ CI.fatptr -> Unsigned.uint64
  = "_19_Hacl_Bignum4096_lt_mask" 

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
    (CI.Pointer _,
     Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t))),
  "Hacl_Bignum4096_lt_mask" ->
  (fun x1 x3 ->
    let CI.CPointer x4 = x3 in
    let CI.CPointer x2 = x1 in _19_Hacl_Bignum4096_lt_mask x2 x4)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Bignum4096_bn_to_bytes_le" ->
  (fun x5 x7 ->
    let CI.CPointer x6 = x5 in _18_Hacl_Bignum4096_bn_to_bytes_le x6 x7)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Bignum4096_bn_to_bytes_be" ->
  (fun x8 x10 ->
    let CI.CPointer x9 = x8 in _17_Hacl_Bignum4096_bn_to_bytes_be x9 x10)
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x13))),
  "Hacl_Bignum4096_new_bn_from_bytes_le" ->
  (fun x11 x12 ->
    CI.make_ptr x13 (_16_Hacl_Bignum4096_new_bn_from_bytes_le x11 x12))
| Function
    (CI.Primitive CI.Uint32_t,
     Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x16))),
  "Hacl_Bignum4096_new_bn_from_bytes_be" ->
  (fun x14 x15 ->
    CI.make_ptr x16 (_15_Hacl_Bignum4096_new_bn_from_bytes_be x14 x15))
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Bignum4096_mod_inv_prime_vartime_precomp" ->
  (fun x17 x19 x21 ->
    let CI.CPointer x22 = x21 in
    let CI.CPointer x20 = x19 in
    let CI.CPointer x18 = x17 in
    _14_Hacl_Bignum4096_mod_inv_prime_vartime_precomp x18 x20 x22)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))))),
  "Hacl_Bignum4096_mod_exp_consttime_precomp" ->
  (fun x23 x25 x27 x28 x30 ->
    let CI.CPointer x31 = x30 in
    let CI.CPointer x29 = x28 in
    let CI.CPointer x26 = x25 in
    let CI.CPointer x24 = x23 in
    _13_Hacl_Bignum4096_mod_exp_consttime_precomp x24 x26 x27 x29 x31)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))))),
  "Hacl_Bignum4096_mod_exp_vartime_precomp" ->
  (fun x32 x34 x36 x37 x39 ->
    let CI.CPointer x40 = x39 in
    let CI.CPointer x38 = x37 in
    let CI.CPointer x35 = x34 in
    let CI.CPointer x33 = x32 in
    _12_Hacl_Bignum4096_mod_exp_vartime_precomp x33 x35 x36 x38 x40)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Bignum4096_mod_precomp" ->
  (fun x41 x43 x45 ->
    let CI.CPointer x46 = x45 in
    let CI.CPointer x44 = x43 in
    let CI.CPointer x42 = x41 in _11_Hacl_Bignum4096_mod_precomp x42 x44 x46)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Bignum4096_mont_ctx_free" ->
  (fun x47 ->
    let CI.CPointer x48 = x47 in _10_Hacl_Bignum4096_mont_ctx_free x48)
| Function (CI.Pointer _, Returns (CI.Pointer x51)),
  "Hacl_Bignum4096_mont_ctx_init" ->
  (fun x49 ->
    let CI.CPointer x50 = x49 in
    CI.make_ptr x51 (_9_Hacl_Bignum4096_mont_ctx_init x50))
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function (CI.Pointer _, Returns (CI.Primitive CI.Bool)))),
  "Hacl_Bignum4096_mod_inv_prime_vartime" ->
  (fun x52 x54 x56 ->
    let CI.CPointer x57 = x56 in
    let CI.CPointer x55 = x54 in
    let CI.CPointer x53 = x52 in
    _8_Hacl_Bignum4096_mod_inv_prime_vartime x53 x55 x57)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.Pointer _,
              Function (CI.Pointer _, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_Bignum4096_mod_exp_consttime" ->
  (fun x58 x60 x62 x63 x65 ->
    let CI.CPointer x66 = x65 in
    let CI.CPointer x64 = x63 in
    let CI.CPointer x61 = x60 in
    let CI.CPointer x59 = x58 in
    _7_Hacl_Bignum4096_mod_exp_consttime x59 x61 x62 x64 x66)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.Pointer _,
              Function (CI.Pointer _, Returns (CI.Primitive CI.Bool)))))),
  "Hacl_Bignum4096_mod_exp_vartime" ->
  (fun x67 x69 x71 x72 x74 ->
    let CI.CPointer x75 = x74 in
    let CI.CPointer x73 = x72 in
    let CI.CPointer x70 = x69 in
    let CI.CPointer x68 = x67 in
    _6_Hacl_Bignum4096_mod_exp_vartime x68 x70 x71 x73 x75)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function (CI.Pointer _, Returns (CI.Primitive CI.Bool)))),
  "Hacl_Bignum4096_mod" ->
  (fun x76 x78 x80 ->
    let CI.CPointer x81 = x80 in
    let CI.CPointer x79 = x78 in
    let CI.CPointer x77 = x76 in _5_Hacl_Bignum4096_mod x77 x79 x81)
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Bignum4096_sqr" ->
  (fun x82 x84 ->
    let CI.CPointer x85 = x84 in
    let CI.CPointer x83 = x82 in _4_Hacl_Bignum4096_sqr x83 x85)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Bignum4096_mul" ->
  (fun x86 x88 x90 ->
    let CI.CPointer x91 = x90 in
    let CI.CPointer x89 = x88 in
    let CI.CPointer x87 = x86 in _3_Hacl_Bignum4096_mul x87 x89 x91)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_Bignum4096_sub" ->
  (fun x92 x94 x96 ->
    let CI.CPointer x97 = x96 in
    let CI.CPointer x95 = x94 in
    let CI.CPointer x93 = x92 in _2_Hacl_Bignum4096_sub x93 x95 x97)
| Function
    (CI.Pointer _,
     Function
       (CI.Pointer _,
        Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_Bignum4096_add" ->
  (fun x98 x100 x102 ->
    let CI.CPointer x103 = x102 in
    let CI.CPointer x101 = x100 in
    let CI.CPointer x99 = x98 in _1_Hacl_Bignum4096_add x99 x101 x103)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
