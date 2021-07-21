module CI = Cstubs_internals

external _1_Hacl_EC_Ed25519_mk_felem_zero : _ CI.fatptr -> unit
  = "_1_Hacl_EC_Ed25519_mk_felem_zero" 

external _2_Hacl_EC_Ed25519_mk_felem_one : _ CI.fatptr -> unit
  = "_2_Hacl_EC_Ed25519_mk_felem_one" 

external _3_Hacl_EC_Ed25519_felem_add
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_3_Hacl_EC_Ed25519_felem_add" 

external _4_Hacl_EC_Ed25519_felem_sub
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_4_Hacl_EC_Ed25519_felem_sub" 

external _5_Hacl_EC_Ed25519_felem_mul
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_5_Hacl_EC_Ed25519_felem_mul" 

external _6_Hacl_EC_Ed25519_felem_inv : _ CI.fatptr -> _ CI.fatptr -> unit
  = "_6_Hacl_EC_Ed25519_felem_inv" 

external _7_Hacl_EC_Ed25519_felem_load
  : bytes CI.ocaml -> _ CI.fatptr -> unit = "_7_Hacl_EC_Ed25519_felem_load" 

external _8_Hacl_EC_Ed25519_felem_store
  : _ CI.fatptr -> bytes CI.ocaml -> unit = "_8_Hacl_EC_Ed25519_felem_store" 

external _9_Hacl_EC_Ed25519_mk_point_at_inf : _ CI.fatptr -> unit
  = "_9_Hacl_EC_Ed25519_mk_point_at_inf" 

external _10_Hacl_EC_Ed25519_mk_base_point : _ CI.fatptr -> unit
  = "_10_Hacl_EC_Ed25519_mk_base_point" 

external _11_Hacl_EC_Ed25519_point_negate
  : _ CI.fatptr -> _ CI.fatptr -> unit = "_11_Hacl_EC_Ed25519_point_negate" 

external _12_Hacl_EC_Ed25519_point_add
  : _ CI.fatptr -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_12_Hacl_EC_Ed25519_point_add" 

external _13_Hacl_EC_Ed25519_point_mul
  : bytes CI.ocaml -> _ CI.fatptr -> _ CI.fatptr -> unit
  = "_13_Hacl_EC_Ed25519_point_mul" 

external _14_Hacl_EC_Ed25519_point_eq : _ CI.fatptr -> _ CI.fatptr -> bool
  = "_14_Hacl_EC_Ed25519_point_eq" 

external _15_Hacl_EC_Ed25519_point_compress
  : _ CI.fatptr -> bytes CI.ocaml -> unit
  = "_15_Hacl_EC_Ed25519_point_compress" 

external _16_Hacl_EC_Ed25519_point_decompress
  : bytes CI.ocaml -> _ CI.fatptr -> bool
  = "_16_Hacl_EC_Ed25519_point_decompress" 

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
    (CI.OCaml CI.Bytes,
     Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))),
  "Hacl_EC_Ed25519_point_decompress" ->
  (fun x1 x2 ->
    let CI.CPointer x3 = x2 in _16_Hacl_EC_Ed25519_point_decompress x1 x3)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_EC_Ed25519_point_compress" ->
  (fun x4 x6 ->
    let CI.CPointer x5 = x4 in _15_Hacl_EC_Ed25519_point_compress x5 x6)
| Function
    (CI.Pointer _, Function (CI.Pointer _, Returns (CI.Primitive CI.Bool))),
  "Hacl_EC_Ed25519_point_eq" ->
  (fun x7 x9 ->
    let CI.CPointer x10 = x9 in
    let CI.CPointer x8 = x7 in _14_Hacl_EC_Ed25519_point_eq x8 x10)
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_EC_Ed25519_point_mul" ->
  (fun x11 x12 x14 ->
    let CI.CPointer x15 = x14 in
    let CI.CPointer x13 = x12 in _13_Hacl_EC_Ed25519_point_mul x11 x13 x15)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_EC_Ed25519_point_add" ->
  (fun x16 x18 x20 ->
    let CI.CPointer x21 = x20 in
    let CI.CPointer x19 = x18 in
    let CI.CPointer x17 = x16 in _12_Hacl_EC_Ed25519_point_add x17 x19 x21)
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_EC_Ed25519_point_negate" ->
  (fun x22 x24 ->
    let CI.CPointer x25 = x24 in
    let CI.CPointer x23 = x22 in _11_Hacl_EC_Ed25519_point_negate x23 x25)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_EC_Ed25519_mk_base_point" ->
  (fun x26 ->
    let CI.CPointer x27 = x26 in _10_Hacl_EC_Ed25519_mk_base_point x27)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_EC_Ed25519_mk_point_at_inf" ->
  (fun x28 ->
    let CI.CPointer x29 = x28 in _9_Hacl_EC_Ed25519_mk_point_at_inf x29)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_EC_Ed25519_felem_store" ->
  (fun x30 x32 ->
    let CI.CPointer x31 = x30 in _8_Hacl_EC_Ed25519_felem_store x31 x32)
| Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_EC_Ed25519_felem_load" ->
  (fun x33 x34 ->
    let CI.CPointer x35 = x34 in _7_Hacl_EC_Ed25519_felem_load x33 x35)
| Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_EC_Ed25519_felem_inv" ->
  (fun x36 x38 ->
    let CI.CPointer x39 = x38 in
    let CI.CPointer x37 = x36 in _6_Hacl_EC_Ed25519_felem_inv x37 x39)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_EC_Ed25519_felem_mul" ->
  (fun x40 x42 x44 ->
    let CI.CPointer x45 = x44 in
    let CI.CPointer x43 = x42 in
    let CI.CPointer x41 = x40 in _5_Hacl_EC_Ed25519_felem_mul x41 x43 x45)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_EC_Ed25519_felem_sub" ->
  (fun x46 x48 x50 ->
    let CI.CPointer x51 = x50 in
    let CI.CPointer x49 = x48 in
    let CI.CPointer x47 = x46 in _4_Hacl_EC_Ed25519_felem_sub x47 x49 x51)
| Function
    (CI.Pointer _,
     Function (CI.Pointer _, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_EC_Ed25519_felem_add" ->
  (fun x52 x54 x56 ->
    let CI.CPointer x57 = x56 in
    let CI.CPointer x55 = x54 in
    let CI.CPointer x53 = x52 in _3_Hacl_EC_Ed25519_felem_add x53 x55 x57)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_EC_Ed25519_mk_felem_one" ->
  (fun x58 ->
    let CI.CPointer x59 = x58 in _2_Hacl_EC_Ed25519_mk_felem_one x59)
| Function (CI.Pointer _, Returns CI.Void), "Hacl_EC_Ed25519_mk_felem_zero" ->
  (fun x60 ->
    let CI.CPointer x61 = x60 in _1_Hacl_EC_Ed25519_mk_felem_zero x61)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
