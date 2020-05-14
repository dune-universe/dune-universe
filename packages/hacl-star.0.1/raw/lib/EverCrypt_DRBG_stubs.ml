module CI = Cstubs_internals

external _1_EverCrypt_DRBG_reseed_interval : unit -> CI.voidp
  = "_1_EverCrypt_DRBG_reseed_interval" 

external _2_EverCrypt_DRBG_max_output_length : unit -> CI.voidp
  = "_2_EverCrypt_DRBG_max_output_length" 

external _3_EverCrypt_DRBG_max_length : unit -> CI.voidp
  = "_3_EverCrypt_DRBG_max_length" 

external _4_EverCrypt_DRBG_max_personalization_string_length
  : unit -> CI.voidp = "_4_EverCrypt_DRBG_max_personalization_string_length" 

external _5_EverCrypt_DRBG_max_additional_input_length : unit -> CI.voidp
  = "_5_EverCrypt_DRBG_max_additional_input_length" 

external _6_EverCrypt_DRBG_min_length : Unsigned.uint8 -> Unsigned.uint32
  = "_6_EverCrypt_DRBG_min_length" 

external _7_EverCrypt_DRBG_create : Unsigned.uint8 -> CI.voidp
  = "_7_EverCrypt_DRBG_create" 

external _8_EverCrypt_DRBG_instantiate_sha1
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_8_EverCrypt_DRBG_instantiate_sha1" 

external _9_EverCrypt_DRBG_instantiate_sha2_256
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_9_EverCrypt_DRBG_instantiate_sha2_256" 

external _10_EverCrypt_DRBG_instantiate_sha2_384
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_10_EverCrypt_DRBG_instantiate_sha2_384" 

external _11_EverCrypt_DRBG_instantiate_sha2_512
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_11_EverCrypt_DRBG_instantiate_sha2_512" 

external _12_EverCrypt_DRBG_reseed_sha1
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_12_EverCrypt_DRBG_reseed_sha1" 

external _13_EverCrypt_DRBG_reseed_sha2_256
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_13_EverCrypt_DRBG_reseed_sha2_256" 

external _14_EverCrypt_DRBG_reseed_sha2_384
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_14_EverCrypt_DRBG_reseed_sha2_384" 

external _15_EverCrypt_DRBG_reseed_sha2_512
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_15_EverCrypt_DRBG_reseed_sha2_512" 

external _16_EverCrypt_DRBG_generate_sha1
  : Bytes.t CI.ocaml -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> bool = "_16_EverCrypt_DRBG_generate_sha1" 

external _17_EverCrypt_DRBG_generate_sha2_256
  : Bytes.t CI.ocaml -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> bool = "_17_EverCrypt_DRBG_generate_sha2_256" 

external _18_EverCrypt_DRBG_generate_sha2_384
  : Bytes.t CI.ocaml -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> bool = "_18_EverCrypt_DRBG_generate_sha2_384" 

external _19_EverCrypt_DRBG_generate_sha2_512
  : Bytes.t CI.ocaml -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> bool = "_19_EverCrypt_DRBG_generate_sha2_512" 

external _20_EverCrypt_DRBG_uninstantiate_sha1 : _ CI.fatptr -> unit
  = "_20_EverCrypt_DRBG_uninstantiate_sha1" 

external _21_EverCrypt_DRBG_uninstantiate_sha2_256 : _ CI.fatptr -> unit
  = "_21_EverCrypt_DRBG_uninstantiate_sha2_256" 

external _22_EverCrypt_DRBG_uninstantiate_sha2_384 : _ CI.fatptr -> unit
  = "_22_EverCrypt_DRBG_uninstantiate_sha2_384" 

external _23_EverCrypt_DRBG_uninstantiate_sha2_512 : _ CI.fatptr -> unit
  = "_23_EverCrypt_DRBG_uninstantiate_sha2_512" 

external _24_EverCrypt_DRBG_instantiate
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_24_EverCrypt_DRBG_instantiate" 

external _25_EverCrypt_DRBG_reseed
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> bool
  = "_25_EverCrypt_DRBG_reseed" 

external _26_EverCrypt_DRBG_generate
  : Bytes.t CI.ocaml -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> bool = "_26_EverCrypt_DRBG_generate" 

external _27_EverCrypt_DRBG_uninstantiate : _ CI.fatptr -> unit
  = "_27_EverCrypt_DRBG_uninstantiate" 

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
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_DRBG_uninstantiate" ->
  (fun x1 -> _27_EverCrypt_DRBG_uninstantiate (CI.cptr x1))
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))))),
  "EverCrypt_DRBG_generate" ->
  (fun x2 x3 x4 x5 x6 ->
    _26_EverCrypt_DRBG_generate x2 (CI.cptr x3) x4 x5 x6)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_reseed" ->
  (fun x7 x8 x9 -> _25_EverCrypt_DRBG_reseed (CI.cptr x7) x8 x9)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_instantiate" ->
  (fun x10 x11 x12 -> _24_EverCrypt_DRBG_instantiate (CI.cptr x10) x11 x12)
| Function (CI.Pointer _, Returns CI.Void),
  "EverCrypt_DRBG_uninstantiate_sha2_512" ->
  (fun x13 -> _23_EverCrypt_DRBG_uninstantiate_sha2_512 (CI.cptr x13))
| Function (CI.Pointer _, Returns CI.Void),
  "EverCrypt_DRBG_uninstantiate_sha2_384" ->
  (fun x14 -> _22_EverCrypt_DRBG_uninstantiate_sha2_384 (CI.cptr x14))
| Function (CI.Pointer _, Returns CI.Void),
  "EverCrypt_DRBG_uninstantiate_sha2_256" ->
  (fun x15 -> _21_EverCrypt_DRBG_uninstantiate_sha2_256 (CI.cptr x15))
| Function (CI.Pointer _, Returns CI.Void),
  "EverCrypt_DRBG_uninstantiate_sha1" ->
  (fun x16 -> _20_EverCrypt_DRBG_uninstantiate_sha1 (CI.cptr x16))
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))))),
  "EverCrypt_DRBG_generate_sha2_512" ->
  (fun x17 x18 x19 x20 x21 ->
    _19_EverCrypt_DRBG_generate_sha2_512 x17 (CI.cptr x18) x19 x20 x21)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))))),
  "EverCrypt_DRBG_generate_sha2_384" ->
  (fun x22 x23 x24 x25 x26 ->
    _18_EverCrypt_DRBG_generate_sha2_384 x22 (CI.cptr x23) x24 x25 x26)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))))),
  "EverCrypt_DRBG_generate_sha2_256" ->
  (fun x27 x28 x29 x30 x31 ->
    _17_EverCrypt_DRBG_generate_sha2_256 x27 (CI.cptr x28) x29 x30 x31)
| Function
    (CI.OCaml CI.Bytes,
     Function
       (CI.Pointer _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))))),
  "EverCrypt_DRBG_generate_sha1" ->
  (fun x32 x33 x34 x35 x36 ->
    _16_EverCrypt_DRBG_generate_sha1 x32 (CI.cptr x33) x34 x35 x36)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_reseed_sha2_512" ->
  (fun x37 x38 x39 ->
    _15_EverCrypt_DRBG_reseed_sha2_512 (CI.cptr x37) x38 x39)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_reseed_sha2_384" ->
  (fun x40 x41 x42 ->
    _14_EverCrypt_DRBG_reseed_sha2_384 (CI.cptr x40) x41 x42)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_reseed_sha2_256" ->
  (fun x43 x44 x45 ->
    _13_EverCrypt_DRBG_reseed_sha2_256 (CI.cptr x43) x44 x45)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_reseed_sha1" ->
  (fun x46 x47 x48 -> _12_EverCrypt_DRBG_reseed_sha1 (CI.cptr x46) x47 x48)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_instantiate_sha2_512" ->
  (fun x49 x50 x51 ->
    _11_EverCrypt_DRBG_instantiate_sha2_512 (CI.cptr x49) x50 x51)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_instantiate_sha2_384" ->
  (fun x52 x53 x54 ->
    _10_EverCrypt_DRBG_instantiate_sha2_384 (CI.cptr x52) x53 x54)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_instantiate_sha2_256" ->
  (fun x55 x56 x57 ->
    _9_EverCrypt_DRBG_instantiate_sha2_256 (CI.cptr x55) x56 x57)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns (CI.Primitive CI.Bool)))),
  "EverCrypt_DRBG_instantiate_sha1" ->
  (fun x58 x59 x60 ->
    _8_EverCrypt_DRBG_instantiate_sha1 (CI.cptr x58) x59 x60)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x62; _},
     Returns (CI.Pointer x64)),
  "EverCrypt_DRBG_create" ->
  (fun x61 ->
    let x63 = x62 x61 in CI.make_ptr x64 (_7_EverCrypt_DRBG_create x63))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x66; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "EverCrypt_DRBG_min_length" ->
  (fun x65 -> let x67 = x66 x65 in _6_EverCrypt_DRBG_min_length x67)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| (CI.Primitive CI.Uint32_t as x68),
  "EverCrypt_DRBG_max_additional_input_length" ->
  (CI.make_ptr x68 (_5_EverCrypt_DRBG_max_additional_input_length ()))
| (CI.Primitive CI.Uint32_t as x69),
  "EverCrypt_DRBG_max_personalization_string_length" ->
  (CI.make_ptr x69 (_4_EverCrypt_DRBG_max_personalization_string_length ()))
| (CI.Primitive CI.Uint32_t as x70), "EverCrypt_DRBG_max_length" ->
  (CI.make_ptr x70 (_3_EverCrypt_DRBG_max_length ()))
| (CI.Primitive CI.Uint32_t as x71), "EverCrypt_DRBG_max_output_length" ->
  (CI.make_ptr x71 (_2_EverCrypt_DRBG_max_output_length ()))
| (CI.Primitive CI.Uint32_t as x72), "EverCrypt_DRBG_reseed_interval" ->
  (CI.make_ptr x72 (_1_EverCrypt_DRBG_reseed_interval ()))
| _, s ->  Printf.ksprintf failwith "No match for %s" s
