module CI = Cstubs_internals

external _1_Hacl_HMAC_DRBG_reseed_interval : unit -> CI.voidp
  = "_1_Hacl_HMAC_DRBG_reseed_interval" 

external _2_Hacl_HMAC_DRBG_max_output_length : unit -> CI.voidp
  = "_2_Hacl_HMAC_DRBG_max_output_length" 

external _3_Hacl_HMAC_DRBG_max_length : unit -> CI.voidp
  = "_3_Hacl_HMAC_DRBG_max_length" 

external _4_Hacl_HMAC_DRBG_max_personalization_string_length
  : unit -> CI.voidp = "_4_Hacl_HMAC_DRBG_max_personalization_string_length" 

external _5_Hacl_HMAC_DRBG_max_additional_input_length : unit -> CI.voidp
  = "_5_Hacl_HMAC_DRBG_max_additional_input_length" 

external _6_Hacl_HMAC_DRBG_min_length : Unsigned.uint8 -> Unsigned.uint32
  = "_6_Hacl_HMAC_DRBG_min_length" 

external _7_Hacl_HMAC_DRBG_create_in : Unsigned.uint8 -> CI.managed_buffer
  = "_7_Hacl_HMAC_DRBG_create_in" 

external _8_Hacl_HMAC_DRBG_instantiate
  : Unsigned.uint8 -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    Bytes.t CI.ocaml -> unit
  = "_8_Hacl_HMAC_DRBG_instantiate_byte8" "_8_Hacl_HMAC_DRBG_instantiate" 

external _9_Hacl_HMAC_DRBG_reseed
  : Unsigned.uint8 -> _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_9_Hacl_HMAC_DRBG_reseed_byte6" "_9_Hacl_HMAC_DRBG_reseed" 

external _10_Hacl_HMAC_DRBG_generate
  : Unsigned.uint8 -> Bytes.t CI.ocaml -> _ CI.fatptr -> Unsigned.uint32 ->
    Unsigned.uint32 -> Bytes.t CI.ocaml -> bool
  = "_10_Hacl_HMAC_DRBG_generate_byte6" "_10_Hacl_HMAC_DRBG_generate" 

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
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.Struct _,
           Function
             (CI.Primitive CI.Uint32_t,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Bool))))))),
  "Hacl_HMAC_DRBG_generate" ->
  (fun x1 x4 x5 x6 x7 x8 ->
    let x3 = x2 x1 in
    _10_Hacl_HMAC_DRBG_generate x3 x4 (CI.cptr (Ctypes.addr x5)) x6 x7 x8)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x10; _},
     Function
       (CI.Struct _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function (CI.OCaml CI.Bytes, Returns CI.Void)))))),
  "Hacl_HMAC_DRBG_reseed" ->
  (fun x9 x12 x13 x14 x15 x16 ->
    let x11 = x10 x9 in
    _9_Hacl_HMAC_DRBG_reseed x11 (CI.cptr (Ctypes.addr x12)) x13 x14 x15 x16)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x18; _},
     Function
       (CI.Struct _,
        Function
          (CI.Primitive CI.Uint32_t,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.OCaml CI.Bytes,
                    Function
                      (CI.Primitive CI.Uint32_t,
                       Function (CI.OCaml CI.Bytes, Returns CI.Void)))))))),
  "Hacl_HMAC_DRBG_instantiate" ->
  (fun x17 x20 x21 x22 x23 x24 x25 x26 ->
    let x19 = x18 x17 in
    _8_Hacl_HMAC_DRBG_instantiate x19 (CI.cptr (Ctypes.addr x20)) x21 x22 x23
    x24 x25 x26)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x28; _},
     Returns (CI.Struct _ as x30)),
  "Hacl_HMAC_DRBG_create_in" ->
  (fun x27 ->
    let x29 = x28 x27 in
    CI.make_structured x30 (_7_Hacl_HMAC_DRBG_create_in x29))
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x32; _},
     Returns (CI.Primitive CI.Uint32_t)),
  "Hacl_HMAC_DRBG_min_length" ->
  (fun x31 -> let x33 = x32 x31 in _6_Hacl_HMAC_DRBG_min_length x33)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| (CI.Primitive CI.Uint32_t as x34),
  "Hacl_HMAC_DRBG_max_additional_input_length" ->
  (CI.make_ptr x34 (_5_Hacl_HMAC_DRBG_max_additional_input_length ()))
| (CI.Primitive CI.Uint32_t as x35),
  "Hacl_HMAC_DRBG_max_personalization_string_length" ->
  (CI.make_ptr x35 (_4_Hacl_HMAC_DRBG_max_personalization_string_length ()))
| (CI.Primitive CI.Uint32_t as x36), "Hacl_HMAC_DRBG_max_length" ->
  (CI.make_ptr x36 (_3_Hacl_HMAC_DRBG_max_length ()))
| (CI.Primitive CI.Uint32_t as x37), "Hacl_HMAC_DRBG_max_output_length" ->
  (CI.make_ptr x37 (_2_Hacl_HMAC_DRBG_max_output_length ()))
| (CI.Primitive CI.Uint32_t as x38), "Hacl_HMAC_DRBG_reseed_interval" ->
  (CI.make_ptr x38 (_1_Hacl_HMAC_DRBG_reseed_interval ()))
| _, s ->  Printf.ksprintf failwith "No match for %s" s
