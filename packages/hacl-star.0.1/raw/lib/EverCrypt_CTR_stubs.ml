module CI = Cstubs_internals

external _1_EverCrypt_CTR_xor8
  : Unsigned.uint8 -> Unsigned.uint8 -> Unsigned.uint8
  = "_1_EverCrypt_CTR_xor8" 

external _2_EverCrypt_CTR_alg_of_state : _ CI.fatptr -> Unsigned.uint8
  = "_2_EverCrypt_CTR_alg_of_state" 

external _3_EverCrypt_CTR_create_in
  : Unsigned.uint8 -> _ CI.fatptr -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    Unsigned.uint32 -> Unsigned.uint32 -> Unsigned.uint8
  = "_3_EverCrypt_CTR_create_in_byte6" "_3_EverCrypt_CTR_create_in" 

external _4_EverCrypt_CTR_init
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    Unsigned.uint32 -> unit = "_4_EverCrypt_CTR_init" 

external _5_EverCrypt_CTR_update_block
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> unit
  = "_5_EverCrypt_CTR_update_block" 

external _6_EverCrypt_CTR_free : _ CI.fatptr -> unit
  = "_6_EverCrypt_CTR_free" 

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
| Function (CI.Pointer _, Returns CI.Void), "EverCrypt_CTR_free" ->
  (fun x1 -> _6_EverCrypt_CTR_free (CI.cptr x1))
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "EverCrypt_CTR_update_block" ->
  (fun x2 x3 x4 -> _5_EverCrypt_CTR_update_block (CI.cptr x2) x3 x4)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.Primitive CI.Uint32_t,
              Function (CI.Primitive CI.Uint32_t, Returns CI.Void))))),
  "EverCrypt_CTR_init" ->
  (fun x5 x6 x7 x8 x9 -> _4_EverCrypt_CTR_init (CI.cptr x5) x6 x7 x8 x9)
| Function
    (CI.View {CI.ty = CI.Primitive CI.Uint8_t; write = x11; _},
     Function
       (CI.Pointer _,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.Primitive CI.Uint32_t,
                 Function
                   (CI.Primitive CI.Uint32_t,
                    Returns
                      (CI.View
                         {CI.ty = CI.Primitive CI.Uint8_t; read = x18; _}))))))),
  "EverCrypt_CTR_create_in" ->
  (fun x10 x13 x14 x15 x16 x17 ->
    let x12 = x11 x10 in
    x18 (_3_EverCrypt_CTR_create_in x12 (CI.cptr x13) x14 x15 x16 x17))
| Function
    (CI.Pointer _,
     Returns (CI.View {CI.ty = CI.Primitive CI.Uint8_t; read = x20; _})),
  "EverCrypt_CTR_alg_of_state" ->
  (fun x19 -> x20 (_2_EverCrypt_CTR_alg_of_state (CI.cptr x19)))
| Function
    (CI.Primitive CI.Uint8_t,
     Function (CI.Primitive CI.Uint8_t, Returns (CI.Primitive CI.Uint8_t))),
  "EverCrypt_CTR_xor8" -> _1_EverCrypt_CTR_xor8
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
