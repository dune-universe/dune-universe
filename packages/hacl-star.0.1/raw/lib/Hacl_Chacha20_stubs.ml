module CI = Cstubs_internals

external _1_Hacl_Impl_Chacha20_chacha20_init
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint32 ->
    unit = "_1_Hacl_Impl_Chacha20_chacha20_init" 

external _2_Hacl_Impl_Chacha20_chacha20_encrypt_block
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    unit = "_2_Hacl_Impl_Chacha20_chacha20_encrypt_block" 

external _3_Hacl_Impl_Chacha20_chacha20_update
  : _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    unit = "_3_Hacl_Impl_Chacha20_chacha20_update" 

external _4_Hacl_Chacha20_chacha20_encrypt
  : Unsigned.uint32 -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  =
  "_4_Hacl_Chacha20_chacha20_encrypt_byte6" "_4_Hacl_Chacha20_chacha20_encrypt"
  

external _5_Hacl_Chacha20_chacha20_decrypt
  : Unsigned.uint32 -> Bytes.t CI.ocaml -> Bytes.t CI.ocaml ->
    Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  =
  "_5_Hacl_Chacha20_chacha20_decrypt_byte6" "_5_Hacl_Chacha20_chacha20_decrypt"
  

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
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes,
                 Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))))),
  "Hacl_Chacha20_chacha20_decrypt" -> _5_Hacl_Chacha20_chacha20_decrypt
| Function
    (CI.Primitive CI.Uint32_t,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function
             (CI.OCaml CI.Bytes,
              Function
                (CI.OCaml CI.Bytes,
                 Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))))),
  "Hacl_Chacha20_chacha20_encrypt" -> _4_Hacl_Chacha20_chacha20_encrypt
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)))),
  "Hacl_Impl_Chacha20_chacha20_update" ->
  (fun x13 x14 x15 x16 ->
    _3_Hacl_Impl_Chacha20_chacha20_update (CI.cptr x13) x14 x15 x16)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.Primitive CI.Uint32_t,
           Function (CI.OCaml CI.Bytes, Returns CI.Void)))),
  "Hacl_Impl_Chacha20_chacha20_encrypt_block" ->
  (fun x17 x18 x19 x20 ->
    _2_Hacl_Impl_Chacha20_chacha20_encrypt_block (CI.cptr x17) x18 x19 x20)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.Primitive CI.Uint32_t, Returns CI.Void)))),
  "Hacl_Impl_Chacha20_chacha20_init" ->
  (fun x21 x22 x23 x24 ->
    _1_Hacl_Impl_Chacha20_chacha20_init (CI.cptr x21) x22 x23 x24)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
