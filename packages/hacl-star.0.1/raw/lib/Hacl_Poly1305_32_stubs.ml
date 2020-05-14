module CI = Cstubs_internals

external _1_Hacl_Poly1305_32_blocklen : unit -> CI.voidp
  = "_1_Hacl_Poly1305_32_blocklen" 

external _2_Hacl_Poly1305_32_poly1305_init
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_2_Hacl_Poly1305_32_poly1305_init" 

external _3_Hacl_Poly1305_32_poly1305_update1
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_3_Hacl_Poly1305_32_poly1305_update1" 

external _4_Hacl_Poly1305_32_poly1305_update
  : _ CI.fatptr -> Unsigned.uint32 -> Bytes.t CI.ocaml -> unit
  = "_4_Hacl_Poly1305_32_poly1305_update" 

external _5_Hacl_Poly1305_32_poly1305_finish
  : Bytes.t CI.ocaml -> Bytes.t CI.ocaml -> _ CI.fatptr -> unit
  = "_5_Hacl_Poly1305_32_poly1305_finish" 

external _6_Hacl_Poly1305_32_poly1305_mac
  : Bytes.t CI.ocaml -> Unsigned.uint32 -> Bytes.t CI.ocaml ->
    Bytes.t CI.ocaml -> unit = "_6_Hacl_Poly1305_32_poly1305_mac" 

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
     Function
       (CI.Primitive CI.Uint32_t,
        Function
          (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)))),
  "Hacl_Poly1305_32_poly1305_mac" -> _6_Hacl_Poly1305_32_poly1305_mac
| Function
    (CI.OCaml CI.Bytes,
     Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void))),
  "Hacl_Poly1305_32_poly1305_finish" ->
  (fun x5 x6 x7 -> _5_Hacl_Poly1305_32_poly1305_finish x5 x6 (CI.cptr x7))
| Function
    (CI.Pointer _,
     Function
       (CI.Primitive CI.Uint32_t,
        Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_Poly1305_32_poly1305_update" ->
  (fun x8 x9 x10 -> _4_Hacl_Poly1305_32_poly1305_update (CI.cptr x8) x9 x10)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Poly1305_32_poly1305_update1" ->
  (fun x11 x12 -> _3_Hacl_Poly1305_32_poly1305_update1 (CI.cptr x11) x12)
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Poly1305_32_poly1305_init" ->
  (fun x13 x14 -> _2_Hacl_Poly1305_32_poly1305_init (CI.cptr x13) x14)
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| (CI.Primitive CI.Uint32_t as x15), "Hacl_Poly1305_32_blocklen" ->
  (CI.make_ptr x15 (_1_Hacl_Poly1305_32_blocklen ()))
| _, s ->  Printf.ksprintf failwith "No match for %s" s
