module CI = Cstubs_internals

external _1_Hacl_Streaming_Poly1305_32_create_in
  : Bytes.t CI.ocaml -> CI.voidp = "_1_Hacl_Streaming_Poly1305_32_create_in" 

external _2_Hacl_Streaming_Poly1305_32_init
  : Bytes.t CI.ocaml -> _ CI.fatptr -> unit
  = "_2_Hacl_Streaming_Poly1305_32_init" 

external _3_Hacl_Streaming_Poly1305_32_update
  : _ CI.fatptr -> Bytes.t CI.ocaml -> Unsigned.uint32 -> unit
  = "_3_Hacl_Streaming_Poly1305_32_update" 

external _4_Hacl_Streaming_Poly1305_32_finish
  : _ CI.fatptr -> Bytes.t CI.ocaml -> unit
  = "_4_Hacl_Streaming_Poly1305_32_finish" 

external _5_Hacl_Streaming_Poly1305_32_free : _ CI.fatptr -> unit
  = "_5_Hacl_Streaming_Poly1305_32_free" 

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
| Function (CI.Pointer _, Returns CI.Void), "Hacl_Streaming_Poly1305_32_free" ->
  (fun x1 -> _5_Hacl_Streaming_Poly1305_32_free (CI.cptr x1))
| Function (CI.Pointer _, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_Streaming_Poly1305_32_finish" ->
  (fun x2 x3 -> _4_Hacl_Streaming_Poly1305_32_finish (CI.cptr x2) x3)
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function (CI.Primitive CI.Uint32_t, Returns CI.Void))),
  "Hacl_Streaming_Poly1305_32_update" ->
  (fun x4 x5 x6 -> _3_Hacl_Streaming_Poly1305_32_update (CI.cptr x4) x5 x6)
| Function (CI.OCaml CI.Bytes, Function (CI.Pointer _, Returns CI.Void)),
  "Hacl_Streaming_Poly1305_32_init" ->
  (fun x7 x8 -> _2_Hacl_Streaming_Poly1305_32_init x7 (CI.cptr x8))
| Function (CI.OCaml CI.Bytes, Returns (CI.Pointer x10)),
  "Hacl_Streaming_Poly1305_32_create_in" ->
  (fun x9 -> CI.make_ptr x10 (_1_Hacl_Streaming_Poly1305_32_create_in x9))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
