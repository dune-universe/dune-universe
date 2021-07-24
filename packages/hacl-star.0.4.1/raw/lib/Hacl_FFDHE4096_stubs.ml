module CI = Cstubs_internals

external _1_Hacl_FFDHE4096_new_ffdhe_precomp_p : unit -> CI.voidp
  = "_1_Hacl_FFDHE4096_new_ffdhe_precomp_p" 

external _2_Hacl_FFDHE4096_ffdhe_secret_to_public_precomp
  : _ CI.fatptr -> bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_2_Hacl_FFDHE4096_ffdhe_secret_to_public_precomp" 

external _3_Hacl_FFDHE4096_ffdhe_secret_to_public
  : bytes CI.ocaml -> bytes CI.ocaml -> unit
  = "_3_Hacl_FFDHE4096_ffdhe_secret_to_public" 

external _4_Hacl_FFDHE4096_ffdhe_shared_secret_precomp
  : _ CI.fatptr -> bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml ->
    Unsigned.uint64 = "_4_Hacl_FFDHE4096_ffdhe_shared_secret_precomp" 

external _5_Hacl_FFDHE4096_ffdhe_shared_secret
  : bytes CI.ocaml -> bytes CI.ocaml -> bytes CI.ocaml -> Unsigned.uint64
  = "_5_Hacl_FFDHE4096_ffdhe_shared_secret" 

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
       (CI.OCaml CI.Bytes,
        Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t)))),
  "Hacl_FFDHE4096_ffdhe_shared_secret" ->
  _5_Hacl_FFDHE4096_ffdhe_shared_secret
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes,
        Function
          (CI.OCaml CI.Bytes,
           Function (CI.OCaml CI.Bytes, Returns (CI.Primitive CI.Uint64_t))))),
  "Hacl_FFDHE4096_ffdhe_shared_secret_precomp" ->
  (fun x4 x6 x7 x8 ->
    let CI.CPointer x5 = x4 in
    _4_Hacl_FFDHE4096_ffdhe_shared_secret_precomp x5 x6 x7 x8)
| Function (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void)),
  "Hacl_FFDHE4096_ffdhe_secret_to_public" ->
  _3_Hacl_FFDHE4096_ffdhe_secret_to_public
| Function
    (CI.Pointer _,
     Function
       (CI.OCaml CI.Bytes, Function (CI.OCaml CI.Bytes, Returns CI.Void))),
  "Hacl_FFDHE4096_ffdhe_secret_to_public_precomp" ->
  (fun x11 x13 x14 ->
    let CI.CPointer x12 = x11 in
    _2_Hacl_FFDHE4096_ffdhe_secret_to_public_precomp x12 x13 x14)
| Function (CI.Void, Returns (CI.Pointer x16)),
  "Hacl_FFDHE4096_new_ffdhe_precomp_p" ->
  (fun x15 -> CI.make_ptr x16 (_1_Hacl_FFDHE4096_new_ffdhe_precomp_p x15))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
