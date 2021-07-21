module CI = Cstubs_internals

external _1_EverCrypt_StaticConfig_hacl : unit -> CI.voidp
  = "_1_EverCrypt_StaticConfig_hacl" 

external _2_EverCrypt_StaticConfig_vale : unit -> CI.voidp
  = "_2_EverCrypt_StaticConfig_vale" 

external _3_EverCrypt_StaticConfig_openssl : unit -> CI.voidp
  = "_3_EverCrypt_StaticConfig_openssl" 

external _4_EverCrypt_StaticConfig_bcrypt : unit -> CI.voidp
  = "_4_EverCrypt_StaticConfig_bcrypt" 

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
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| (CI.Primitive CI.Bool as x1), "EverCrypt_StaticConfig_bcrypt" ->
  (CI.make_ptr x1 (_4_EverCrypt_StaticConfig_bcrypt ()))
| (CI.Primitive CI.Bool as x2), "EverCrypt_StaticConfig_openssl" ->
  (CI.make_ptr x2 (_3_EverCrypt_StaticConfig_openssl ()))
| (CI.Primitive CI.Bool as x3), "EverCrypt_StaticConfig_vale" ->
  (CI.make_ptr x3 (_2_EverCrypt_StaticConfig_vale ()))
| (CI.Primitive CI.Bool as x4), "EverCrypt_StaticConfig_hacl" ->
  (CI.make_ptr x4 (_1_EverCrypt_StaticConfig_hacl ()))
| _, s ->  Printf.ksprintf failwith "No match for %s" s
