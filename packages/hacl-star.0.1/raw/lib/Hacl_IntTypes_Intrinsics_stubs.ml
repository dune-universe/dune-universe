module CI = Cstubs_internals

external _1_Hacl_IntTypes_Intrinsics_add_carry_u64
  : Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64 -> _ CI.fatptr ->
    Unsigned.uint64 = "_1_Hacl_IntTypes_Intrinsics_add_carry_u64" 

external _2_Hacl_IntTypes_Intrinsics_sub_borrow_u64
  : Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64 -> _ CI.fatptr ->
    Unsigned.uint64 = "_2_Hacl_IntTypes_Intrinsics_sub_borrow_u64" 

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
    (CI.Primitive CI.Uint64_t,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.Primitive CI.Uint64_t,
           Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t))))),
  "Hacl_IntTypes_Intrinsics_sub_borrow_u64" ->
  (fun x1 x2 x3 x4 ->
    _2_Hacl_IntTypes_Intrinsics_sub_borrow_u64 x1 x2 x3 (CI.cptr x4))
| Function
    (CI.Primitive CI.Uint64_t,
     Function
       (CI.Primitive CI.Uint64_t,
        Function
          (CI.Primitive CI.Uint64_t,
           Function (CI.Pointer _, Returns (CI.Primitive CI.Uint64_t))))),
  "Hacl_IntTypes_Intrinsics_add_carry_u64" ->
  (fun x5 x6 x7 x8 ->
    _1_Hacl_IntTypes_Intrinsics_add_carry_u64 x5 x6 x7 (CI.cptr x8))
| _, s ->  Printf.ksprintf failwith "No match for %s" s


let foreign_value : type a. string -> a Ctypes.typ -> a Ctypes.ptr =
  fun name t -> match t, name with
| _, s ->  Printf.ksprintf failwith "No match for %s" s
