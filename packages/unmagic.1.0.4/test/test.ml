[@@@ocaml.warning "-32"]

open Unmagic
open Typerep_lib.Std

let test ?(sharing=false) name tyrep v =
  prerr_endline name;
  tag_check ~sharing tyrep (Obj.repr v);
  prerr_endline "ok"

let () = test "int" typerep_of_int 1

type t2 = int list [@@deriving typerep]
let () = test "int list" typerep_of_t2 [1;2;3]

type t4 = Foo of int * int | Bar of float | Zee [@@deriving typerep]

let () = test "char" typerep_of_char 'a'

let () = test "bool" typerep_of_bool true

let () = test "variant nullary" typerep_of_t4 Zee

let () = test "None" (typerep_of_option typerep_of_t4) None

let () = test "[]" (typerep_of_list typerep_of_t4) []

type t5 = t4 [@@deriving typerep]

let () = test "manifest (int)" typerep_of_t5 Zee

type t6 = t4 lazy_t [@@deriving typerep]

let () = test "Lazy.from_val (int)" (typerep_of_lazy_t typerep_of_t5) (Lazy.from_val Zee)

let () = test "variant unary" typerep_of_t4 (Bar 1.3)

let () = test "variant > unary" typerep_of_t4 (Foo (1,2))

type t7 = { x : int; y : float } [@@deriving typerep]
type t8 = { a : float; b : float } [@@deriving typerep]

let () = test "record" typerep_of_t7 { x = 1; y = 1.2 }
let () = test "record (float)" typerep_of_t8 { a = 1.0; b = 1.2 }

let () = test "string" typerep_of_string "hello"

let () = test "int32" typerep_of_int32 32l

let () = test "int64" typerep_of_int64 64L

let () = test "natint" typerep_of_nativeint 48n

let () = test "float" typerep_of_float 1.2
  
let () = test "Some _ " (typerep_of_option typerep_of_t4) (Some (Foo (1,2)))

let () = test "list" (typerep_of_list typerep_of_t4) [Foo (1,2); Bar 1.2; Zee]

let () = test "float array" (typerep_of_array typerep_of_float) [|1.2; 3.4; 5.6|]

let () = test "non float array" (typerep_of_array typerep_of_int) [|1;2;3|]

type t3 = int * int * int [@@deriving typerep]
let () = test "tuple" typerep_of_t3 (1,2,3)

let () = test "Lazy.from_val float" (typerep_of_lazy_t typerep_of_float) (Lazy.from_val 1.2)

let () = test "Lazy.from_val lazy" (typerep_of_lazy_t (typerep_of_lazy_t typerep_of_float)) (Lazy.from_val (Lazy.from_val 1.2))

let () = test "Lazy.from_val block" (typerep_of_lazy_t typerep_of_t4) (Lazy.from_val (Foo (1,2)))

let () = test "ref" (typerep_of_ref typerep_of_t4) (ref (Foo (1,2)))
  
let () = test "manifest (block)" typerep_of_t5 (Foo (1,2))

let rec ones = 1 :: ones
  
let () = test ~sharing:true "cycle" (typerep_of_list typerep_of_int) ones

type t9 = [`Foo | `Bar | `Zee of int | `Jar of float * float] [@@deriving typerep]
    
let () = test ~sharing:false "polyvar" typerep_of_t9 `Foo

let () = test ~sharing:false "polyvar" typerep_of_t9 (`Zee 1)

let () = test ~sharing:false "polyvar" typerep_of_t9 (`Jar (1.2, 3.4))
