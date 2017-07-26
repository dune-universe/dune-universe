open Core
open Typerep_experimental.Std

let%test_module _ = (module struct

  let inverts_composition_is_ident typerep value : bool =
    let `generic to_typed = Tagged.Typed_of.of_typerep typerep in
    let `generic of_typed = Tagged.Of_typed.of_typerep typerep in
    Polymorphic_compare.equal value (to_typed (of_typed value))

  let check rep v = assert (inverts_composition_is_ident rep v)

  let%test_unit _ =
    let module M = struct
      type t = int [@@deriving typerep]
    end in
    check (M.typerep_of_t) 42

  let%test_unit _ =
    let module M = struct
      type t = int32 [@@deriving typerep]
    end in
    check (M.typerep_of_t) (Int32.of_int_exn 1337)

  let%test_unit _ =
    let module M = struct
      type t = int64 [@@deriving typerep]
    end in
    check (M.typerep_of_t) (Int64.of_int_exn 1980)

  let%test_unit _ =
    let module M = struct
      type t = char [@@deriving typerep]
    end in
    check (M.typerep_of_t) 'a'

  let%test_unit _ =
    let module M = struct
      type t = float [@@deriving typerep]
    end in
    check (M.typerep_of_t) 3.1415

  let%test_unit _ =
    let module M = struct
      type t = bool [@@deriving typerep]
    end in
    check (M.typerep_of_t) true;
    check (M.typerep_of_t) false

  let%test_unit _ =
    let module M = struct
      type t = string [@@deriving typerep]
    end in
    check (M.typerep_of_t) "foo"

  let%test_unit _ =
    let module M = struct
      type t = unit [@@deriving typerep]
    end in
    check (M.typerep_of_t) ()

  let%test_unit _ =
    let module M = struct
      type t = int option [@@deriving typerep]
    end in
    check (M.typerep_of_t) (None) ;
    check (M.typerep_of_t) (Some 2012)

  let%test_unit _ =
    let module M = struct
      type t = string list [@@deriving typerep]
    end in
    check (M.typerep_of_t) [] ;
    check (M.typerep_of_t) ["foo"; "bar"; "baz"]

  let%test_unit _ =
    let module M = struct
      type t = bool array [@@deriving typerep]
    end in
    check (M.typerep_of_t) [||] ;
    check (M.typerep_of_t) [| true ; false ; true |]

  let%test_unit _ =
    let module M = struct
      type 'a t = 'a ref [@@deriving typerep]
    end in
    check (M.typerep_of_t typerep_of_int) (ref 0)

  let%test_unit _ =
    let module M = struct
      type ('a, 'b) t = { foo : 'a ; bar : 'b ; baz : unit } [@@deriving typerep]
    end in
    check (M.typerep_of_t typerep_of_int typerep_of_char) { M. foo=0 ; bar='a' ; baz=() }

  let%test_unit _ =
    let module M = struct
      type t = int * int [@@deriving typerep]
    end in
    check (M.typerep_of_t) (1,2)

  let%test_unit _ =
    let module M = struct
      type t = int * int * int [@@deriving typerep]
    end in
    check (M.typerep_of_t) (1,2,3)

  let%test_unit _ =
    let module M = struct
      type t = int * int * int * int [@@deriving typerep]
    end in
    check (M.typerep_of_t) (1,2,3,4)

  let%test_unit _ =
    let module M = struct
      type t = int * int * int * int * int [@@deriving typerep]
    end in
    check (M.typerep_of_t) (1,2,3,4,5)

  let%test_unit _ =
    let module M = struct
      type 'a t = Nil | Cons of 'a * 'a t [@@deriving typerep]
    end in
    M.(check (typerep_of_t typerep_of_int) (Cons (1, Cons (2, Cons (3, Nil)))))

  let%test_unit _ =
    let module M = struct
      type china = unit [@@deriving typerep]
      type t = [ `The | `Republic of china ] [@@deriving typerep]
    end in
    M.(check typerep_of_t `The) ;
    M.(check typerep_of_t (`Republic ()))
end)
