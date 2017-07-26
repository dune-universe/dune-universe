open Core
open Typerep_experimental.Std
open Json_typerep.Jsonrep

module Jt = Json.Json_type

let%test_module _ = (module struct

  module type S = sig
    type t [@@deriving compare, sexp_of, typerep]
  end

  module Test_variant = struct
    type t =
      | Foo
      | Bar of int
      | Baz of float * float
    [@@deriving compare, sexp_of, typerep]
  end

  module Test_record = struct
    type t = {
      foo: int;
      bar: float option
    }
    [@@deriving compare, sexp_of, typerep]
  end

  module Test_tree = struct
    type t = Leaf | Node of t * t
    [@@deriving compare, sexp_of, typerep]
  end

  module Int_option = struct
    type t = int option [@@deriving compare, sexp_of, typerep]
  end

  module Int_list = struct
    type t = int list [@@deriving compare, sexp_of, typerep]
  end

  module Int_array = struct
    type t = int array [@@deriving compare, sexp_of, typerep]
  end

  module Test_tuple2 = struct
    type t = int * int [@@deriving compare, sexp_of, typerep]
  end

  module Test_tuple4 = struct
    type t = int * float * string * bool [@@deriving compare, sexp_of, typerep]
  end

  let test (type s) (module T : S with type t = s)
        (t_list : s list) (str_list : string list) =
    let open T in
    let roundtrip t_of_json json_of_t =
      (* roundtrip t -> json -> t *)
      let `generic of_json = t_of_json typerep_of_t in
      let `generic to_json = json_of_t typerep_of_t in
      List.iter t_list ~f:(fun t ->
        [%test_result: t] (to_json t |> of_json) ~expect:t
      );
      (* roundtrip for a very common use case: t -> json -> string -> json -> t *)
      let open Json.Json_io in
      let of_json_string str = json_of_string ~recursive:true str |> of_json in
      let to_json_string t = to_json t |> string_of_json ~recursive:true in
      List.iter str_list ~f:(fun str ->
        (* we do not start the roundtrip from the string because its details may change
           in the output *)
        let t = of_json_string str in
        [%test_result: t] (to_json_string t |> of_json_string) ~expect:t
      )
    in
    roundtrip V1.t_of_json V1.json_of_t;
    roundtrip V2.t_of_json V2.json_of_t;
    roundtrip V2.t_of_json V1.json_of_t
  ;;

  let%test_unit _ = test (module Int        ) [5]                       ["2"]
  let%test_unit _ = test (module Char       ) ['m']                     ["\"m\""]
  let%test_unit _ = test (module Float      ) [5.0]                     ["2"; "2.0"]
  let%test_unit _ = test (module String     ) ["hello, world"]          ["\"2\""]
  let%test_unit _ = test (module Bool       ) [true; false]             ["true"; "false"]
  let%test_unit _ = test (module Unit       ) [()]                      []
  let%test_unit _ = test (module Int_option ) [ None; Some 42 ]         ["42"]
  let%test_unit _ = test (module Int_list   ) [[ 1; 2; 3; 4; 5 ]]       ["[1,2,3]"]
  let%test_unit _ = test (module Int_array  ) [[|6; 7; 8; 9; 10|]]      ["[1,2,3]"]
  let%test_unit _ = test (module Test_tuple2) [(52, 78)]                ["[1,2]"]
  let%test_unit _ =
    test (module Test_tuple4) [(100, 3.14, "hi", true)] ["[100,3.14,\"hi\",true]"]
  let%test_unit _ =
    test (module Test_variant) Test_variant.[ Foo; Bar 9; Baz (6.2, 7.566)] []
  let%test_unit _ =
    test (module Test_record)
      Test_record.[ { foo = 5; bar = Some 76.2 } ]
      ["{ \"foo\": 5, \"bar\": 76.2 }"]
  let%test_unit _ =
    test (module Test_tree) Test_tree.[ Node ((Node ((Node (Leaf, Leaf)), Leaf)), Leaf) ]
      []
end)

let%test_module _ = (module struct
  module Jt = Json.Json_type

  type t = {
    a : int option;
  } [@@deriving typerep]

  let `generic t_of_json_v1    = V1.t_of_json typerep_of_t
  let `generic json_of_t_v1    = V1.json_of_t typerep_of_t

  let `generic t_of_json_v2    = V2.t_of_json typerep_of_t
  let `generic json_of_t_v2    = V2.json_of_t typerep_of_t

  module Ocaml = struct
    let some = { a = Some 42 }
    let none = { a = None }
  end

  module Json = struct
    let some      = Jt.Object [ "a",  Jt.Int 42 ]
    let none_with = Jt.Object [ "a",  Jt.Null ]
    let none_sans = Jt.Object []
  end

  let%test _ = json_of_t_v1   Ocaml.none      = Json.none_with
  let%test _ = json_of_t_v2   Ocaml.none      = Json.none_sans

  let%test _ = json_of_t_v1   Ocaml.some      = Json.some
  let%test _ = json_of_t_v2   Ocaml.some      = Json.some

  let%test _ = t_of_json_v2   Json.none_sans  = Ocaml.none

  let%test _ = t_of_json_v1   Json.none_with  = Ocaml.none
  let%test _ = t_of_json_v2   Json.none_with  = Ocaml.none

  let%test _ = t_of_json_v1   Json.some       = Ocaml.some
  let%test _ = t_of_json_v2   Json.some       = Ocaml.some

end)
