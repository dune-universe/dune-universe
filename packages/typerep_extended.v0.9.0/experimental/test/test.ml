open Core
open Typerep_experimental.Std

let hash_variant = Typerep_obj.hash_variant

let print_rep name typerep =
  let sexp = Type_struct.sexp_of_typerep typerep in
  print_endline ("struct representation of "^name);
  print_endline (Sexp.to_string_hum sexp)

(* manual examples *)

let print_typestruct str =
  let sexp = Type_struct.sexp_of_t str in
  print_endline (Sexp.to_string_hum sexp)

module S = Type_struct
module V = S.Variant.Kind

let vr index name array =
  let array = Farray.of_array array ~f:(fun _ x -> x) in
  { S.Variant. label=name ; ocaml_repr = hash_variant name ; index ; args_labels = []}
, array
;;

let vu ?(args_labels = []) index ocaml_repr name array =
  let array = Farray.of_array array ~f:(fun _ x -> x) in
  { S.Variant. label=name ; index ; ocaml_repr; args_labels }, array
;;

let fields t = Farray.of_array t ~f:(fun index (label, value, is_mutable) ->
  let field = { S.Field.index ; label; is_mutable } in
  field, value)

let tags t = Farray.of_array t ~f:(fun _ x -> x)

let stuple array = S.Tuple (Farray.of_array array ~f:(fun _ x -> x))

let simple_array = { S.Record_infos.has_double_array_tag = false }
let double_array = { S.Record_infos.has_double_array_tag = true }

let polymorphic = { S.Variant_infos.kind = V.Polymorphic }
let usual = { S.Variant_infos.kind = V.Usual }

(* General tests about code generation + structure building *)
let%test_module _ = (module struct

  let base_check name expected typestruct =
    (* polymorphic equality is ok for Type_struct.t *)
    if Type_struct.are_equivalent typestruct expected then true else begin
      print_endline ("testing "^name);
      print_endline "expected:";
      print_typestruct expected;
      print_endline "built:";
      print_typestruct typestruct;
      false
    end

  let check expected typerep =
    let result = ref true in
    let typestruct = Type_struct.of_typerep typerep in
    result := base_check "typestruct" expected typestruct && !result;
    let Typerep.T typerep_of_t = Type_struct.to_typerep typestruct in
    let typestruct2 : Type_struct.t = Type_struct.of_typerep typerep_of_t in
    result := base_check "typerep" expected typestruct2 && !result;
    let check_version vn =
      let versioned =
        try
          Some (Type_struct.Versioned.serialize ~version:vn typestruct)
        with
        | Type_struct.Not_downgradable _ -> None
      in
      match versioned with
      | Some versioned ->
        let typestruct_of_t = Type_struct.Versioned.unserialize versioned in
        let name = Sexp.to_string (Type_struct.Versioned.Version.sexp_of_t vn) in
        result := base_check name expected typestruct_of_t && !result;
      | None -> ()
    in
    List.iter ~f:check_version Type_struct.Versioned.Version.([
      v1;
      v2;
      v3;
      v4;
      v5;
    ]);
    !result
  ;;

  (* simple cases *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = int
      [@@deriving typerep]
    end in
    let exp = S.Int in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: int])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = int32
      [@@deriving typerep]
    end in
    let exp = S.Int32 in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: int32])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = int64
      [@@deriving typerep]
    end in
    let exp = S.Int64 in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: int64])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = char
      [@@deriving typerep]
    end in
    let exp = S.Char in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: char])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float
      [@@deriving typerep]
    end in
    let exp = S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = string
      [@@deriving typerep]
    end in
    let exp = S.String in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: string])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool
      [@@deriving typerep]
    end in
    let exp = S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: bool])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = unit
      [@@deriving typerep]
    end in
    let exp = S.Unit in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: unit])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool option
      [@@deriving typerep]
    end in
    let exp = S.Option S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: bool option])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float option
      [@@deriving typerep]
    end in
    let exp = S.Option S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float option])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool list
      [@@deriving typerep]
    end in
    let exp = S.List S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: bool list])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float list
      [@@deriving typerep]
    end in
    let exp = S.List S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float list])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool array
      [@@deriving typerep]
    end in
    let exp = S.Array S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: bool array])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float array
      [@@deriving typerep]
    end in
    let exp = S.Array S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float array])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool lazy_t
      [@@deriving typerep]
    end in
    let exp = S.Lazy S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: bool lazy_t])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float lazy_t
      [@@deriving typerep]
    end in
    let exp = S.Lazy S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float lazy_t])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool ref
      [@@deriving typerep]
    end in
    let exp = S.Ref S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: bool ref])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float ref
      [@@deriving typerep]
    end in
    let exp = S.Ref S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float ref])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float * string
      [@@deriving typerep]
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float * string])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float * string * bool
      [@@deriving typerep]
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ; S.Bool ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float * string * bool])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float * string * bool * unit
      [@@deriving typerep]
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ; S.Bool ; S.Unit ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float * string * bool * unit])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = float * string * bool * unit * int
      [@@deriving typerep]
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ; S.Bool ; S.Unit ; S.Int ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: float * string * bool * unit * int])
  ;;


  (* nested with previous types *)

  let%test_unit _ =
    let module A : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool
      [@@deriving typerep]
    end in
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = A.t option
      [@@deriving typerep]
    end in
    let exp = S.Option S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: A.t option])
  ;;

  let%test_unit _ =
    let module A : sig
      type t [@@deriving typerep]
    end = struct
      type t = bool
      [@@deriving typerep]
    end in
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = A.t list
      [@@deriving typerep]
    end in
    let exp = S.List S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  (* records *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
      module A : sig
        type nonrec t = t [@@deriving typerep]
      end
    end = struct
      module T = struct
        type t = {
          float : float;
          string : string;
          bool : bool;
          unit : unit;
          int : int;
        } [@@deriving typerep]
      end
      module A = struct
        type t = T.t = {
          float : float;
          string : string;
          bool : bool;
          unit : unit;
          int : int;
        } [@@deriving typerep]
      end
      include T
    end in
    let exp = S.Record (simple_array, fields [|
      "float", S.Float, false;
      "string", S.String, false;
      "bool", S.Bool, false;
      "unit", S.Unit, false;
      "int", S.Int, false;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t]);
    assert (check exp M.A.typerep_of_t);
    assert (check exp [%typerep_of: M.A.t])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = {
        f1 : float;
        f2 : float;
      } [@@deriving typerep]
    end in
    let exp = S.Record (double_array, fields [|
      "f1", S.Float, false;
      "f2", S.Float, false;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type 'a poly = {
        f1 : float;
        f2 : 'a;
      } [@@deriving typerep]
      type t = float poly [@@deriving typerep]
    end in
    let exp = S.Record (simple_array, fields [|
      "f1", S.Float, false;
      "f2", S.Float, false;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  (* variants arity 1 *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = [
      | `float of float
      | `string of string
      | `bool of bool
      | `unit of unit
      | `int of int
      ] [@@deriving typerep]
    end in
    let exp = S.Variant (polymorphic, tags [|
      vr 0 "float" [| S.Float |];
      vr 1 "string" [| S.String |];
      vr 2 "bool" [| S.Bool |];
      vr 3 "unit" [| S.Unit |];
      vr 4 "int" [| S.Int |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  (* variants arity n *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = [
      | `zero
      | `one of unit
      | `two of bool * bool
      | `three of unit * unit * unit
      | `five of unit * unit * unit * unit * unit
      ] [@@deriving typerep]
    end in
    let exp = S.Variant (polymorphic, tags [|
      vr 0 "zero" [||];
      vr 1 "one" [| S.Unit |];
      vr 2 "two" [| S.Tuple (Farray.of_list [ S.Bool ; S.Bool ]) |];
      vr 3 "three" [| S.Tuple (Farray.of_list [ S.Unit ; S.Unit ; S.Unit ]) |];
      vr 4 "five" [| S.Tuple (Farray.init 5 ~f:(fun _ -> S.Unit)) |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  (* sum arity 1 *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t =
      | Float of float
      | String of string
      | Bool of bool
      | Unit of unit
      | Int of int
      [@@deriving typerep]
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "Float" [| S.Float |];
      vu 1 1 "String" [| S.String |];
      vu 2 2 "Bool" [| S.Bool |];
      vu 3 3 "Unit" [| S.Unit |];
      vu 4 4 "Int" [| S.Int |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  (* sum arity n *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t =
      | Zero
      | One of unit
      | Two of bool * bool
      | Two_tuple of (bool * bool)
      | Three of unit * unit * unit
      | Three_tuple of (unit * unit * unit)
      | Five of unit * unit * unit * unit * unit
      | Five_tuple of (unit * unit * unit * unit * unit)
      [@@deriving typerep]
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "Zero" [||];
      vu 1 0 "One" [| S.Unit |];
      vu 2 1 "Two" [| S.Bool ; S.Bool |];
      vu 3 2 "Two_tuple" [| S.Tuple (Farray.of_list [ S.Bool ; S.Bool ]) |];
      vu 4 3 "Three" [| S.Unit ; S.Unit ; S.Unit |];
      vu 5 4 "Three_tuple" [| S.Tuple (Farray.of_list [ S.Unit ; S.Unit ; S.Unit ]) |];
      vu 6 5 "Five" (Array.init 5 ~f:(fun _ -> S.Unit));
      vu 7 6 "Five_tuple" [| S.Tuple (Farray.init 5 ~f:(fun _ -> S.Unit)) |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: M.t])
  ;;

  (* inline record *)

  let%test_unit _ =
    let module M : sig
      type t [@@deriving typerep]
    end = struct
      type t = A of { x : int; y : string } [@@deriving typerep]
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "A" [| S.Int ; S.String |] ~args_labels:[ "x" ; "y" ] ;
    |]) in
    assert (check exp M.typerep_of_t);
  ;;

  let%test_unit _ =
    (* [Type_struct.to_typerep] makes the assumptions that those types have the same
       runtime representation.  Test this separately. *)
    let module M1 = struct
      type t = A of { x : int; y : string }
    end in
    let module M2 = struct
      type t = A of int * string
    end in
    let m1 = M1.A { x = 42; y = "y" } in
    let m2 = M2.A (42, "y") in
    assert (Obj.tag (Obj.repr m1) = Obj.tag (Obj.repr m2));
    assert (Pervasives.(=) (Obj.repr m1) (Obj.repr m2));
  ;;

  (* polymorphism *)

  (* records *)

  let%test_unit _ =
    let module A : sig
      type 'a t [@@deriving typerep]
    end = struct
      type 'a t = 'a * int [@@deriving typerep]
    end in
    let module M : sig
      type ('a, 'b, 'c) t [@@deriving typerep]
    end = struct
      type ('a, 'b, 'c) t = {
        a : 'a * 'a * 'a ;
        b : 'b;
        c : 'c;
        int : int;
        t_A : 'c A.t;
      } [@@deriving typerep]
    end in
    let module M1 : sig
      type t = (float, int A.t, int A.t A.t) M.t [@@deriving typerep]
    end = struct
      type t = (float, int A.t, int A.t A.t) M.t [@@deriving typerep]
    end in
    let exp = S.Record (simple_array, fields [|
      "a", S.Tuple (Farray.of_list [ S.Float; S.Float ; S.Float ]), false;
      "b", S.Tuple (Farray.of_list [ S.Int ; S.Int ]), false;
      "c", S.Tuple (Farray.of_list [ S.Tuple (Farray.of_list [ S.Int ; S.Int ])
                                   ; S.Int ]), false;
      "int", S.Int, false;
      "t_A", S.Tuple (Farray.of_list
        [ S.Tuple (Farray.of_list [ S.Tuple (Farray.of_list [ S.Int ; S.Int ]) ; S.Int ]);
          S.Int ]), false;
    |]) in
    assert (check exp M1.typerep_of_t);
    assert (check exp [%typerep_of: (float, int A.t, int A.t A.t) M.t])
  ;;

  (* variants *)

  let%test_unit _ =
    let module A : sig
      type 'a t [@@deriving typerep]
    end = struct
      type 'a t = [
      | `a of 'a
      | `int of int * string
      ] [@@deriving typerep]
    end in
    let module M : sig
      type ('a, 'b, 'c) t [@@deriving typerep]
    end = struct
      type ('a, 'b, 'c) t = [
      | `a of 'a * 'a * 'a
      | `b of 'b
      | `c of 'c
      | `int of int
      | `t_A of 'a A.t
      | `no_arg
      ] [@@deriving typerep]
    end in
    let module M1 : sig
      type t = (float, int A.t, int A.t A.t) M.t [@@deriving typerep]
    end = struct
      type t = (float, int A.t, int A.t A.t) M.t [@@deriving typerep]
    end in
    let exp = S.Variant (polymorphic, tags [|
      vr 0 "a" [| stuple [| S.Float; S.Float ; S.Float |] |];
      vr 1 "b" [| S.Variant (polymorphic, tags [|
        vr 0 "a" [| S.Int |] ;
        vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
      vr 2 "c" [| S.Variant (polymorphic, tags [|
        vr 0 "a" [| S.Variant (polymorphic, tags
                                [| vr 0 "a" [| S.Int |] ;
                                   vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
                           vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
      vr 3 "int" [| S.Int |];
      vr 4 "t_A" [| S.Variant (polymorphic, tags [| vr 0 "a" [| S.Float |] ;
                            vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
      vr 5 "no_arg" [||];
    |]) in
    assert (check exp M1.typerep_of_t);
    assert (check exp [%typerep_of: (float, int A.t, int A.t A.t) M.t])
  ;;

  (* sums *)

  let%test_unit _ =
    let module A : sig
      type 'a t [@@deriving typerep]
    end = struct
      type 'a t =
      | A of 'a
      | Int of int * string
      [@@deriving typerep]
    end in
    let module M : sig
      type ('a, 'b, 'c) t [@@deriving typerep]
    end = struct
      type ('a, 'b, 'c) t =
      | A of ('a * 'a * 'a)
      | B of 'b
      | C of 'c
      | Int of int
      | T_A of 'a A.t
      | No_arg
      [@@deriving typerep]
    end in
    let module M1 : sig
      type t = (float, int A.t, int A.t A.t) M.t [@@deriving typerep]
    end = struct
      type t = (float, int A.t, int A.t A.t) M.t [@@deriving typerep]
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "A" [| stuple [| S.Float; S.Float ; S.Float |] |];
      vu 1 1 "B" [| S.Variant (usual, tags [|
        vu 0 0 "A" [| S.Int |] ;
        vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
      vu 2 2 "C" [| S.Variant (usual, tags [| vu 0 0 "A"
                           [| S.Variant (usual, tags
                               [| vu 0 0 "A" [| S.Int |] ;
                                  vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
                           vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
      vu 3 3 "Int" [| S.Int |];
      vu 4 4 "T_A" [| S.Variant (usual, tags [| vu 0 0 "A" [| S.Float |] ;
                            vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
      vu 5 0 "No_arg" [||];
    |]) in
    assert (check exp M1.typerep_of_t);
    assert (check exp [%typerep_of: (float, int A.t, int A.t A.t) M.t])
  ;;

  (* phantom and mutability *)
  let%test_unit _ =
    let module A = struct
      type ('a,'b) t = { mutable foo: 'a } [@@deriving typerep]
    end in
    let module M = struct
      type t = (unit, int) A.t [@@deriving typerep]
    end in
    let exp = S.Record (simple_array, fields [|
      "foo", S.Unit, false;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp [%typerep_of: (unit, int) A.t])
  ;;

  (* sort of a real case *)

  let%test_unit _ =
    let module Transaction_type = struct
      module V1 = struct
        type t =
        | Trade
        | Order
        [@@deriving typerep]
      end
      module V2 = struct
        module Account : sig
          type t = private string [@@deriving typerep]
        end = struct
          type t = string [@@deriving typerep]
        end
        type t =
        | Trade
        | Order
        | Journal of Account.t * Account.t
        [@@deriving typerep]
      end
    end in
    let tt_v1 = S.Variant (usual, tags [|
      vu 0 0 "Trade" [||];
      vu 1 1 "Order" [||];
    |]) in
    assert (check tt_v1 Transaction_type.V1.typerep_of_t);
    let tt_v2 = S.Variant (usual, tags [|
      vu 0 0 "Trade" [||];
      vu 1 1 "Order" [||];
      vu 2 0 "Journal" [| S.String ; S.String |];
    |]) in
    assert (check tt_v2 Transaction_type.V2.typerep_of_t);
    let module V1 = struct
      type t = {
        transaction_type : Transaction_type.V1.t;
        username : string;
      } [@@deriving typerep]
    end in
    let module V2 = struct
      type t = {
        transaction_type : Transaction_type.V2.t;
        username : string;
        tags : (string * string) list;
      } [@@deriving typerep]
    end in
    let module M_v1 = struct
      type t =
      | V1 of V1.t
      [@@deriving typerep]
    end in
    let module M_v2 = struct
      type t =
      | V1 of V1.t
      | V2 of V2.t
      [@@deriving typerep]
    end in
    let v1 = S.Record (simple_array, fields [|
      "transaction_type", tt_v1, false;
      "username", S.String , false;
    |]) in
    let v2 = S.Record (simple_array, fields [|
      "transaction_type", tt_v2, false;
      "username", S.String , false;
      "tags", S.List (stuple [| S.String ; S.String |]), false;
    |]) in
    let exp_v1 = S.Variant (usual, tags [| vu 0 0 "V1" [| v1 |] |]) in
    let exp_v2 = S.Variant (usual, tags [|
      vu 0 0 "V1" [| v1 |] ;
      vu 1 1 "V2" [| v2 |] ;
    |]) in
    assert (check exp_v1 M_v1.typerep_of_t);
    assert (check exp_v2 M_v2.typerep_of_t)

  (* recursive types *)

  (* sum *)

  let%test_unit _ =
    let module M = struct
      type t =
      | Zero
      | Succ of t
      [@@deriving typerep]
    end in
    let exp = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Zero" [||];
      vu 1 0 "Succ" [| S.Named (0, None) |];
    |]))) in
    let cyclic = S.Named (42, Some (S.Variant (usual, tags [|
      vu 0 0 "Zero" [||];
      vu 1 0 "Succ" [| S.Named (42, None) |];
    |]))) in
    assert (check cyclic M.typerep_of_t);
    assert (S.are_equivalent exp cyclic);
    assert (check exp M.typerep_of_t)
  ;;

  let%test_unit _ =
    let module M = struct
      type t =
      | Leaf
      | Node of t * t
      [@@deriving typerep]
    end in
    let exp = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Leaf" [||];
      vu 1 0 "Node" [| S.Named (0, None) ; S.Named (0, None) |];
    |]))) in
    assert (check exp M.typerep_of_t)
  ;;

  (* polymorphic *)

  let%test_unit _ =
    let module M = struct
      type t = [
      | `Zero
      | `Succ of t
      ] [@@deriving typerep]
    end in
    let exp = S.Named (0, Some (S.Variant (polymorphic, tags [|
      vr 0 "Zero" [||];
      vr 1 "Succ" [| S.Named (0, None) |];
    |]))) in
    let cyclic = S.Named (42, Some (S.Variant (polymorphic, tags [|
      vr 0 "Zero" [||];
      vr 1 "Succ" [| S.Named (42, None) |];
    |]))) in
    assert (check cyclic M.typerep_of_t);
    assert (S.are_equivalent exp cyclic);
    assert (check exp M.typerep_of_t)
  ;;

  (* record *)

  let%test_unit _ =
    let module M = struct
      type t = {
        int : int;
        self : t;
      } [@@deriving typerep]
    end in
    let exp = S.Named (0, Some (S.Record (simple_array, fields [|
      "int", S.Int, false;
      "self", S.Named (0, None), false;
    |]))) in
    let cyclic = S.Named (0, Some (S.Record (simple_array, Farray.of_list [
      { S.Field.label="int";index=0;is_mutable=false}, S.Int;
      { S.Field.label="self";index=1;is_mutable=false}, S.Named (0, None);
    ]))) in
    let exp2 = S.Record (simple_array, fields [|
      "int", S.Int, false;
      "self", S.Named (0, Some (
        S.Record (simple_array, fields [|
          "int", S.Int, false;
          "self", S.Named (0, Some (
            S.Record (simple_array, fields [|
              "int", S.Int, false;
              "self", S.Named (0, None), false;
            |]))), false;
        |]))), false;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check cyclic M.typerep_of_t);
    assert (check exp2 M.typerep_of_t)
  ;;

  (* with parameters *)

  (* sum *)

  let%test_unit _ =
    let module M = struct
      type 'a tree =
      | Leaf of 'a
      | Tree of 'a * 'a tree * 'a tree
      [@@deriving typerep]
    end in
    let exp arg = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Leaf" [| arg |];
      vu 1 1 "Tree" [| arg ; S.Named (0, None) ; S.Named (0, None) |];
    |]))) in
    assert (check (exp S.Int) (M.typerep_of_tree typerep_of_int));
    assert (check (exp S.Float) (M.typerep_of_tree typerep_of_float))
  ;;

  let%test_unit _ =
    let module T = struct
      type ('a, 'b) t =
      | Empty
      | Node of ('b, 'a) t
      | A of 'a
      | B of 'b
      [@@deriving typerep]
    end in
    let module M = struct
      type t = (int, string) T.t
      [@@deriving typerep]
    end in
    let exp = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Empty" [||];
      vu 1 0 "Node" [| S.Variant (usual, tags [|
        vu 0 0 "Empty" [||];
        vu 1 0 "Node" [| S.Named (0, None) |];
        vu 2 1 "A" [| S.String |];
        vu 3 2 "B" [| S.Int |];
      |])|];
      vu 2 1 "A" [| S.Int |];
      vu 3 2 "B" [| S.String |];
    |]))) in
    assert (check exp M.typerep_of_t)
  ;;

  (* inlining not named polymorphic variant types *)

  let%test_unit _ =
    let module A = struct
      type t = A of [ `A ] [@@deriving typerep]
      let exp = S.Variant (usual, tags [|
        vu 0 0 "A" [|
          S.Variant (polymorphic, tags [|
            vr 0 "A" [||];
          |]);
        |];
      |])
    end in
    assert (check A.exp A.typerep_of_t)
  ;;

  let%test_unit _ =
    let module A = struct
      type 'a t = [ `A of 'a | `B ] [@@deriving typerep]
      let exp arg = S.Variant (polymorphic, tags [|
        vr 0 "A" [| arg |];
        vr 1 "B" [||];
      |])
      let param = S.Variant (polymorphic, tags [|
        vr 0 "A" [||];
      |])
    end in
    let module B = struct
      type ('a, 't, 'b) t = [ `A of 'a * 't | `B of 'b ] [@@deriving typerep]
      let exp a t b = S.Variant (polymorphic, tags [|
        vr 0 "A" [| S.Tuple (Farray.of_list [ a ; t ]) |];
        vr 1 "B" [| b |];
      |])
    end in
    let module M = struct
      type 'a t =
      | Leaf of [ `A of 'a * 'a t | `B of [ `A of 'a | `B ] ]
      | Tree of 'a * 'a t * 'a t
      [@@deriving typerep]
      let exp arg = S.Named (0, Some (S.Variant (usual, tags [|
        vu 0 0 "Leaf" [| B.exp arg (S.Named (0, None)) (A.exp arg) |];
        vu 1 1 "Tree" [| arg ; S.Named (0, None) ; S.Named (0, None) |];
      |])))
    end in
    assert (check (A.exp A.param) [%typerep_of: [ `A ] A.t]);
    assert (check (A.exp S.Int) (A.typerep_of_t typerep_of_int));
    assert (check (A.exp S.Int) [%typerep_of: [ `A of int | `B ]]);
    assert (check (A.exp S.String) (A.typerep_of_t typerep_of_string));
    assert (check (A.exp S.String) [%typerep_of: [ `A of string | `B ]]);
    assert (check (B.exp S.Int S.String S.Float)
              (B.typerep_of_t typerep_of_int typerep_of_string typerep_of_float));
    assert (check (B.exp S.String S.Float S.Int)
              (B.typerep_of_t typerep_of_string typerep_of_float typerep_of_int));
    assert (check (M.exp S.Int) (M.typerep_of_t typerep_of_int));
    assert (check (M.exp S.String) (M.typerep_of_t typerep_of_string))
  ;;

  let%test_unit _ =
    let module A = struct
      type t = True of int | False [@@deriving typerep]
    end in
    (* the preprocessor for polymorphic variants should do the right thing, but camlp4
       doesn't so the generated code doesn't even type (the type definition is that caml
       receives contains the constructor ` True (ie " True"))
       let module B = struct
         type t = [ `True | `False of int ]
         let _ = (`True : t)
       end in *)
    assert (check (S.Variant (usual, tags [|
        vu 1 0 "True" [| S.Int |];
        vu 0 0 "False" [| |];
      |])) A.typerep_of_t)

end)

(* breaking the abstraction ? *)
let%test_module _ = (module struct

  module A : sig
    type 'a t
    include Typerepable.S1 with type 'a t := 'a t
    val create : 'a -> 'a t
  end = struct
    type 'a t = 'a option [@@deriving typerep]
    let create a = Some a
  end

  module B : sig
    type 'a t
    include Typerepable.S1 with type 'a t := 'a t
    val read : 'a t -> 'a
  end = struct
    type 'a t = 'a option [@@deriving typerep]
    let read = function
      | Some a -> a
      | None -> assert false
  end

  let%test_unit _ =
    let module M = struct
      type a = int A.t [@@deriving typerep]
      type b = int B.t [@@deriving typerep]
    end in
    let break (a : M.a) : int option =
      match
        Typename.same_witness
          (Typerep.typename_of_t M.typerep_of_a)
          (Typerep.typename_of_t M.typerep_of_b)
      with
      | Some proof ->
        let b = Type_equal.conv proof a in
        Some (B.read b)
      | None -> None
    in
    let a = A.create 42 in
    assert (break a = None)

  let%test_unit _ =
    let module M = struct
      type t =
      | Foo1 of int
      | Foo2 of int
      | Foo3 of int
      [@@deriving typerep]

      let get_int =
        match Typerep.head typerep_of_t with
        | Typerep.Variant variant -> (fun t ->
          let Typerep.Variant.Value (tag, arg) = Typerep.Variant.value variant t in
          let witness =
            Typerep.same_witness_exn (Typerep.Tag.traverse tag) typerep_of_int
          in
          Type_equal.conv witness arg
        )
        | _ -> assert false
    end
    in
    assert (M.get_int (M.Foo1 1) = 1);
    assert (M.get_int (M.Foo2 2) = 2)
end)
