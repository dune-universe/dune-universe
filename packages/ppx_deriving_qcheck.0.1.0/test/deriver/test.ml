(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Module test for ppx_deriving_qcheck *)
open Ppxlib

(** Primitive types tests *)
let loc = Location.none

let f = Ppx_deriving_qcheck.derive_arbitrary ~loc

let f' xs = List.map f xs |> List.concat

let extract stri =
  match stri.pstr_desc with Pstr_type (x, y) -> (x, y) | _ -> assert false

let extract' xs = List.map extract xs

let check_eq ~expected ~actual name =
  let f = Ppxlib.Pprintast.string_of_structure in
  Alcotest.(check string) name (f expected) (f actual)

let test_int () =
  let expected = [ [%stri let arb = QCheck.int] ] in
  let actual = f @@ extract [%stri type t = int [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int"

let test_float () =
  let expected = [ [%stri let arb = QCheck.float] ] in
  let actual = f @@ extract [%stri type t = float [@@deriving arb]] in

  check_eq ~expected ~actual "deriving float"

let test_char () =
  let expected = [ [%stri let arb = QCheck.char] ] in
  let actual = f @@ extract [%stri type t = char [@@deriving arb]] in

  check_eq ~expected ~actual "deriving char"

let test_string () =
  let expected = [ [%stri let arb = QCheck.string] ] in
  let actual = f @@ extract [%stri type t = string [@@deriving arb]] in

  check_eq ~expected ~actual "deriving string"

let test_unit () =
  let expected = [ [%stri let arb = QCheck.unit] ] in
  let actual = f @@ extract [%stri type t = unit [@@deriving arb]] in

  check_eq ~expected ~actual "deriving unit"

let test_bool () =
  let expected = [ [%stri let arb = QCheck.bool] ] in
  let actual = f @@ extract [%stri type t = bool [@@deriving arb]] in

  check_eq ~expected ~actual "deriving bool"

let test_int32 () =
  let expected = [ [%stri let arb = QCheck.int32] ] in
  let actual = f @@ extract [%stri type t = int32 [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int32"

let test_int32' () =
  let expected = [ [%stri let arb = QCheck.int32] ] in
  let actual = f @@ extract [%stri type t = Int32.t [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int32'"

let test_int64 () =
  let expected = [ [%stri let arb = QCheck.int64] ] in
  let actual = f @@ extract [%stri type t = int64 [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int64"

let test_int64' () =
  let expected = [ [%stri let arb = QCheck.int64] ] in
  let actual = f @@ extract [%stri type t = Int64.t [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int64'"

let test_bytes () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.map
            (fun n -> Bytes.create n)
            QCheck.(0 -- Sys.max_string_length)];
    ]
  in
  let actual = f @@ extract [%stri type t = Bytes.t [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int64"

let test_tuple () =
  let actual =
    f'
    @@ extract'
         [
           [%stri type t = int * int];
           [%stri type t = int * int * int];
           [%stri type t = int * int * int * int];
           [%stri type t = int * int * int * int * int];
           [%stri type t = int * int * int * int * int * int];
         ]
  in
  let expected =
    [
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, arb_1) -> (arb_0, arb_1))
            (QCheck.pair QCheck.int QCheck.int)];
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, (arb_1, arb_2)) -> (arb_0, arb_1, arb_2))
            (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))];
      [%stri
        let arb =
          QCheck.map
            (fun ((arb_0, arb_1), (arb_2, arb_3)) ->
              (arb_0, arb_1, arb_2, arb_3))
            (QCheck.pair
               (QCheck.pair QCheck.int QCheck.int)
               (QCheck.pair QCheck.int QCheck.int))];
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, ((arb_1, arb_2), (arb_3, arb_4))) ->
              (arb_0, arb_1, arb_2, arb_3, arb_4))
            (QCheck.pair
               QCheck.int
               (QCheck.pair
                  (QCheck.pair QCheck.int QCheck.int)
                  (QCheck.pair QCheck.int QCheck.int)))];
      [%stri
        let arb =
          QCheck.map
            (fun ((arb_0, (arb_1, arb_2)), (arb_3, (arb_4, arb_5))) ->
              (arb_0, arb_1, arb_2, arb_3, arb_4, arb_5))
            (QCheck.pair
               (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))
               (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int)))];
    ]
  in

  check_eq ~expected ~actual "deriving tuples"

let test_option () =
  let expected =
    [
      [%stri
        let arb arb_a =
          QCheck.frequency
            [
              (1, QCheck.always None);
              (1, QCheck.map (fun arb_0 -> Some arb_0) arb_a);
            ]];
      [%stri let arb = arb_my_option QCheck.int];
      [%stri let arb = QCheck.option QCheck.int];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type 'a t = None | Some of 'a];
           [%stri type t = int my_option];
           [%stri type t = int option];
         ]
  in
  check_eq ~expected ~actual "deriving option"

let test_list () =
  let expected =
    [
      [%stri let arb = QCheck.list QCheck.string];
      [%stri
        let arb =
          QCheck.frequency
            [
              (1, QCheck.map (fun arb_0 -> A arb_0) (QCheck.list QCheck.string));
              (1, QCheck.map (fun arb_0 -> B arb_0) (QCheck.list QCheck.int));
            ]];
    ]
  in

  let actual =
    f'
    @@ extract'
         [
           [%stri type t = string list];
           [%stri type t = A of string list | B of int list];
         ]
  in
  check_eq ~expected ~actual "deriving list"

let test_alpha () =
  let expected =
    [
      [%stri let arb arb_a = arb_a];
      [%stri let arb arb_a = QCheck.list arb_a];
      [%stri
        let arb arb_a =
          QCheck.frequency [ (1, QCheck.map (fun arb_0 -> A arb_0) arb_a) ]];
      [%stri
        let arb arb_a arb_b =
          QCheck.frequency
            [
              ( 1,
                QCheck.map
                  (fun (arb_0, arb_1) -> A (arb_0, arb_1))
                  (QCheck.pair arb_a arb_b) );
            ]];
      [%stri
        let arb arb_left arb_right =
          QCheck.map
            (fun (arb_0, arb_1) -> (arb_0, arb_1))
            (QCheck.pair arb_left arb_right)];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type 'a t = 'a];
           [%stri type 'a t = 'a list];
           [%stri type 'a t = A of 'a];
           [%stri type ('a, 'b) t = A of 'a * 'b];
           [%stri type ('left, 'right) t = 'left * 'right];
         ]
  in
  check_eq ~expected ~actual "deriving alpha"

let test_equal () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.frequency
            [ (1, QCheck.always A); (1, QCheck.always B); (1, QCheck.always C) ]];
      [%stri
        let arb_t' =
          QCheck.frequency
            [ (1, QCheck.always A); (1, QCheck.always B); (1, QCheck.always C) ]];
    ]
  in
  let actual =
    f'
    @@ extract'
         [ [%stri type t = A | B | C]; [%stri type t' = t = A | B | C] ]
  in
  check_eq ~expected ~actual "deriving equal"

let test_dependencies () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.frequency
            [
              (1, QCheck.map (fun arb_0 -> Int arb_0) SomeModule.arb);
              ( 1,
                QCheck.map
                  (fun arb_0 -> Float arb_0)
                  SomeModule.SomeOtherModule.arb );
            ]];
      [%stri let arb = gen_something];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri
             type t =
               | Int of SomeModule.t
               | Float of SomeModule.SomeOtherModule.t];
           [%stri type t = (Something.t[@arb gen_something])];
         ]
  in

  check_eq ~expected ~actual "deriving dependencies"

let test_konstr () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.frequency [ (1, QCheck.map (fun arb_0 -> A arb_0) QCheck.int) ]];
      [%stri
        let arb =
          QCheck.frequency
            [
              (1, QCheck.map (fun arb_0 -> B arb_0) QCheck.int);
              (1, QCheck.map (fun arb_0 -> C arb_0) QCheck.int);
            ]];
      [%stri
        let arb =
          QCheck.frequency
            [
              (1, QCheck.map (fun arb_0 -> X arb_0) arb_t1);
              (1, QCheck.map (fun arb_0 -> Y arb_0) arb_t2);
              (1, QCheck.map (fun arb_0 -> Z arb_0) QCheck.string);
            ]];
      [%stri
        let arb =
          QCheck.frequency [ (1, QCheck.always Left); (1, QCheck.always Right) ]];
      [%stri
        let arb =
          QCheck.frequency
            [
              (1, QCheck.map (fun arb_0 -> Simple arb_0) QCheck.int);
              ( 1,
                QCheck.map
                  (fun (arb_0, arb_1) -> Double (arb_0, arb_1))
                  (QCheck.pair QCheck.int QCheck.int) );
              ( 1,
                QCheck.map
                  (fun (arb_0, (arb_1, arb_2)) -> Triple (arb_0, arb_1, arb_2))
                  (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))
              );
            ]];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type t = A of int];
           [%stri type t = B of int | C of int];
           [%stri type t = X of t1 | Y of t2 | Z of string];
           [%stri type t = Left | Right];
           [%stri
             type t =
               | Simple of int
               | Double of int * int
               | Triple of int * int * int];
         ]
  in
  check_eq ~expected ~actual "deriving constructors"

let test_record () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, arb_1) -> { a = arb_0; b = arb_1 })
            (QCheck.pair QCheck.int QCheck.string)];
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, arb_1) -> { a = arb_0; b = arb_1 })
            (QCheck.pair QCheck.int QCheck.string)];
      [%stri
        let arb =
          QCheck.frequency
            [
              (1, QCheck.map (fun arb_0 -> A arb_0) arb_t');
              ( 1,
                QCheck.map
                  (fun (arb_0, arb_1) -> B { left = arb_0; right = arb_1 })
                  (QCheck.pair QCheck.int QCheck.int) );
            ]];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type t = { a : int; b : string }];
           [%stri type t = { mutable a : int; mutable b : string }];
           [%stri type t = A of t' | B of { left : int; right : int } [@@arb]];
         ]
  in
  check_eq ~expected ~actual "deriving record"

let test_variant () =
  let expected =
    [
      [%stri
        let arb =
          (QCheck.frequency
             [
               (1, QCheck.always `A);
               (1, QCheck.map (fun arb_0 -> `B arb_0) QCheck.int);
               (1, QCheck.map (fun arb_0 -> `C arb_0) QCheck.string);
             ]
            : t QCheck.arbitrary)];
      [%stri
        include struct
          let rec arb () = arb' 5

          and arb' = function
            | 0 ->
                (QCheck.frequency
                   [
                     (1, QCheck.always `A);
                     (1, QCheck.map (fun arb_0 -> `B arb_0) QCheck.int);
                     (1, QCheck.map (fun arb_0 -> `C arb_0) QCheck.string);
                   ]
                  : t QCheck.arbitrary)
            | n ->
                (QCheck.frequency
                   [
                     (1, QCheck.always `A);
                     (1, QCheck.map (fun arb_0 -> `B arb_0) QCheck.int);
                     (1, QCheck.map (fun arb_0 -> `C arb_0) QCheck.string);
                     (1, QCheck.map (fun arb_0 -> `D arb_0) (arb' (n - 1)));
                   ]
                  : t QCheck.arbitrary)

          let arb = arb ()
        end];
      [%stri
        let arb_t' =
          (QCheck.frequency [ (1, QCheck.always `B); (1, arb) ]
            : t' QCheck.arbitrary)];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type t = [ `A | `B of int | `C of string ]];
           [%stri type t = [ `A | `B of int | `C of string | `D of t ]];
           [%stri type t' = [ `B | t ]];
         ]
  in
  check_eq ~expected ~actual "deriving variant"

let test_tree () =
  let expected =
    [
      [%stri
        include struct
          let rec arb_tree () = arb_tree' 5

          and arb_tree' = function
            | 0 -> QCheck.frequency [ (1, QCheck.always Leaf) ]
            | n ->
                QCheck.frequency
                  [
                    (1, QCheck.always Leaf);
                    ( 1,
                      QCheck.map
                        (fun (arb_0, (arb_1, arb_2)) ->
                          Node (arb_0, arb_1, arb_2))
                        (QCheck.pair
                           QCheck.int
                           (QCheck.pair (arb_tree' (n - 1)) (arb_tree' (n - 1))))
                    );
                  ]

          let arb_tree = arb_tree ()
        end];
      [%stri
        include struct
          let rec arb_expr () = arb_expr' 5

          and arb_expr' = function
            | 0 ->
                QCheck.frequency
                  [ (1, QCheck.map (fun arb_0 -> Value arb_0) QCheck.int) ]
            | n ->
                QCheck.frequency
                  [
                    (1, QCheck.map (fun arb_0 -> Value arb_0) QCheck.int);
                    ( 1,
                      QCheck.map
                        (fun (arb_0, (arb_1, arb_2)) ->
                          If (arb_0, arb_1, arb_2))
                        (QCheck.pair
                           (arb_expr' (n - 1))
                           (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))))
                    );
                    ( 1,
                      QCheck.map
                        (fun (arb_0, arb_1) -> Eq (arb_0, arb_1))
                        (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))) );
                    ( 1,
                      QCheck.map
                        (fun (arb_0, arb_1) -> Lt (arb_0, arb_1))
                        (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))) );
                  ]

          let arb_expr = arb_expr ()
        end];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type tree = Leaf | Node of int * tree * tree];
           [%stri
             type expr =
               | Value of int
               | If of expr * expr * expr
               | Eq of expr * expr
               | Lt of expr * expr];
         ]
  in
  check_eq ~expected ~actual "deriving tree"

let test_recursive () =
  let expected =
    [
      [%stri
        include struct
          let rec arb_expr () = arb_expr' 5

          and arb_expr' = function
            | 0 ->
                QCheck.frequency
                  [ (1, QCheck.map (fun arb_0 -> Value arb_0) (arb_value ())) ]
            | n ->
                QCheck.frequency
                  [
                    (1, QCheck.map (fun arb_0 -> Value arb_0) (arb_value ()));
                    ( 1,
                      QCheck.map
                        (fun (arb_0, (arb_1, arb_2)) ->
                          If (arb_0, arb_1, arb_2))
                        (QCheck.pair
                           (arb_expr' (n - 1))
                           (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))))
                    );
                    ( 1,
                      QCheck.map
                        (fun (arb_0, arb_1) -> Eq (arb_0, arb_1))
                        (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))) );
                    ( 1,
                      QCheck.map
                        (fun (arb_0, arb_1) -> Lt (arb_0, arb_1))
                        (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))) );
                  ]

          and arb_value () =
            QCheck.frequency
              [
                (1, QCheck.map (fun arb_0 -> Bool arb_0) QCheck.bool);
                (1, QCheck.map (fun arb_0 -> Int arb_0) QCheck.int);
              ]

          let arb_expr = arb_expr ()

          let arb_value = arb_value ()
        end];
    ]
  in
  let actual =
    f
    @@ extract
         [%stri
           type expr =
             | Value of value
             | If of expr * expr * expr
             | Eq of expr * expr
             | Lt of expr * expr

           and value = Bool of bool | Int of int]
  in
  check_eq ~expected ~actual "deriving recursive"

let test_fun_axiom () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(QCheck.Observable.int @-> QCheck.Observable.int @-> o_nil)
              QCheck.string)];
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.float @-> QCheck.Observable.float @-> o_nil)
              QCheck.string)];
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.string @-> QCheck.Observable.string @-> o_nil)
              QCheck.string)];
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.bool @-> QCheck.Observable.bool @-> o_nil)
              QCheck.string)];
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.char @-> QCheck.Observable.char @-> o_nil)
              QCheck.string)];
      [%stri
        let arb =
          QCheck.(
            fun_nary Tuple.(QCheck.Observable.unit @-> o_nil) QCheck.string)];
    ]
  in

  let actual =
    f'
    @@ extract'
         [
           [%stri type t = int -> int -> string];
           [%stri type t = float -> float -> string];
           [%stri type t = string -> string -> string];
           [%stri type t = bool -> bool -> string];
           [%stri type t = char -> char -> string];
           [%stri type t = unit -> string];
         ]
  in
  check_eq ~expected ~actual "deriving fun axioms"

let test_fun_n () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.bool @-> QCheck.Observable.int
                @-> QCheck.Observable.float @-> QCheck.Observable.string
                @-> QCheck.Observable.char @-> o_nil)
              QCheck.unit)];
    ]
  in
  let actual =
    f @@ extract [%stri type t = bool -> int -> float -> string -> char -> unit]
  in
  check_eq ~expected ~actual "deriving fun n"

let test_fun_option () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(QCheck.Observable.option QCheck.Observable.int @-> o_nil)
              QCheck.unit)];
    ]
  in
  let actual = f @@ extract [%stri type t = int option -> unit] in
  check_eq ~expected ~actual "deriving fun option"

let test_fun_list () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(QCheck.Observable.list QCheck.Observable.int @-> o_nil)
              QCheck.unit)];
    ]
  in
  let actual = f @@ extract [%stri type t = int list -> unit] in
  check_eq ~expected ~actual "deriving fun list"

let test_fun_array () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(QCheck.Observable.array QCheck.Observable.int @-> o_nil)
              QCheck.unit)];
    ]
  in
  let actual = f @@ extract [%stri type t = int array -> unit] in
  check_eq ~expected ~actual "deriving fun array"

let test_fun_tuple () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.pair
                  QCheck.Observable.int
                  QCheck.Observable.int
                @-> o_nil)
              QCheck.unit)];
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.triple
                  QCheck.Observable.int
                  QCheck.Observable.int
                  QCheck.Observable.int
                @-> o_nil)
              QCheck.unit)];
      [%stri
        let arb =
          QCheck.(
            fun_nary
              Tuple.(
                QCheck.Observable.quad
                  QCheck.Observable.int
                  QCheck.Observable.int
                  QCheck.Observable.int
                  QCheck.Observable.int
                @-> o_nil)
              QCheck.unit)];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type t = int * int -> unit];
           [%stri type t = int * int * int -> unit];
           [%stri type t = int * int * int * int -> unit];
         ]
  in
  check_eq ~expected ~actual "deriving fun tuple"

let test_weight_konstrs () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.frequency
            [ (5, QCheck.always A); (6, QCheck.always B); (1, QCheck.always C) ]];
    ]
  in
  let actual =
    f @@ extract [%stri type t = A [@weight 5] | B [@weight 6] | C]
  in
  check_eq ~expected ~actual "deriving weight konstrs"

let () =
  Alcotest.(
    run
      "ppx_deriving_qcheck tests"
      [
        ( "deriving arbitrary good",
          [
            test_case "deriving int" `Quick test_int;
            test_case "deriving float" `Quick test_float;
            test_case "deriving char" `Quick test_char;
            test_case "deriving string" `Quick test_string;
            test_case "deriving unit" `Quick test_unit;
            test_case "deriving bool" `Quick test_bool;
            test_case "deriving int32" `Quick test_int32;
            test_case "deriving int32'" `Quick test_int32';
            test_case "deriving int64" `Quick test_int64;
            test_case "deriving int64'" `Quick test_int64';
            test_case "deriving bytes" `Quick test_bytes;
            test_case "deriving tuple" `Quick test_tuple;
            test_case "deriving option" `Quick test_option;
            test_case "deriving list" `Quick test_list;
            test_case "deriving alpha" `Quick test_alpha;
            test_case "deriving equal" `Quick test_equal;
            test_case "deriving dependencies" `Quick test_dependencies;
            test_case "deriving constructors" `Quick test_konstr;
            test_case "deriving record" `Quick test_record;
            test_case "deriving variant" `Quick test_variant;
            test_case "deriving tree like" `Quick test_tree;
            test_case "deriving recursive" `Quick test_recursive;
            test_case "deriving fun axioms" `Quick test_fun_axiom;
            test_case "deriving fun n" `Quick test_fun_n;
            test_case "deriving fun option" `Quick test_fun_option;
            test_case "deriving fun list" `Quick test_fun_list;
            test_case "deriving fun array" `Quick test_fun_array;
            test_case "deriving fun tuple" `Quick test_fun_tuple;
            test_case "deriving weight constructors" `Quick test_weight_konstrs;
          ] );
      ])
