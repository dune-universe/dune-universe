module StrMap = Map.Make (String)

type table = {
    unary : Pratter.priority StrMap.t
  ; binary : (Pratter.priority * Pratter.associativity) StrMap.t
}
(** Data structure containing the operators. *)

let empty : table = { unary = StrMap.empty; binary = StrMap.empty }

(** A simple term structure. *)
type term = Appl of term * term | Symb of string

(** [symb id] creates a term from identifier [id]. *)
let symb id = Symb id

module Support : Pratter.SUPPORT with type term = term and type table = table =
struct
  type nonrec term = term
  type nonrec table = table

  let get { unary; binary } t =
    match t with
    | Symb id -> (
        try Some (Pratter.Una, StrMap.find id unary)
        with Not_found -> (
          try
            let bp, assoc = StrMap.find id binary in
            Some (Bin assoc, bp)
          with Not_found -> None ) )
    | _ -> None

  let make_appl t u = Appl (t, u)
end

module SupPrat = Pratter.Make (Support)

(** [add_args tbl t args] creates the application of [t] to the list of
    arguments [args]. *)
let rec add_args : table -> term -> term list -> term =
 fun tbl hd args ->
  match args with
  | [] -> hd
  | a :: args -> add_args tbl (Support.make_appl hd a) args

(** Module of testable terms for Alcotest. *)
module TTerm : Alcotest.TESTABLE with type t = term = struct
  type t = term

  (** Syntactic equality *)
  let rec equal t u =
    match (t, u) with
    | Symb t, Symb u -> t = u
    | Appl (t, t'), Appl (u, u') -> equal t u && equal t' u'
    | _ -> false

  let rec pp oc t =
    match t with
    | Appl (t, u) -> Format.fprintf oc "@(%a, %a)" pp t pp u
    | Symb id -> Format.pp_print_string oc id
end

let tterm : (module Alcotest.TESTABLE with type t = term) = (module TTerm)

let simple_binary () =
  let tbl = StrMap.add "+" (1.0, Pratter.Left) StrMap.empty in
  let tbl = { empty with binary = tbl } in
  let x = symb "x" in
  let y = symb "y" in
  let not_parsed = Stream.of_list [ x; symb "+"; y ] in
  let parsed = add_args tbl (symb "+") [ x; y ] in
  Alcotest.(check tterm) "x + y" (SupPrat.expression tbl not_parsed) parsed

let two_operators () =
  let tbl =
    StrMap.(empty |> add "+" (1.0, Pratter.Left) |> add "*" (1.1, Pratter.Left))
  in
  let tbl = { empty with binary = tbl } in
  let x = symb "x" in
  let y = symb "y" in
  let z = symb "z" in
  let not_parsed = Stream.of_list [ x; symb "+"; y; symb "*"; z ] in
  let parsed =
    let right = add_args tbl (symb "*") [ y; z ] in
    add_args tbl (symb "+") [ x; right ]
  in
  Alcotest.(check tterm) "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let appl_opertor () =
  let tbl =
    { empty with binary = StrMap.add "+" (1.0, Pratter.Left) StrMap.empty }
  in
  let f = symb "f" in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ f; x; symb "+"; x ] in
  let parsed = add_args tbl (symb "+") [ Support.make_appl f x; x ] in
  Alcotest.(check tterm) "f x + x" (SupPrat.expression tbl not_parsed) parsed

let simple_unary () =
  let tbl = { empty with unary = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ symb "!"; x ] in
  let parsed = Support.make_appl (symb "!") x in
  Alcotest.(check tterm) "! x" (SupPrat.expression tbl not_parsed) parsed

let unary_appl () =
  let tbl = { empty with unary = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ symb "!"; f; x ] in
  let parsed = Support.(make_appl (symb "!") (make_appl f x)) in
  Alcotest.(check tterm) "! f x" (SupPrat.expression tbl not_parsed) parsed

let unary_appl_in () =
  let tbl = { empty with unary = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let fac = symb "!" in
  let not_parsed = Stream.of_list [ f; fac; x ] in
  let parsed =
    let inside = Support.make_appl fac x in
    Support.(make_appl f inside)
  in
  Alcotest.(check tterm) "f ! x" (SupPrat.expression tbl not_parsed) parsed

let double_unary () =
  (* --x = -(-x) *)
  let tbl = { empty with unary = StrMap.(add "-" 1.0 empty) } in
  let not_parsed = Stream.of_list [ symb "-"; symb "-"; symb "x" ] in
  let parsed = Appl (symb "-", Appl (symb "-", symb "x")) in
  Alcotest.check tterm "--x" (SupPrat.expression tbl not_parsed) parsed

let precedences_left_same () =
  (* x + y * z = (x + y) * z when bp(+) = bp( * ) and both are left
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Left) |> add "*" (1.0, Left)))
  in
  let tbl = { empty with binary = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let left = add_args tbl (symb "+") [ symb "x"; symb "y" ] in
    add_args tbl (symb "*") [ left; symb "z" ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let precedences_right_same () =
  (* x + y * z = x + (y * z) when bp(+) = bp( * ) and both are right
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Right) |> add "*" (1.0, Right)))
  in
  let tbl = { empty with binary = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let right = add_args tbl (symb "*") [ symb "y"; symb "z" ] in
    add_args tbl (symb "+") [ symb "x"; right ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let precedences_lt_not_assoc () =
  (* x + y * z = x + (y * z) when bp(+) < bp( * ) and both are not
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (0., Neither) |> add "*" (0.1, Neither)))
  in
  let tbl = { empty with binary = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let right = add_args tbl (symb "*") [ symb "y"; symb "z" ] in
    add_args tbl (symb "+") [ symb "x"; right ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let precedences_gt_not_assoc () =
  (* x + y * z = (x + y) * z when bp(+) > bp( * ) and both are not
     associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (-1., Neither) |> add "*" (-1.1, Neither)))
    (* NOTE that negative binding powers are accepted. *)
  in
  let tbl = { empty with binary = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  let parsed =
    let left = add_args tbl (symb "+") [ symb "x"; symb "y" ] in
    add_args tbl (symb "*") [ left; symb "z" ]
  in
  Alcotest.check tterm "x + y * z" (SupPrat.expression tbl not_parsed) parsed

let mixing_una_bin () =
  (* !x + y = (!x) + y when bp(!) > bp(+) *)
  let binary = StrMap.singleton "+" (1.0, Pratter.Neither) in
  let unary = StrMap.singleton "!" 1.1 in
  let tbl = { binary; unary } in
  let not_parsed = Stream.of_list [ symb "!"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    add_args tbl (symb "+") [ add_args tbl (symb "!") [ symb "x" ]; symb "y" ]
  in
  Alcotest.check tterm "(!x) + y" (SupPrat.expression tbl not_parsed) parsed

let mixing_una_bin_bis () =
  (* !x + y = !(x + y) when bp(+) > bp(!) *)
  let binary = StrMap.singleton "+" (1.0, Pratter.Neither) in
  let unary = StrMap.singleton "!" 0.9 in
  let tbl = { binary; unary } in
  let not_parsed = Stream.of_list [ symb "!"; symb "x"; symb "+"; symb "y" ] in
  let parsed =
    add_args tbl (symb "!") [ add_args tbl (symb "+") [ symb "x"; symb "y" ] ]
  in
  Alcotest.check tterm "!(x + y)" (SupPrat.expression tbl not_parsed) parsed

let precedences_eq_not_assoc () =
  (* x + y * z fails when bp(+) = bp( * ) and both are not associative *)
  let tbl =
    Pratter.(StrMap.(empty |> add "+" (1.0, Neither) |> add "*" (1.0, Neither)))
  in
  let tbl = { empty with binary = tbl } in
  let not_parsed =
    Stream.of_list [ symb "x"; symb "+"; symb "y"; symb "*"; symb "z" ]
  in
  Alcotest.(check bool)
    "x + y * z"
    ( try
        ignore (SupPrat.expression tbl not_parsed);
        false
      with SupPrat.OpConflict (_, _) -> true )
    true

let partial_binary () =
  let tbl = StrMap.singleton "+" (1.0, Pratter.Left) in
  let tbl = { empty with binary = tbl } in
  let not_parsed = Stream.of_list [ symb "x"; symb "+" ] in
  Alcotest.(check bool)
    "x +"
    ( try
        ignore (SupPrat.expression tbl not_parsed);
        false
      with SupPrat.TooFewArguments -> true )
    true

let partial_unary () =
  let tbl = StrMap.singleton "!" 1.0 in
  let tbl = { empty with unary = tbl } in
  let not_parsed = Stream.of_list [ symb "!" ] in
  Alcotest.(check bool)
    "!"
    ( try
        ignore (SupPrat.expression tbl not_parsed);
        false
      with SupPrat.TooFewArguments -> true )
    true

let bin_start_expr () =
  (* [+ x x] raises [UnexpectBin +]: [+] has no left context. *)
  let tbl =
    { empty with binary = StrMap.singleton "+" (1.0, Pratter.Neither) }
  in
  let not_parsed = Stream.of_list [ symb "+"; symb "x"; symb "x" ] in
  Alcotest.check tterm "+ x x"
    ( try
        ignore (SupPrat.expression tbl not_parsed);
        assert false
      with SupPrat.UnexpectedBin t -> t )
    (symb "+")

let bin_bin () =
  (* x + + x raises [UnexpectBin +]: the second [+] has no left context. *)
  let tbl =
    { empty with binary = StrMap.singleton "+" (1.0, Pratter.Neither) }
  in
  let not_parsed = Stream.of_list [ symb "x"; symb "+"; symb "+"; symb "x" ] in
  Alcotest.check tterm "x + + x"
    ( try
        ignore (SupPrat.expression tbl not_parsed);
        assert false
      with SupPrat.UnexpectedBin t -> t )
    (symb "+")

let _ =
  let open Alcotest in
  run "Simple terms"
    [
      ( "binary"
      , [
          test_case "simple" `Quick simple_binary
        ; test_case "two" `Quick two_operators
        ; test_case "appl-bin" `Quick appl_opertor
        ; test_case "left assoc, same bp" `Quick precedences_left_same
        ; test_case "right assoc, same bp" `Quick precedences_right_same
        ; test_case "not assoc, lt bp" `Quick precedences_lt_not_assoc
        ; test_case "not assoc, gt bp" `Quick precedences_gt_not_assoc
        ; test_case "not assoc, same bp" `Quick precedences_eq_not_assoc
        ] )
    ; ( "unary"
      , [
          test_case "simple" `Quick simple_unary
        ; test_case "application head" `Quick unary_appl
        ; test_case "application argument" `Quick unary_appl_in
        ; test_case "double" `Quick double_unary
        ] )
    ; ( "mixes"
      , [
          test_case "prefix infix" `Quick mixing_una_bin
        ; test_case "infix prefix" `Quick mixing_una_bin_bis
        ] )
    ; ( "errors"
      , [
          test_case "partial binary" `Quick partial_binary
        ; test_case "partial unary" `Quick partial_unary
        ; test_case "binary no left" `Quick bin_start_expr
        ; test_case "binary successive" `Quick bin_bin
        ] )
    ]
