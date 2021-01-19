module StrMap = Map.Make (String)

(** Data structure that allow to create the [get_unary] and [get_binary]
    functions in particular. [make_appl] can use this data structure as well. *)
type table = {
    unary : Pratter.priority StrMap.t
  ; binary : (Pratter.priority * Pratter.associativity) StrMap.t
}

let empty : table = { unary = StrMap.empty; binary = StrMap.empty }

(** A simple term structure. *)
type term = Appl of term * term | Symb of string

(** [symb id] creates a term from identifier [id]. *)
let symb id = Symb id

module Support : Pratter.SUPPORT with type term = term and type table = table =
struct
  type nonrec term = term
  type nonrec table = table

  let get_unary { unary; _ } t =
    match t with Symb id -> StrMap.find_opt id unary | _ -> None

  let get_binary { binary; _ } t =
    match t with Symb id -> StrMap.find_opt id binary | _ -> None

  let make_appl _ t u = Appl (t, u)
end

module SupPrat = Pratter.Make (Support)

(** [add_args tbl t args] creates the application of [t] to the list of
    arguments [args]. *)
let rec add_args : table -> term -> term list -> term =
 fun tbl hd args ->
  match args with
  | [] -> hd
  | a :: args -> add_args tbl (Support.make_appl tbl hd a) args

(** Module of testable terms for Alcotest. *)
module TTerm : Alcotest.TESTABLE with type t = term = struct
  type t = term

  (** Syntactic equality *)
  let rec equal t u =
    match (t, u) with
    | Symb t, Symb u -> String.equal t u
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
    [ ("+", (1.0, Pratter.Left)); ("*", (1.1, Pratter.Left)) ]
    |> List.to_seq |> StrMap.of_seq
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
  let parsed = add_args tbl (symb "+") [ Support.make_appl tbl f x; x ] in
  Alcotest.(check tterm) "f x + x" (SupPrat.expression tbl not_parsed) parsed

let simple_unary () =
  let tbl = { empty with unary = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ symb "!"; x ] in
  let parsed = Support.make_appl tbl (symb "!") x in
  Alcotest.(check tterm) "! x" (SupPrat.expression tbl not_parsed) parsed

let unary_appl () =
  let tbl = { empty with unary = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ symb "!"; f; x ] in
  let parsed = Support.(make_appl tbl (symb "!") (make_appl tbl f x)) in
  Alcotest.(check tterm)
    "! f x"
    (SupPrat.expression tbl not_parsed)
    parsed

let unary_appl_in () =
  let tbl = { empty with unary = StrMap.add "!" 1.0 StrMap.empty } in
  let x = symb "x" in
  let f = symb "f" in
  let fac = symb "!" in
  let not_parsed = Stream.of_list [ f; fac; x ] in
  let parsed =
    let inside = Support.make_appl tbl fac x in
    Support.(make_appl tbl f inside)
  in
  Alcotest.(check tterm)
    "f ! x"
    (SupPrat.expression tbl not_parsed)
    parsed

let _ =
  let open Alcotest in
  run "Binary"
    [
      ( "binary"
      , [
          test_case "simple" `Quick simple_binary
        ; test_case "two" `Quick two_operators
        ; test_case "appl-bin" `Quick appl_opertor
        ] )
    ; ( "unary"
      , [
          test_case "simple" `Quick simple_unary
        ; test_case "application head" `Quick unary_appl
        ; test_case "application argument" `Quick unary_appl_in
        ] )
    ]
