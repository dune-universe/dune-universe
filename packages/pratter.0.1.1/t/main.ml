type ident = string
(** Type of identifiers. *)

(** A simple term structure. *)
type term =
  | Appl of term * term
  | Symb of ident
  | BinO of term * ident * term
  | UnaO of ident * term

(** [symb id] creates a term from identifier [id]. *)
let symb id = Symb id

module Support : Pratter.SUPPORT with type ident = string and type term = term =
struct
  type nonrec ident = ident
  type pos = unit (* We ignore positions. *)

  type popt = pos option
  type nonrec term = term

  let get_ident = function Symb id -> Some (id, None) | _ -> None
  let make_appl t u = Appl (t, u)
  let make_bin_appl t _ (id, _, _) u = BinO (t, id, u)
  let make_una_appl _ (id, _) t = UnaO (id, t)
end

module SupPrat = Pratter.Make (Support)

(* We compare the printed form of terms. *)
let pp_ident oc id = Format.pp_print_string oc id

let rec pp_term oc t =
  match t with
  | Appl (t, u) -> Format.fprintf oc "@(%a, %a)" pp_term t pp_term u
  | Symb id -> pp_ident oc id
  | BinO (t, op, u) -> Format.fprintf oc "([%a %s %a])" pp_term t op pp_term u
  | UnaO (op, t) -> Format.fprintf oc "([%s %a])" op pp_term t

let to_string t = Format.asprintf "%a" pp_term t

let simple_binary () =
  SupPrat.flush ();
  SupPrat.add_binary "+" 1.0 Pratter.Left;
  let x = symb "x" in
  let not_parsed = Stream.of_list [ x; symb "+"; x ] in
  let parsed = BinO (x, "+", x) in
  Alcotest.(check string)
    "simple"
    (to_string (SupPrat.expression not_parsed))
    (to_string parsed)

let two_operators () =
  SupPrat.flush ();
  SupPrat.add_binary "+" 1.0 Pratter.Left;
  SupPrat.add_binary "*" 1.1 Pratter.Left;
  let x = symb "x" in
  let not_parsed = Stream.of_list [ x; symb "+"; x; symb "*"; x ] in
  let parsed =
    let right = Support.make_bin_appl x None ("*", Pratter.Left, 1.1) x in
    Support.make_bin_appl x None ("+", Pratter.Left, 1.0) right
  in
  Alcotest.(check string)
    "two-ops"
    (to_string (SupPrat.expression not_parsed))
    (to_string parsed)

let appl_opertor () =
  SupPrat.flush ();
  SupPrat.add_binary "+" 1.0 Pratter.Left;
  let f = symb "f" in
  let x = symb "x" in
  let not_parsed = Stream.of_list [ f; x; symb "+"; x ] in
  let parsed =
    let plus = ("+", Pratter.Left, 1.0) in
    Support.(make_bin_appl (make_appl f x) None plus x)
  in
  Alcotest.(check string)
    "appl-bin"
    (to_string (SupPrat.expression not_parsed))
    (to_string parsed)

let simple_unary () =
  SupPrat.flush ();
  SupPrat.add_unary "!" 1.0;
  let x = symb "x" in
  let not_parsed = Stream.of_list [ symb "!"; x ] in
  let parsed = Support.make_una_appl None ("!", 1.0) x in
  Alcotest.(check string)
    "simple"
    (to_string (SupPrat.expression not_parsed))
    (to_string parsed)

let unary_appl () =
  SupPrat.flush ();
  SupPrat.add_unary "!" 1.0;
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ symb "!"; f; x ] in
  let parsed = Support.(make_una_appl None ("!", 1.0) (make_appl f x)) in
  Alcotest.(check string)
    "application"
    (to_string (SupPrat.expression not_parsed))
    (to_string parsed)

let unary_appl_in () =
  SupPrat.flush ();
  SupPrat.add_unary "!" 1.0;
  let x = symb "x" in
  let f = symb "f" in
  let not_parsed = Stream.of_list [ f; symb "!"; x ] in
  let parsed =
    let inside = Support.make_una_appl None ("!", 1.0) x in
    Support.(make_appl f inside)
  in
  Alcotest.(check string)
    "application not top"
    (to_string (SupPrat.expression not_parsed))
    (to_string parsed)

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
        ; test_case "application" `Quick unary_appl
        ; test_case "application arg" `Quick unary_appl_in
        ] )
    ]
