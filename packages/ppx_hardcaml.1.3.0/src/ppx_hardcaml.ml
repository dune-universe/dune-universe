(*
 * Copyright (c) 2016 Xavier R. Guérin <copyright@applepine.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Migrate_parsetree
open Ast_403

open Ast_mapper
open Ast_convenience_403
open Asttypes
open StdLabels
open Longident
open Parsetree
open Printf

(* Exception *)

let location_exn ~loc msg =
  Location.Error (Location.error ~loc msg)
  |> raise
;;

(*
  rules:
    1. in [%hw ...] only allow +ve constants
    2. in [%hw.signed ...] leading bit always represents sign
    3. outside [%hw{.signed} ...] smallest bit pattern that represents constant

     | general |  %hw | %hw.signed
----------------------------------
-10  | 10110   |      | 10110
 -9  | 10111   |      | 10111
 -8  |  1000   |      |  1000
 -7  |  1001   |      |  1001
 -6  |  1010   |      |  1010
 -5  |  1011   |      |  1011
 -4  |   100   |      |   100
 -3  |   101   |      |   101
 -2  |    10   |      |    10
 -1  |     1   |      |     1
  0  |     0   |    0 |     0
  1  |     1   |    1 |    01
  2  |    10   |   10 |   010
  3  |    11   |   11 |   011
  4  |   100   |  100 |  0100
  5  |   101   |  101 |  0101
  6  |   110   |  110 |  0110
  7  |   111   |  111 |  0111
  8  |  1000   | 1000 | 01000
  9  |  1001   | 1001 | 01001
 10  |  1010   | 1010 | 01010
*)
let consti_mapper ~loc ~signed v =
  let rec nbits x = match x with 0 | 1 -> 1 | x -> 1 + (nbits (x/2)) in
  let rec sbits i = if i >= -1 then nbits (abs i) else 1 + sbits (abs (i+1)) in
  let v = int_of_string v in
  let nbits =
    match signed with
    | `unsigned when v < 0 ->
      location_exn ~loc "Invalid constant format - expecting unsigned value"
    | `signed when v > 0 -> 1 + nbits v
    | _ when v < 0 -> sbits v
    | _ -> nbits v
  in
  [%expr consti [%e int nbits] [%e int v]]

let const_mapper ~signed = function
  | { pexp_desc = Pexp_constant(Pconst_integer(txt, Some('h'))) } as expr
    when String.length txt > 2 && String.sub txt ~pos:0 ~len:2 = "0b" ->
    let l = String.length txt - 2 in
    let s = String.sub txt ~pos:2 ~len:l in
    let v = { expr with pexp_desc = Pexp_constant(Pconst_string(s, None)) } in
    [%expr constb [%e v]]
  | { pexp_desc = Pexp_constant(Pconst_integer(txt, Some('h'))); pexp_loc } ->
    consti_mapper ~loc:pexp_loc ~signed txt
  | { pexp_loc } -> location_exn ~loc:pexp_loc "Invalid constant format"

let match_mapper ~loc resize sel cases =
  let is_catchall case = case.pc_guard = None && case.pc_lhs.ppat_desc = Ppat_any in

  (* no exceptions *)
  let exns, cases =
    List.partition
      ~f:(function
      | {pc_lhs = [%pat? exception [%p? _]]; _} -> true
      | _ -> false) cases
  in
  let () = if exns <> [] then location_exn ~loc "exceptions are not supported" in

  (* must have (1) wildcard case *)
  let wildcard,cases = List.partition ~f:is_catchall cases in
  let default = match wildcard with [x] -> x.pc_rhs | _ -> location_exn ~loc "expecting wildcard" in

  (* extract lhs of cases *)
  let cases =
    List.map
    ~f:(function
      | { pc_lhs={ppat_desc=Ppat_constant(Pconst_integer(i, None)); _};
          pc_guard=None; pc_rhs } ->
        int_of_string i, pc_rhs
      | _ -> location_exn ~loc "match pattern must be an (unguarded) integer")
    cases
  in

  (* check that each case is unique. note; this is also done at runtime. *)
  let check_unique_cases cases =
    let module S = Set.Make(struct type t = int let compare = compare end) in
    let add s (i,_) = S.add i s in
    let s = List.fold_left ~f:add ~init:S.empty cases in
    if S.cardinal s <> List.length cases then
      location_exn ~loc "match patterns must be unique"
  in
  check_unique_cases cases;

  (* turn cases into an [(int*signal) list] expression *)
  let cases = List.fold_right
    ~f:(fun (x,y) l -> [%expr ([%e int x], [%e y]) :: [%e l]]) cases ~init:[%expr []]
  in

  (* build muxes *)
  [%expr matches ~resize:[%e resize] ~default:[%e default] [%e sel] [%e cases]]

let resize signed =
  if signed=`signed then [%expr sresize] else [%expr uresize]

let app ~signed f a b =
  [%expr resize_op2 ~resize:[%e resize signed] [%e f] [%e a] [%e b]]

let expr_mapper ~signed m expr =
  let app' = app ~signed in
  let resize' = resize signed in
  let mul, lt, lte, gt, gte =
    if signed=`signed then
      [%expr ( *+ )], [%expr (<+)], [%expr (<=+)], [%expr (>+)], [%expr (>=+)]
    else
      [%expr ( *: )], [%expr (<:)], [%expr (<=:)], [%expr (>:)], [%expr (>=:)]
  in
  (* Check the type of the expression *)
  begin match expr with
    (* Bitwise operators *)
    | [%expr [%e? a] lor  [%e? b]] -> `Recurse (app' [%expr (|:)] a b)
    | [%expr [%e? a] land [%e? b]] -> `Recurse (app' [%expr (&:)] a b)
    | [%expr [%e? a] lxor [%e? b]] -> `Recurse (app' [%expr (^:)] a b)
    | [%expr         lnot [%e? a]] -> `Recurse [%expr ~: [%e a]]
    (* Arithmetic operators *)
    | [%expr [%e? a] +    [%e? b]] -> `Recurse (app' [%expr (+:)] a b)
    | [%expr [%e? a] *    [%e? b]] -> `Recurse (app' mul a b)
    | [%expr [%e? a] -    [%e? b]] -> `Recurse (app' [%expr (-:)] a b)
    (* Comparison operators *)
    | [%expr [%e? a] <    [%e? b]] -> `Recurse (app' lt  a b)
    | [%expr [%e? a] <=   [%e? b]] -> `Recurse (app' lte a b)
    | [%expr [%e? a] >    [%e? b]] -> `Recurse (app' gt  a b)
    | [%expr [%e? a] >=   [%e? b]] -> `Recurse (app' gte a b)
    | [%expr [%e? a] ==   [%e? b]] -> `Recurse (app' [%expr (==:)] a b)
    | [%expr [%e? a] <>   [%e? b]] -> `Recurse (app' [%expr (<>:)] a b)
    (* Concatenation operator *)
    | [%expr [%e? a] @    [%e? b]] -> `Recurse [%expr [%e a] @: [%e b]]
    (* Process valid signal index operator *)
    | [%expr [%e? s].[[%e? i0], [%e? i1]]] ->
      let beh = m.expr m s in
      `Return [%expr select [%e beh] [%e i0] [%e i1]]
    (* Process valid signal single bit operator *)
    | [%expr [%e? s].[[%e? i]]] ->
      let beh = m.expr m s in
      `Return [%expr bit [%e beh] [%e i]]
    (* unsigned if/then/else construct *)
    | [%expr [%hw if [%e? cnd] then [%e? e0] else [%e? e1]]] ->
      if signed = `unsigned then
        `Recurse (app' [%expr mux2 [%e cnd]] e0 e1)
      else
        location_exn ~loc:expr.pexp_loc "swapping from %hw.signed to %hw prohibited"
    | [%expr [%hw if [%e? cnd] then [%e? e0]]] ->
      location_exn ~loc:expr.pexp_loc "'if%hw' statement much have an 'else' clause"
    (* signed if/then/else construct *)
    | [%expr [%hw.signed if [%e? cnd] then [%e? e0] else [%e? e1]]] ->
      if signed = `signed then
        `Recurse (app' [%expr mux2 [%e cnd]] e0 e1)
      else
        location_exn ~loc:expr.pexp_loc "swapping from %hw to %hw.signed prohibited"
    | [%expr [%hw.signed if [%e? cnd] then [%e? e0]]] ->
      location_exn ~loc:expr.pexp_loc "'if%hw.signed' statement much have an 'else' clause"
    (* unsigned match construct *)
    | [%expr [%hw [%e? { pexp_desc=Pexp_match(sel, cases); pexp_loc=loc }]]] ->
      if signed = `unsigned then
        let sel = m.expr m sel in
        let cases = List.map (fun c -> { c with pc_rhs = m.expr m c.pc_rhs }) cases in
        `Return (match_mapper ~loc resize' sel cases)
      else
        location_exn ~loc:expr.pexp_loc "swapping from %hw.signed to %hw prohibited"
    (* signed match construct *)
    | [%expr [%hw.signed [%e? { pexp_desc=Pexp_match(sel, cases); pexp_loc=loc }]]] ->
      if signed = `signed then
        let sel = m.expr m sel in
        let cases = List.map (fun c -> { c with pc_rhs = m.expr m c.pc_rhs }) cases in
        `Return (match_mapper ~loc resize' sel cases)
      else
        location_exn ~loc:expr.pexp_loc "swapping from %hw to %hw.signed prohibited"
    (* [%hw ] expression *)
    | [%expr [%hw [%e? e]]] ->
      if signed = `unsigned then `Recurse e
      else location_exn ~loc:expr.pexp_loc "swapping from %hw.signed to %hw prohibited"
    | [%expr [%hw.signed [%e? e]]] ->
      if signed = `signed then `Recurse e
      else location_exn ~loc:expr.pexp_loc "swapping from %hw to %hw.signed prohibited"
    (* Constant *)
    | { pexp_desc = Pexp_constant(Pconst_integer(_, Some('h'))) } ->
      `Recurse (const_mapper ~signed expr)
    (* Default *)
    | expr -> `Default
  end
  (* Call the proper mapper if the expression was rewritten or not *)
  |> function
  | `Recurse (expr) -> m.expr m expr
  | `Return (expr)  -> expr
  | `Default        -> default_mapper.expr m expr

let smapper m =
  expr_mapper ~signed:`signed { m with expr = expr_mapper ~signed:`signed }

and umapper m =
  expr_mapper ~signed:`unsigned { m with expr = expr_mapper ~signed:`unsigned }

(* Top level mapper *)

let let_binding ~signed mapper bindings expr nexp =
  let wb = List.map
      (fun ({ pvb_pat; pvb_expr } as binding) ->
         { binding with
           pvb_pat = mapper.pat mapper pvb_pat;
           pvb_expr = smapper mapper pvb_expr;
         })
      bindings
  and next = mapper.expr mapper nexp
  in
  { expr with pexp_desc = Pexp_let(Nonrecursive, wb, next) }

let expr_mapper config mapper expr =
  match expr with
  (* let%hw expression *)
  | [%expr [%hw [%e? { pexp_desc = Pexp_let(Nonrecursive, bindings, nexp) } ]]] ->
    let_binding ~signed:`unsigned mapper bindings expr nexp
  | [%expr [%hw.signed [%e? { pexp_desc = Pexp_let(Nonrecursive, bindings, nexp) } ]]] ->
    let_binding ~signed:`signed mapper bindings expr nexp
  (* unsigned if/then/else construct *)
  | [%expr [%hw if [%e? cnd] then [%e? e0] else [%e? e1]]] ->
    let e = app ~signed:`unsigned [%expr mux2 [%e cnd]] e0 e1 in
    [%expr [%e umapper mapper e]]
  | [%expr [%hw if [%e? cnd] then [%e? e0]]] ->
    location_exn ~loc:expr.pexp_loc "'if%hw' statement much have an 'else' clause"
  (* signed if/then/else construct *)
  | [%expr [%hw.signed if [%e? cnd] then [%e? e0] else [%e? e1]]] ->
    let e = app ~signed:`signed [%expr mux2 [%e cnd]] e0 e1 in
    [%expr [%e smapper mapper e]]
  | [%expr [%hw.signed if [%e? cnd] then [%e? e0]]] ->
    location_exn ~loc:expr.pexp_loc "'if%hw.signed' statement much have an 'else' clause"
  (* unsigned match construct *)
  | [%expr [%hw [%e? { pexp_desc=Pexp_match(sel, cases); pexp_loc=loc }]]] ->
    let sel = umapper mapper sel in
    let cases = List.map (fun c -> { c with pc_rhs = umapper mapper c.pc_rhs }) cases
    in
    umapper mapper (match_mapper ~loc (resize `unsigned) sel cases)
  (* signed match construct *)
  | [%expr [%hw.signed [%e? { pexp_desc=Pexp_match(sel, cases); pexp_loc=loc }]]] ->
    let sel = smapper mapper sel in
    let cases = List.map (fun c -> { c with pc_rhs = smapper mapper c.pc_rhs }) cases
    in
    smapper mapper (match_mapper ~loc (resize `signed) sel cases)
  (* [%hw ] expression *)
  | [%expr [%hw [%e? e]]] -> [%expr [%e umapper mapper e]]
  | [%expr [%hw.signed [%e? e]]] -> [%expr [%e smapper mapper e]]
  (* Constant *)
  | { pexp_desc = Pexp_constant(Pconst_integer(_, Some('h'))) } ->
    const_mapper ~signed:`smallest expr
  (* Default mapper *)
  | _ -> default_mapper.expr mapper expr

let structure_item_mapper config mapper stri =
  match stri with
  (* [%hw let pat = <expr>] or 'let%hw pat = <expr>' *)
  | [%stri [%%hw let [%p? var] = [%e? e0]]] ->
    [%stri let [%p mapper.pat mapper var] = [%e umapper mapper e0]]
  | [%stri [%%hw.signed let [%p? var] = [%e? e0]]] ->
    [%stri let [%p mapper.pat mapper var] = [%e smapper mapper e0]]
  | _ -> default_mapper.structure_item mapper stri

let rewriter config cookies = {
  Ast_mapper.default_mapper with
  expr = expr_mapper config;
  structure_item = structure_item_mapper config
}

let () =
  Driver.register ~name:"ppx_hardcaml" ~args:[] Versions.ocaml_403 rewriter
;;
