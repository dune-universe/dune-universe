open Migrate_parsetree

(* Define the rewriter on OCaml 4.05 AST *)
open Ast_405
let ocaml_version = Versions.ocaml_405

type atom = string

module CSet = Set.Make(Char)

(** Regular expressions *)
module Re = struct

  type t =
    | Epsilon
    | Chars of CSet.t
    | Atom of atom
    | Concat of t list
    | Alt of t * t
    | Inter of t * t
    | Rep of int * int option * t

  let compare = Pervasives.compare
  let equal x y = compare x y = 0

  let epsilon = Epsilon
  let void = Chars CSet.empty
  let atom s = Atom s
  let char c = atom (String.make 1 c)
  let charset cs = Chars (CSet.of_list cs)
  let enumerate c1 c2 =
    if c1 > c2 then None
    else
      let rec aux i m =
        if i > m then []
        else Char.chr i :: aux (i+1) m
      in
      Some (aux (Char.code c1) (Char.code c2))

  let concat l =
    let rec aux = function
      | [] -> []
      | Concat l :: l' -> aux (l @ l')
      | Epsilon :: l -> aux l
      | Chars cs :: _ when CSet.is_empty cs -> [void]
      | x :: l -> x :: aux l
    in
    match aux l with
    | [] -> Epsilon
    | [x] -> x
    | l -> Concat l

  let alt re re' = match re, re' with
    | x, x' when equal x x' -> x
    | Chars cs, x when CSet.is_empty cs -> x
    | x, Chars cs when CSet.is_empty cs -> x
    | Chars cs1, Chars cs2 -> Chars (CSet.union cs1 cs2)
    | a, b ->
      let i = compare a b in
      if i >= 0 then Alt (a, b) else Alt (b, a)

  let inter re re' = match re, re' with
    | x, x' when equal x x' -> x
    | _, Chars cs when CSet.is_empty cs -> void
    | Chars cs, _ when CSet.is_empty cs -> void
    | a, b ->
      let i = compare a b in
      if i >= 0 then Inter (a, b) else Inter (b, a)

  let rec rep i j x = match i, j, x with
    | 0, Some 0, _ -> Epsilon
    | 1, Some 1, x -> x
    | _, _, Epsilon -> epsilon
    | _, _, Chars cs when CSet.is_empty cs -> epsilon
    | _, _, Rep (i', None, x)
    | _, None, Rep (i', Some _, x) -> rep (i * i') None x
    | _, Some j, Rep (i', Some j', x) -> rep (i * i') (Some (j * j')) x
    | i, j, x -> Rep (i, j, x)

  let star x = rep 0 None x
  let plus x = rep 1 None x
  let opt x = rep 0 (Some 1) x

  module Infix = struct
    let ( + ) = alt
    let ( * ) x y = concat [ x ; y ]
    let ( ! ) = atom
  end
end
module ReMap = Map.Make(Re)
module SMap = Map.Make(String)

module Var : sig
  type t
  val pp : t Fmt.t
  val gen : unit -> t
  val name : t -> string
end = struct
  type t = string
  let pp fmt x = Fmt.pf fmt "'%s" x
  let gen =
    let r = ref 0 in
    fun () ->
      incr r ;
      "v" ^ string_of_int !r
  let name x = x
end

(** Type grammar *)

type ty =
  | Alt of transition list
  | As of ty * Var.t
  | Alias of Var.t
and transition =
  | A of atom * ty
  | E

let rec pp_transition ppf = function
  | A (a, ty) ->
    Fmt.pf ppf "`%s of %a" a pp_ty ty
  | E ->
    Fmt.pf ppf "`End"
and pp_ty ppf = function
  | Alias v -> Var.pp ppf v
  | As (ty, v) -> Fmt.pf ppf "(%a as %a)" pp_ty ty Var.pp v
  | Alt l ->
    Fmt.pf ppf "@[<hv2>[@ %a@ ]@]"
      Fmt.(list ~sep:(unit "@ | ") pp_transition)
      l

(** Derivatives *)

let rec has_epsilon = function
  | Re.Epsilon -> true
  | Atom _ -> false
  | Concat el ->
    List.for_all has_epsilon el
  | Alt (e1, e2) ->
    has_epsilon e1 || has_epsilon e2
  | Rep (0, _, _) -> true
  | Rep (_, _, _) -> false
  | Inter (e1, e2) -> has_epsilon e1 && has_epsilon e2
  | Chars _ -> false

let prefix l re =
  let f re_c = Re.concat [re_c; re] in
  SMap.map f l

let union =
  SMap.union
    (fun _c re1 re2 -> Some (Re.alt re1 re2))

let inter =
  SMap.merge @@ fun _c re1 re2 -> match re1, re2 with
  | Some re1, Some re2 -> Some (Re.inter re1 re2)
  | _, _ -> None

let charset cs =
  CSet.fold
    (fun c m -> SMap.add (String.make 1 c) Re.epsilon m)
    cs
    SMap.empty

let rec heads = function
  | Re.Epsilon -> SMap.empty
  | Atom a -> SMap.singleton a Re.epsilon
  | Concat el ->
    let rec aux = function
      | [] -> SMap.empty
      | e :: t ->
        let h = prefix (heads e) (Re.concat t) in
        if has_epsilon e
        then union h (aux t)
        else h
    in
    aux el
  | Alt (e1, e2) -> union (heads e1) (heads e2)
  | Rep (i, None, e) ->
    prefix (heads e) (Re.rep (max 0 (i-1)) None e)
  | Rep (i, Some j, e) ->
    prefix (heads e) (Re.rep (max 0 (i-1)) (Some (max 0 (j-1))) e)
  | Inter (e1,e2) -> inter (heads e1) (heads e2)
  | Chars cs -> charset cs

let add_new_ty re m =
  let v = Var.gen () in
  let m = ReMap.add re (Alias v) m in
  m, v

let rec goto c re (map,l) =
  if ReMap.mem re map then
    (map, A (c, ReMap.find re map) :: l)
  else
    let map, var = add_new_ty re map in
    let map, ty = explore map re in
    (map, A (c, As (ty, var)) :: l)

and explore map re =
  let l = heads re in
  let init = if has_epsilon re then [E] else [] in
  let map, alts = SMap.fold goto l (map, init) in
  map, Alt alts

let make_type re =
  let map, var = add_new_ty re ReMap.empty in
  let _, ty = explore map re in
  As (ty, var)

(** Posix parser, borrowed from Re *)
module Posix = struct

  exception Parse_error
  exception Not_supported

  let parse s =
    let i = ref 0 in
    let l = String.length s in
    let eos () = !i = l in
    let test c = not (eos ()) && s.[!i] = c in
    let accept c = let r = test c in if r then incr i; r in
    let get () = let r = s.[!i] in incr i; r in
    let unget () = decr i in

    let rec regexp () = regexp' (branch ())
    and regexp' left =
      if accept '|' then regexp' (Re.alt left (branch ()))
      else if accept '&' then regexp' (Re.inter left (branch ()))
      else left
    and branch () = branch' []
    and branch' left =
      if eos () || test '|' || test '&' || test ')' then Re.concat (List.rev left)
      else branch' (piece () :: left)
    and piece () =
      let r = atom () in
      if accept '*' then Re.star r else
      if accept '+' then Re.plus r else
      if accept '?' then Re.opt r else
      if accept '{' then
        match integer () with
          Some i ->
          let j = if accept ',' then integer () else Some i in
          if not (accept '}') then raise Parse_error;
          begin match j with
              Some j when j < i -> raise Parse_error | _ -> ()
          end;
          Re.rep i j r
        | None ->
          unget (); r
      else
        r
    and atom () =
      if accept '.' then begin
        raise Not_supported
        (* if newline then Re.notnl else Re.any *)
      end else if accept '(' then begin
        let r = regexp () in
        if not (accept ')') then raise Parse_error;
        r
      end else
      if accept '^' then begin
        raise Not_supported
        (* if newline then Re.bol else Re.bos *)
      end else if accept '$' then begin
        raise Not_supported
        (* if newline then Re.eol else Re.eos *)
      end else if accept '[' then begin
        if accept '^' then
          raise Not_supported
          (* Re.diff (Re.compl (bracket [])) (Re.char '\n') *)
        else
          Re.charset (bracket [])
      end else
      if accept '\\' then begin
        if eos () then raise Parse_error;
        match get () with
          '|' | '&' | '(' | ')' | '*' | '+' | '?'
        | '[' | '.' | '^' | '$' | '{' | '\\' as c -> Re.char c
        |                 _                       -> raise Parse_error
      end else begin
        if eos () then raise Parse_error;
        match get () with
          '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
        |                 c            -> Re.char c
      end
    and integer () =
      if eos () then None else
        match get () with
          '0'..'9' as d -> integer' (Char.code d - Char.code '0')
        |     _        -> unget (); None
    and integer' i =
      if eos () then Some i else
        match get () with
          '0'..'9' as d ->
          let i' = 10 * i + (Char.code d - Char.code '0') in
          if i' < i then raise Parse_error;
          integer' i'
        | _ ->
          unget (); Some i
    and bracket s =
      if s <> [] && accept ']' then s else begin
        let c = char () in
        if accept '-' then begin
          if accept ']' then c :: '-' :: s else begin
            let c' = char () in
            match Re.enumerate c c' with
            | None -> raise Parse_error
            | Some l -> bracket (l @ s)
          end
        end else
          bracket (c :: s)
      end
    and char () =
      if eos () then raise Parse_error;
      let c = get () in
      if c = '[' then begin
        if accept '=' then raise Not_supported
        else if accept ':' then begin
          raise Not_supported (*XXX*)
        end else if accept '.' then begin
          if eos () then raise Parse_error;
          let c = get () in
          if not (accept '.') then raise Not_supported;
          if not (accept ']') then raise Parse_error;
          c
        end else
          c
      end else
        c
    in
    let res = regexp () in
    if not (eos ()) then raise Parse_error;
    res

  let rec simplify (re : Re.t) = match re with
    | Re.Concat l ->
      let rec aux = function
        | [] -> []
        | Re.Atom s :: Re.Atom s' :: l ->
          aux (Re.atom (s ^ s') :: l)
        | x :: l -> simplify x :: aux l
      in
      Re.concat (aux l)
    | Re.Rep (i, j, re) -> Re.rep i j (simplify re)
    | Re.Alt (re1, re2) -> Re.alt (simplify re1) (simplify re2)
    | Re.Inter (re1, re2) -> Re.inter (simplify re1) (simplify re2)
    | Re.Atom _
    | Re.Epsilon
    | Re.Chars _ as re -> re

  let make loc x =
    try
      simplify @@ parse x
    with
    | Parse_error ->
      Location.raise_errorf ~loc "This posix regular expression is invalid."
    | Not_supported ->
      Location.raise_errorf ~loc "This posix regular expression uses unsuported features."
end

(** Syntax *)

open Parsetree
module A = Ast_helper

let rec re_of_parsetree pat =
  let loc = pat.ppat_loc in
  match pat.ppat_desc with
  | Ppat_tuple l ->
    Re.concat @@ List.map re_of_parsetree l
  | Ppat_or (p1, p2) ->
    Re.alt (re_of_parsetree p1) (re_of_parsetree p2)

  | Ppat_construct ({txt=Longident.Lident "Star"}, None) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Star constructors take an argument."
  | Ppat_construct ({txt=Longident.Lident "Star"}, Some p) ->
    Re.star (re_of_parsetree p)

  | Ppat_construct ({txt=Longident.Lident ("Eps"|"Epsilon")}, None) ->
    Re.epsilon
  | Ppat_construct ({txt=Longident.Lident ("Eps"|"Epsilon")}, Some _) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Epsilon constructors take no arguments."

  | Ppat_construct ({txt=Longident.Lident ("()")}, None) ->
    Re.void
  | Ppat_construct ({txt=Longident.Lident ("()")}, Some _) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Void constructors take no arguments."

  | Ppat_construct ({txt=Longident.Lident ("Inter"|"::")},
      Some {ppat_desc = Ppat_tuple [p1; p2]}) ->
    Re.inter (re_of_parsetree p1) (re_of_parsetree p2)
  | Ppat_construct ({txt=Longident.Lident ("Inter"|"::")}, _) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Inter constructors take exactly two arguments."

  | Ppat_variant (s, None) ->
    Re.atom s
  | Ppat_variant (_, Some _) ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "Symbols can not have arguments."

  | Ppat_interval (Pconst_char c1, Pconst_char c2) ->
    begin match Re.enumerate c1 c2 with
      | Some l -> Re.charset l
      | None ->
        Location.raise_errorf ~loc "This character range is ill-formed."
    end
  | _ ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "This is not a valid regular expression constructor."

let make_variant ~loc dir l = match dir with
  | `Eq -> A.Typ.variant ~loc l Closed None
  | `GEq -> A.Typ.variant ~loc l Open None
  | `LEq -> A.Typ.variant ~loc l Closed (Some [])

let rec parsetree_of_ty loc dir ty = match ty with
  | Alt l ->
    let aux = function
      | E -> Rtag ("End", [], true, [])
      | A (c, ty) -> Rtag (c, [], false, [parsetree_of_ty loc dir ty])
    in
    make_variant ~loc dir (List.map aux l)
  | As (ty,v) ->
    A.Typ.alias ~loc (parsetree_of_ty loc dir ty) (Var.name v)
  | Alias v ->
    A.Typ.var ~loc (Var.name v)

let get_extension s = match Str.split (Str.regexp_string ".") s with
  | ["pumping";"re"] | ["re"] -> Some `Eq
  | ["pumping";"re";("le"|"less") ] | ["re";("le"|"less")] -> Some `LEq
  | ["pumping";"re";("ge"|"greater") ] | ["re";("ge"|"greater")] -> Some `GEq
  | _ -> None

let mapper =
  let module AM = Ast_mapper in
  let typ mapper x =
    let loc = x.ptyp_loc in
    match x.ptyp_desc with
    | Ptyp_extension ({txt}, payload) when get_extension txt <> None ->
      let dir = match get_extension txt with Some x -> x | None -> assert false in
      let p = match payload with
        | PStr [{pstr_desc = Pstr_eval
                     ({pexp_loc=loc; pexp_desc = Pexp_constant (Pconst_string (s,_))},_)}]
        | PPat ({ppat_loc=loc; ppat_desc = Ppat_constant (Pconst_string (s,_))}, None)
            -> Posix.make loc s
        | PPat (p,None) -> re_of_parsetree p
        | PStr _ | PSig _ | PTyp _ | PPat (_, Some _) ->
          Location.raise_errorf ~loc
            "The payload of this extension should be either a pattern or a string."
      in
      parsetree_of_ty loc dir @@ make_type p
    | _ -> AM.default_mapper.typ mapper x
  in
  { AM.default_mapper with typ }


(* Register the rewriter in the driver *)
let () =
  Driver.register
    ~name:"pumping"
    ocaml_version
    (fun _config _cookies -> mapper)

(*
   Pumping - Regular languages in types

   Copyright (C) 2017 Gabriel Radanne <drupyog@zoho.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)
