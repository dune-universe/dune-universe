
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Bindings to Quantor} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a sequence = ('a -> unit) -> unit

type assignment =
  | True
  | False
  | Undef

let pp_assignment fmt = function
  | True -> Format.pp_print_string fmt "true"
  | False -> Format.pp_print_string fmt "false"
  | Undef -> Format.pp_print_string fmt "undef"

type quantifier =
  | Forall
  | Exists

let pp_quantifier fmt = function
  | Forall -> Format.pp_print_string fmt "forall"
  | Exists -> Format.pp_print_string fmt "exists"

(** {2 a QBF literal} *)
module Lit = struct
  type t = int
  (** A boolean literal is only a non-zero integer *)

  let make i =
    if i=0 then raise (Invalid_argument "Lit.make");
    i

  let neg i = ~- i
  let to_int i = abs i

  let sign i = i>0
  let abs = abs
  let neg i = -i
  let apply_sign b i = if b then i else neg i
  let set_sign b i = if b then abs i else neg (abs i)

  let fresh =
    let r = ref 0 in
    fun () ->
      incr r;
      !r

  let equal (i:int) j = i=j
  let compare (i:int) j = Stdlib.compare i j
  let hash_fun i h = Int64.of_int (Hashtbl.hash (i,h))
  let hash i = i land max_int
  let print fmt i =
    if i>0
    then Format.fprintf fmt "L%i" i
    else Format.fprintf fmt "¬L%d" (-i)
end

let print_l ?(sep=",") pp_item fmt l =
  let rec print fmt l = match l with
    | x::((y::xs) as l) ->
      pp_item fmt x;
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      print fmt l
    | x::[] -> pp_item fmt x
    | [] -> ()
  in
  print fmt l

let _print_quant fmt = function
  | Forall -> Format.pp_print_string fmt "∀"
  | Exists -> Format.pp_print_string fmt "∃"

(** {2 A QBF Formula in CNF} *)
module CNF = struct
  type clause = Lit.t list
  type t = clause list

  let equal = (=)
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let _print_clause ~pp_lit fmt c = match c with
    | [] -> Format.pp_print_string fmt "[]"
    | [x] -> pp_lit fmt x
    | _::_::_ ->
        Format.fprintf fmt "@[<hov 2>(%a)@]" (print_l ~sep:" ∨ " pp_lit) c

  let print_with ~pp_lit fmt l =
    Format.fprintf fmt "@[<hv>%a@]"
      (print_l ~sep:", " (_print_clause ~pp_lit)) l

  let print = print_with ~pp_lit:Lit.print
end

module QCNF = struct
  type t =
    | Quant of quantifier * Lit.t list * t
    | Prop of CNF.t

  let quantify q lits f = match lits, f with
    | [], _ -> f
    | _, Quant (q', lits', f') when q=q' ->
        Quant (q, List.rev_append lits lits', f')
    | _ -> Quant (q, lits, f)

  let forall lits f = quantify Forall lits f
  let exists lits f = quantify Exists lits f
  let prop c = Prop c

  let equal = (=)
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let print_with ~pp_lit fmt f =
    let rec _print fmt f = match f with
      | Prop l ->
          Format.fprintf fmt "@[<hov 2>%a@]" (CNF.print_with ~pp_lit) l
      | Quant (q,lits,cnf) ->
          Format.fprintf fmt "%a@ @[<hv 0>%a@].@ %a" _print_quant q
            (print_l ~sep:" " pp_lit) lits _print cnf
    in
    _print fmt f

  let print = print_with ~pp_lit:Lit.print
end

(** {2 a QBF formula} *)
module Formula = struct
  type t =
    | And of t list
    | Or of t list
    | Imply of t * t
    | XOr of t list
    | Equiv of t list
    | True
    | False
    | Not of t
    | Atom of Lit.t

  let true_ = True
  let false_ = False
  let atom l = Atom l

  let neg = function
    | Not f -> f
    | f -> Not f

  let and_l = function
    | [] -> True
    | [x] -> x
    | l -> And l

  let or_l = function
    | [] -> False
    | [x] -> x
    | l -> Or l

  let xor_l = function
    | [] -> False
    | [x] -> x
    | l -> XOr l

  let equiv_l = function
    | []
    | [_] -> True
    | l -> Equiv l

  let imply a b = match a, b with
    | _, True
    | False, _ -> True
    | True, _ -> b
    | _ -> Imply (a,b)

  let seq_to_list_ seq =
    let l = ref [] in
    seq (fun x -> l := x :: !l);
    !l

  let and_seq seq = and_l (seq_to_list_ seq)
  let or_seq seq = or_l (seq_to_list_ seq)

  let map_list_ f seq =
    let l = ref [] in
    seq
      (fun x ->
        let form = f x in
        l := form :: !l
      );
    !l

  let and_map ~f seq = and_l (map_list_ f seq)
  let or_map ~f seq = or_l (map_list_ f seq)

  let equal = (=)
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let print_with ~pp_lit fmt f =
    let rec print fmt f = match f with
      | Atom a -> pp_lit fmt a
      | True -> Format.pp_print_string fmt "true"
      | False -> Format.pp_print_string fmt "false"
      | Not f -> Format.fprintf fmt "@[<h>¬%a@]" print_f' f
      | And l ->
          Format.fprintf fmt "@[<0>%a@]" (print_l ~sep:" ∧ " print_f') l
      | Or l ->
          Format.fprintf fmt "@[<0>%a@]" (print_l ~sep:" v " print_f') l
      | XOr l -> Format.fprintf fmt "@[<h>Xor %a@]"
          (print_l ~sep:" " print_f') l
      | Equiv l ->
          Format.fprintf fmt "@[<h>Equiv %a@]" (print_l ~sep:" " print_f') l
      | Imply (a,b) ->
          Format.fprintf fmt "@[<hov2>%a =>@ %a@]" print_f' a print_f' b
    and print_f' fmt f = match f with
      | Atom _
      | True
      | False -> print fmt f
      | _ -> Format.fprintf fmt "(%a)" print f
    in print fmt f

  let print = print_with ~pp_lit:Lit.print

  let rec simplify = function
    | Not f -> _neg_simplify f
    | And l -> and_l (List.rev_map simplify l)
    | Or l -> or_l (List.rev_map simplify l)
    | Atom _ as f -> f
    | Imply (a, b) -> imply (simplify a) (simplify b)
    | XOr l -> xor_l (List.map simplify l)  (* TODO *)
    | Equiv l -> equiv_l (List.map simplify l)
    | (True | False) as f -> f
  and _neg_simplify = function
    | Atom l -> Atom (Lit.neg l)
    | And l -> Or (List.map _neg_simplify l)
    | Or l -> And (List.map _neg_simplify l)
    | XOr l -> Not (XOr l)
    | Equiv l -> Not (Equiv l)
    | Imply (a,b) -> and_l [a; _neg_simplify b]
    | Not f -> simplify f
    | True -> False
    | False -> True

  (* polarity of a subformula: number of negation on the path to the root *)
  type polarity =
    | Plus
    | Minus

  let _neg_pol = function
    | Plus -> Minus
    | Minus -> Plus

  (* Reduce formula to cnf *)
  module CnfAlgo = struct
    type ctx = {
      mk_new_lit : unit -> Lit.t; (* get a fresh literal *)
      add_clauses : CNF.clause list -> unit; (* declare clauses *)
      get_clauses : unit -> CNF.clause list; (* all clauses so far *)
      get_newlits : unit -> Lit.t list;  (* gensym'd literals *)
    }

    let mk_ctx gensym =
      let clauses = ref [] in
      let newlits = ref [] in
      let mk_new_lit () =
        let x = gensym() in
        newlits := x :: !newlits;
        x
      in
      { mk_new_lit;
        add_clauses=(fun cs ->
          clauses := List.rev_append cs !clauses
        );
        get_clauses=(fun () -> !clauses);
        get_newlits=(fun () -> !newlits);
      }

    (* rename [And_{c in clauses} c] into an atom *)
    let rename_clauses ~ctx clauses =
      let x = ctx.mk_new_lit () in
      (* definition of [x]: [not x or c] for every [c] in [clauses] *)
      let side_clauses = List.rev_map
        (fun c -> Lit.neg x :: c)
        clauses
      in
      ctx.add_clauses side_clauses;
      x

    (* list of [f (x,y)] for [x,y] elements of [l] with
       [x] occurring before [y] *)
    let map_diagonal f l =
      let rec gen acc l = match l with
      | [] -> acc
      | x::l' ->
        let acc = List.fold_left (fun acc y -> f x y :: acc) acc l' in
        gen acc l'
      in
      gen [] l

    (* reduce quantifier-free formula to CNF, with Tseitin transformation
      (see https://en.wikipedia.org/wiki/Tseitin_transformation).
      @param acc the list of clauses produced so far
      @param pol the polarity of [f]
      @param gensym generator of fresh *)
    let rec cnf ~ctx ~pol acc f = match f, pol with
      | Not f', _ -> cnf ~ctx ~pol:(_neg_pol pol) acc f'
      (* trivial cases *)
      | True, Plus
      | False, Minus -> acc  (* tautology *)
      | True, Minus
      | False, Plus -> []::acc  (* empty clause *)
      | Atom a, Plus -> [a]::acc
      | Atom a, Minus -> [Lit.neg a]::acc
      (* and/or-cases *)
      | (Or [] | And [] | XOr [] | Equiv []), _ -> assert false
      | And l, Plus
      | Or l, Minus ->
          List.fold_left (fun acc f' -> cnf ~ctx ~pol acc f' ) acc l
      | And (a::l), Minus ->
          let a' = cnf ~ctx ~pol [] a in
          let l' = List.map (cnf ~ctx ~pol []) l in
          cnf_list ~ctx acc a' l'
      | Or (a::l), Plus ->
          (* CNF of each sub-formula *)
          let a' = cnf ~ctx ~pol [] a in
          let l' = List.map (cnf ~ctx ~pol []) l in
          (* now express the disjunction of those lists of clauses. For each
              list, we can rename it or just use it *)
          cnf_list ~ctx acc a' l'
      (* specials *)
      | Imply (a,b), Plus ->
          cnf_list ~ctx acc
            (cnf ~ctx ~pol:Minus [] a)
            [cnf ~ctx ~pol:Plus [] b]
      | Imply (a,b), Minus ->
          (* not (a=>b) ----->   a and not b *)
          let acc = cnf ~ctx ~pol:Plus acc a in
          let acc = cnf ~ctx ~pol:Minus acc b in
          acc
      | XOr l, _ ->
          (* TODO: efficient version *)
          (* right now, it's
            (Or_{f in l} f) and (And_{f,f' in l, f!=f'} not f or not f')
            *)
          let f = and_l
            (or_l l :: map_diagonal (fun a b -> or_l [neg a; neg b]) l)
          in
          cnf ~ctx ~pol acc f
      | Equiv l, _ ->
          (* TODO: efficient version *)
          (* right now, it's  (And_{f in l} f) or (And_{f in l} not f) *)
          let f = or_l [ and_l l; and_l (List.map neg l) ] in
          cnf ~ctx ~pol acc f

    (* basically, [cartesian_product previous (cnf l)], but smarter. Adds
        its result to [acc] *)
    and cnf_list ~ctx acc previous l = match l with
      | [] -> List.rev_append previous acc
      | [] :: _ -> [] :: acc (* absurd, so product is absurd *)
      | [c] :: tail ->
          (* add [c] to every clause so far *)
          let previous = List.rev_map
            (fun c' -> List.rev_append c c')
            previous
          in
          cnf_list ~ctx acc previous tail
      | clauses :: tail ->
          (* rename [clauses] into a new atom *)
          let x = rename_clauses ~ctx clauses in
          let previous = List.rev_map (fun c' -> x::c') previous in
          cnf_list ~ctx acc previous tail
  end

  let cnf ?(gensym=Lit.fresh) f =
    let ctx = CnfAlgo.mk_ctx gensym in
    ctx.CnfAlgo.add_clauses (CnfAlgo.cnf ~ctx ~pol:Plus [] f);
    ctx.CnfAlgo.get_clauses (), ctx.CnfAlgo.get_newlits()
end

module QFormula = struct
  type t =
    | Quant of quantifier * Lit.t list * t
    | Prop of Formula.t

  let equal = (=)
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let quantify q lits f = match lits, f with
    | [], _ -> f
    | _, Quant (q', lits', f') when q=q'->
        Quant (q, List.rev_append lits lits', f')
    | _ -> Quant (q, lits, f)

  let forall lits f = quantify Forall lits f
  let exists lits f = quantify Exists lits f
  let prop f = Prop f

  let rec simplify = function
    | Quant (q, lits, f) -> Quant (q, lits, simplify f)
    | Prop f -> Prop (Formula.simplify f)

  let print_with ~pp_lit fmt f =
    let rec print fmt f = match f with
      | Quant (q,lits,f') ->
          Format.fprintf fmt "@[<hov>%a %a.@ %a@]"
            _print_quant q (print_l ~sep:" " pp_lit) lits print f'
      | Prop f -> Formula.print fmt f
    in print fmt f

  let print = print_with ~pp_lit:Lit.print

  let cnf ?(gensym=Lit.fresh) f =
    (* traverse prenex quantifiers, and convert inner formula into CNF *)
    let rec traverse f = match f with
      | Quant (q, lits, f') ->
          QCNF.quantify q lits (traverse f')
      | Prop f ->
          (* CNF of [f], plus a list of new variables to quantify on
            and side clauses that define those variables *)
          let clauses, new_lits = Formula.cnf ~gensym f in
          let cnf = QCNF.prop clauses in
          QCNF.exists new_lits cnf
    in
    traverse f
end

(** {2 Solvers} *)

type result =
  | Unknown
  | Sat of (Lit.t -> assignment)
  | Unsat
  | Timeout
  | Spaceout

let pp_result fmt = function
  | Unknown -> Format.pp_print_string fmt "unknown"
  | Sat _ -> Format.pp_print_string fmt "sat"
  | Unsat -> Format.pp_print_string fmt "unsat"
  | Timeout -> Format.pp_print_string fmt "timeout"
  | Spaceout -> Format.pp_print_string fmt "spaceout"


type solver = {
  name : string;
  solve : QCNF.t -> result;
}

let solve ~solver cnf =
  solver.solve cnf
