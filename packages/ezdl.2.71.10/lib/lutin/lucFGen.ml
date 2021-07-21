(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: fGen.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
*)

let debug = false

open Exp
open List
open Prog 


(****************************************************************************)


(** A runtime automaton is a set of atomic automata that we will
    handled in sequence.
    
    The automaton stops if one of the sub-automata stops.
*)

type node = Prog.atomic_ctrl_state

type t =
  | Cont of (unit -> t * Exp.formula * node list)
  | Finish (* no more solutions *)
  | RStop of string (* Normal Termination (for Lutin) *)


(** wt is traversed using continuations *)
type wt_cont = 
  | WCont of (unit -> wt_cont * Exp.formula * node)
  | WFinish (* no more solutions *)
  | WStop of string (* ditto, but stop at once raising an exception *)

     
(****************************************************************************)
(* Exported *)
(* use those of FGen
exception NoMoreFormula
exception NormalStop of string
*)
(* open FGen *)

let (call_cont :  t -> t * formula * node list) =
  fun cont -> 
    let _ = if debug then (print_string "XXX call_cont\n"; flush stdout) in
      match cont with
        | Cont f -> f ()
        | Finish -> raise FGen.NoMoreFormula 
        | RStop str -> raise (FGen.NormalStop str) 

let (call_wt_cont : wt_cont -> wt_cont * formula * node) =
  fun cont -> 
    let _ = if debug then (print_string "XXX call_wt_cont\n"; flush stdout) in
    match cont with
      | WCont f -> f ()
      | WFinish -> WFinish, False, ""  (* dummy *)
      | WStop _msg -> cont, True (* dummy *), "" (* dummy *)
          

(* exported *)
let (choose_one_formula: t ->  t * Exp.formula * Prog.ctrl_state) = 
  fun t  -> 
    let (t,f,cs) = call_cont t in
      (t, f, List.map Prog.string_to_atomic_ctrl_state cs)

(****************************************************************************)

let (no_more_formula : t -> bool) = fun t -> t = Finish

(* exported *)
let (get_all_formula: t -> formula list) =
  fun a ->
    let rec aux a acc =
      let (a', f, _nl) = choose_one_formula a in
   if no_more_formula a' then acc else (aux a' (f::acc))
    in
      aux a []
    
(****************************************************************************)

let rec (wt_list_to_cont : Var.env_in -> Prog.state ref -> wt_cont list ->
         formula ->  node list -> t -> t) =
  fun input state wtl facc nl fgen ->
    (* [nl] is the list of nodes that correspond to [facc] *)
    let _ = if debug then (print_string "XXX wt_list_to_cont\n"; flush stdout) in
    match wtl with
    | [] -> Cont (fun () -> (fgen, facc, nl))
    | wt::wtl' ->
      if wt = WFinish then
        fgen
      else
        match choose_one_formula_atomic input state facc wt with
        | WFinish, False, "" -> 
          fgen
        | WStop str, _, "" -> 
          RStop str
        | wt2, f2, n -> 
          let fgen' = 
            Cont (fun () -> 
                call_cont (wt_list_to_cont input state (wt2::wtl') facc nl fgen))
          in
          wt_list_to_cont input state wtl' f2 (n::nl) fgen'
and
  (choose_one_formula_atomic : Var.env_in -> Prog.state ref -> 
   Exp.formula -> wt_cont -> wt_cont * formula * node) = 
  fun input state facc cont ->
    let _ = if debug then (print_string "xxx choose_one_formula_atomic\n"; flush stdout) in
    match cont with
    | WFinish ->  WFinish, False, ""
    | WStop _ ->  cont, True, ""
    | WCont _ ->
      let (cont', f, n) = call_wt_cont cont in
      let _ = if debug then (print_string ("XXX "^ n ^ "\n"); flush stdout) in
      let facc' = 
        match f,facc with
          True, True -> True
        | True, f -> f
        | f, True -> f
        | _,_ -> And(f,facc) 
      in
      let ctx_msg = Prog.ctrl_state_to_string_long !state.d.ctrl_state in
      Utils.time_C "is_sat";
      let snt, sat =
        Solver.is_satisfiable !state.d.snt input !state.d.memory !state.d.verbose ctx_msg facc' ""
      in
      state := { !state with d = { !state.d with snt = snt } } ;
      Utils.time_R "is_sat";
      if  sat then (cont', facc', n)
      else choose_one_formula_atomic input state facc cont'


and (wt_to_cont : Var.env_in -> Prog.state ref -> wt -> wt_cont -> wt_cont) =
  fun input state (tbl, n) cont ->
    let _ = if debug then (print_string ("XXX wt_to_cont "^ n ^"\n"); flush stdout) in
    let children = Util.StringMap.find n tbl in
    match children with
    | Prog.Stop str -> WStop str
    | Leave (f,nstate) -> WCont(fun () -> (cont, f, nstate))
    | Children l ->
      if l = [] then
        cont
      else
        let (l1, l2) = List.partition (fun (dw,_) -> dw = Infin) l in
        (match l1 with
         | [] -> 
           let get_weigth dw = 
             match dw with
             | V i -> if i < 0 then 0 else i (* a negative weight means null weigth *)
             | Infin -> assert false
           in
           let w_sum =
             List.fold_left (fun acc (dw,_) -> acc+(get_weigth dw)) 0 l2
           in
           if w_sum = 0 then cont else 
             let j = 1 + Random.int w_sum in
             let rec get_jth_trans j list acc =
               match list with
                 [] -> assert false
               | (dw,nt)::tail ->
                 let newj = j - (get_weigth dw) in
                 if (newj < 1) then 
                   nt, (rev_append acc tail) 
                 else 
                   get_jth_trans newj tail ((dw,nt)::acc)   
             in
             let (nt,l2') = get_jth_trans j l2 [] in
             let tbl' = Util.StringMap.add n (Children l2') tbl in
             let tbl'' = Util.StringMap.remove n tbl in (* to optimize mem *)
             let cont' =  WCont(fun () ->
                 call_wt_cont (wt_to_cont input state (tbl', n) cont)
               )
             in
             wt_to_cont input state (tbl'', nt) cont'

         | [(_,nt)] -> 
           let tbl' = Util.StringMap.add n (Children l2) tbl in
           let tbl'' = Util.StringMap.remove n tbl in
           let cont' =  WCont(fun () ->
               call_wt_cont (wt_to_cont input state (tbl', n) cont)
             )
           in
           wt_to_cont input state (tbl'', nt) cont'

         | _::_ -> 
           failwith 
             "Only one transition with a infinite weigth is allowed"
        ) 


      
(****************************************************************************)
(* NO LONGER EXPORTED *)
let (_internal_get : Var.env_in -> Prog.state ref -> t list) =
  fun input state ->
    let _ = if debug then (print_string "XXX get\n"; flush stdout) in
    let nll = !state.d.ctrl_state in
    List.map 
    (fun nl -> 
Utils.time_C "get_wtl";
       let wtl = !state.s.get_wtl input !state nl in
Utils.time_R "get_wtl";
       let _ = if debug then List.iter Prog.print_wt wtl in
Utils.time_C "wt_to_cont";
       let wt_cont_l = List.map (fun wt -> wt_to_cont input state wt WFinish) wtl in
Utils.time_R "wt_to_cont";
Utils.time_C "wt_list_to_cont";
       let res = wt_list_to_cont input state wt_cont_l True [] Finish in
Utils.time_R "wt_list_to_cont";
   res
    ) 
    nll 

(****************************************************************************)
(* EXPORTED *)

let rec (fgen_of_t : t -> FGen.t) =
  fun t  ->
    {
      FGen.choose_one_formula = (
        fun () ->
          let (t',s,f) = choose_one_formula t in
          (fgen_of_t t',s,f)
      ) ;
      FGen.get_all_formula = (
        fun () -> get_all_formula t
      )
    }

let get i s = List.map fgen_of_t (_internal_get i s)

