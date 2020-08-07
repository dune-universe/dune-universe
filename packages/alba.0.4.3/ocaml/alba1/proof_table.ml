(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Fmlib
open Term
open Signature
open Proof
open Container
open Support
open Printf

module ASeq = Ass_seq

type desc = {nbenv0:     int;
             c:          Context.t;
             term:       term;
             proof_term: proof_term;}

type t = {seq:       desc Ass_seq.t;
          names:     int array;
          c:         Context.t;
          depth:     int;
          count0:    int;      (* number of assertions at the start of the context *)
          mutable nreq:  int;  (* number of local assumptions *)
          mutable maxreq: int; (* first index with no more assumptions *)
          mutable reqs: int list; (* reversed indices of assumptions *)
          prev:      t option;
          verbosity: int}

let context (at:t): Context.t = at.c
let class_table (at:t):   Class_table.t   = Context.class_table at.c
let feature_table (at:t): Feature_table.t = Context.feature_table at.c


let is_private (at:t): bool = Context.is_private at.c
let is_public  (at:t): bool = Context.is_public  at.c
let is_interface_use   (at:t): bool = Context.is_interface_use  at.c
let is_interface_check (at:t): bool = Context.is_interface_check  at.c


let add_used_module (m:Module.M.t) (at:t): unit =
  Context.add_used_module m at.c

let add_current_module (m:Module.M.t) (at:t): unit =
  Context.add_current_module m at.c

let set_interface_check (at:t): unit =
  Context.set_interface_check at.c


let depth (at:t): int = at.depth

let is_global (at:t): bool = not (Option.has at.prev)

let is_local (at:t): bool =
  not (is_global at)

let previous (at:t): t =
  match at.prev with
    None -> assert false
  | Some x -> x


let is_toplevel (at:t): bool =
  not (is_global at) && is_global (previous at)


let count (at:t): int =
  Ass_seq.count at.seq

let count_previous (at:t): int =
  at.count0



let count_global (at:t): int =
  Ass_seq.count_first at.seq


let count_last_local (pt:t): int =
  (count pt) - (count_previous pt)


let count_variables (at:t): int = Context.count_variables at.c

let count_last_type_variables (at:t): int = Context.count_last_type_variables at.c

let count_last_arguments (at:t): int =
  Context.count_last_arguments at.c

let count_last_variables (at:t): int =
  Context.count_last_variables at.c

let local_argnames (at:t): int array =
  Context.local_argnames at.c

let local_formals (at:t): formals0 =
  Context.local_formals at.c

let local_fgs (at:t): formals0 =
  Context.local_fgs at.c

let has_result (at:t): bool =
  Context.has_result at.c

let has_result_variable (at:t): bool =
  Context.has_result_variable at.c

let last_arguments_string (at:t): string =
  Context.local_arguments_string at.c

let descriptor (i:int) (at:t): desc =
  assert (i < count at);
  Ass_seq.elem i at.seq


let names (at:t): int array =
  at.names


let tvars (at:t): Tvars.t = Context.tvars at.c

let imp_id (at:t): int =
  Context.implication_index at.c

let split_implication (t:term) (at:t): term * term =
  Term.binary_split t (imp_id at)

let split_all_quantified (t:term) (at:t): Formals.t * Formals.t * term =
  Term.all_quantifier_split t

let split_some_quantified (t:term) (at:t): Formals.t * term =
  Term.some_quantifier_split t

let implication (a:term) (b:term) (at:t): term =
  Term.binary (imp_id at) a b

let implication_chain (ps_rev: term list) (tgt:term) (at:t): term =
  Context.implication_chain ps_rev tgt at.c

let split_implication_chain (t:term) (at:t): term list * term =
  Term.split_implication_chain t (imp_id at)

let quantified
    (is_all:bool) (tps:Formals.t) (fgs:Formals.t) (t:term) (at:t)
    : term =
  Context.quantified is_all tps fgs t at.c

let all_quantified (tps:Formals.t) (fgs:Formals.t)(t:term) (at:t): term =
  Context.all_quantified tps fgs t at.c

let some_quantified (tps:Formals.t) (fgs:Formals.t) (t:term) (at:t): term =
  Context.some_quantified tps fgs t at.c


let string_of_term (t:term) (at:t): string =
  Context.string_of_term t at.c

let string_long_of_term (t:term) (at:t): string =
  Context.string_long_of_term t at.c

let string_of_term_anon (t:term) (nb:int) (at:t): string =
  Context.string_of_term0 t true false nb at.c


let prenex_term (t:term) (at:t): term =
  (* The term [t] in prenex normal form with respect to universal quantifiers *)
  Context.prenex_term t at.c


let prenex_term_bubble_one (t:term) (at:t): term =
  (* The term [t] in prenex normal form with respect to universal quantifiers *)
  Context.prenex_term_bubble_one t at.c



let make (comp:Module.Compile.t): t =
  {seq      = Ass_seq.empty ();
   depth    = 0;
   count0   = 0;
   names    = [||];
   nreq    = 0;
   maxreq  = 0;
   reqs    = [];
   prev    = None;
   c = Context.make comp;
   verbosity = Module.Compile.verbosity comp}


let push0 (names: int array) (c:Context.t) (at:t): t =
  {at with
   seq = Ass_seq.clone at.seq;
   names = names;
   c        = c;
   depth    = 1 + at.depth;
   count0   = count at;
   nreq     = 0;
   maxreq   = count at;
   reqs     = [];
   prev     = Some at}



let push
    (entlst:entities list withinfo)
    (rt:return_type)
    (is_pred:bool)
    (is_func:bool)
    (rvar: bool)
    (at:t): t =
  let c = context at in
  assert (depth at = Context.depth c);
  let c = Context.push entlst rt is_pred is_func rvar c in
  let names = Context.local_argnames c in
  push0 names c at




let push_typed (tps:Formals.t) (fgs:Formals.t) (rvar:bool) (at:t): t =
  let c = Context.push_typed tps fgs rvar at.c in
  let nms = Formals.names tps in
  push0 nms c at


let push_typed0 (tps:Formals.t) (fgs:Formals.t) (at:t): t =
  push_typed tps fgs false at


let push_empty (at:t): t =
  push_typed Formals.empty Formals.empty false at

let pop (at:t): t =
  assert (is_local at);
  assert (depth at >= Context.depth (context at));
  previous at



let term (i:int) (at:t): term * Context.t =
  (** The [i]th proved term and its context.
   *)
  assert (i < count at);
  let desc = descriptor i at in
  desc.term, desc.c


let transformed_to_current (t:term) (idx:int) (at:t): term =
  (* The term [t] transformed form the environment of the term at [idx] into the
     current environment.
   *)
  Context.transformed_term t (descriptor idx at).c (context at)


let nbenv_term (i:int) (at:t): int =
  (** The number of variables of the environment of the  [i]th proved term.
   *)
  assert (i < count at);
  (descriptor i at).nbenv0


let ntvs_term (i:int) (at:t): int =
  (* The complete number of type variables of the environment of the [i]th
     proved term.  *)
  Tvars.count_all (Context.tvars (descriptor i at).c)


let local_term (i:int) (at:t): term =
  (** The [i]th proved term in the local environment.
   *)
  assert (i < count at);
  let desc = descriptor i at in
  if desc.c == at.c then
    desc.term
  else
    Context.transformed_term desc.term desc.c at.c


let string_of_term_i (i:int) (at:t): string =
  string_of_term (local_term i at) at


let string_long_of_term_i (i:int) (at:t): string =
  string_long_of_term (local_term i at) at


let variant (i:int) (bcls:int) (cls:int) (at:t): term =
  assert (is_global at);
  let t,c = term i at in
  assert (c == at.c);
  let tps,fgs,t0 =
    try Term.all_quantifier_split t
    with Not_found -> assert false in
  let n = Formals.count tps in
  assert (Formals.count fgs = 1); (* Only one formal generic *)
  let bcls0,_ = split_type (Formals.typ 0 fgs) in
  assert (bcls0 = bcls + 1);
  let ft = feature_table at
  and ct = class_table at in
  let ctp,tvs = Class_table.class_type cls ct in
  let ags = [|ctp|] in
  let nall = Tvars.count_all tvs in
  let tps  = Formals.map
               (fun tp -> Term.subst tp nall ags)
               tps
  and fgnms, fgcon = Tvars.fgnames tvs, Tvars.fgconcepts tvs in
  let t0 = Feature_table.substituted t0 n 0 0 [||] 0 ags tvs ft in
  Term.all_quantified tps (Formals.make fgnms fgcon) t0



let count_local_assumptions (at:t): int =
  at.nreq


let  is_local_assumption (i:int) (at:t): bool =
  let cnt0 = count_previous at in
  cnt0 <= i && i < cnt0 + at.nreq


let assumptions (at:t): term list =
  (* The assumptions of the current context *)
  List.fold_left
    (fun lst i ->
      (local_term i at)::lst)
    []
    at.reqs



let assumption_indices (at:t): int list =
  (* in reversed order *)
  at.reqs



let discharged_assumptions (i:int) (at:t): term =
  assert (is_local at);
  assert (not (has_result at));
  let tgt = local_term i at in
  List.fold_left
    (fun tgt i -> implication (local_term i at) tgt at)
    tgt
    at.reqs


let discharged_term (i:int) (at:t): term =
  (* The [i]th term of the current environment with all local variables and
     assumptions discharged.
   *)
  assert (not (is_global at));
  let t0 = discharged_assumptions i at in
  let tps = local_formals at
  and fgs = local_fgs at
  in
  Term.all_quantified (Formals.from_pair tps) (Formals.from_pair fgs) t0



let is_axiom (i:int) (at:t): bool =
  assert (i < count at);
  let desc = descriptor i at in
  match desc.proof_term with
    Axiom _ -> true
  | _       -> false


let is_assumption (i:int) (at:t): bool =
  assert (i < count at);
  let desc = descriptor i at in
  match desc.proof_term with
    Assumption _ -> true
  | _            -> false


let proof_term (i:int) (at:t): proof_term =
  assert (i < count at);
  (descriptor i at).proof_term


let add_proved_0 (t:term) (pt:proof_term) (at:t): unit =
  (** Add the term [t] and its proof term [pt] to the table.
   *)
  (*assert (Context.is_well_typed t at.c);*)
  let raw_add () =
    Ass_seq.push {nbenv0 = count_variables at;
                  c      = at.c;
                  term   = t;
                  proof_term = pt} at.seq
  in
  match pt with
    Assumption _ ->
      let idx = count at in
      assert (count_previous at + at.nreq = idx);
      raw_add ();
      at.reqs <- idx :: at.reqs;
      at.nreq <- at.nreq + 1;
      at.maxreq <- idx + 1
  | _ ->
      raw_add ()


exception Illegal_proof_term


let definition_term (idx:int) (nb:int) (ags:agens) (at:t): int * int array * term =
  let c = context at in
  Context.definition_term idx nb ags c


let arity (idx:int) (nb:int) (at:t): int =
  Context.arity idx nb at.c


let split_equality (t:term) (nb:int) (at:t): int * term * term =
  let nargs, eq_id, left, right =
    Feature_table.split_equality t (nb + count_variables at) (feature_table at) in
  nargs,left,right



let specialized (i:int) (args:term array) (nb:int) (at:t): term =
  assert (i < count at);
  assert false
  (*let nbenv = count_variables at in
  let t, nbenv_t = term i at in
  assert (nbenv_t <= nbenv);
  let nbenv_delta = nb + nbenv - nbenv_t in
  if Array.length args = 0 then
    Term.up nbenv_delta t
  else
    let nargs, _, t0 = Term.all_quantifier_split t in
    assert (nargs = Array.length args);
    Term.sub t0 args nbenv_delta*)


let beta_reduce
    (n:int) (t:term) (tup_tp:type_term) (args:term array) (nb:int) (at:t)
    : term =
  Context.beta_reduce n t tup_tp args nb at.c


let apply_term (t:term) (args:term array) (nb:int) (at:t): term =
  Term.apply t args


let term_of_specialize (i:int) (args:term array) (ags:agens) (at:t): term =
  (* Specialize the assertion [i] with the actual arguments [args] and the
     actual generics [ags] coming from the current context.
   *)
  assert (i < count at);
  let tvs   = tvars at in
  let nargs = Array.length args
  and desc  = descriptor i at
  and nall  = Tvars.count_all tvs
  in
  assert (Context.has_no_type_variables at.c);
  assert (Context.has_no_type_variables desc.c);
  let nvars_i = Context.count_variables desc.c
  and ntvs_i  = ntvs_term i at
  in
  let d1 = count_variables at - nvars_i in
  let tps,fgs,t0 =
    try Term.all_quantifier_split desc.term
    with Not_found -> Formals.empty, Formals.empty, desc.term
  in
  let n = Formals.count tps in
  assert (nargs <= n);
  let tsub =
    Feature_table.substituted
      t0 n nvars_i ntvs_i
      args d1
      ags tvs (feature_table at)
  and nms = Array.sub (Formals.names tps) nargs (n-nargs)
  and tps = Array.sub (Formals.types tps) nargs (n-nargs)
  in
  let tps = Term.subst_array tps (nall-ntvs_i) ags in
  if nargs < n then
    let imp_id0 = (imp_id at)           in
    let imp_id1 = imp_id0 + (n-nargs)   in
    try
      let a,b = Term.binary_split tsub imp_id1 in
      Term.binary
        imp_id0
        (Term.down (n-nargs) a)
        (Term.all_quantified (Formals.make nms tps) Formals.empty b)
    with Term_capture ->
      printf "term capture\n";
      raise Illegal_proof_term
    | Not_found ->
        printf "not found\n";
        raise Illegal_proof_term
  else
    tsub




let reconstruct_evaluation (e:Eval.t) (at:t): term * term =
  (* Return the unevaluated and the evaluated term *)
  let rec reconstruct (e:Eval.t) (nb:int) =
    let domain_id = nb + count_variables at + Constants.domain_index
    and reconstr_args args =
      let n = Array.length args in
      let args = Array.map (fun e -> reconstruct e nb) args in
      let argsa = Array.init n (fun i -> fst args.(i))
      and argsb = Array.init n (fun i -> snd args.(i)) in
      argsa, argsb
    in
    let ta,tb =
    match e with
      Eval.Term t -> t,t
    | Eval.Exp (idx,ags,args,e) when idx = domain_id ->
        let doma, domb = reconstruct e nb in
        if doma <> domb then raise Illegal_proof_term;
        if Array.length args <> 1 then raise Illegal_proof_term;
        let argsa,argsb = reconstr_args args in
        assert (argsa = argsb); (* must be valid in case of domain_id *)
        begin
          match argsa.(0) with
          |  Lam(tps,fgs,pres,t0,rt) ->
              if rt = None then raise Illegal_proof_term;
              if Context.domain_of_lambda tps fgs pres nb (context at) <> doma then
                raise Illegal_proof_term
          | VAppl(idx2,args,ags,_) when arity idx2 nb at > 0 ->
             if Context.domain_of_feature idx2 nb ags (context at) <> doma then
               raise Illegal_proof_term
          | _ -> ()
        end;
        VAppl(idx,argsa,ags,false), doma
    | Eval.Exp (idx,ags,args,e) ->
        let n,nms,t =
          try definition_term idx nb ags at
          with Not_found -> raise Illegal_proof_term
        in
        let ta,tb = reconstruct e nb
        and argsa,argsb = reconstr_args args in
        let uneval = VAppl(idx,argsa,ags,false) in
        let exp =
          try apply_term t argsb nb at
          with Not_found ->
            raise Illegal_proof_term in
        if not (Term.equivalent exp ta) then begin
          printf "reconstruct exp   %s\n" (string_of_term_anon exp nb at);
          printf "            ta    %s\n" (string_of_term_anon ta nb at);
        end;
        if not (Term.equivalent exp ta) then raise Illegal_proof_term;
        uneval, tb
    | Eval.VApply (i,args,ags) ->
        let argsa, argsb = reconstr_args args in
        VAppl (i,argsa,ags,false), VAppl (i,argsb,ags,false)
    | Eval.Apply (f,args,e) ->
        let fa,fb = reconstruct f nb
        and argsa, argsb = reconstr_args args
        and resa,resb = reconstruct e nb in
        assert (Term.equivalent resa (Application (fb,argsb,false)));
        Application (fa,argsa,false), resb
    | Eval.Lam (tps,fgs,pres,e,rt) ->
        let ta,tb = reconstruct e (Formals.count tps + nb) in
        Lam (tps,fgs,pres,ta,rt), Lam (tps,fgs,pres,tb,rt)
    | Eval.QExp (n,tps,fgs,e,is_all) ->
       let ta,tb = reconstruct e (nb+n)
       and tps = Formals.from_pair tps
       and fgs = Formals.from_pair fgs in
       QExp (tps,fgs,ta,is_all), QExp (tps,fgs,tb,is_all)
    | Eval.Beta (e_redex, e_reduct) ->
        let ta_redex,tb_redex   = reconstruct e_redex nb
        and ta_reduct,tb_reduct = reconstruct e_reduct nb in
        begin
          match tb_redex with
          | Application(Lam(tps,fgs,_,t0,rt), args, _) ->
             let n = Formals.count tps
             and tup_tp =
               Context.tuple_type_of_types (Formals.types tps) at.c
             in
             let reduct = beta_reduce n t0 tup_tp args nb at in
             assert (Term.equivalent ta_reduct reduct);
             ta_redex,tb_reduct
          | _ ->
             raise Illegal_proof_term
        end
    | Eval.Simpl (e,idx,args,ags) ->
        let eq = term_of_specialize idx args ags at in
        let eq = Term.up nb eq in
        (*let eq = specialized idx args nb at in*)
        let n,left,right = split_equality eq nb at in
        assert (n = 0);
        assert (Array.length args = 0);
        assert (Array.length ags  = 0);
        let ta,tb = reconstruct e nb in
        if not (Term.equivalent tb left) then begin
          printf "reconstruct ta    %s\n" (string_of_term_anon ta nb at);
          printf "            tb    %s %s\n"
            (string_of_term_anon tb nb at) (Term.to_string tb);
          printf "            left  %s %s\n"
            (string_of_term_anon left nb at) (Term.to_string left);
          printf "            right %s\n" (string_of_term_anon right nb at);
          if nb = 0 then begin
            let c = context at in
            assert (Context.is_well_typed left c);
            assert (Context.is_well_typed tb c);
          end;
          raise Illegal_proof_term
        end;
        ta,right
    | Eval.If (cond,idx,args) ->
        assert (Array.length args = 3);
        let argsa, argsb = reconstr_args args in
        Ifexp (argsa.(0),argsa.(1),argsa.(2)),
        if cond then argsb.(1) else argsb.(2)
    | Eval.As (cond, inspe, tps, pat) ->
       let inspa, inspb = reconstruct inspe nb in
       let res =
         let nvars = count_variables at in
         if cond then Feature_table.true_constant nvars
         else Feature_table.false_constant nvars
       in
       Asexp (inspa,tps,pat), res
    | Eval.AsExp t ->
        let nvars = count_variables at
        and ft = feature_table at
        and tvs = tvars at in
        let exp = Feature_table.evaluated_as_expression t (nb+nvars) tvs ft in
        t, exp
    | Eval.Inspect (t,inspe,icase,rese) ->
        let inspa,inspb = reconstruct inspe nb
        and resa,resb   = reconstruct rese nb in
        begin
          match t with
          | Inspect(insp,cases) ->
            let ncases = Array.length cases in
            if icase < 0 || ncases <= icase then raise Illegal_proof_term;
            let fs,pat,res = cases.(icase) in
            let n1 = Array2.count fs in
            begin
              match Pattern.unify_with_pattern inspb n1 pat at.c with
              | None | Some (Error _ ) ->
                 printf "inspect no match\n";
                 printf "  term      %s\n" (string_of_term_anon inspa nb at);
                 printf "  eval      %s\n" (string_of_term_anon inspb nb at);
                 printf "  case %d   %s\n"
                        icase (string_of_term_anon pat (n1+nb) at);
                 raise Illegal_proof_term
              | Some (Ok (args,pres) ) ->
                 (*if pres <> [] then
                   begin
                     printf "reconstruct_evaluation\n  check requirements\n";
                     List.iter
                       (fun t ->
                         printf "     %s\n" (string_of_term t at))
                       pres
                   end;*)
                 let res = Term.apply res args in
                 if not (Term.equivalent res resa) then
                   begin
                     printf "inspect result different\n";
                     printf "  res       %s\n" (string_of_term_anon res nb at);
                     printf "  resa      %s\n" (string_of_term_anon resa nb at);
                     raise Illegal_proof_term
                   end;
                 t, resb
            end
        | _ ->
            printf "no inspect expression %s\n" (string_of_term_anon t nb at);
            raise Illegal_proof_term
        end
    in
    let tb = Context.downgrade_term tb nb at.c in
    ta, tb
  in
  reconstruct e 0




let term_of_mp (a:int) (b:int) (at:t): term =
  assert (a < count at);
  assert (b < count at);
  let ta = local_term a at
  and tb = local_term b at
  in
  let b1,b2 =
    try Term.binary_split tb (imp_id at)
    with Not_found ->
      printf "tb is not an implication\n";
      printf "  ta %d:%s\n" a (string_of_term ta at);
      printf "  tb %d:%s\n" b (string_of_term tb at);
      raise Illegal_proof_term
  in
  let ok = Term.equivalent ta b1 in
  if not ok then begin
    printf "antecedent of tb does not conincide with ta (ok %b)\n" ok;
      printf "  ta %d:%s\n" a (string_of_term ta at);
      printf "  tb %d:%s\n" b (string_of_term tb at);
    raise Illegal_proof_term
  end;
  b2



let term_of_eval (i:int) (e:Eval.t) (at:t): term =
  let ta,tb = reconstruct_evaluation e at
  and t = local_term i at in
  let ok = (Term.equivalent t  ta) in
  if not ok then begin
    printf "evaluated terms do not coincide\n";
    printf "   term %3d  %s  %s\n" i (string_long_of_term t at)(Term.to_string t);
    printf "   eval      %s  %s\n" (string_long_of_term ta at) (Term.to_string ta);
    printf "   evaluated %s  %s\n" (string_long_of_term tb at) (Term.to_string tb);
    raise Illegal_proof_term
  end;
  tb


let term_of_eval_bwd (t:term) (e:Eval.t) (at:t): term =
  let ta,tb = reconstruct_evaluation e at in
  let ok = (Term.equivalent t ta) in
  if not ok then begin
    printf "evaluated terms (bwd) do not coincide\n";
    printf "   term      %s  %s\n" (string_long_of_term t at) (Term.to_string t);
    printf "   eval      %s  %s\n" (string_long_of_term ta at) (Term.to_string ta);
    printf "   evaluated %s\n" (string_long_of_term tb at);
    raise Illegal_proof_term
  end;
  implication tb ta at



let term_of_witness
    (i:int) (nms:names) (tps:types) (t:term) (args:term array) (at:t)
    : term =
  Array.iter
    (fun t ->
      match t with Variable i when i = -1 ->
        raise Illegal_proof_term
      | _ -> ()
    )
    args;
  let some_term = Term.some_quantified (Formals.make nms tps) t in
  let ti  = local_term i at in
  let wt  = Term.apply t args in
  if not (Term.equivalent ti wt) then begin
    printf "illegal witness ti  \"%s\"\n" (string_of_term ti at);
    printf "                wt  \"%s\"\n" (string_of_term wt at);
    printf "                for \"%s\"\n" (string_of_term some_term at);
    raise Illegal_proof_term
  end;
  implication ti some_term at


let someelim (i:int) (at:t): term =
  (* If the term [i] has not the form [some(a:A,b:B,...) t] then raise
     Not_found.

     If the term is an existentially quantified assertion then transform it
     into

         all(u:BOOLEAN) (all(a:A,b:B,...) t ==> u) ==> u

   *)
  assert (i < count at);
  let t_i = local_term i at in
  let tps,t0 = split_some_quantified t_i at in
  let nargs = Formals.count tps in
  let imp_id  = imp_id at
  in
  let imp_id_inner = imp_id + (nargs+1)
  and imp_id_outer = imp_id + 1
  and e_name = ST.symbol "$e"
  and e_tp   = Context.boolean at.c
  in
  let t1 = Term.up_from 1 nargs t0
  in
  let t_impl_u   = Term.binary imp_id_inner t1 (Variable nargs) in
  let all_inner  = Term.all_quantified tps Formals.empty t_impl_u in
  let impl_outer = Term.binary imp_id_outer all_inner (Variable 0) in
  Term.all_quantified (Formals.make [|e_name|] [|e_tp|]) Formals.empty impl_outer



let term_of_someelim (i:int) (at:t): term =
  try
    someelim i at
  with Not_found ->
    printf "term_of_someelim\n";
    printf "  %s\n" (string_of_term_i i at);
    raise Illegal_proof_term


let is_inductive_set (i:int) (at:t): bool =
  Context.is_inductive_set i at.c


let inductive_set (t:term) (at:t): term =
  Context.inductive_set t at.c


let closure_rule (i:int) (t:term) (at:t): term =
  try
    let set = inductive_set t at in
    Term.closure_rule i set t
  with Not_found
  | Invalid_argument _ ->
    raise Illegal_proof_term


let function_property (idx:int) (i:int) (args:term array) (at:t): term =
  try
    Context.function_property idx i args at.c
  with Invalid_argument _ ->
    raise Illegal_proof_term


let set_induction_law (t:term) (at:t): term =
  let p =
    try
      Context.inductive_set t at.c
    with Not_found ->
      raise Illegal_proof_term in
  let set_tp = Context.type_of_term p at.c in
  let elem_tp = Class_table.domain_type set_tp in
  Term.induction_law (imp_id at) p t elem_tp set_tp


let type_induction_law (cls:int) (at:t): term =
  Feature_table.induction_law cls (count_variables at) (feature_table at)


let count_assumptions (pt_arr:proof_term array): int =
  let p (pt:proof_term): bool =
    match pt with
      Assumption _ -> false | _ -> true
  in
  try
    Search.array_find_min p pt_arr
  with Not_found ->
    Array.length pt_arr




let arguments_string (at:t): string =
  let names = Array.to_list at.names in
  let str = String.concat "," (List.map ST.string names) in
  if str = "" then str
  else "(" ^ str ^ ")"



let reconstruct_term (pt:proof_term) (trace:bool) (at:t): term =
  let depth_0 = depth at
  in
  let rec reconstruct (pt:proof_term) (at:t): term =
    let prefix (d:int): string = String.make (4*(d+1-depth_0)) ' '
    in
    let print (t:term) (at:t) =
      printf "%s%3d %s"
        (prefix (depth at)) (count at) (string_long_of_term t at)
    and print_all (at:t) =
      let pre = prefix (depth at - 1) in
      printf "%s%3d all%s\n%s    require\n"
        pre (count at) (arguments_string at) pre
    and print_str (str:string) (at:t):unit =
      let pre = prefix (depth at - 1) in
      printf "%s    %s\n" pre str
    in
    let print0 (t:term) at = print t at; printf "\n"
    and print1 (t:term) (i:int) at = print t at; printf "\t{%d}\n" i
    and print2 (t:term) (i:int) (j:int) at = print t at; printf "\t{%d,%d}\n" i j
    and printstr (t:term) (str:string) at =
      print t at; printf "\t{%s}\n" str
    in
    let cnt = count at in
    match pt with
      Axiom t | Assumption t ->
        if trace then print0 t at;
        t
    | Funprop (idx,i,args) ->
        let t = function_property idx i args at in
        if trace then printstr t "funprop" at;
        t
    | Indset_rule (t,i) ->
        let t = closure_rule i t at in
        if trace then printstr t "rule" at;
        t
    | Indset_ind t ->
        let t = set_induction_law t at in
        if trace then printstr t "indset" at;
        t
    | Indtype cls ->
        let t = type_induction_law cls at in
        if trace then printstr t "indlaw" at;
        t
    | Detached (a,b) ->
        let t = term_of_mp a b at in
        if trace then print2 t a b at;
        t
    | Specialize (i,args,ags) ->
        let t = term_of_specialize i args ags at in
        if trace then print1 t i at;
        t
    | Eval (i,e) ->
        let t = term_of_eval i e at in
        if trace then print1 t i at;
        t
    | Eval_bwd (t,e) ->
        let t = term_of_eval_bwd t e at in
        if trace then printstr t "evalbwd" at;
        t
    | Witness (idx,(nms,tps),t,args) ->
        let t = term_of_witness idx nms tps t args at in
        if trace then print1 t idx at;
        t
    | Someelim idx ->
        let t = term_of_someelim idx at in
        if trace then print1 t idx at;
        t
    | Inherit (idx,bcls,cls) ->
        let t =  variant idx bcls cls at in
        if trace then print1 t idx at;
        t
    | Subproof (tps,fgs,res_idx,pt_arr,bubble) ->
        let at = push_typed0 (Formals.from_pair tps) (Formals.from_pair fgs) at in
        let pt_len = Array.length pt_arr in
        let pt_nass =
          if trace then count_assumptions pt_arr else 0
        in
        assert (res_idx < cnt + pt_len);
        if trace then print_all at;
        for i = 0 to pt_len - 1 do
          if trace && i = pt_nass then print_str "check" at;
          let t = reconstruct pt_arr.(i) at in
          add_proved_0 t pt_arr.(i) at
        done;
        if trace then begin
          print_str "ensure" at;
          print1 (local_term res_idx at) res_idx at;
          print_str "end" at;
        end;
        let term = discharged_term res_idx at in
        if bubble then
          prenex_term_bubble_one term (pop at)
        else
          term
  in
  reconstruct pt at




let term_of_pt (pt:proof_term) (at:t): term =
  (** Construct a term from the proof term [pt].
   *)
  reconstruct_term pt false at


let print_pt (pt:proof_term) (at:t): unit =
  let _ = reconstruct_term pt true at in ()



let is_proof_pair (t:term) (pt:proof_term) (at:t): bool =
  try
    (*let t_pt = reconstruct_term pt true at in*)
    let t_pt = term_of_pt pt at in
    let res = Term.equivalent t t_pt in
    if not res then begin
      printf "unequal t    %s\n" (string_of_term t at);
      printf "        t_pt %s\n" (string_of_term t_pt at)
    end;
    res
  with Illegal_proof_term ->
    printf "Illegal proof term\n";
    printf "   term \"%s\"\n" (string_long_of_term t at);
    print_pt pt at;
    false


let is_well_typed (t:term) (at:t): bool =
  Context.is_well_typed t at.c


let add_proved (t:term) (pt:proof_term) (delta:int) (at:t): unit =
  (** Add the term [t] and its proof term [pt] to the table.
   *)
  assert (delta <= count at);
  let start = count at - delta in
  let pt = Proof_term.adapt start delta pt in
  assert (not (is_global at) || is_well_typed t at);
  assert (not (is_global at) || is_proof_pair t pt at);
  add_proved_0 t pt at


let add_axiom (t:term) (at:t): unit =
  assert (is_toplevel at);
  let pt = Axiom t in
  add_proved_0 t pt at


let add_assumption (t:term) (at:t): unit =
  let pt = Assumption t in
  add_proved_0 t pt at


let add_inherited (t:term) (idx:int) (bcls:int) (cls:int) (at:t): unit =
  assert (is_global at);
  let pt = Inherit (idx,bcls,cls) in
  add_proved_0 t pt at


let add_mp (t:term) (i:int) (j:int) (at:t): unit =
  let ti = local_term i at
  and imp = local_term j at in
  let a,b = split_implication imp at in
  assert (Term.equivalent ti a);
  assert (Term.equivalent b  t);
  let pt = Detached (i,j) in
  add_proved_0 t pt  at


let add_specialize (t:term) (i:int) (args:term array) (ags:agens) (at:t): unit =
  let pt = Specialize (i,args,ags) in
  add_proved_0 t pt  at


let add_eval (t:term) (i:int) (e:Eval.t) (at:t): unit =
  add_proved_0 t (Eval (i,e)) at


let add_eval_backward (t:term) (impl:term) (e:Eval.t) (at:t): unit =
  (* [impl = (teval => t)] *)
  add_proved_0 impl (Eval_bwd (t,e)) at



let add_witness
    (impl:term)
    (i:int)
    (nms:names)
    (tps:types)
    (t:term)
    (args:arguments) (at:t): unit =
  add_proved_0 impl (Witness (i,(nms,tps),t,args)) at


let add_someelim (i:int) (t:term) (at:t): unit =
  add_proved_0 t (Someelim i) at


(*  Discharging
    ===========

    Suppose the prover has accepted the assertion:

        all[G:CG,H:CH,...](a:A,b:B,...)
            require
                r0;r1;...
            ensure
                e
            end

    I.e. at the end we get a proof term for 'e' valid in the context.

    Discharged premises:  'r0 ==> r1 ==> ... ==> e'

    In the implication chain not all variables 'a,b,...' might be
    used. I.e. we can calculate a map 'f:int->int' which maps old variables to
    new variables and a set of used variables 'usd'. The map is undefined for
    old variables which are unused.

    Having this we get a type sequence 'UT1,UT2,...' of the used types which
    is 'A,B,...' with all types corresponding to unused variables removed.

    Formal generics not occurring in 'UT1,UT2,...' are unused as well. This
    gives rise to a map 'ffg:int->int' which maps old formal generics to new
    formal generics and a set of used formal generics 'usdfg'. The map is
    undefined for unused formal generics.

    With this we get a formal generics sequence 'UG1:CUG1,UG2,CUG2,..'.

    We have to map all terms with 'f' and all types with 'ffg' in the term and
    the proof term. In case that in the proof term some a function is applied
    to an undefined argument, then the proof term uses more variables than the
    final term. This has to be considered as an error.

 *)


let discharged0 (i:int) (bubble:bool) (at:t)
    : term * proof_term =
   (* Special cases for discharging:

      1. The target is an axiom: Just discharge the term (with bubbling up if
         the flag [bubble] is set and removing of unneeded variables) and use
         an axiom as a proof term.

      2. Only one proof term in the subproof, no assumptions and no variables:

         The only possibility to create such a situation is with `ensure ass
         end` which should be syntactically forbidden or reduced to the
         equivalent form `ass`.

     3. The target is from an outer context: In that case the target does not
        contain any of the variables of the inner context and does not need
        the assumptions of the inner context to be proved.

     4. Otherwise: Discharge the target with bubbling up if [bubble] is set and
        generate the corresponding proof term. Reorder variable if necessary.
 *)
  let tgt = local_term i at
  and pt  = proof_term i at in
  if count_last_arguments at = 0 &&
    count_local_assumptions at = 0 &&
    count_last_local at = 1 &&
    i = count_previous at
  then begin
    assert (i = count_previous at);
    tgt,pt
  end else begin
    let t0 = discharged_assumptions i at in
    let tps, args, fgs, ags =
      let tps  = local_formals at
      and ntvs = count_last_type_variables at
      and fgs  = local_fgs at in
      Term.unused_transform tps ntvs fgs t0
    in
    let n1new, n2new = count_formals tps, count_formals fgs in
    let t0 = Term.subst0 t0 n1new args n2new ags in
    let t  =
      Term.all_quantified (Formals.from_pair tps) (Formals.from_pair fgs) t0 in
    let t  =
      if bubble then
        prenex_term_bubble_one t (pop at)
      else
        t
    in
    match pt with
      Axiom _ ->
        let pt = Axiom t in
        t, pt
    | _ ->
        let cnt0  = count_previous at in
        let narr = if at.maxreq <= i then i+1-cnt0 else at.maxreq-cnt0 in
        if narr = 0 then begin
          assert (i < cnt0);
          t, Subproof (empty_formals,empty_formals,i,[||],bubble)
        end else begin
          let nargs = Array.length args in
          let ptarr = Array.init narr (fun j -> proof_term (cnt0+j) at) in
          let i,ptarr  = Proof_term.remove_unused_proof_terms i cnt0 ptarr in
          let uvars_pt = Proof_term.used_variables nargs ptarr in
          if not (n1new = IntSet.cardinal uvars_pt &&
                  IntSet.for_all
                    (fun i ->
                      assert (i < nargs);
                      args.(i) <> Variable (-1))
                    uvars_pt)
          then begin
            printf "n1new %d, IntSet.cardinal uvars_pt %d\n"
              n1new (IntSet.cardinal uvars_pt);
            printf "uvars_pt %s\n" (string_of_intset uvars_pt);
            printf "#ptarr %d\n" (Array.length ptarr);
            Proof_term.print_pt_arr "    " cnt0 ptarr;
            assert false (* Cannot happen *)
          end;
          let ptarr =
            Proof_term.remove_unused_variables args n1new ags n2new ptarr in
          let pt = Subproof (tps,fgs,i,ptarr,bubble) in
          t,pt
        end
  end


let discharged (i:int) (at:t): term * proof_term =
  assert (count_last_type_variables at = 0);
  let tgt = local_term i at
  and pt  = proof_term i at in
  if count_last_arguments at = 0
     && count_local_assumptions at = 0
     && count_last_local at = 1
     && i = count_previous at
  then
    begin
      assert (i = count_previous at);
      tgt,pt
    end
  else
    begin
      let t0 = discharged_assumptions i at
      and args = local_formals at
      and fgs  = local_fgs at in
      let t =
        Term.all_quantified (Formals.from_pair args) (Formals.from_pair fgs) t0
      and pt =
        let cnt0  = count_previous at in
        let narr = if at.maxreq <= i then i+1-cnt0 else at.maxreq-cnt0 in
        if narr = 0 then
          begin
            assert (i < cnt0);
            Subproof (empty_formals,empty_formals,i,[||],false)
          end
        else
          begin
            let ptarr = Array.init narr (fun j -> proof_term (cnt0+j) at) in
            Subproof (args,fgs,i,ptarr,false)
          end
      in
      t,pt
   end



let discharged_bubbled (i:int) (at:t): term * proof_term =
  discharged0 i true at
