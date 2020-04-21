(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Container
open Support
open Term
open Proof
open Printf

module PC = Proof_context

(*

          |   pointer to subgoal
          |
          |
     +------------------------------------------+
     | s1 s2 ...   |                            | ^  subgoals
     |  alt1       |  alt2   ...                | |  backward reasoning
     |------------------------------------------|
     |  all(...) p1 ==> p2 ==> ... ==> tgt      | ^
     |                                          | |  enter
     |            goal                          |
     |------------------------------------------|
     | par1 | par2 | ...                        |
     +------------------------------------------+
               |
               |  backpointer to parent
               |  with goal #, alternative and subgoal within alternative


    - An alternative fails if one of its subgoals fails
    - A (sub)goal becomes obsolete all alternatives which need it are failed
      or obsolete.
    - All alternatives to prove an obsolete goal become obsolete.

 *)


module Statistics:
sig
  type t
  val make: unit -> t
  val add_proof: t -> unit
  val add_goal:  t -> unit
  val add_alternatives: int -> t -> unit
  val add_falses: int -> t -> unit
  val push: t -> unit
  val pop: t -> unit
  val print_and_pop: Format.formatter -> t -> unit
  val print: Format.formatter -> t -> unit
end =
  struct
    type entry = {
        mutable nproofs: int;
        mutable ngoals: int;
        mutable nalternatives: int;
        mutable nfalses: int;
      }
    type t = {
        mutable stack: entry list;
        entry: entry
      }

    let make_entry (): entry =
      { nproofs = 0;
        ngoals = 0;
        nalternatives = 0;
        nfalses = 0;
      }

    let copy_entry (e:entry): entry =
      { nproofs = e.nproofs;
        ngoals  = e.ngoals;
        nalternatives = e.nalternatives;
        nfalses = e.nfalses}

    let make (): t =
      {stack = [];
       entry = make_entry ()
      }

    let add_proof (s:t): unit =
      s.entry.nproofs <- s.entry.nproofs + 1

    let add_goal (s:t): unit =
      s.entry.ngoals <- s.entry.ngoals + 1

    let add_alternatives (n:int) (s:t): unit =
      s.entry.nalternatives <- s.entry.nalternatives + n - 1

    let add_falses (n:int) (s:t): unit =
      s.entry.nfalses <- s.entry.nfalses + n - 1

    let push (s:t): unit =
      s.stack <- copy_entry s.entry :: s.stack

    let pop (s:t): unit =
      match s.stack with
      | [] ->
         assert false (* Illegal call *)
      | hd :: tl ->
         s.stack <- tl

    let print_delta (f:Format.formatter) (e:entry ) (s:t): unit =
      let nproofs = s.entry.nproofs - e.nproofs
      and ngoals = s.entry.ngoals - e.ngoals
      and nalternatives = s.entry.nalternatives - e.nalternatives
      and nfalses = s.entry.nfalses - e.nfalses in
      let open Format in
      fprintf f "@[<v>Statistics@,";
      if nproofs > 1 then
        fprintf f "nproofs %d@," nproofs;
      fprintf
        f "%s%s(false)@,%6d%16d(%5d)"
        "ngoals" "   nalternatives"
        ngoals (nalternatives + nfalses) nfalses;
      if nproofs > 1 then
        fprintf f "@,%6.1f%16.1f(%5.1f)"
                (float_of_int ngoals /. float_of_int nproofs)
                (float_of_int (nalternatives + nfalses) /. float_of_int nproofs)
                (float_of_int nfalses /. float_of_int nproofs);
      fprintf f "@,@]"

    let print_and_pop (f:Format.formatter) (s:t): unit =
      match s.stack with
      | [] ->
         assert false (* Illegal call *)
      | hd :: tl ->
         s.stack <- tl;
         print_delta f hd s

    let print (f:Format.formatter) (s:t): unit =
      print_delta f (make_entry ()) s
  end


let statistics = Statistics.make ()

let push_statistics (): unit =
  Statistics.push statistics

let print_statistics_and_pop (str:string): unit =
  let open Format in
  printf "@[%s" str;
  Statistics.print_and_pop std_formatter statistics;
  printf "@]@."

type context = {pc:PC.t; mutable map: int TermMap.t}


type alternative = {
    premises: (int * int option) array; (* subgoal, pos in proved assertions*)
    mutable npremises: int;      (* number of premises still to prove *)
    bwd_idx: int;                (* index of the backward rule *)
    mutable failed: bool;
    mutable obsolete: bool;
  }



type goal = {
    goal: term;
    ctxt: context;
    mutable black:  IntSet.t; (* Blacklist of rules which are no longer to be
                                 considered *)
    mutable target:   term;
    (* The target is the same as the goal, if the goal is not a general
       implication chain, otherwise its the target of the chain. *)
    mutable tgt_ctxt: context;
    mutable visited:  bool;
    mutable ancestors: IntSet.t;
    mutable parents:  (int*int*int) list;  (* parent, alternative, subgoal *)
    (* Backpointer to parent, alternative in the parent and subgoal
       within the alternative. *)
    mutable alternatives: alternative array;
    mutable nfailed: int;    (* number of failed alternatives *)
    mutable pos: int option; (* position in proof table *)
    mutable obsolete: bool;
    mutable failed: bool
  }


type t = {
    goals: goal Seq.t;
    mutable reactivated: int list;
    verbosity: int;
    trace:     bool;
  }



let goal_report_threshold = 500
let goal_limit_ref = ref 2000

let goal_limit () = !goal_limit_ref



let count (gs:t): int = Seq.count gs.goals


let item (i:int) (gs:t): goal =
  assert (i < count gs);
  Seq.elem i gs.goals


let goal (g:term) (i:int) (black:IntSet.t)
         (parents:(int*int*int) list)
         (ctxt:context)
         (gs:t)
    : goal =
  (*assert (PC.is_well_typed g pc);*)
  {goal      = g;
   ctxt;
   black;
   target    = g;
   tgt_ctxt  = ctxt;
   visited   = false;
   ancestors =
     IntSet.add
       i
       (List.fold_left
          (fun set (ipar,_,_) ->
            IntSet.union set (item ipar gs).ancestors
          )
          IntSet.empty
          parents);
   parents;
   alternatives = [||];
   nfailed   = 0;
   pos       = None;
   obsolete = false;
   failed = false
 }



let root_goal (g:term) (pc: PC.t) (gs:t): goal =
  goal g 0 IntSet.empty [] {pc; map = TermMap.empty} gs



exception Root_succeeded



let has_succeeded (i:int) (gs:t): bool =
  (item i gs).pos <> None


let is_visitable (i:int) (gs:t): bool =
  let g = item i gs in
  not (g.visited || g.obsolete)



let init (g:term) (pc:PC.t): t =
  let gs = {goals = Seq.empty ();
            reactivated = [];
            verbosity = PC.verbosity pc;
            trace = PC.is_tracing pc}
  in
  let goal = root_goal g pc gs in
  Seq.push goal gs.goals;
  gs



let rec reactivate_goal (i:int) (gs:t): unit =
  (* Reactivate the goal [i] if it has become obsolete and has not yet
     failed. *)
  let g = item i gs in
  if g.obsolete && not g.failed && g.pos = None then
    begin
      assert (g.pos = None);
      g.obsolete <- false;
      gs.reactivated <- i :: gs.reactivated;
      Array.iter
        (fun alt ->
          reactivate_alternative alt gs
        )
        g.alternatives
    end

and reactivate_alternative (alt:alternative) (gs:t): unit =
  (* Reactivate the alternative [alt] if it has become obsolete and has not
     yet failed. *)
  if alt.obsolete && not alt.failed then
    begin
      alt.obsolete <- false;
      Array.iter
        (fun (j,jpos) ->
          if jpos = None then
            reactivate_goal j gs
        )
        alt.premises
    end



let rec set_goal_obsolete (i:int) (gs:t): unit =
  (* A goal becomes obsolete if alternatives which use it are either failed
     or obsolte. *)
  let g = item i gs in
  if
    List.for_all
      (fun (ipar,ialt,isub) ->
        assert (0 <= ialt);
        assert (ialt < Array.length (item ipar gs).alternatives);
        let alt = (item ipar gs).alternatives.(ialt) in
        alt.failed || alt.obsolete
      )
      g.parents
  then
    begin
      g.obsolete <- true;
      for ialt = g.nfailed to Array.length g.alternatives - 1 do
        set_alternative_obsolete ialt i gs
      done
    end

and set_alternative_obsolete (ialt:int) (i:int) (gs:t): unit =
  (* The alternative [ialt] of the goal [i] becomes obsolete because the goal
     has become obsolete. *)
  let g   = item i gs in
  let alt = g.alternatives.(ialt) in
  alt.obsolete <- true;
  Array.iter
    (fun (p,pos) ->
      match pos with
      | None ->
         set_goal_obsolete p gs
      | _ ->
         ()
    )
    alt.premises

and set_alternative_failed (ialt:int) (i:int) (gs:t): unit =
  (* The alternative [ialt] of the goal [i] fails because one of its subgoals
     failed. The other subgoals of the alternative become obsolete. *)
  let g   = item i gs in
  let alt = g.alternatives.(ialt) in
  if not alt.failed then
    begin
      g.nfailed <- 1 + g.nfailed;
      alt.failed <- true;
      Array.iter
        (fun (p,pos) ->
          match pos with
          | None ->
             set_goal_obsolete p gs
          | _ ->
             ()
    )
        alt.premises
    end;
  if g.nfailed = Array.length g.alternatives then
    set_goal_failed i gs


and set_goal_failed (i:int) (gs:t): unit =
  (* The goal [i] has failed. If the goal is the root goal then the whole
     proof is failed.

     If the goal is not the root goal then it belongs to an alternative of its
     parent. The alternative is failed. All other subgoals of the parent
     become obsolete. If the alternative is the last alternative then the
     parent fails as well.  *)
  let g = item i gs in
  if not g.failed then
    begin
      if gs.trace then
        begin
          let prefix = PC.trace_prefix g.ctxt.pc in
          printf "%sfailed  goal %d: %s\n"
                 prefix i (PC.string_of_term g.goal g.ctxt.pc);
        end;
      g.failed <- true;
      if g.parents = [] then
        begin
          assert (i = 0);
          raise (Proof_failed "")
        end;
      List.iter
        (fun (ipar,ialt,isub) ->
          set_alternative_failed ialt ipar gs
        )
        g.parents
    end



let pc_discharged (pos:int) (pc:PC.t): term * proof_term =
  try
    PC.discharged pos pc
  with Not_found ->
    assert false (* cannot happen in generated proof *)



let discharged (pos:int) (pc:PC.t): int * PC.t =
  let t,pt = pc_discharged pos pc
  and cnt0 = PC.count_previous pc
  and pc = PC.pop pc in
  assert (cnt0 <= PC.count pc);
  let delta = PC.count pc - cnt0 in
  let pos = PC.add_proved_with_delta t pt delta pc in
  PC.clear_work pc;
  pos, pc



let discharge_target (pos:int) (g:goal): unit =
  (* The target of the goal [g] has succeeded and it is at [pos] in the target
     context. The target has to be discharged with all variables and assumptions and
     the discharged term and proof term has to be inserted into the goal context *)
  let depth = PC.depth g.ctxt.pc in
  let rec discharge pos pc =
    if PC.depth pc = depth then
      pos
    else
      let pos, pc = discharged pos pc in
      discharge pos pc
  in
  let pos = discharge pos g.tgt_ctxt.pc in
  g.pos <- Some pos




let succeed_alternative (ialt:int) (g:goal): int =
  (* The alternative [ialt] of the goal [g] has succeeded because all subgoals
     of the alternative have succeeded. Apply the modus ponens law and
     calculate the position of the goal in the assertion table. *)
  let alt = g.alternatives.(ialt) in
  let n   = Array.length alt.premises in
  let rec premise (i:int) (a_idx:int): int =
    if i = n then
      a_idx
    else
      begin
        let _,pos = alt.premises.(i) in
        match pos with
        | None ->
           assert false
        | Some pos ->
           let a_idx = PC.add_mp pos a_idx false g.tgt_ctxt.pc in
           premise (i+1) a_idx
      end
  in
  premise 0 alt.bwd_idx




let rec set_goal_succeeded (i:int) (gs:t): unit =
  (* The goal [i] has succeeded. If the goal is the root goal then the proof
     is done.  If the goal is not the root goal then it belongs to an
     alternative of its parent.  *)
  let g = item i gs in
  if gs.trace then
    begin
      let prefix = PC.trace_prefix g.ctxt.pc
      and obs = if g.obsolete then " of obsolete" else "" in
      printf "%ssuccess%s goal %d: %s\n"
             prefix obs i (PC.string_long_of_term g.goal g.ctxt.pc);
    end;
  let g_pos =
    match g.pos with
    | Some pos -> pos
    | _ -> assert false (* The goal has succeeded *)
  in
  assert (g_pos < PC.count g.ctxt.pc);
  if g.parents = [] then
    begin
      assert (i = 0);
      raise Root_succeeded
    end;
  List.iter
    (fun (ipar,ialt,isub) ->
        let par = item ipar gs in
        assert (ialt < Array.length par.alternatives);
        let alt = par.alternatives.(ialt) in
        assert (isub < Array.length alt.premises);
        let p,pos = alt.premises.(isub) in
        assert (p = i);
        if pos = None then
          begin
            assert (pos = None);
            assert (0 < alt.npremises);
            alt.premises.(isub) <- p, Some g_pos;
            alt.npremises <- alt.npremises - 1;
            if alt.npremises = 0 then
              begin
                (* The alternative has succeeded. *)
                let pos = succeed_alternative ialt par in
                discharge_target pos par;
                set_goal_succeeded ipar gs;
                (* All other alternatives become obsolete. *)
                for jalt = 0 to Array.length par.alternatives - 1 do
                  if jalt <> ialt then
                    set_alternative_obsolete jalt ipar gs
                done
              end
          end
    )
    g.parents




let enter (i:int) (gs:t): unit =
  (* Check if the goal is a generalized implication chain i.e. universally
     quantified and/or and implication chain.

     If yes, create a new target and a target context, push the assumptions
     and close the context. Iterate the procedure if the target is universally
     quantified and/or an implication chain.
   *)
  let g = item i gs in
  let rec do_enter gl ctxt =
    let tps,fgs,ps_rev,tgt =
      PC.split_general_implication_chain gl ctxt.pc in
    let n = Formals.count tps in
    if n = 0 && ps_rev = [] then
      ()
    else
      begin
        let pc =
          PC.push_typed0 tps fgs ctxt.pc in
        let tgt_ctxt = {pc; map = TermMap.empty} in
        List.iter
          (fun p ->
            ignore (PC.add_assumption p true pc);
          )
          (List.rev ps_rev);
        if ps_rev <> [] then
          PC.close_assumptions pc;
        g.target <- tgt;
        g.tgt_ctxt <- tgt_ctxt;
        do_enter tgt tgt_ctxt
      end
  in
  do_enter g.goal g.ctxt




let prove_trivially (g:goal): unit =
  let idx = PC.find_goal g.target g.tgt_ctxt.pc in
  discharge_target idx g




let calc_blacklist (cons:bool) (idx:int) (used:IntSet.t) (pc:PC.t): IntSet.t =
  let used =
    if cons then
      used
    else
      List.fold_left
        (fun set i ->
          IntSet.add i set
        )
        used
        (PC.previous_schematics idx pc)
  in
  IntSet.add idx used




let trace_target_subgoals (i:int) (gs:t): unit =
  let g = item i gs in
  let pc = g.tgt_ctxt.pc in
  let prefix = PC.trace_prefix pc in
  printf "%starget %d: %s\n" prefix i (PC.string_of_term g.target pc);
  for ialt = 0 to Array.length g.alternatives - 1 do
    let alt = g.alternatives.(ialt) in
    Array.iter
      (fun (i,_) ->
        let sg = item i gs in
        printf "%s  %2d subgoal %d: %s\n"
          prefix ialt i (PC.string_of_term sg.goal sg.ctxt.pc))
      alt.premises
  done


exception Cyclic

let generate_subgoal
    (p:term) (cons:bool) (j:int) (j_idx:int) (jsub:int) (i:int) (gs:t): int =
  (* Generate a subgoal [p] where [cons] indicates if the subgoal is
     conservative for the alternative [j] (at position [j_idx] in the assertion
     table) of the goal [i]. *)
  let cnt = count gs in
  let g   = item i gs in (* Parent goal *)
  let generate (): int =
    let black =
      calc_blacklist cons j_idx g.black g.tgt_ctxt.pc
    in
    let sub = goal p cnt black [i,j,jsub] g.tgt_ctxt gs in
    Statistics.add_goal statistics;
    Seq.push sub gs.goals;
    cnt
  in
  try
    let isub = TermMap.find p g.tgt_ctxt.map in
    if gs.trace then
      printf "%sduplicate subgoal %d: %s\n"
             (PC.trace_prefix g.tgt_ctxt.pc)
             isub (PC.string_of_term p g.tgt_ctxt.pc);
    if IntSet.mem isub g.ancestors then
      begin
        if gs.trace then
          printf "%scyclic subgoal %d: %s\n"
                 (PC.trace_prefix g.ctxt.pc)
                 isub (PC.string_of_term p g.ctxt.pc);
        raise Cyclic
      end;
    (*let sub = item isub gs in
    let black = calc_blacklist cons j_idx g.black g.tgt_ctxt.pc in
    sub.black <- IntSet.union black sub.black;
    sub.parents <- (i,j,jsub) :: sub.parents;
    if (item isub gs).obsolete then
      reactivate_goal isub gs;*)
    isub
  with Not_found ->
    g.tgt_ctxt.map <- TermMap.add p cnt g.tgt_ctxt.map;
    generate ()



let generate_subgoals (i:int) (gs:t): unit =
  (* Generate the subgoals of all alternatives of the goal [i]. *)
  let g     = item i gs in
  let alts = PC.find_backward_goal g.target g.black g.tgt_ctxt.pc in
  let _, alts, patches =
    List.fold_left (* all alternatives *)
      (fun (j,alts,patches) bwd_idx ->
        let cnt = count gs in
        try
          let ps = PC.premises bwd_idx g.tgt_ctxt.pc in
          let ps,_,npremises,patches =
            List.fold_left (* all premises i.e. subgoals *)
              (fun (ps,jsub,npremises,patches) (p,cons) ->
                let cnt = count gs in
                let k = generate_subgoal p cons j bwd_idx jsub i gs in
                let k_pos = (item k gs).pos in
                (k, k_pos) :: ps,
                jsub+1,
                npremises + (if k_pos = None then 1 else 0),
                if k < cnt then
                  (k,cons,i,j,bwd_idx,jsub) :: patches
                else
                  patches
              )
              ([],0,0,patches)
              ps
          in
          let ps = List.rev ps in
          let ps = Array.of_list ps in
          (j+1),
          {premises = ps; bwd_idx; npremises; failed = false; obsolete = false}
          :: alts,
          patches
        with Cyclic ->
          interval_iter
            (fun k ->
              g.tgt_ctxt.map <- TermMap.remove (item k gs).goal g.tgt_ctxt.map
            )
            cnt (count gs);
          Seq.keep cnt gs.goals;
          j, alts, patches
      )
      (0,[],[])
      alts
  in
  g.alternatives <- Array.of_list (List.rev alts);
  let nalts = Array.length g.alternatives in
  if Term.equivalent g.target (PC.false_constant g.tgt_ctxt.pc) then
    Statistics.add_falses nalts statistics
  else
    Statistics.add_alternatives nalts statistics;
  List.iter
    (fun (k,cons,ipar,ialt,bwd_idx,isub) ->
      let sub = item k gs
      and par = item ipar gs in
      let black = calc_blacklist cons bwd_idx par.black par.tgt_ctxt.pc in
      sub.black <- IntSet.union black sub.black;
      sub.parents <- (ipar,ialt,isub) :: sub.parents;
      if sub.obsolete then
        reactivate_goal k gs;
      if sub.failed then
        set_alternative_failed ialt i gs
    )
    patches;
  if Array.length g.alternatives = 0 then
    set_goal_failed i gs;
  if gs.trace then
    trace_target_subgoals i gs;
  try
    let ialt =
      Search.array_find_min (fun alt -> alt.npremises = 0) g.alternatives in
    let pos = succeed_alternative ialt g in
    discharge_target pos g;
    set_goal_succeeded i gs;
  with Not_found ->
    ()





let ancestors (i:int) (gs:t): int list =
  IntSet.elements (item i gs).ancestors


let trace_ancestors (i:int) (gs:t): unit =
  let g = item i gs in
  let ancs = ancestors i gs in
  let str = String.concat "," (List.map string_of_int ancs) in
  let prefix = PC.trace_prefix g.ctxt.pc in
  printf "%sancestors [%s]\n" prefix str


let trace_visit (i:int) (gs:t): unit =
  let g = item i gs in
  let prefix = PC.trace_prefix g.ctxt.pc in
  printf "\n%svisit goal %d: %s\n"
    prefix i
    (PC.string_long_of_term g.goal g.ctxt.pc);
  printf "                     %s\n" (Term.to_string g.goal);
  List.iter
    (fun (ipar,ialt,isub) ->
      let par = item ipar gs in
      trace_ancestors i gs;
      printf "%s  parent %d %s\n" prefix ipar
             (PC.string_of_term par.goal par.ctxt.pc);
      if par.goal <> par.target then
        printf "%s  parent target %s\n"
          prefix (PC.string_of_term par.target par.tgt_ctxt.pc)(*;
      let alt = par.alternatives.(ialt) in
      printf "%salternative   %s\n"
        prefix (PC.string_of_term_i alt.a_idx par.tgt_ctxt)*)
    )
    g.parents



let visit (i:int) (gs:t): unit =
  assert (i < count gs);
  assert (is_visitable i gs);
  assert (not (has_succeeded i gs));
  let g = item i gs in
  if gs.trace then trace_visit i gs;
  g.visited <- true;
  try
    prove_trivially g;
    set_goal_succeeded i gs
  with Not_found ->
    enter i gs;
    try
      prove_trivially g;
      set_goal_succeeded i gs
    with Not_found ->
      generate_subgoals i gs


let trace_viable_subgoals (gs:t): unit =
  let max_level = 10 in
  let prefix level = String.make (2*level) ' '
  in
  let rec trace (i:int) (level:int): unit =
    let pref = prefix level in
    let g = item i gs in
    printf "%sgoal %d %s\n" pref i (PC.string_of_term g.goal g.ctxt.pc);
    printf "%s    failed %b, obsolete %b, proved %b\n"
           pref g.failed g.obsolete (g.pos <> None);
    if level = 10 then
      printf "level %d reached\n" max_level;
    if not (g.failed || g.obsolete || g.pos <> None) && level < max_level then
      Array.iteri
        (fun i (alt:alternative) ->
          if not (alt.obsolete || alt.failed) then
            begin
              assert (alt.bwd_idx >= 0);
              printf "%salternative %d %s\n"
                     pref i (PC.string_of_term_i alt.bwd_idx g.tgt_ctxt.pc);
              Array.iter
                (fun (j,pos) ->
                  trace j (level+1))
                alt.premises
            end
        )
        g.alternatives
  in
  trace 0 0


let proof_term (g:term) (pc:PC.t): term * proof_term =
  let pc = PC.push_empty pc in
  PC.close_assumptions pc;
  Statistics.push statistics;
  Statistics.add_proof statistics;
  let gs = init g pc in
  if gs.trace then begin
    printf "\n%strying to prove: %s\n"
      (PC.trace_prefix pc)
      (PC.string_long_of_term g pc);
    if PC.verbosity pc > 3 then
      printf "%s                 %s\n"
        (PC.trace_prefix pc)
        (Term.to_string g);
    printf "\n"
  end;
  if not (PC.is_global pc) then
    PC.close pc;
  let rec round (i:int) (start:int): unit =
    if PC.is_interface_check pc && 1 < i then
      raise (Proof_failed "");
    if PC.verbosity pc >= 1 && start >= goal_report_threshold then
      begin
        printf "next round entered with %d goals\n" start;
        let g0 = Seq.elem 0 gs.goals in
        printf "  hard to prove %s\n"
               (PC.string_of_term g0.goal g0.ctxt.pc);
        flush_all ()
      end;
    if goal_limit () <= start then
      raise (Proof_failed (", goal limit " ^ (string_of_int (goal_limit())) ^
                           " exceeded"));
    let cnt = count gs in
    if PC.is_tracing pc then
      printf "%s-- round %d with %d goals starting from %d --\n"
             (PC.trace_prefix pc) i (cnt - start) start;
    for j = start to cnt - 1 do
      if is_visitable j gs then
        visit j gs
    done;
    while gs.reactivated <> [] do
      let lst = List.rev gs.reactivated in
      gs.reactivated <- [];
      List.iter
        (fun j -> if is_visitable j gs then visit j gs)
        lst;
    done;
    assert (gs.reactivated = []);
    if cnt = count gs then
      begin
        (*if PC.is_tracing pc then
          trace_viable_subgoals gs;*)
        raise (Proof_failed (" (subgoals exhausted)"))
      end;
    assert (cnt < count gs);
    if PC.is_tracing pc then printf "\n";
    round (i+1) cnt
  in
  try
    round 0 0;
    assert false (* shall not happen, because either we succeed or we fail,
                    but we cannot fall through *)
  with
  | Root_succeeded ->
    let g = item 0 gs in
    let g_pos =
      match g.pos with
      | Some pos -> pos
      | _ -> assert false (* Root goal has succeeded *)
    in
    assert (g_pos < PC.count g.ctxt.pc);
    if gs.trace then
      print_statistics_and_pop (PC.trace_prefix pc)
    else
      Statistics.pop statistics;
    pc_discharged g_pos g.ctxt.pc
  | Proof_failed msg ->
     Statistics.pop statistics;
     raise (Proof_failed msg)




let is_provable (g:term) (pc:PC.t): bool =
  try
    let _ = proof_term g pc in true
  with Proof_failed _ ->
    false


let prove (g:term) (pc:PC.t): unit =
  let _ = proof_term g pc in ()


let prove_and_insert (g:term) (pc:PC.t): int =
  let t,pt = proof_term g pc in
  PC.add_proved_with_delta t pt 0 pc
