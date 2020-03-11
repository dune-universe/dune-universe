(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Container
open Term

module Option = Fmlib.Option

type sublist = (int*Term_sub.t) list

type substitutions =
    Initial
  | Sub_list of sublist


module IntPairMap = Map.Make(struct
  let compare = Pervasives.compare
  type t = int*int
end)


let intmap_find (i:int) (def:'a) (map:'a IntMap.t): 'a =
  try
    IntMap.find i map
  with Not_found ->
    def



type t = {
    terms: (int*int*int*int*term) list;    (* idx,nb,nargs,nbenv,term *)
    avars: (int*int*int) list;             (* [idx, argument variable, nb,nargs] *)
    bvars: int list IntMap.t;              (* bvar -> list of indices *)
    fvars: (int * int list IntMap.t) list; (* [nbenv, fvar -> list of indices] *)
    apps:  (t array * int list) IntMap.t;  (* one for each function variable *)
    fapps: (t * t array) IntMap.t;         (* one for each number of arguments *)
    lams:  (t list * t) IntMap.t;
                              (* one for each number of preconditions *)
    alls:  t IntMap.t;        (* one for each number of arguments *)
    somes: t IntMap.t;        (* one for each number of arguments *)
    ifs:   (t * t * t) option;
    ases:  (t * t) IntMap.t;  (* one for each number of pattern variables *)
    inspects: (t * ((t * t) IntMap.t) array) IntMap.t;
    (* one for each number of cases and each case one for each number of
       pattern variables *)
    inds: (t array) IntPairMap.t;
         (* one for each pair (nsets,nrules) *)
  }


let empty = {
  terms = [];
  avars = [];
  bvars = IntMap.empty;
  fvars = [];
  apps  = IntMap.empty;
  fapps = IntMap.empty;
  lams  = IntMap.empty;
  alls  = IntMap.empty;
  somes = IntMap.empty;
  ifs   = None;
  ases  = IntMap.empty;
  inspects = IntMap.empty;
  inds  = IntPairMap.empty}


let count  (tab:t): int =
  List.length tab.terms

exception Term_found of term

let find_lam (n:int) (tab: t): t list * t =
  IntMap.find n tab.lams

let add_lam (n:int) (lamtab:t list * t) (tab:t): t =
  {tab with lams = IntMap.add n lamtab tab.lams}


let terms0 (tab:t): (int*int*int*int*term) list =
  (** All the terms as a list [obj,nb,nargs,nbenv,term] of the table [tab] in the
      reverse order in which they have been inserted.  *)
  tab.terms


let terms (tab:t): (int*int*int*term) list =
  (** All the terms as a list [idx,nargs,nbenv,term] of the table [tab] in the
      order in which they have been inserted.  *)
  List.rev_map (fun (idx,_,nargs,nbenv,term) -> idx,nargs,nbenv,term) tab.terms





let qmap (is_all:bool) (tab:t): t IntMap.t =
  if is_all then
    tab.alls
  else
    tab.somes



let merge_avars (avars:(int*int*int) list) (t:term) (r:substitutions): sublist =
  let rec merge avars lst reslst =
    (* avars and lst are descending *)
    match avars, lst with
      [], _ ->
        reslst
    | _, [] ->
        reslst
    | (idx1,avar,nargs)::tail1, (idx2,sub)::tail2 ->
        if idx1 < idx2 then (* idx2 cannot be merged *)
          merge avars tail2 reslst
        else if idx1 > idx2 then (* idx1 cannot be merged *)
          merge tail1 lst reslst
        else
          try
            let t2 = Term_sub.find avar sub in
            if Term.equivalent t t2 then
              merge tail1 tail2 ((idx2,sub)::reslst)
            else
              merge tail1 tail2 reslst
          with Not_found ->
            let sub = Term_sub.add avar t sub in
            merge tail1 tail2 ((idx2,sub)::reslst)
  in
  match r with
    Initial ->
      List.rev_map
        (fun (idx,avar,nargs) -> idx,Term_sub.singleton avar t)
        avars
  | Sub_list lst ->
      merge avars lst []




let merge_idxlst (idxlst:int list) (r:substitutions): sublist =
  let rec merge idxlst lst reslst =
    (* idxlst and lst are descending *)
    match idxlst, lst with
      [], _ ->
        reslst
    | _, [] ->
        reslst
    | idx1::tail1, (idx2,sub)::tail2 ->
        if idx1 < idx2 then
          merge idxlst tail2 reslst
        else if idx1 > idx2 then
          merge tail1 lst reslst
        else
          merge tail1 tail2 ((idx2,sub)::reslst)
  in
  try
    match r with
      Initial ->
        List.rev_map (fun idx -> idx,Term_sub.empty) idxlst
    | Sub_list lst ->
        merge idxlst lst []
  with Not_found ->
    []



let join_sublist (l1: sublist) (l2:sublist): sublist =
  (* [l1] and [l2] are ascending and disjoint. The result is ascending. *)
  let rec join l1 l2 res =
    match l1, l2 with
      [], _ ->
        List.rev_append l2 res
    | _, [] ->
        List.rev_append l1 res
    | (idx1,sub1)::tail1, (idx2,sub2)::tail2 ->
        assert (idx1 <> idx2); (* lists are disjoint! *)
        if idx1 < idx2 then
          join tail1 l2 ((idx1,sub1)::res)
        else
          join l1 tail2 ((idx2,sub2)::res)
  in
  List.rev (join l1 l2 [])



let extract_idxlst_locvars
    (v:int)
    (nbenvt: int)
    (fvars: (int * int list IntMap.t) list)
    : int list =
  let idxlst =
    List.fold_left
      (fun idxlst (nbenv,map) ->
        if nbenvt - nbenv <= v then
          try
            let idxlstvar =
            IntMap.find (v - (nbenvt - nbenv)) map
            in
            List.rev_append idxlstvar idxlst
        with Not_found ->
          idxlst
      else
          idxlst
      )
      []
      fvars
  in
  List.rev idxlst



type unifier = term -> t -> int -> substitutions -> sublist

let uni_args
    (args: term array)
    (nb:int)
    (argtabs: t array)
    (r: substitutions)
    (uni: unifier)
    : sublist =
  let len = Array.length args in
  assert (len = Array.length argtabs);
  let sublst, _ =
    interval_fold
      (fun (sublst,r) i ->
        let r =
          if sublst = [] then
            r
          else
            Sub_list (List.rev sublst)
        in
        let sublst = uni args.(i) argtabs.(i) nb r in
        if sublst = [] then begin
          raise Not_found
        end;
        sublst, Initial (* Initial is dummy, because sublst is never
                           empty *)
      )
      ([],r)
      0 len
  in
  sublst


let substitutions (lst:sublist): substitutions =
  match lst with
  | [] ->
     raise Not_found
  | _ ->
     Sub_list (List.rev lst)


let uni_core
    (t:term) (nb:int) (nb0:int) (nargs:int) (nvars:int)
    (r:substitutions)
    (uni:unifier)
    (sfun: int->int)
    (tab:t)
    : sublist =
  (* The core part of the unifier which handles all parts except argument
     variables.

     The term [t] has [nb] bound variables and the first [nb0] of them are
     universally quantied. Then come [nargs] argument variables and [nvars]
     free variables.

     The unifier tries to find an exact match between the toplevel part of
     the term [t] and the current [tab]. If no exact match is possible the
     exception [Not_found] is thrown.
   *)
  assert (nb0 <= nb);
  let nbb = nb - nb0 in
  match t with
    Variable i when i < nbb ->
      (* bound variable of [t] *)
      let idxlst = IntMap.find i tab.bvars in
      merge_idxlst idxlst r
  | Variable i when i < nb ->
      (* bound variable of [t] which can match an argument variable *)
      assert (nbb <= i);
      let idxlst =
        List.fold_left
          (fun lst (idx,avar,nargs) ->
            if nargs = nb0 && i - nbb = avar then
              idx :: lst
            else
              lst
          )
          []
          tab.avars
      in
      merge_idxlst idxlst r
  | Variable i ->
      assert (nb + nargs <= i);
      let idxlst = extract_idxlst_locvars (i-nb-nargs) nvars tab.fvars in
      merge_idxlst idxlst r
  | VAppl (i,args,_,_) ->
      assert (nb + nargs + nvars <= i);
      let idx = i - nb - nargs - nvars in
      let idx = sfun idx in
      let argtabs,idxlst = IntMap.find idx tab.apps
      in
      let len = Array.length args in
      assert (len = Array.length argtabs);
      if len = 0 then
        merge_idxlst idxlst r
      else
        uni_args args nb argtabs r uni
  | Application (f,args,_) ->
      let len = Array.length args in
      let ftab, argtabs = IntMap.find len tab.fapps in
      assert (len = Array.length argtabs);
      let sublst = uni f ftab nb r in
      interval_fold
        (fun sublst i ->
          uni args.(i) argtabs.(i) nb (substitutions sublst)
        )
        sublst
        0 len
  | Lam (tps,_,pres,t,_) ->
     let nb = nb + Formals.count tps in
     let len = List.length pres in
     let prestablst,ttab = find_lam len tab in
     let sublst = uni t ttab nb r in
     let rec addpres pres prestablst sublst =
       let r = substitutions sublst in
       match pres, prestablst with
         [], [] ->
         sublst
       | p::pres, tab::prestablst ->
          let sublst = uni p tab nb r in
            addpres pres prestablst sublst
       | _ ->
          assert false (* lists must have the same size *)
     in
     addpres pres prestablst sublst
  | QExp (tps,_,t,is_all) ->
     let n = Formals.count tps in
     let ttab = IntMap.find n (qmap is_all tab) in
     uni t ttab (n+nb) r
  | Ifexp (cond,a,b) ->
     begin
       match tab.ifs with
       | None ->
          raise Not_found
       | Some (tabc, taba, tabb) ->
          uni_args [|cond;a;b|] nb [|tabc;taba;tabb|] r uni
     end
  | Asexp (insp, tps, pat) ->
     let n = Array.length tps in
     let itab,ptab = IntMap.find n tab.ases in
     let sublst = uni insp itab nb r in
     uni pat ptab (nb+n) (substitutions sublst)
  | Inspect(insp,cases) ->
     let ncases = Array.length cases in
     let itab,ctabmaps = IntMap.find ncases tab.inspects in
     interval_fold
       (fun sublst i ->
         let fs,pat,res = cases.(i) in
         let n = Array2.count fs in
         let ptab,restab = IntMap.find n ctabmaps.(i) in
         let sublst = uni pat ptab (nb+n) (substitutions sublst) in
         uni res restab (nb+n) (substitutions sublst)
       )
       (uni insp itab nb r) 0 ncases
  | Indset (nme,tp,rs) ->
      let nrules = Array.length rs in
      let argtabs = IntPairMap.find (1,nrules) tab.inds in
      uni_args rs nb argtabs r uni





let unify (t:term) (nbt:int) (sfun:int->int) (table:t)
    :  (int * Term_sub.t) list =
  (* Unify the term [t] which comes from an environment with [nbt] bound
     variables with the terms in the table 'table'.

     The result is a list of tuples (idx,sub) where the unified
     term [ut] has the index [idx], and applying
     the substitution [sub] to [ut] yields the term [t].

     Note: The substitutions are valid in the environment of [t] because
           they consist of subterms of [t]. Before applying them to the
           the term [ut] at the corresponding index [idx] the term [ut]
           has to be transformed into the environment of [t].
   *)
  let rec uni (t:term) (tab:t) (nb:int) (r:substitutions): sublist =
    let avar_subs: sublist = (* merge all assertions which have at this position
                                an argument variable into the result *)
      try
        let t = Term.down nb t in
        merge_avars tab.avars t r
      with Term_capture ->
        []
    and other_subs: sublist =
      try
        uni_core t nb 0 0 nbt r uni sfun tab
      with Not_found ->
        []
    in
    join_sublist avar_subs other_subs
  in
  uni t table 0 Initial




let merge_avars_exact
    (avars:(int*int*int) list) (avar:int) (nargs:int) (r:substitutions)
    : sublist =
  (* The term has the argument variable [avar] of [nargs] at the current
     position. The term table has terms in [avars] with argument variables at
     the same position. The unifier has already generated the substitutions in
     [r]. We select the terms in avars which have the same argument at that
     position and which occur already in [r].

     Note: All substitutions must be empty.
*)
  let rec merge avars lst reslst =
    (* avars and lst are descending *)
    match avars, lst with
      [], _ ->
        reslst
    | _, [] ->
        reslst
    | (idx1,avar1,nargs1)::tail1, (idx2,sub)::tail2 ->
        if idx1 < idx2 then (* idx2 cannot be merged *)
          merge avars tail2 reslst
        else if idx1 > idx2 then (* idx1 cannot be merged *)
          merge tail1 lst reslst
        else if nargs1 = nargs && avar1 = avar then
          merge tail1 tail2 ((idx2,sub)::reslst)
        else
          merge tail1 tail2 reslst
  in
  match r with
    Initial ->
      List.rev_map
        (fun (idx,avar,nargs) -> idx,Term_sub.empty)
        avars
  | Sub_list lst ->
      merge avars lst []



let find (t:term) (nargs:int) (nvars:int) (sfun:int->int) (table:t)
    :  (int * Term_sub.t) list =
  (* Find the indices of all terms which are identical to the term [t] which has
     [nargs] argument variables and comes from an environment with [nvars] local
     variables.*)
  let rec uni (t:term) (tab:t) (nb:int) (r:substitutions): sublist =
    match t with
      Variable i when nb <= i && i < nb + nargs ->
        merge_avars_exact tab.avars (i - nb) nargs r
    | _ ->
        uni_core t nb 0 nargs nvars r uni sfun tab
  in
  uni t table 0 Initial



let merge_terms
    (var:int)
    (nbenv:int)
    (terms:(int*int*int*int*term)list)
    (nb0:int)
    (bvars_flg:bool) (* schematic terms will be considered only if nargs = nb0 *)
    (r:substitutions)
    : sublist =
  (* Merge the terms of a table entry which can substitute the argument
     variable [var] in a local environment with [nbenv] variables into the
     result [r]
   *)
  let down nb nargs t =
    let nb =
      if bvars_flg then begin
        assert (nargs = nb0);
        nb + nargs
      end else
        nb
    in
    Term.down nb t
  in
  let rec merge terms lst reslst =
    (* terms and lst (sublist)  are descending *)
    match terms,lst with
      [], _ ->
        reslst
    | _, [] ->
        reslst
    | (idx1,nb1,nargs1,nbenv1,t1)::tail1, (idx2,sub2)::tail2 ->
        assert (nbenv1 <= nbenv);
        if idx1 < idx2 then (* idx2 not possible *)
          merge terms tail2 reslst
        else if idx1 > idx2 then (* idx1 not possible *)
          merge tail1 lst reslst
        else if bvars_flg && nargs1 <> nb0 then (* idx1 = idx2 *)
          merge tail1 tail2 reslst
        else begin (* idx1 = idx2, i.e. substitutions can be merged *)
          try
            let t1 = down nb1 nargs1 t1 in
            begin try
              let t2 = Term_sub.find var sub2 in
              if Term.equivalent t1 t2 then
                merge tail1 tail2 ((idx2,sub2)::reslst)
              else begin
                merge tail1 tail2 reslst
              end
            with Not_found ->
              let sub2 = Term_sub.add var t1 sub2 in
              merge tail1 tail2 ((idx2,sub2)::reslst)
            end
          with Term_capture ->
            merge tail1 tail2 reslst
        end
  in
  match r with
    Initial ->
      List.fold_left
        (fun lst (idx,nb_0,nargs_0,nbenv_0,t_0) ->
          assert (nbenv_0 <= nbenv);
          assert (not (List.exists (fun (i,_) -> i=idx) lst));
          if bvars_flg && nargs_0 <> nb0 then
            lst
          else
            let t_0 = down nb_0 nargs_0 t_0 in
            try
              let t_0 = Term.down nb_0 t_0 in
              (idx, Term_sub.singleton var t_0)::lst
            with Term_capture ->
              lst)
        []
        terms
  | Sub_list lst ->
      merge terms lst []






let unify_with
    (t:term) (nb0:int) (nargs:int) (nbenv:int)
    (bvars_flg:bool) (sfun:int->int)
    (table:t)
    :  (int * Term_sub.t) list =
  (** Unify the terms in the table [table] with term [t] which has [nb0]
      universally quantied variables below [nargs] arguments and comes from an
      environment with [nbenv] variables.

      The result is a list of tuples (idx,sub) where applying the substitution
      [sub] to the term [t] yields the term at [idx].

      Note: The substitutions are valid in the environment of the term
            at idx because they are subterms of the term at [idx]. Before
            applying the substitutions to [t] [t] has to be transformed
            into the environment of the term at [idx] (e.g. in the term [t]
            space has to be made for the variables of the term at [idx]).
   *)
  let rec uniw (t:term) (tab:t) (nb:int) (r:substitutions): sublist =
    match t with
      Variable i when nb <= i && i < nb + nargs ->
        (* argument variable of [t] *)
        merge_terms (i - nb) nbenv tab.terms nb0 bvars_flg r
    | _ ->
        try
          uni_core t nb nb0 nargs nbenv r uniw sfun tab
        with Not_found ->
          []
  in
  uniw t table nb0 Initial






let newmap (i:int) (idx:int) (map: int list IntMap.t): int list IntMap.t =
  try
    let lst = IntMap.find i map in
    IntMap.add i (idx::lst) map
  with Not_found ->
    IntMap.add i [idx] map





let has (idx:int) (table:t): bool =
  List.exists (fun (i,_,_,_,_) -> i = idx) table.terms





let add_base
    (t:term) (nb:int) (nargs:int) (nbenv:int)
    (idx:int)
    (sfun:int->int)
    (tab:t): t =
  (** Associate the term [t] which has [nargs] arguments and comes from an
      environment with [nbenv] variables with the index [idx]
      within the node [tab].
   *)
  let rec add0 (t:term) (nb:int) (tab:t): t =
    let tab =
      match t with
        Variable i when nb<=i && i<nb+nargs ->
          (* variable is a formal argument which can be substituted *)
          (*assert (not (List.exists (fun (i,_,_) -> i=idx) tab.avars));*)
          {tab with
           avars = (idx, (i-nb),nargs) :: tab.avars(*;
           bvars = newmap i idx tab.bvars*)
         }
      | Variable i when nb+nargs <= i ->
          (* variable is a free variable (i.e. not substitutable *)
          let fvar = i - nargs - nb in
          {tab with fvars =
           let fvars = tab.fvars in
           match fvars with
             [] ->
               (nbenv, newmap fvar idx IntMap.empty) :: fvars
           | (nbnd,map)::tl ->
               assert (nbnd <= nbenv);
               if nbnd = nbenv then
                 (nbnd, newmap fvar idx map) :: tl
               else
                 (nbenv, newmap fvar idx IntMap.empty) :: fvars}
      | Variable i ->
          (* variable is bound by some abstraction *)
          assert (i < nb);
          {tab with bvars = newmap i idx tab.bvars}
      | VAppl (i,args,_,_) ->
          assert (nb + nargs + nbenv <= i);
          let len  = Array.length args
          and fidx = i - nb - nargs - nbenv in
          let fidx = sfun fidx in
          let argtabs,idxlst =
            intmap_find fidx (Array.make len empty, []) tab.apps
          in
          let idxlst = if len = 0 then idx::idxlst else idxlst
          in
          let argtabs =
            Array.mapi (fun i tab  -> add0 args.(i) nb tab) argtabs in
          {tab with apps = IntMap.add fidx (argtabs,idxlst) tab.apps}
      | Application (f,args,_) ->
          let len = Array.length args in
          let ftab,argtabs =
            intmap_find len (empty, Array.make len empty) tab.fapps
          in
          let ftab    = add0 f nb ftab
          and argtabs =
            Array.mapi (fun i tab  -> add0 args.(i) nb tab) argtabs
          in
          {tab with fapps = IntMap.add len (ftab,argtabs) tab.fapps}
      | Lam (tps,_,pres,t,_) ->
          let nb = nb + Formals.count tps in
          let len = List.length pres in
          let rec addpres pres prestablst =
            match pres, prestablst with
              [], [] -> []
            | p::pres, tab::tablst ->
                let tablst = addpres pres tablst in
                (add0 p nb tab)::tablst
            | _ ->
                assert false (* lists must have the same length *)
          in
          let prestab,ttab =
            try find_lam len tab
            with Not_found ->
              let lst = Array.to_list (Array.make (List.length pres) empty) in
              lst, empty
          in
          let ttab = add0 t nb ttab
          and prestab = addpres pres prestab
          in
          add_lam len (prestab,ttab) tab
      | QExp (tps,_,t,is_all) ->
         let n = Formals.count tps in
          let ttab = intmap_find n empty (qmap is_all tab)
          in
          let ttab = add0 t (nb+n) ttab in
          if is_all then
            {tab with alls = IntMap.add n ttab tab.alls}
          else
            {tab with somes = IntMap.add n ttab tab.somes}
      | Ifexp (cond,a,b) ->
         let ctab,atab,btab =
           match tab.ifs with
           | None ->
              empty, empty, empty
           | Some(ctab,atab,btab) ->
              ctab,atab,btab
         in
         let ctab = add0 cond nb ctab
         and atab = add0 a    nb atab
         and btab = add0 b    nb btab
         in
         {tab with ifs = Some(ctab,atab,btab)}
      | Asexp (insp,tps,pat) ->
         let n = Array.length tps in
         let itab,ptab = intmap_find n (empty,empty) tab.ases in
         let itab = add0 insp nb itab
         and ptab = add0 pat (nb+n) ptab in
         {tab with ases = IntMap.add n (itab,ptab) tab.ases}
      | Inspect (insp,cases) ->
         let ncases = Array.length cases in
         let itab,ctabs =
           intmap_find ncases (empty,Array.make ncases IntMap.empty) tab.inspects
         in
         let itab = add0 insp nb itab
         and ctabs = Array.copy ctabs in
         interval_iter
           (fun i ->
             let fs,pat,res = cases.(i) in
             let ptab,rtab = intmap_find i (empty,empty) ctabs.(i) in
             let n = Array2.count fs in
             let ptab = add0 pat (n+nb) ptab
             and rtab = add0 res (n+nb) rtab in
             ctabs.(i) <- IntMap.add i (ptab,rtab) ctabs.(i)
           )
           0 ncases;
         {tab with inspects = IntMap.add ncases (itab,ctabs) tab.inspects}
      | Indset (_,_,rs) ->
          let n = 1 in
          let nrules = Array.length rs in
          let argtabs =
            try IntPairMap.find (n,nrules) tab.inds
            with Not_found -> Array.make (nrules) empty in
          let argtabs =
            Array.mapi (fun i tab -> add0 rs.(i) (n+nb) tab) argtabs in
          {tab with inds = IntPairMap.add (n,nrules) argtabs tab.inds}
    in
    {tab with terms = (idx,nb,nargs,nbenv,t)::tab.terms}
  in
  add0 t nb tab



let add
    (t:term) (nargs:int) (nbenv:int)
    (idx:int)
    (sfun:int->int)
    (tab:t): t =
  (** Associate the term [t] which has [nargs] arguments and comes from an
      environment with [nbenv] variables with the index [idx]
      within the node [tab].
   *)
  add_base t 0 nargs nbenv idx sfun tab




let filter (f:int -> bool) (tab:t): t =
  let filt_idx idxlst = List.filter f idxlst in
  let rec filt tab =
    {terms = List.filter (fun (i,_,_,_,_) -> f i) tab.terms;
     avars = List.filter (fun (i,_,_) ->     f i) tab.avars;
     bvars = IntMap.map filt_idx tab.bvars;
     fvars = List.map (fun (nbenv,map) -> nbenv, IntMap.map filt_idx map) tab.fvars;
     apps  = IntMap.map
       (fun (args,idxlst) -> Array.map filt args, filt_idx idxlst)
       tab.apps;
     fapps = IntMap.map (fun (f,args) -> filt f, Array.map filt args) tab.fapps;
     lams  = IntMap.map (fun (pres,exp) -> List.map filt pres, filt exp) tab.lams;
     alls  = IntMap.map filt tab.alls;
     somes = IntMap.map filt tab.somes;
     ifs   = Option.map (fun (c,a,b) -> filt c, filt a, filt b) tab.ifs;
     ases  = IntMap.map (fun (insp,pat) -> filt insp, filt pat) tab.ases;
     inspects =
       IntMap.map
         (fun (itab,ctabs) ->
           filt itab,
           Array.map
             (fun cmap ->
               IntMap.map (fun (ptab,rtab) -> filt ptab, filt rtab) cmap)
             ctabs
         )
         tab.inspects;
     inds  = IntPairMap.map (fun args -> Array.map filt args) tab.inds}
  in
  filt tab



let filter_and_remap (f:int->bool) (sfun:int->int) (tab:t): t =
  List.fold_left
    (fun tab (idx,nb,nargs,nbenv,t) ->
      assert (nb = 0);
      if f idx then
        add t nargs nbenv idx sfun tab
      else
        tab)
    empty
    (List.rev tab.terms)



let merge_tabs (t1:t) (t2:t) (sfun:int->int): t =
  (* Merge the two tables [t1] and [t2] into one table. *)
  let rec merge ts1 ts2 tab =
    let add_merge (idx,nb,nargs,nbenv,t) tab ts1 ts2 =
      let tab = add_base t nb nargs nbenv idx sfun tab in
      merge ts1 ts2 tab
    in
    match ts1, ts2 with
      [], [] ->
        tab
    | h1::tl1, [] ->
        add_merge h1 tab tl1 []
    | [], h2::tl2 ->
        add_merge h2 tab [] tl2
    | h1::tl1, h2::tl2 ->
        let idx1,_,_,_,_ = h1
        and idx2,_,_,_,_ = h2
        in
        assert (idx1 <> idx2);
        if idx1 < idx2 then
          add_merge h1 tab tl1 ts2
        else
          add_merge h2 tab ts1 tl2
  in
  let ts1 = List.rev t1.terms
  and ts2 = List.rev t2.terms in
  merge ts1 ts2 empty



let remove (idx:int) (tab:t): t =
  filter (fun i -> i <> idx) tab


let unify0 (t:term) (nbt:int) (table:t)
    :  (int * Term_sub.t) list =
  unify t nbt (fun i -> i) table

let unify0_with (t:term) (nargs:int) (nbenv:int) (table:t)
    :  (int * Term_sub.t) list =
  unify_with t 0 nargs nbenv false (fun i -> i) table

let add0
    (t:term) (nargs:int) (nbenv:int)
    (idx:int)
    (table:t): t =
  add t nargs nbenv idx (fun i -> i) table
