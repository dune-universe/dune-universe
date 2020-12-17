open Term
open Container

let extract_pattern (n:int) (t:term): (int*int*int*term) list =
  (* Extract a pattern of each variable in the term [t]. The result is a list of
     triples [i,j,n,p] where
         i: original variable
         j: variable in the pattern
         n: number of variables in the pattern
         p: pattern
   *)
  assert false (* nyi *)
  (*let rec extract (var:int) (pos:int) (npat:int) (pat:term) (t:term) (nb:int) =
    let bvs = Term.bound_variables t (n+nb) in
    if not (IntSet.mem (var+nb) bvs) then
      var, pos, (pos+1), Variable (0+nb)
    else
      let extract_args pos npat pat args =
        Array.fold_left
          (fun (var,pos,npat,p) arg ->
            extract var pos npat pat arg nb)
          (var,pos,npat,pat)
          args in
      match t with
        Variable i ->
          assert (var = i + nb);
          var, pos, npat, Variable (pos+nb)
      | VAppl(i,args) ->
          assert (var <> i + nb);
          extract_args pos npat pat args
      | Application(f,args,pr) ->
          assert false (* nyi *)
      | Lam(n,nms,ps,t0,pr) ->
          assert false (* nyi *)
      | QExp(n,nms,t0,is_all) ->
          assert false (* nyi *)
      | Flow(ctrl,args) ->
          assert false (* nyi *)
      | Indset(n,nms,rs) ->
          assert false (* nyi *)
  in
  let vars = Term.bound_variables t n in
  IntSet.fold
    (fun i lst ->
      (extract i 0 1 (Variable 0) t 0) :: lst)
    vars []
*)






let unify_pattern
    (n1:int) (p1:term) (n2:int) (p2:term): term array =
  (* Find a unification of the pattern [p1] with [n1] variables and [p2] with [n2]
     variables. Both terms above their variables are from the same environment.

     The result (if unification is possible) is an array of size [n1+n2] which
     contains substitutions for all variables of the two pattern. The substitution
     applied to both pattern (with sufficiently space made for the variables of the
     other pattern) results in the same term.

     If unification is not possible then [Not_found] is raised
   *)
  let n = n1 + n2 in
  let subargs = Array.init n (fun i -> Variable i)
  and subflgs = Array.make n false
  and pat1 = Term.up_from n2 n1 p1
  and pat2 = Term.up n1 p2
  in
  let do_sub i t =
    assert (i < n);
    if subflgs.(i) && t <> subargs.(i) then
      raise Not_found
    else begin
      subflgs.(i) <- true;
      subargs.(i) <- t
    end in
  let rec uni t1 t2 =
    let uni_args args1 args2 =
      assert (Array.length args1 = Array.length args2);
      Array.iteri
        (fun i arg ->
          uni arg args2.(i))
        args1
    in
    match t1, t2 with
      Variable i, Variable j when i < n && j < n ->
        do_sub i t1;
        do_sub j t1
    | Variable i, _ when i < n ->
        assert (i < n1);
        do_sub i t2
    | _ , Variable j when j < n ->
        do_sub j t1
    | Variable i1, Variable i2 when i1 = i2 ->
        ()
    | VAppl(i1,args1,_,_), VAppl(i2,args2,_,_) when i1 = i2 ->
        uni_args args1 args2
    | Application(f1,args1,_), Application(f2,args2,_)
      when Array.length args1 = Array.length args2 ->
        assert false (* nyi: *)
    | Lam(tps1,fgs1,ps1,t01,rt1), Lam(tps2,fgs2,ps2,t02,rt2)
      when (rt1 = None) = (rt2 = None) ->
        assert false (* nyi: *)
    | QExp(tps1,_,t01,all1), QExp(tps2,_,t02,all2)
      when Formals.count tps1 = Formals.count tps2 && all1 = all2 ->
        assert false (* nyi: *)
    | Ifexp(cond1,a1,b1), Ifexp(cond2,a2,b2) ->
       assert false (* nyi *)
    | Asexp(insp1,tps1,pat1), Asexp(insp2,tps2,pat2) ->
       assert false (* nyi *)
    | Inspect(insp1,cases1), Inspect(insp2,cases2) ->
       assert false (* nyi *)
    | _ ->
        raise Not_found
  in
  uni pat1 pat2;
  assert begin
    let ok =
      Term.equivalent
        (Term.subst pat1 n subargs)
        (Term.subst pat2 n subargs) in
    if not ok then begin
      Printf.printf "unify_pattern\n";
      Printf.printf "   n1 %d, p1 %s\n" n1 (Term.to_string p1);
      Printf.printf "   n2 %d, p2 %s\n" n2 (Term.to_string p2);
      Printf.printf "   pat1 %s\n" (Term.to_string pat1);
      Printf.printf "   pat2 %s\n" (Term.to_string pat2);
      Array.iteri (fun i t ->
        Printf.printf "   %d %s\n" i (Term.to_string t))
        subargs
    end;
    ok
  end;
  subargs







(* Comparison of two terms and finding their differences

   We make a preorder traversal of the common part. Each node in this traversal
   has a unique position.

   Two nodes are structurally different if

   - they have different constructors
   - they are both unbound but different variables
   - one of them is a bound variable and the other is not the same bound variable
   - they are both applications with different number of arguments or different
     predicate flag
   - they are both lambda expression with different number of bound variables or
     different predicate flag

   If they are structurally different and contain no bound variables they are put
   outside the context and pushed to the pair list, the position is pushed onto the
   position list and the nextvar variable is incremented by one.

   If they have the same structure then some difference might appear in subterms.
 *)




let compare (t1:term) (t2:term) (eq:term->term->'a)
    : term * 'a array * term array * term array =
  (* Compare the terms and return an inner lambda term and two argument arrays so
     that the lambda term applied to the fist argument array results in the first
     term and the lambda term applied to the second argument array results in the
     second term.

     Furthermore the function [eq] is used to compute for each pair of different
     subsexpression a user defined value of arbitrary type ['a].

     Raise [Not_found] if the terms are equivalent.
   *)
  (* return n positions checked,
     positions with different subterms,
     pair list of different subterms *)
  let rec comp (t1:term) (t2:term) (nb:int)
      (pos:int) (poslst:int list) (elst:'a list) (tlst:(term*term) list)
      : int * int list * 'a list * (term*term) list =
    let different t1 t2 pos poslst elst tlst =
      try
        let t1 = Term.down nb t1
        and t2 = Term.down nb t2 in
        let e = eq t1 t2 in
        pos+1, pos::poslst, e::elst, (t1,t2)::tlst
      with Term_capture ->
        raise Not_found
    and comp_args pos poslst elst tlst args1 args2 =
      let args = Myarray.combine args1 args2 in
      Array.fold_left
        (fun (pos,poslst,elst,tlst) (a1,a2) ->
          comp a1 a2 nb pos poslst elst tlst)
        (pos,poslst,elst,tlst)
        args
    in
    match t1, t2 with
      Variable k, _ when k < nb ->
        if t1 = t2 then pos+1, poslst, elst, tlst
        else raise Not_found
    | _ , Variable k when k < nb ->
        if t1 = t2 then pos+1, poslst, elst, tlst
        else raise Not_found
    | Variable k, Variable l when k = l ->
        pos+1, poslst, elst, tlst
    | VAppl(i1,args1,_,_), VAppl(i2,args2,_,_)
      when i1 = i2 && Array.length args1 = Array.length args2 ->
        begin try
          let pos  = pos + 1 in
          comp_args pos poslst elst tlst args1 args2
        with Not_found ->
          different t1 t2 pos poslst elst tlst
        end
    | Application(f1,args1,_), Application(f2,args2,_)
      when Array.length args1 = Array.length args2 ->
        begin try
          let pos,poslst,elst,tlst = comp f1 f2 nb (1+pos) poslst elst tlst in
          comp_args pos poslst elst tlst args1 args2
        with Not_found ->
          different t1 t2 pos poslst elst tlst
        end
    | Lam(tps1,fgs1,ps1,t01,rt1), Lam(tps2,fgs2,ps2,t02,rt2)
         when Formals.count tps1 = Formals.count tps2
              && (rt1 = None) = (rt2 = None)
              && Term.equivalent_list ps1 ps2
      (*&& Term.equivalent tp1 tp2*) ->
        let nb = 1 + nb in
        begin try
          let pos,poslst,elst,tlst =
            List.fold_left2
              (fun (pos,poslst,elst,tlst) p1 p2 ->
                comp p1 p2 nb pos poslst elst tlst)
              (1+pos,poslst,elst,tlst)
              ps1 ps2 in
          comp t01 t02 nb pos poslst elst tlst
        with Not_found ->
          different t1 t2 pos poslst elst tlst
        end
    | QExp(tps1,fgs1,t01,is_all1),
      QExp(tps2,fgs2,t02,is_all2)
         when Formals.count tps1 = Formals.count tps2 && is_all1 = is_all2
              (*&& let nfgs = Array.length fgtps1 in
                 nfgs = Array.length fgtps2
                 && Term.equivalent_array fgtps1 fgtps2
                 && Term.equivalent_array tps1 tps2*)
      ->
       let n1 = Formals.count tps1 in
        begin try
          comp t01 t02 (n1+nb) (1+pos) poslst elst tlst
        with Not_found ->
          different t1 t2 pos poslst elst tlst
        end
    | Ifexp(cond1,a1,b1), Ifexp(cond2,a2,b2) ->
       begin
         try
           let pos = pos + 1 in
           comp_args pos poslst elst tlst [|cond1;a1;b1|] [|cond2;a1;b2|]
         with Not_found ->
           different t1 t2 pos poslst elst tlst
       end
    | Asexp(insp1,tps1,pat1), Asexp(insp2,tps2,pat2)
         when Term.equivalent_array tps1 tps2
              && Term.equivalent pat1 pat2 ->
       begin
         try
           comp insp1 insp2 nb (1+pos) poslst elst tlst
         with Not_found ->
           different t1 t2 pos poslst elst tlst
       end
    | Inspect(insp1,cases1), Inspect(insp2,cases2)
         when Array.length cases1 = Array.length cases2 ->
       begin
         try
           let pos = pos + 1 in
           interval_fold
             (fun (pos,poslst,elst,tlst) i ->
               let fs1,pat1,res1 = cases1.(i)
               and fs2,pat2,res2 = cases2.(i) in
               if not (Term.equivalent_array
                         (Array2.second fs1)
                         (Array2.second fs2)
                       && Term.equivalent pat1 pat2) then
                 raise Not_found;
               comp res1 res2 (nb+Array2.count fs1) pos poslst elst tlst
             )
             (comp insp1 insp2 nb pos poslst elst tlst)
             0 (Array.length cases1)
         with Not_found ->
           different t1 t2 pos poslst elst tlst
       end
    | _, _ ->
        different t1 t2 pos poslst elst tlst
  in
  let npos, poslst, elst, tlst = comp t1 t2 0 0 [] [] [] in
  let nargs = List.length poslst
  and poslst  = List.rev poslst in
  if nargs = 0 then raise Not_found;
  (* return nextpos, nextvar, poslst, lambda term *)
  let rec mklambda (nextpos:int) (nextvar:int) (poslst:int list) (t:term) (nb:int)
      : int * int * int list * term =
    assert (nextpos < npos);
    let hd,tl =
      match poslst with
        [] -> -1,[]
      | hd::tl -> hd,tl
    in
    let mk_args nextpos nextvar poslst args =
      let nextpos,nextvar,poslst,arglst =
        Array.fold_left
          (fun (nextpos,nextvar,poslst,arglst) arg ->
            let nextpos,nextvar,poslst,arg =
              mklambda nextpos nextvar poslst arg nb in
            nextpos, nextvar, poslst, arg::arglst)
          (nextpos,nextvar,poslst,[])
          args in
      nextpos,nextvar,poslst, Array.of_list (List.rev arglst)
    in
    match t with
      Variable k when k < nb ->
        assert (nextpos <> hd);
        nextpos+1, nextvar, poslst, t
    | Variable k ->
        if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
        else nextpos+1, nextvar, poslst, Variable (k+nargs)
    | VAppl (i,args,ags,oo) ->
        if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
        else
          let nextpos = nextpos + 1 in
          let nextpos,nextvar,poslst,args =
            mk_args nextpos nextvar poslst args in
          nextpos, nextvar, poslst, VAppl(i+nargs,args,ags,oo)
    | Application (f,args,inop) ->
        if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
        else
          let nextpos,nextvar,poslst,f =
            mklambda (nextpos+1) nextvar poslst f nb in
          let nextpos,nextvar,poslst,args =
            mk_args nextpos nextvar poslst args in
          nextpos, nextvar, poslst, Application(f,args,inop)
    | Lam(tps,fgs,pres,t0,rt) ->
        if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
        else
          let nb = 1 + nb in
          let nextpos,nextvar,poslst,pres_rev =
            List.fold_left
              (fun (nextpos,nextvar,poslst,ps) p ->
                let nextpos,nextvar,poslst,p =
                  mklambda nextpos nextvar poslst p nb in
                nextpos, nextvar, poslst, p::ps)
              (1+nextpos,nextvar,poslst,[])
              pres in
          let pres = List.rev pres_rev in
          let nextpos,nextvar,poslst,t0 =
            mklambda nextpos nextvar poslst t0 nb in
          nextpos, nextvar, poslst, Lam(tps,fgs,pres,t0,rt)
    | QExp(tps,fgs,t0,is_all) ->
       let n = Formals.count tps in
       if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
       else
         let nextpos,nextvar,poslst,t0 =
           mklambda (nextpos+1) nextvar poslst t0 (n+nb) in
         nextpos, nextvar, poslst, QExp(tps,fgs,t0,is_all)
    | Ifexp (cond,a,b) ->
        if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
        else
          let nextpos = nextpos + 1 in
          let nextpos,nextvar,poslst,args =
            mk_args nextpos nextvar poslst [|cond;a;b|] in
          nextpos, nextvar, poslst, Ifexp(cond,a,b)
    | Asexp (insp,tps,pat) ->
       assert false (* nyi *)
    | Inspect (insp,cases) ->
       assert false (* nyi *)
    (*| Flow (ctrl,args) ->
        if nextpos = hd then (nextpos+1), (nextvar+1), tl, Variable (nextvar+nb)
        else
          let nextpos = nextpos + 1 in
          let nextpos,nextvar,poslst,args =
            mk_args nextpos nextvar poslst args in
          nextpos, nextvar, poslst, Flow(ctrl,args)*)
    | Indset (nme,tp,rs) ->
        assert false (* nyi *)
  in
  let nextpos, nextvar, poslst, tlam = mklambda 0 0 poslst t1 0 in
  assert (nextvar = nargs);
  assert (poslst = []);
  let tarr = Array.of_list (List.rev tlst)
  and earr = Array.of_list (List.rev elst) in
  let args1, args2 = Myarray.split tarr in
  let equi t1 t2 =
    if Term.equivalent t1 t2 then true
    else begin
      let argsstr args = "[" ^
        (String.concat "," (List.map Term.to_string (Array.to_list args))) ^ "]"
      in
      Printf.printf " tlam  %s\n" (Term.to_string tlam);
      Printf.printf " args1 %s\n" (argsstr args1);
      Printf.printf " args2 %s\n" (argsstr args2);
      Printf.printf " tappl %s\n" (Term.to_string t1);
      Printf.printf " torig %s\n" (Term.to_string t2);
      false
    end
  in
  assert (equi (Term.apply tlam args1) t1);
  assert (equi (Term.apply tlam args2) t2);
  tlam, earr, args1, args2
