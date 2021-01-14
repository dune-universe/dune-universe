
module D = Depqbf

let mk = Qbf.Lit.make

let add_clause s l =
  List.iter (fun x -> D.add s (mk x)) l;
  D.add0 s

let test_depqbf3 _ =
  let s = D.create () in
  D.configure s "--incremental-use";
  let level1 = D.new_scope s Qbf.Forall in
  D.add s (mk 1);
  D.add s (mk 2);
  D.add0 s;
  let _level2 = D.new_scope s Qbf.Exists in
  D.add s (mk 4);
  D.add s (mk 5);
  D.add0 s;
  D.add_var_to_scope s (mk 3) level1;
  D.gc s;
  add_clause s [1;2;3];
  add_clause s [1;2;5];
  let _l1 = D.push s in
  add_clause s [1;-2;3];
  add_clause s [-3];
  let res = D.check s in
  Format.eprintf "(res: %a)@?" Qbf.pp_result res;
  D.reset s;
  ignore (D.pop s);
  D.gc s;
  let res = D.check s in
  Format.eprintf "(res': %a)@?" Qbf.pp_result res;
  ()
