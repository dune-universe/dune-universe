
module D = Depqbf

let mk = Qbf.Lit.make

let test_depqbf1 _ =
  let s = D.create () in
  let level1 = D.new_scope s Qbf.Forall in
  D.add s (mk 1);
  D.add s (mk 2);
  D.add0 s;
  let _level2 = D.new_scope s Qbf.Exists in
  D.add s (mk 4);
  D.add s (mk 5);
  D.add0 s;
  D.add_var_to_scope s (mk 3) level1;
  ()
