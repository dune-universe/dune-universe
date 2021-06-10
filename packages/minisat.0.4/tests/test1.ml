
print_endline "test1...";;

let s = Minisat.create();;
let l1 = Minisat.Lit.make 1;;
let l2 = Minisat.Lit.make 2;;
let l3 = Minisat.Lit.make 3;;
l1;;
Minisat.Lit.neg l1;;
Minisat.Lit.neg l2;;
l1, Minisat.Lit.neg l1, l2, Minisat.Lit.neg l2;;
Minisat.add_clause_l s [l1; Minisat.Lit.neg l2];;
Minisat.add_clause_l s [Minisat.Lit.neg l1; l2];;
Minisat.add_clause_l s [Minisat.Lit.neg l1; Minisat.Lit.neg l3];;
Minisat.add_clause_l s [l1; Minisat.Lit.neg l3];;
Minisat.solve s;;
Minisat.value s l1;;
Minisat.value s l2;;
print_endline "should succeed...";;
Minisat.solve s;; (* should not fail *)
print_endline "ok!";;
print_endline "should fail...";;
try Minisat.solve ~assumptions:[|l3|] s; assert false
with Minisat.Unsat -> print_endline "ok!";; (* should fail *)
print_endline "should succeed...";;
Minisat.solve s;; (* should not fail *)
l3, Minisat.value s l3, Minisat.value s (Minisat.Lit.neg l3);;
assert (Minisat.value s l3 = Minisat.V_false);;
assert (Minisat.value s (Minisat.Lit.neg l3) = Minisat.V_true);;
print_endline "ok!";;
print_endline "should succeed...";;
let l2000 = Minisat.Lit.make 2000 ;;
assert (Minisat.value s l2000 = Minisat.V_undef);;
assert (Minisat.value s (Minisat.Lit.neg l2000) = Minisat.V_undef);;
print_endline "ok!";;
