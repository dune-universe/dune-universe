use batsat::callbacks::Basic;
use batsat::clause::{lbool, Lit};
use batsat::core::Solver;
use batsat::SolverInterface;

pub fn main() {
    //env_logger::init();
    let mut solver: Solver<Basic> = Solver::new(Default::default(), Default::default());

    let a = Lit::new(solver.new_var_default(), false);
    let b = Lit::new(solver.new_var_default(), false);
    assert!(solver.add_clause_reuse(&mut vec![a, b]));
    assert_eq!(solver.solve_limited(&[!b]), lbool::TRUE);
    assert!(solver.add_clause_reuse(&mut vec![!a, b]));
    assert!(solver.add_clause_reuse(&mut vec![!a, !b]));
    solver.solve_limited(&[]);
    println!("done");
}
