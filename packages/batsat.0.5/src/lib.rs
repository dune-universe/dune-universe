extern crate batsat;
extern crate ocaml;

#[link(name = "batsat")]
use ocaml::{Pointer, Value};
use std::default::Default;

use batsat::{lbool, BasicSolver as InnerSolver, Lit, SolverInterface, Var};
use std::ops;

pub struct Solver {
    s: InnerSolver,
    vars: Vec<Var>, // int->var
    cur_clause: Vec<Lit>,
    assumptions: Vec<Lit>,
}

/// Use a custom block to manipulate the solver
impl ocaml::Custom for Solver {
    const TYPE: ocaml::custom::CustomType = ocaml::custom::CustomType {
        name: "solver",
        fixed_length: None,
        ops: ocaml::custom::CustomOps {
            identifier: b"solver\0".as_ptr() as *const ocaml::sys::Char,
            finalize: Some(delete_solver_value),
            compare_ext: None,
            compare: None,
            hash: None,
            serialize: None,
            deserialize: None,
            fixed_length: std::ptr::null(),
        },
    };
    const USED: usize = 1;
    const MAX: usize = 1000;
}

unsafe extern "C" fn delete_solver_value(v: Value) {
    eprintln!("delete value");
    std::ptr::drop_in_place(v.custom_mut_ptr_val::<Solver>())
}

impl Solver {
    fn new() -> Self {
        Solver {
            s: InnerSolver::default(),
            vars: Vec::new(),
            cur_clause: vec![],
            assumptions: vec![],
        }
    }
}

impl Solver {
    fn decompose(&mut self) -> (&mut InnerSolver, &mut Vec<Lit>, &mut Vec<Lit>) {
        (&mut self.s, &mut self.cur_clause, &mut self.assumptions)
    }

    /// Allocate variables until we get the one corresponding to `x`
    fn get_var(&mut self, x: usize) -> Var {
        while x >= self.vars.len() {
            self.vars.push(self.s.new_var_default());
        }
        self.vars[x]
    }

    #[inline]
    fn get_lit(&mut self, lit: isize) -> Lit {
        assert!(lit != 0);
        let v = self.get_var(lit.abs() as usize);
        Lit::new(v, lit > 0)
    }
}

impl ops::Deref for Solver {
    type Target = InnerSolver;
    fn deref(&self) -> &Self::Target {
        &self.s
    }
}

impl ops::DerefMut for Solver {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.s
    }
}

#[ocaml::func]
pub fn ml_batsat_new() -> Pointer<Solver> {
    let r = Pointer::alloc_custom(Solver::new());
    r
}

#[ocaml::func]
pub fn ml_batsat_simplify(mut solver: Pointer<Solver>) -> bool {
    solver.as_mut().simplify()
}

/// Add literal, or add clause if the lit is 0
#[ocaml::func]
pub fn ml_batsat_addlit(mut solver: Pointer<Solver>, lit: isize) -> bool {
    let solver = solver.as_mut();
    let mut r = true;
    if lit == 0 {
        // push current clause into vector `clauses`, reset it
        //println!("add-lit {:?}", 0);
        let (solver, cur, _) = solver.decompose();
        r = solver.add_clause_reuse(cur);
        cur.clear();
    } else {
        // push literal into clause
        let lit = solver.get_lit(lit);
        //println!("add-lit {:?}", lit);
        solver.cur_clause.push(lit);
    }
    r
}

/// Add assumption into the solver
#[ocaml::func]
pub fn ml_batsat_assume(mut solver: Pointer<Solver>, lit: isize) {
    let solver = solver.as_mut();
    assert!(lit != 0);
    let lit = solver.get_lit(lit);
    solver.assumptions.push(lit);
}

#[ocaml::func]
pub fn ml_batsat_solve(mut solver: Pointer<Solver>) -> bool {
    let solver = solver.as_mut();
    let r = {
        let (s, _, assumptions) = solver.decompose();
        let lb = s.solve_limited(&assumptions);
        assumptions.clear(); // reset assumptions
        assert_ne!(lb, lbool::UNDEF); // can't express that in a bool
        lb != lbool::FALSE
    };
    //println!("res: {:?}, model: {:?}", r, solver.get_model());
    r
}

#[ocaml::func]
pub fn ml_batsat_value(mut solver: Pointer<Solver>, lit: isize) -> isize {
    let solver = solver.as_mut();
    let r = if lit.abs() >= solver.num_vars() as isize {
        lbool::UNDEF
    } else {
        let lit = solver.get_lit(lit);
        solver.s.value_lit(lit)
    };
    r.to_u8() as isize
}

#[ocaml::func]
pub fn ml_batsat_value_lvl_0(mut solver: Pointer<Solver>, lit: isize) -> isize {
    let solver = solver.as_mut();
    let r = if lit.abs() >= solver.num_vars() as isize {
        lbool::UNDEF
    } else {
        let lit = solver.get_lit(lit);
        solver.s.value_lvl_0(lit)
    };
    //println!("val for {:?}: {:?}", lit, r);
    r.to_u8() as isize
}

#[ocaml::func]
pub fn ml_batsat_check_assumption(mut solver: Pointer<Solver>, lit: isize) -> bool {
    let solver = solver.as_mut();

    // check unsat-core
    let lit = solver.get_lit(lit);
    solver.s.unsat_core_contains_var(lit.var())
}

/// Convert a literal into a signed integer for the OCaml frontend
#[inline]
fn int_of_lit(lit: Lit) -> isize {
    lit.var().idx() as isize * if lit.sign() { 1 } else { -1 }
}

#[ocaml::func]
pub fn ml_batsat_unsat_core(solver: Pointer<Solver>) -> Vec<isize> {
    let solver = solver.as_ref();
    let core = solver
        .s
        .unsat_core()
        .iter()
        .map(|&lit| int_of_lit(lit))
        .collect::<Vec<_>>();
    core
}

#[ocaml::func]
pub fn ml_batsat_nvars(solver: Pointer<Solver>) -> isize {
    let solver = solver.as_ref();
    solver.s.num_vars() as isize
}

#[ocaml::func]
pub fn ml_batsat_nclauses(solver: Pointer<Solver>) -> isize {
    let solver = solver.as_ref();
    solver.s.num_clauses() as isize
}

#[ocaml::func]
pub fn ml_batsat_nconflicts(solver: Pointer<Solver>) -> isize {
    let solver = solver.as_ref();
    solver.s.num_conflicts() as isize
}

#[ocaml::func]
pub fn ml_batsat_nprops(solver: Pointer<Solver>) -> isize {
    let solver = solver.as_ref();
    solver.s.num_propagations() as isize
}

#[ocaml::func]
pub fn ml_batsat_ndecisions(solver: Pointer<Solver>) -> isize {
    let solver = solver.as_ref();
    solver.s.num_decisions() as isize
}

/*
caml!(ml_batsat_nrestarts, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_restarts();
        res = Value::isize(r as isize);
    })
} -> res);
*/

#[ocaml::func]
pub fn ml_batsat_n_proved(solver: Pointer<Solver>) -> isize {
    let solver = solver.as_ref();
    solver.s.proved_at_lvl_0().len() as isize
}

#[ocaml::func]
pub fn ml_batsat_get_proved(solver: Pointer<Solver>, i: usize) -> isize {
    let solver = solver.as_ref();
    let lit = solver.s.proved_at_lvl_0()[i];
    let lit = lit.var().idx() as isize * if lit.sign() { 1 } else { -1 };
    lit
}
