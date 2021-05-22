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

struct SolverPtr(*mut Solver);

/// Use a custom block to manipulate the solver
impl ocaml::Custom for SolverPtr {
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
    //eprintln!("delete value");
    let p: *mut SolverPtr = v.custom_mut_ptr_val();
    let solver_ptr = &mut *p;
    if solver_ptr.0.is_null() {
        panic!("double free")
    }

    std::ptr::drop_in_place(solver_ptr.0);
    solver_ptr.0 = std::ptr::null_mut(); // prevent double free
}

impl SolverPtr {
    fn new() -> Self {
        let s = Solver {
            s: InnerSolver::default(),
            vars: Vec::new(),
            cur_clause: vec![],
            assumptions: vec![],
        };
        let b = Box::new(s);
        SolverPtr(Box::into_raw(b))
    }

    /// Access the inner solver.
    #[inline]
    fn get(&mut self) -> &mut Solver {
        assert!(!self.0.is_null());
        unsafe { &mut *self.0 }
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
pub fn ml_batsat_new() -> Pointer<SolverPtr> {
    let r = Pointer::alloc_custom(SolverPtr::new());
    r
}

#[ocaml::func]
pub fn ml_batsat_simplify(mut solver: Pointer<SolverPtr>) -> bool {
    solver.as_mut().get().simplify()
}

/// Add literal, or add clause if the lit is 0
#[ocaml::func]
pub fn ml_batsat_addlit(mut solver: Pointer<SolverPtr>, lit: isize) -> bool {
    let solver = solver.as_mut().get();
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
pub fn ml_batsat_assume(mut solver: Pointer<SolverPtr>, lit: isize) {
    let solver = solver.as_mut().get();
    assert!(lit != 0);
    let lit = solver.get_lit(lit);
    solver.assumptions.push(lit);
}

#[ocaml::func]
pub fn ml_batsat_solve(mut solver: Pointer<SolverPtr>) -> bool {
    // the inner pointer cannot move, even though the ocaml value will
    // once we release the lock
    let solver = solver.as_mut().get();

    ocaml::release_lock();
    let r = {
        let (s, _, assumptions) = solver.decompose();
        let lb = s.solve_limited(&assumptions);
        assumptions.clear(); // reset assumptions
        assert_ne!(lb, lbool::UNDEF); // can't express that in a bool
        lb != lbool::FALSE
    };
    //println!("res: {:?}, model: {:?}", r, solver.get_model());
    ocaml::acquire_lock();
    r
}

#[ocaml::func]
pub fn ml_batsat_value(mut solver: Pointer<SolverPtr>, lit: isize) -> isize {
    let solver = solver.as_mut().get();
    let r = if lit.abs() >= solver.num_vars() as isize {
        lbool::UNDEF
    } else {
        let lit = solver.get_lit(lit);
        solver.s.value_lit(lit)
    };
    r.to_u8() as isize
}

#[ocaml::func]
pub fn ml_batsat_value_lvl_0(mut solver: Pointer<SolverPtr>, lit: isize) -> isize {
    let solver = solver.as_mut().get();
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
pub fn ml_batsat_check_assumption(mut solver: Pointer<SolverPtr>, lit: isize) -> bool {
    let solver = solver.as_mut().get();

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
pub fn ml_batsat_unsat_core(mut solver: Pointer<SolverPtr>) -> Vec<isize> {
    let core = solver
        .as_mut()
        .get()
        .s
        .unsat_core()
        .iter()
        .map(|&lit| int_of_lit(lit))
        .collect::<Vec<_>>();
    core
}

#[ocaml::func]
pub fn ml_batsat_nvars(mut solver: Pointer<SolverPtr>) -> isize {
    let solver = solver.as_mut().get();
    solver.s.num_vars() as isize
}

#[ocaml::func]
pub fn ml_batsat_nclauses(mut solver: Pointer<SolverPtr>) -> isize {
    let solver = solver.as_mut().get();
    solver.s.num_clauses() as isize
}

#[ocaml::func]
pub fn ml_batsat_nconflicts(mut solver: Pointer<SolverPtr>) -> isize {
    let solver = solver.as_mut().get();
    solver.s.num_conflicts() as isize
}

#[ocaml::func]
pub fn ml_batsat_nprops(mut solver: Pointer<SolverPtr>) -> isize {
    let solver = solver.as_mut().get();
    solver.s.num_propagations() as isize
}

#[ocaml::func]
pub fn ml_batsat_ndecisions(mut solver: Pointer<SolverPtr>) -> isize {
    let solver = solver.as_mut().get();
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
pub fn ml_batsat_n_proved(mut solver: Pointer<SolverPtr>) -> isize {
    let solver = solver.as_mut().get();
    solver.s.proved_at_lvl_0().len() as isize
}

#[ocaml::func]
pub fn ml_batsat_get_proved(mut solver: Pointer<SolverPtr>, i: usize) -> isize {
    let solver = solver.as_mut().get();
    let lit = solver.s.proved_at_lvl_0()[i];
    let lit = lit.var().idx() as isize * if lit.sign() { 1 } else { -1 };
    lit
}
