
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>

#include "string.h"
#include "stdlib.h"
#include "assert.h"
#include "solver.h"


CAMLprim value caml_minisat_new(value unit)
{
  CAMLparam0 ();
  CAMLlocal1 (block);

  solver *s = solver_new();

  // allocate a block to store the pointer
  block = caml_alloc_small(sizeof(s), Abstract_tag);
  *((solver**)(Data_custom_val(block))) = s;

  CAMLreturn (block);
}

// fast access to the solver
static inline solver* get_solver(value block)
{
  solver *s = *((solver**)Data_custom_val(block));
  return s;
}

CAMLprim value caml_minisat_delete(value block)
{
  CAMLparam1 (block);

  // already cleaned?
  if (*((solver**)(Data_custom_val(block)))==0) {
    goto exit;
  }

  solver *s = get_solver(block);
  solver_delete(s);

  // clear block content
  memset(Data_custom_val(block), 0, sizeof(solver*));

exit:
  CAMLreturn (Val_unit);
}

CAMLprim value caml_minisat_simplify(value block)
{
  CAMLparam1 (block);

  solver *s = get_solver(block);
  bool res = solver_simplify(s);

  CAMLreturn (Val_bool(res));
}

// we now directly use the minisat convention!
static inline lit lit_of_int(int i) { return i; }

CAMLprim value caml_minisat_solve(value block, value v_lits)
{
  CAMLparam2 (block, v_lits);

  // build an array out of [v_lits]
  size_t lits_size = Wosize_val(v_lits);

  lit* lits = malloc(lits_size * sizeof(lit));
  assert (lits_size == 0 || lits != NULL);

  for (size_t i = 0; i < lits_size; ++i)
  {
    int lit = lit_of_int(Int_val(Field(v_lits, i)));
    lits[i] = lit;
  }

  // solve
  caml_release_runtime_system();
  solver *s = get_solver(block);
  bool res = solver_solve(s, lits, lits+lits_size);
  caml_acquire_runtime_system();

  free(lits);

  CAMLreturn (Val_bool(res));
}

#define SMALL_CLAUSE_SIZE 16

CAMLprim value caml_minisat_add_clause_a(value block, value v_lits)
{
  CAMLparam2 (block, v_lits);

  // build an array out of [v_lits]
  size_t lits_size = Wosize_val(v_lits);

  lit smallc[SMALL_CLAUSE_SIZE]; // for small clauses

  lit* lits;
  if (lits_size > SMALL_CLAUSE_SIZE) {
    lits = malloc(lits_size * sizeof(lit));
    assert (lits != NULL);
  } else {
    lits = smallc;
  }

  for (size_t i = 0; i < lits_size; ++i)
  {
    int lit = lit_of_int(Int_val(Field(v_lits, i)));
    lits[i] = lit;
  }

  solver *s = get_solver(block);
  bool res = solver_addclause(s, lits, lits+lits_size);

  if (lits_size > SMALL_CLAUSE_SIZE) {
    free(lits);
  }

  CAMLreturn (Val_bool(res));
}

CAMLprim value caml_minisat_value(value block, value v_lit)
{
  CAMLparam1 (block);

  lit lit = lit_of_int(Int_val(v_lit));

  bool sign = !lit_sign(lit);
  int var = lit_var(lit);
  solver *s = get_solver(block);
  int ret;

  // access literal in model, unless it's out-of-bounds
  lbool cur_val = var < s->model.size ? s->model.ptr[var] : l_Undef;

  // put sign back
  if (!sign) { cur_val = -cur_val; }

  /* convert lbool to int */
  if (cur_val == l_Undef)
    ret = 0;
  else if (cur_val == l_True)
    ret = 1;
  else if (cur_val == l_False)
    ret = -1;
  else
    ret = -2;

  CAMLreturn (Val_int(ret));
}


CAMLprim value caml_minisat_set_verbose(value block, value v_lev)
{
  CAMLparam1 (block);

  int lev = Int_val(v_lev);

  solver *s = get_solver(block);
  s->verbosity = lev;

  CAMLreturn (Val_unit);
}

CAMLprim value caml_minisat_nvars(value block)
{
  CAMLparam1 (block);

  solver *s = get_solver(block);
  CAMLreturn (Val_int (solver_nvars(s)));
}

CAMLprim value caml_minisat_nclauses(value block)
{
  CAMLparam1 (block);

  solver *s = get_solver(block);
  CAMLreturn (Val_int (solver_nclauses(s)));
}

CAMLprim value caml_minisat_nconflicts(value block)
{
  CAMLparam1 (block);

  solver *s = get_solver(block);
  CAMLreturn (Val_int (solver_nconflicts(s)));
}

CAMLprim value caml_minisat_set_nvars(value block, value n_var)
{
  CAMLparam2 (block, n_var);

  solver *s = get_solver(block);
  solver_setnvars(s, Int_val(n_var));

  CAMLreturn (Val_unit);
}
