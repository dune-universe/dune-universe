
#include <assert.h>
#include "qdpll.h"

int main()
{
  QDPLL* s = qdpll_create();

  qdpll_configure(s, "--incremental-use");

  Nesting l1 = qdpll_new_scope(s, QDPLL_QTYPE_FORALL);
  qdpll_add(s, 1);
  qdpll_add(s, 2);
  qdpll_add(s, 0);

  Nesting l2 = qdpll_new_scope(s, QDPLL_QTYPE_EXISTS);
  qdpll_add(s, 4);
  qdpll_add(s, 5);
  qdpll_add(s, 0);

  qdpll_add_var_to_scope(s, 3, l1);

  /* clause */
  qdpll_add(s, 1);
  qdpll_add(s, 2);
  qdpll_add(s, 3);
  qdpll_add(s, 0);

  qdpll_add(s, 3);
  qdpll_add(s, 5);
  qdpll_add(s, 0);

  /* push and add other clauses */
  qdpll_push(s);
  qdpll_add(s, 1);
  qdpll_add(s, -2);
  qdpll_add(s, 3);
  qdpll_add(s, 0);

  qdpll_add(s, -3);
  qdpll_add(s, 0);

  QDPLLResult res = qdpll_sat(s);
  assert(res = QDPLL_RESULT_UNSAT);
  qdpll_reset(s);

  /* pop, keeping only the 2 first clauses */
  qdpll_pop(s);
  qdpll_gc(s);


  res = qdpll_sat(s);
  assert(res = QDPLL_RESULT_SAT);

  return 0;
}
