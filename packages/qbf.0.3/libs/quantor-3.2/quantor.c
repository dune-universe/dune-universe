/*
Copyright (c) 2003 - 2014 Armin Biere, ETH Zurich, JKU Linz.

All rights reserved. Redistribution and use in source and binary forms, with
or without modification, are permitted provided that the following
conditions are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

  3. All advertising materials mentioning features or use of this software
     must display the following acknowledgement:

	  This product includes software developed by 
	  Armin Biere, Johannes Kepler University, Linz, Austria.

  4. Neither the name of the University nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.
   
THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#define QUANTOR_COPYRIGHT \
"Copyright (c) 2003 - 2014 Armin Biere, ETH Zurich, JKU Linz."

#define QUANTOR_ID \
"$<still-need-to-extract-git-id>$"

/*------------------------------------------------------------------------*/

#define NSTRENGTHEN /* STILL BUGGY */

/*------------------------------------------------------------------------*/

#include "config.h"
#include "quantor.h"

/*------------------------------------------------------------------------*/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <unistd.h>
#ifndef __MINGW32__
#include <sys/resource.h>
#endif

/*------------------------------------------------------------------------*/

#define QUANTOR_NOT(l) ((Lit*)(sizeof(Lit) ^ (Word)(l)))
#define QUANTOR_SIGN(l) ((sizeof(Lit) & (Word) (l)) != 0)

/*------------------------------------------------------------------------*/

#define QUANTOR_UNASSIGNED ((Lit*)~0)
#define QUANTOR_FALSE ((Lit*)0)
#define QUANTOR_TRUE QUANTOR_NOT(QUANTOR_FALSE)

/*------------------------------------------------------------------------*/

#define QUANTOR_LOG_MIN_CHUNK_SIZE 16
#define QUANTOR_MIN_CHUNK_SIZE (1 << QUANTOR_LOG_MIN_CHUNK_SIZE)

/*------------------------------------------------------------------------*/

#define QUANTOR_UNDEFINED_SCORE (INT_MAX-0)
#define QUANTOR_STICKY_SCORE (INT_MAX-1)
#define QUANTOR_ASSIGNED_SCORE (INT_MAX-2)
#define QUANTOR_NONREPRESENTATIVE_SCORE (INT_MAX-3)

#define QUANTOR_OVERFLOW (INT_MAX-5)	/* KEEP IT EVEN */

/*------------------------------------------------------------------------*/

#define QUANTOR_DEFAULT_SCOPE_NESTING INT_MAX

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_STATS1
#define INCSTATS1(s) do { (s) += 1; } while(0)
#else
#define INCSTATS1(s) do { } while(0)
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_STATS2
#define INCSTATS2(s) do { (s) += 1; } while(0)
#else
#define INCSTATS2(s) do { } while(0)
#endif
/*------------------------------------------------------------------------*/

extern char **environ;

/*------------------------------------------------------------------------*/

typedef struct BinDB BinDB;
typedef struct Cell Cell;
typedef struct Clause Clause;
typedef struct EquivalenceClass EquivalenceClass;
typedef struct IntStack IntStack;
typedef struct IO IO;
typedef struct Lit Lit;
typedef struct LitIt LitIt;
typedef struct LitItAPI LitItAPI;
typedef struct AllClausesLitIt AllClausesLitIt;
typedef struct IntStackLitIt IntStackLitIt;
typedef struct AssignedLitIt AssignedLitIt;
typedef struct Opts Opts;
typedef struct Parser Parser;
typedef struct PtrStack PtrStack;
typedef struct RHS RHS;
typedef struct RHSDB RHSDB;
typedef struct Function Function;
typedef struct Scope Scope;
typedef struct Stats Stats;
typedef struct CacheStats CacheStats;
typedef struct CountStats CountStats;
typedef struct Var Var;
typedef struct SatSolver SatSolver;
typedef struct SatSolverAPI SatSolverAPI;
typedef struct IOpt IOpt;
typedef struct DOpt DOpt;
typedef union OptData OptData;
typedef struct Opt Opt;

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_FAST_ALLOC
/*------------------------------------------------------------------------*/

typedef struct Chunk Chunk;
typedef struct ChunkHeader ChunkHeader;
typedef union ChunkData ChunkData;

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

struct IntStack
{
  int *start, *top, *end;
};

/*------------------------------------------------------------------------*/

struct PtrStack
{
  void **start, **top, **end;
};

/*------------------------------------------------------------------------*/

enum InternalQuantificationType
{
  QUANTOR_UNDEFINED_TYPE = 0,
  QUANTOR_EXISTENTIAL = 1,
  QUANTOR_UNIVERSAL = 2,
};

typedef enum InternalQuantificationType InternalQuantificationType;

/*------------------------------------------------------------------------*/

enum ClauseProcessingType
{
  QUANTOR_NO_PROCESSING = 0,
  QUANTOR_FORWARD_PROCESSING = 1,
  QUANTOR_BACKWARD_PROCESSING = 2,
};

typedef enum ClauseProcessingType ClauseProcessingType;

/*------------------------------------------------------------------------*/

#define DeclareDoublyLinkedList(Name,Type) \
 \
typedef struct Name ## Link Name ## Link; \
typedef struct Name ## Anchor Name ## Anchor; \
 \
struct Name ## Link \
{ \
  Type * prev; \
  Type * next; \
}; \
 \
struct Name ## Anchor \
{ \
  Type * first; \
  Type * last; \
  unsigned len; \
}

/*------------------------------------------------------------------------*/

DeclareDoublyLinkedList (Generic, void *);
DeclareDoublyLinkedList (Var, Var);
DeclareDoublyLinkedList (Cell, Cell);
DeclareDoublyLinkedList (Scope, Scope);
DeclareDoublyLinkedList (Clause, Clause);
DeclareDoublyLinkedList (Lit, Lit);
DeclareDoublyLinkedList (EquivalenceClass, EquivalenceClass);
DeclareDoublyLinkedList (Function, Function);

/*------------------------------------------------------------------------*/

enum VarState
{
  QUANTOR_ALLOCATED_VAR = 0,
  QUANTOR_NEW_VAR = 1,
  QUANTOR_UNATE_VAR = 2,
  QUANTOR_UNIT_VAR = 3,
  QUANTOR_REGULAR_VAR = 4,
  QUANTOR_SAVED_VAR = 5,
  QUANTOR_DYING_VAR = 6,
  QUANTOR_GARBAGE_VAR = 7,
};

typedef enum VarState VarState;

/*------------------------------------------------------------------------*/

struct Var
{
  int idx;
  unsigned exported:1;		/* whether to export this variable */
  unsigned eliminated:1;	/* through 'forall' or 'exists' */
  unsigned zombie:1;		/* whether this is a zombie */
  unsigned dead:1;		/* for debugging purposes only */
  unsigned to_be_mapped:1;	/* ... forall */
  int mark;			/* for simplifying clauses */
  int score;			/* rel. est. cost if eliminated */
  int rank;			/* rank in 'order' sorted by 'cost' */
  Scope *scope;
  Lit *lits;			/* positive and negative, actually Lit[2] */
  VarLink unit_link;		/* Quantor.units */
  VarLink unate_link;		/* Quantor.unates */
  VarLink proposed_unate_link;	/* Quantor.proposed_unates */
  VarLink zombie_link;		/* Quantor.zombies */
  VarLink proposed_zombie_link;	/* Quantor.proposed_zombies */
  VarLink scope_link;		/* scope->vars */
  VarLink reorder_link;		/* scope->reorder */

  Lit *assignment;		/* UNASSIGNED, FALSE, TRUE, or other lit */
  Lit *unit_assignment;		/* registered unit assignment */
  Lit *unate_assignment;	/* registered unate assignment */

  Var *mapped;			/* copy for 'forall' */

  /* The equivalence class to which this variable belongs.  In principle,
   * equivalences are among literals, which in turn induces equivalences
   * among variables.  Equivalent variables are linked together through
   * 'eqclass_link'.  To lift variable equivalences back to literal
   * equivalences the sign bit 'eqclass_sign' is used.
   */
  EquivalenceClass *eqclass;
  unsigned eqclass_sign:1;
  VarLink eqclass_link;		/* eqclass->elems */

  FunctionAnchor functions;	/* with this variable on the LHS */

  /* This field caches the cheapest function to substitute while updating
   * the score of a variable.
   */
  Function *cheapest_function_to_substitute;
};

/*------------------------------------------------------------------------*/
/* Since literals can be negated, they always come in pairs and are
 * allocated in such a way that their address is aligned to their size,
 * allowing to switch within in the pair flipping one bit.
 */
struct Lit
{
  Var *var;
  CellAnchor column;
  CellAnchor binary_clauses;
  LitLink unprocessed_link;

  unsigned mark;

  unsigned hash;		/* cached hash value */
  unsigned valid_hash:1;

  unsigned valid_ldsig:1;
  unsigned ldsig:6;		/* 0 <= ... < 8*sizeof(Signature) < 64 */

  Signature sigsum;		/* either 8, 16, 32, or 64 bits */

#ifdef QUANTOR_SIGREF
  unsigned char *sigref;
#endif
  /* This is the sum of the size of clauses in 'column'.
   */
  unsigned sum;

#ifndef QUANTOR_FAST_ALLOC
  int delete_offset;
#endif

#ifndef QUANTOR_LIT_ALIGNMENT_IS_ZERO
  /* Align 'sizeof(Lit)' to be a power of two.  This is determined by the
   * configure script.
   */
  char alignment[QUANTOR_LIT_ALIGNMENT];
#endif
};

/*------------------------------------------------------------------------*/
/* The clauses are stored in a large sparse matrix.  A matrix cell contains
 * one negated or unnegated occurrence of a var, called a literal.
 * The horizontal links are called 'row_links' and chain together the lits
 * that occur in one particular clause.  The vertical links are called
 * 'column_links' and chain together the occurrences of one var in a
 * certain polarity.  The horizontal links have their anchor in 'clause',
 * the vertical links in 'var'.
 */
struct Cell
{
  Lit *lit;
  Clause *clause;
  CellLink column_link;
  CellLink binary_clauses_link;
};

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_FAST_ALLOC
/*------------------------------------------------------------------------*/

struct ChunkHeader
{
  Chunk *next;
  size_t bytes;
  size_t size;
  int nelems;
};

/*------------------------------------------------------------------------*/

union ChunkData
{
  void *as_pointer;
  double as_double;
  Word as_word;
};

/*------------------------------------------------------------------------*/

struct Chunk
{
  ChunkHeader header;
  ChunkData data;
};

#define CHUNK_DATA_OFFSET ((long)&((Chunk*)0)->data)

/*------------------------------------------------------------------------*/
#endif /* QUANTOR_FAST_ALLOC */
/*------------------------------------------------------------------------*/

enum ClauseState
{
  QUANTOR_ALLOCATED_CLAUSE = 0,
  QUANTOR_NEW_CLAUSE = 1,
  QUANTOR_ACTIVE_CLAUSE = 2,
  QUANTOR_DYING_CLAUSE = 3,
  QUANTOR_GARBAGE_CLAUSE = 4,
  QUANTOR_SAVED_CLAUSE = 5,
};

typedef enum ClauseState ClauseState;

/*------------------------------------------------------------------------*/
/*  This is the life cycle of a clause.
 *
 * allocate             new      activate
 * -------> allocated -----> new ---------.
 *                            |           |      dump
 *                       kill |           |    ,----> (garbage)
 *                            |           |   /
 *  collect           dump    v    kill   v  /
 * <------- garbage <------ dying <---- active <------.
 *             ^                          |           |
 *             |                     save |           |
 *             |                          |           |
 *             |            dump          v   restore |
 *             `----------------------- saved --------'
 *
 * All states except 'active' are called 'inactive'.  The non-determinism at
 * 'active' is resolved by checking whether backtracking is necessary.
 */
struct Clause
{
  unsigned idx;
  unsigned size;
  int mark;
  unsigned original:1;
  unsigned to_be_mapped:1;
  unsigned part_of_substituted_function:1;
  ClauseProcessingType processing_type;

  Signature sig;

  ClauseState state;
  Scope *scope;

  Clause *next_in_bindb;

  ClauseLink scope_link;
  ClauseLink dying_link;
  ClauseLink marked_link;
  ClauseLink unprocessed_clauses_link;
  ClauseLink link;

  FunctionAnchor functions;	/* for which this is a 'base clause' */

  Cell row[1];
};

/*------------------------------------------------------------------------*/

struct BinDB
{
  Clause **table;
  unsigned size, count;
};

/*------------------------------------------------------------------------*/

struct EquivalenceClass
{
  VarAnchor elems;
  Var *representative;
  EquivalenceClassLink link;
  EquivalenceClassLink unprocessed_link;
  EquivalenceClassLink dying_link;
  unsigned num_zombies;
  unsigned num_eliminated;
  unsigned processing:1;
#ifdef QUANTOR_LOG1
  unsigned idx;
#endif
};

/*------------------------------------------------------------------------*/

struct RHS
{
  RHS *next;			/* collision chain */
  unsigned size;
  FunctionAnchor functions;
#ifdef QUANTOR_LOG1
  unsigned idx;
#endif
  Lit *literals[1];
};

/*------------------------------------------------------------------------*/

struct RHSDB
{
  RHS **table;
  unsigned size, count;
};

/*------------------------------------------------------------------------*/

enum FunctionType
{
  QUANTOR_OR_GATE,
  QUANTOR_ITE_GATE,
  QUANTOR_XOR_GATE
};

typedef enum FunctionType FunctionType;

/*------------------------------------------------------------------------*/

struct Function
{
  FunctionType type;

  Lit *lhs;
  FunctionLink lhs_link;	/* Var.functions */

  /* Since the same RHS can have multiple LHS we link all functions with the
   * same RHS together.  As soon this list becomes empty the RHS can be
   * deleted.
   */
  RHS *rhs;
  FunctionLink rhs_link;	/* RHS.functions */

  /* This 'clause' is the 'base clause' from which this function was
   * extracted.  For instance given the clauses
   *   
   *   -1 2 -3 4
   *   1 -2
   *   -2 3
   *   -2 -4
   *
   * from which the function
   *   
   *   -2 = (-1 | -3 | 4)      (equivalent to  2 = (1 & 3 & -4)  )
   *
   * can be extracted. The base clause in this is example is '-1 2 -3 4'.
   *
   * We assume that functions only exists if equivalence reasoning is
   * enabled.  Thus the size of 'clause' is always larger than 2.  Killing
   * this clause will kill this function.  Since the same clause can be the
   * base clause for multiple functions, all these functions are linked
   * together.
   *
   * For XOR functions the 'base clause' is an arbitrary clause.
   */
  Clause *clause;
  FunctionLink clause_link;	/* Clause.functions */

  FunctionLink link;		/* Quantor.functions */
  FunctionLink dying_link;	/* Quantor.dying_functions */

#ifdef QUANTOR_LOG1
  unsigned idx;
  unsigned type_idx;
#endif
};

/*------------------------------------------------------------------------*/

struct Scope
{
  int nesting;
  InternalQuantificationType type;

  VarAnchor vars;
  VarAnchor reorder;

  ScopeLink link;
  PtrStack order;

  unsigned sum;
  ClauseAnchor clauses;
};

/*------------------------------------------------------------------------*/

struct Parser
{
  const char *err;
  unsigned lineno;
  int look_ahead;
  int parsing;
};

/*------------------------------------------------------------------------*/

struct Opts
{
  int check;
  int verbose;

  /* As long there are variables which, if eliminated by resolution
   * ('exists'), add less than 'hard_exists_limit' literals, we eliminate
   * them. See also the comment to 'resolve_exported'.  
   */
  int hard_exists_limit;

  /* In addition to the hard limit described above there is a soft limit
   * which depends on the average of the number of literals added by the
   * last 'soft_exists_length' eliminations of existential variables and the
   * expected number of literals added by the proposed to eliminate variable.
   * Number of eliminated literals eliminated are 'literals_added_by_exists'.
   */
  int soft_exists_limit;
  int soft_exists_length;
  int *literals_added_by_exists;

  /* This flag controls whether we allow elimination of exported variables
   * by resolution.  If this is enabled, the satisfying assignment may be
   * be a partial assignment.  For propositional formulas, we can
   * automatically lift the assignment to a total assignment.  For real QBF
   * formulas this does not work yet.
   */
  int resolve_exported;

  int equivalences;
  int functions;
  int function_resolution;

  int hyper1res;

  int forall;
  int exists;
  int trivial_truth;
  int trivial_falsity;
  int forward_subsume;
  int forward_subsume_instead_recalc;
  int backward_subsume;
  int strengthen;
  int binstrengthen;
  int forward_strengthen;
  int forward_processing;
  int backward_processing;
  int sigref;
  int only_early_subnew;
  int recycle_indices;
  int reduce_scope;
  int log_clauses_of_scopes;
  int recalc_sigs;
  double literals_per_clause_limit;
  double literals_per_clause_factor;
  double forall_bias;
  double time_limit;		/* no limit if negative */
  double space_limit;		/* no limit if negative */

  PtrStack setup;
};

/*------------------------------------------------------------------------*/

struct IOpt
{
  int def;			/* default value */
  int *ptr;
};

/*------------------------------------------------------------------------*/

struct DOpt
{
  double def;			/* default value */
  double *ptr;
};

/*------------------------------------------------------------------------*/

union OptData
{
  IOpt as_iopt;
  DOpt as_dopt;
};

/*------------------------------------------------------------------------*/

struct Opt
{
  char *env;
  char *opt;
  int is_iopt;
  OptData data;
};

/*------------------------------------------------------------------------*/

struct CountStats
{
  double new;
  double gc;
  unsigned num;
  unsigned max;
};

/*------------------------------------------------------------------------*/

struct CacheStats
{
  double checks;
  double hits;
  double spurious;		/* ... misses = (checks - hits) */
};

/*------------------------------------------------------------------------*/

struct Stats
{
  int specified_vars;		/* the <vars> in 'p cnf <vars> <cls>' */
  int specified_clauses;	/* the <cls> in 'p cnf <vars> <cls>' */

  int external_clauses;
  int external_scopes;
  int external_vars;
  int external_lits;

#ifdef QUANTOR_STATS1
  double exists;
#ifdef QUANTOR_STATS2
  double smaller_exists_limit;
  double exists_function_resolution;
#endif
#endif

#ifdef QUANTOR_STATS1
  double foralls;
#ifdef QUANTOR_STATS2
  double literals_per_clause_limit_exceeded;
#endif
#endif

#ifdef QUANTOR_STATS1
  double units;
#ifdef QUANTOR_STATS2
  unsigned hyper1res_units;
#endif
  double unates;
  double bcps;
#ifdef QUANTOR_STATS2
  double bcp_rounds;
#endif
#endif

#ifdef QUANTOR_STATS2
  CacheStats hyper1res;
  CacheStats hyper1respivot;
  double reordered;
  double swaps;
  double cmps;
  double ups;
  double downs;
#endif

#ifdef QUANTOR_STATS1
  double subsumed;
  double forward_subsumed;
  double backward_subsumed;
#ifdef QUANTOR_STATS2
  CacheStats subsume;
#endif
#endif

#ifndef NSTRENGTHEN
#ifdef QUANTOR_STATS1
  double strengthend;
  double forward_strengthend;
  double backward_strengthend;
#ifdef QUANTOR_STATS2
  CacheStats strengthen;
#endif
#endif
#endif

#ifdef QUANTOR_STATS1
  CountStats functions;
#ifdef QUANTOR_STATS2
  double args_functions;
#endif
#endif

#ifdef QUANTOR_STATS1
  CountStats or;
#ifdef QUANTOR_STATS2
  CacheStats or_lhs;
  CacheStats or_extractions;
#endif
#endif

#ifdef QUANTOR_STATS2
  double processed;
  double forward_processed;
  double backward_processed;
#endif

#ifdef QUANTOR_STATS2
  double substitutions;
  double self_subsuming_resolution;
  double trivial_truth;
  double trivial_falsity;
#endif

#ifdef QUANTOR_STATS2
  int sat_solver;
#endif

#ifdef QUANTOR_STATS1
  CountStats vars;
#endif
#ifdef QUANTOR_STATS2
  double recycled_vars;
#endif
  unsigned num_exported;
  unsigned num_unassigned_exported;
  unsigned num_assigned;

#ifdef QUANTOR_STATS1
  CountStats clauses;
#ifdef QUANTOR_STATS2
  double recycled_clauses;
  double trivial_clauses;
  double already_exists;
  CountStats binary_clauses;
#endif
#endif
  unsigned num_active_clauses;

#ifdef QUANTOR_STATS1
  CountStats cells;
#ifdef QUANTOR_STATS2
  double duplicated_lits;
  double forall_reduced_lits;
  double binary_self_subsuming_resolution;
  double self_subsuming_resolution_lits;
  double false_lits;
#endif
#endif
  int eliminated_cells;
#ifdef QUANTOR_STATS1
  CountStats rhs;
  CountStats eqclasses;
  double equivalences;
#ifdef QUANTOR_STATS2
  double equivalences_from_functions;
#endif
#endif
#ifdef QUANTOR_STATS2
  double num_recalc_sigs;
#ifdef QUANTOR_SIGREF
  double sticky_sigref;
#endif
#endif
  size_t bytes, max_bytes;
#ifdef QUANTOR_STATS1
  size_t sat_solver_bytes;
#endif
#ifdef QUANTOR_STATS2
  unsigned num_chunks;
#endif
  double time;
#ifdef QUANTOR_STATS1
  double sat_solver_time;
#ifdef QUANTOR_STATS2
  double sat_remaining_time;
  double sat_trivial_truth_time;
  double sat_trivial_falsity_time;
  double sat_original_time;
#endif
#endif
};

/*------------------------------------------------------------------------*/

struct IO
{
  FILE *in;
  int close_in;
  int pclose_in;
  const char *in_name;

  FILE *out;
  const char *out_name;
  int close_out;
};

/*------------------------------------------------------------------------*/

struct Quantor
{
  int max_idx;			/* of external vars only */
  int nesting;

  PtrStack vars;
  IntStack free_var_indices;
  int no_more_external_vars;
  VarAnchor units;
  VarAnchor unates;
  VarAnchor proposed_unates;
  VarAnchor zombies;
  VarAnchor proposed_zombies;
  PtrStack marked_vars;

  EquivalenceClassAnchor equivalence_classes;
  EquivalenceClassAnchor unprocessed_equivalence_classes;
  EquivalenceClassAnchor dying_equivalence_classes;

  ScopeAnchor scopes;
  PtrStack new_scope;
  int max_external_nesting;

  ClauseAnchor clauses;
  PtrStack idx2clause;
  int invalid;
  IntStack free_clause_indices;
  PtrStack new_clause;
  ClauseAnchor dying_clauses;
  ClauseAnchor marked_clauses;
  IntStack dead_original_clauses;
  int save_dead_original_clauses;
  int original_problem_is_propositional;
  ClauseAnchor unprocessed_clauses;
  LitAnchor unprocessed_literals;

  BinDB *bindb;

  RHSDB *rhsdb;
  PtrStack new_rhs;
  FunctionAnchor functions;
  FunctionAnchor dying_functions;

  SatSolverAPI *sat_api;

  IO io;
  char cost_buffer[20];
  char scope_buffer[20];
  char assignment_buffer[20];
  IntStack external_literals;
  IntStack external_scope;
  InternalQuantificationType external_type;
  Parser parser;
  IntStack external_assignment;

  Lit *free_literal_pairs;
  Var *free_vars;
#ifdef QUANTOR_FAST_ALLOC
  Chunk *allocated_chunks;
#endif
  char *prefix;

  Opts opts;
  Stats stats;

#ifndef QUANTOR_QBF_EVALUATION_FORMAT
  int line_buffer_len;
  char line_buffer[80];		/* 80 - |'v'| + |'\0'| */
  char num_buffer[20];
#endif

  PtrStack environment;

  unsigned recalc_sigs_count_down;
#ifndef NDEBUG
  Var *entered_resolve_lit;
  Var *entered_forall;
#endif
};

/*------------------------------------------------------------------------*/

struct SatSolverAPI
{
  const char *name;
  SatSolver *(*new) (Quantor *);
  void (*delete) (SatSolver *);
  int (*add) (SatSolver *, int);
    QuantorResult (*run) (SatSolver *);
  void (*assignment) (SatSolver *);
};

/*------------------------------------------------------------------------*/

struct SatSolver
{
  SatSolverAPI *api;
  Quantor *quantor;
  int max_pidx;
  int assignment_initialized;
  PtrStack assignment;
  IntStack idx2pidx;
};

/*------------------------------------------------------------------------*/

struct LitItAPI
{
  int (*done) (LitIt *, int *next_int_lit_ptr);
  void (*delete) (LitIt *);
};

/*------------------------------------------------------------------------*/

struct LitIt
{
  LitItAPI *api;
};

/*------------------------------------------------------------------------*/

struct AllClausesLitIt
{
  LitIt super;
  Quantor *quantor;
  int existential_literals_only;
  Clause *current_clause;
  Cell *current_cell;
};

/*------------------------------------------------------------------------*/

struct AssignedLitIt
{
  LitIt super;
  Quantor *quantor;
  IntStack clause;		/* in reverse order */
  int idx;
};

/*------------------------------------------------------------------------*/

struct IntStackLitIt
{
  LitIt super;
  Quantor *quantor;
  IntStack *stack;
  int *current;
};

/*------------------------------------------------------------------------*/

static double
get_time (void)
{
#ifndef __MINGW32__
  struct rusage u;
#endif
  double res;

  res = 0;
#ifndef __MINGW32__
  if (!getrusage (RUSAGE_SELF, &u))
    {
      res += u.ru_utime.tv_sec + 1e-6 * u.ru_utime.tv_usec;
      res += u.ru_stime.tv_sec + 1e-6 * u.ru_stime.tv_usec;
    }
#endif

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_STATS1
/*------------------------------------------------------------------------*/

static void
inc_count_stats (CountStats * stats)
{
  stats->new += 1;
  stats->num++;
  if (stats->num > stats->max)
    stats->max = stats->num;
}

/*------------------------------------------------------------------------*/

static void
dec_count_stats (CountStats * stats)
{
  assert (stats->num > 0);
  stats->num--;
  stats->gc += 1;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void *
new (Quantor * quantor, size_t size)
{
  size_t real_size, *res;

  assert (size);

  real_size = size;
#ifndef NDEBUG
  real_size += sizeof (size_t);
#endif
  res = malloc (real_size);
  assert (res);
#ifndef NDEBUG
  *res++ = size;
#endif
  quantor->stats.bytes += size;
  memset (res, 0, size);

  if (quantor->stats.bytes > quantor->stats.max_bytes)
    quantor->stats.max_bytes = quantor->stats.bytes;

  return res;
}

/*------------------------------------------------------------------------*/

static void
delete (Quantor * quantor, void *ptr, size_t size)
{
#ifndef NDEBUG
  size_t real_size;
#endif
  size_t *real_ptr;

  if (ptr)
    {
      real_ptr = ptr;
#ifndef NDEBUG
      real_size = size + sizeof (size_t);
      real_ptr--;
      assert (*real_ptr == size);
#endif
      assert (quantor->stats.bytes >= size);
      quantor->stats.bytes -= size;
#ifndef NDEBUG
      memset (real_ptr, 0, real_size);
#endif
      free (real_ptr);
    }
  else
    assert (!size);
}

/*------------------------------------------------------------------------*/

static char *
copy_string (Quantor * quantor, const char *str)
{
  char *res;

  res = new (quantor, strlen (str) + 1);
  strcpy (res, str);

  return res;
}


/*------------------------------------------------------------------------*/

static void
delete_string (Quantor * quantor, char *str)
{
  if (str)
    delete (quantor, str, strlen (str) + 1);
}

/*------------------------------------------------------------------------*/

static void *
enlarge (Quantor * quantor, void *ptr, size_t old_size, size_t new_size)
{
  size_t real_size, *res;

  if (!ptr)
    {
      assert (!old_size);
      return new (quantor, new_size);
    }

  res = ptr;
  real_size = new_size;
#ifndef NDEBUG
  real_size += sizeof (*res);
  assert (old_size);
  res--;
  assert (*res == old_size);
#endif
  assert (quantor->stats.bytes >= old_size);
  quantor->stats.bytes -= old_size;
  quantor->stats.bytes += new_size;
  res = realloc (res, real_size);
  assert (res);
#ifndef NDEBUG
  *res++ = new_size;
#endif
  if (new_size > old_size)
    memset (((char *) res) + old_size, 0, new_size - old_size);

  if (quantor->stats.bytes > quantor->stats.max_bytes)
    quantor->stats.max_bytes = quantor->stats.bytes;

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG2
/*------------------------------------------------------------------------*/

static void
append_string (Quantor * quantor, char **prefix_ptr, const char *new_suffix)
{
  size_t old_len, new_len, old_prefix_len;
  char *res;

  old_prefix_len = *prefix_ptr ? strlen (*prefix_ptr) : 0;
  old_len = *prefix_ptr ? old_prefix_len + 1 : 0;
  new_len = old_prefix_len + strlen (new_suffix) + 1;

  res = enlarge (quantor, *prefix_ptr, old_len, new_len);
  strcpy (res + old_prefix_len, new_suffix);

  *prefix_ptr = res;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static unsigned
count_IntStack (IntStack * stack)
{
  return stack->top - stack->start;
}

/*------------------------------------------------------------------------*/

static unsigned
size_IntStack (IntStack * stack)
{
  return stack->end - stack->start;
}

/*------------------------------------------------------------------------*/

static void
reset_IntStack (Quantor * quantor, IntStack * stack, unsigned new_count)
{
  (void) quantor;
  assert (new_count <= count_IntStack (stack));
  stack->top = stack->start + new_count;
}

/*------------------------------------------------------------------------*/

static void
enlarge_IntStack (Quantor * quantor, IntStack * stack)
{
  size_t new_bytes, old_bytes;
  unsigned count, size, new_size;

  count = count_IntStack (stack);
  size = size_IntStack (stack);
  old_bytes = size * sizeof (int);
  new_size = (size ? (2 * size) : 1);
  new_bytes = sizeof (int) * new_size;
  stack->start = enlarge (quantor, stack->start, old_bytes, new_bytes);
  stack->top = stack->start + count;
  stack->end = stack->start + new_size;
}

/*------------------------------------------------------------------------*/

static void
push_IntStack (Quantor * quantor, IntStack * stack, int data)
{
  if (stack->top >= stack->end)
    enlarge_IntStack (quantor, stack);

  *stack->top++ = data;
}

/*------------------------------------------------------------------------*/

static int *
access_IntStack (Quantor * quantor, IntStack * stack, unsigned idx)
{
  int *res, *min_top;

  while (size_IntStack (stack) <= idx)
    enlarge_IntStack (quantor, stack);

  res = stack->start + idx;
  min_top = res + 1;
  assert (min_top <= stack->end);

  if (stack->top < min_top)
    stack->top = min_top;

  return res;
}

/*------------------------------------------------------------------------*/

static int
pop_IntStack (IntStack * stack)
{
  assert (count_IntStack (stack));
  return *--stack->top;
}

/*------------------------------------------------------------------------*/

static void
release_IntStack (Quantor * quantor, IntStack * stack)
{
  size_t bytes;
  bytes = size_IntStack (stack) * sizeof (int);
  delete (quantor, stack->start, bytes);
}

/*------------------------------------------------------------------------*/

static void
init_PtrStack (Quantor * quantor, PtrStack * stack)
{
  (void) quantor;
  memset (stack, 0, sizeof (*stack));
}

/*------------------------------------------------------------------------*/

static unsigned
count_PtrStack (PtrStack * stack)
{
  return stack->top - stack->start;
}

/*------------------------------------------------------------------------*/

static unsigned
size_PtrStack (PtrStack * stack)
{
  return stack->end - stack->start;
}

/*------------------------------------------------------------------------*/

static void
enlarge_PtrStack (Quantor * quantor, PtrStack * stack)
{
  size_t new_bytes, old_bytes;
  int count, size, new_size;

  count = count_PtrStack (stack);
  size = size_PtrStack (stack);
  old_bytes = size * sizeof (void *);
  new_size = (size ? (2 * size) : 1);
  new_bytes = sizeof (void *) * new_size;
  stack->start = enlarge (quantor, stack->start, old_bytes, new_bytes);
  stack->top = stack->start + count;
  stack->end = stack->start + new_size;
}

/*------------------------------------------------------------------------*/

static int
is_full_PtrStack (PtrStack * stack)
{
  return (stack->top >= stack->end);
}

/*------------------------------------------------------------------------*/

static void
push_PtrStack (Quantor * quantor, PtrStack * stack, void *data)
{
  if (is_full_PtrStack (stack))
    enlarge_PtrStack (quantor, stack);

  *stack->top++ = data;
}

/*------------------------------------------------------------------------*/

static void **
access_PtrStack (Quantor * quantor, PtrStack * stack, unsigned idx)
{
  void **res, **min_top;

  while (size_PtrStack (stack) <= idx)
    enlarge_PtrStack (quantor, stack);

  res = stack->start + idx;
  min_top = res + 1;
  assert (min_top <= stack->end);

  if (stack->top < min_top)
    stack->top = min_top;

  return res;
}

/*------------------------------------------------------------------------*/

static void *
pop_PtrStack (Quantor * quantor, PtrStack * stack)
{
  void *res;

  (void) quantor;
  assert (count_PtrStack (stack));
  res = *--stack->top;

  return res;
}

/*------------------------------------------------------------------------*/

static void
release_PtrStack (Quantor * quantor, PtrStack * stack)
{
  size_t bytes;
  bytes = size_PtrStack (stack) * sizeof (void *);
  delete (quantor, stack->start, bytes);
}

/*------------------------------------------------------------------------*/

static void
reset_PtrStack (Quantor * quantor, PtrStack * stack, unsigned new_count)
{
  (void) quantor;
  assert (new_count <= count_PtrStack (stack));
  stack->top = stack->start + new_count;
}

/*------------------------------------------------------------------------*/

static void
check_anchor_integrity (GenericAnchor * anchor, unsigned offset)
{
#ifndef NDEBUG
  GenericLink *first_link, *last_link;

  assert (!anchor->first == !anchor->last);

  if (anchor->first)
    {
      first_link = (GenericLink *) (anchor->first + offset);
      last_link = (GenericLink *) (anchor->last + offset);

      assert (!first_link->prev);
      assert (!last_link->next);

      assert (first_link->next != anchor->first);
      assert (last_link->prev != anchor->last);
    }
#else
  (void) anchor;
  (void) offset;
#endif
}

/*------------------------------------------------------------------------*/

static int
is_linked (void *anchor_as_void_ptr,
	   void *this_as_void_ptr, void *link_as_void_ptr)
{
  GenericAnchor *anchor;
  GenericLink *link;
  void **this;

  anchor = anchor_as_void_ptr;

  if (!anchor->first)
    return 0;

  this = this_as_void_ptr;

  if (anchor->first == this)
    return 1;

  link = link_as_void_ptr;

  return link->prev || link->next;
}

/*------------------------------------------------------------------------*/

static void
dlink (void *anchor_as_void_ptr,
       void *this_as_void_ptr, void *this_link_as_void_ptr)
{
  GenericLink *prev_link, *this_link;
  GenericAnchor *anchor;
  void **prev, **this;
  unsigned offset;

  anchor = anchor_as_void_ptr;

  this = this_as_void_ptr;
  this_link = this_link_as_void_ptr;

  offset = ((void **) this_link) - this;

  check_anchor_integrity (anchor, offset);

  assert (offset < sizeof (Var) / sizeof (void **));

  assert (!is_linked (anchor, this, this_link));

  prev = anchor->last;

  if (anchor->first)
    {
      prev_link = (GenericLink *) (prev + offset);
      assert (!prev_link->next);
      prev_link->next = this;
    }
  else
    anchor->first = this;

  this_link->prev = prev;
  anchor->last = this;
  this_link->next = 0;

  anchor->len++;

  check_anchor_integrity (anchor, offset);
  assert (is_linked (anchor, this, this_link));
}

/*------------------------------------------------------------------------*/

static void
link_before (void *anchor_as_void_ptr,
	     void *this_as_void_ptr, void *this_link_as_void_ptr,
	     void *next_as_void_ptr)
{
  GenericLink *prev_link, *this_link, *next_link;
  void **prev, **this, **next;
  GenericAnchor *anchor;
  unsigned offset;

  anchor = anchor_as_void_ptr;
  this = this_as_void_ptr;
  this_link = this_link_as_void_ptr;

  next = next_as_void_ptr;
  assert (next);

  offset = ((void **) this_link) - this;
  check_anchor_integrity (anchor, offset);
  assert (offset < sizeof (Var) / sizeof (void **));

  next_link = (GenericLink *) (offset + next);

  assert (!is_linked (anchor, this, this_link));
  assert (is_linked (anchor, next, next_link));

  prev = next_link->prev;
  if (prev)
    {
      prev_link = (GenericLink *) (prev + offset);
      assert (prev_link->next == next);
      prev_link->next = this;
    }
  else
    {
      assert (anchor->first == next);
      anchor->first = this;
    }

  this_link->prev = prev;
  this_link->next = next;
  next_link->prev = this;

  anchor->len++;

  check_anchor_integrity (anchor, offset);
  assert (is_linked (anchor, this, this_link));
}

/*------------------------------------------------------------------------*/

static void
undlink (void *anchor_as_void_ptr,
	 void *this_as_void_ptr, void *this_link_as_void_ptr)
{
  GenericLink *prev_link, *this_link, *next_link;
  void **prev, **this, **next;
  GenericAnchor *anchor;
  unsigned offset;

  anchor = anchor_as_void_ptr;

  assert (anchor->first);
  assert (anchor->last);

  this = this_as_void_ptr;
  this_link = this_link_as_void_ptr;

  offset = ((void **) this_link) - this;

  assert (offset < sizeof (Var) / sizeof (void *));

  check_anchor_integrity (anchor, offset);
  assert (is_linked (anchor, this, this_link));

  prev = this_link->prev;
  next = this_link->next;

  if (prev)
    {
      prev_link = (GenericLink *) (prev + offset);
      prev_link->next = next;
    }
  else
    {
      assert (anchor->first == this);
      anchor->first = next;
    }

  if (next)
    {
      next_link = (GenericLink *) (next + offset);
      next_link->prev = prev;
    }
  else
    {
      assert (anchor->last == this);
      anchor->last = prev;
    }

  this_link->prev = this_link->next = 0;

  assert (anchor->len > 0);
  anchor->len--;

  check_anchor_integrity (anchor, offset);
  assert (!is_linked (anchor, this, this_link));
}

/*------------------------------------------------------------------------*/
#if 0 /* THIS WAS COMPLICATED TO IMPLEMENT SO I LEAVE IN THE CODE */
/*------------------------------------------------------------------------*/

static int
list_length (void *anchor_as_void_ptr, void *zero_link_as_void_ptr)
{
  int res, offset;
  void **link;
  char *next;

  assert (anchor_as_void_ptr);

  offset = (int) zero_link_as_void_ptr;
  next = *(void **) anchor_as_void_ptr;
  res = 0;

  while (next)
    {
      link = 1 + (void **) (next + offset);
      next = *link;
      res++;
    }

  return res;
}

#endif
/*------------------------------------------------------------------------*/
#ifndef NDEBUG
/*------------------------------------------------------------------------*/

static int
member (void *anchor_as_void_ptr,
	void *this_as_void_ptr, void *this_link_as_void_ptr)
{
  GenericAnchor *anchor;
  void **this, **p;
  GenericLink *link;
  unsigned offset;

  anchor = anchor_as_void_ptr;
  this = this_as_void_ptr;
  link = this_link_as_void_ptr;

  if (!is_linked (anchor, this, link))
    return 0;

  offset = ((void **) link) - this;

  assert (offset < sizeof (Var) / sizeof (void *));

  check_anchor_integrity (anchor, offset);

  for (p = anchor->first; p; p = link->next)
    {
      if (this == p)
	return 1;

      link = (GenericLink *) (p + offset);
    }

  return 0;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
undlink_if_linked (void *anchor, void *this, void *link)
{
  if (is_linked (anchor, this, link))
    undlink (anchor, this, link);
}

/*------------------------------------------------------------------------*/

static int
dlink_if_not_linked (void *anchor, void *this, void *link)
{
  int res;

  if ((res = !is_linked (anchor, this, link)))
    dlink (anchor, this, link);

  return res;
}

/*------------------------------------------------------------------------*/

static const char *
quantor_prefix (Quantor * quantor)
{
#ifdef QUANTOR_LOG2
  char buffer[20];
#endif

  if (!quantor->prefix)
    {
      quantor->prefix = copy_string (quantor, "c qnt ");
#ifdef QUANTOR_LOG2
      if (quantor->opts.verbose >= 2)
	{
	  sprintf (buffer, "%d ", quantor->nesting);
	  append_string (quantor, &quantor->prefix, buffer);
	}
#endif
    }

  return quantor->prefix;
}

/*------------------------------------------------------------------------*/

#define REPORT \
{ \
  va_list ap; \
 \
  assert (!strchr (fmt, '\n')); \
 \
  fputs (quantor_prefix (quantor), quantor->io.out); \
 \
  va_start (ap, fmt); \
  vfprintf (quantor->io.out, fmt, ap); \
  va_end (ap); \
  fputc ('\n', quantor->io.out); \
  fflush (quantor->io.out); \
} while(0)

/*------------------------------------------------------------------------*/

static void
report (Quantor * quantor, const char *fmt, ...)
{
#ifndef QUANTOR_QBF_EVALUATION_FORMAT
  REPORT;
#else
  (void) quantor;
  (void) fmt;
#endif
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG1
/*------------------------------------------------------------------------*/

static void
LOG (Quantor * quantor, int level, const char *fmt, ...)
{
  if (level > quantor->opts.verbose)
    return;

  REPORT;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static Opt *
new_Opt (Quantor * quantor, const char *env, const char *opt, int is_iopt)
{
  Opt *res;

  res = new (quantor, sizeof (*res));
  res->env = copy_string (quantor, env);
  res->opt = copy_string (quantor, opt);
  res->is_iopt = is_iopt;

  return res;
}

/*------------------------------------------------------------------------*/

static void
iopt (Quantor * quantor, const char *env, const char *opt, int val, int *ptr)
{
  Opt *res;

  res = new_Opt (quantor, env, opt, 1);
  res->data.as_iopt.def = val;
  res->data.as_iopt.ptr = ptr;
  *ptr = val;

  push_PtrStack (quantor, &quantor->opts.setup, res);
}

/*------------------------------------------------------------------------*/

static void
dopt (Quantor * quantor, const char *env, const char *opt,
      double val, double *ptr)
{
  Opt *res;

  res = new_Opt (quantor, env, opt, 0);
  res->is_iopt = 0;
  res->data.as_dopt.def = val;
  res->data.as_dopt.ptr = ptr;
  *ptr = val;

  push_PtrStack (quantor, &quantor->opts.setup, res);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG3
/*------------------------------------------------------------------------*/

static const char *
quantificationtype2str (InternalQuantificationType type)
{
  assert (type == QUANTOR_UNIVERSAL || type == QUANTOR_EXISTENTIAL);
  return type == QUANTOR_UNIVERSAL ? "UNIVERSAL" : "EXISTENTIAL";
}

/*------------------------------------------------------------------------*/

static const char *
scopenesting2str (Quantor * quantor, int nesting)
{
  if (nesting == QUANTOR_DEFAULT_SCOPE_NESTING)
    strcpy (quantor->scope_buffer, "<DEFAULT>");
  else
    sprintf (quantor->scope_buffer, "%d", nesting);

  assert (strlen (quantor->scope_buffer) + 1 <=
	  sizeof (quantor->scope_buffer));

  return quantor->scope_buffer;
}

/*------------------------------------------------------------------------*/

static const char *
scope2str (Quantor * quantor, Scope * scope)
{
  if (!scope)
    strcpy (quantor->scope_buffer, "<EMPTY>");
  else
    (void) scopenesting2str (quantor, scope->nesting);

  return quantor->scope_buffer;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static Scope *
new_Scope (Quantor * quantor, InternalQuantificationType type, int nesting)
{
  Scope *res;

  assert (nesting < QUANTOR_DEFAULT_SCOPE_NESTING ||
	  type == QUANTOR_EXISTENTIAL);

  assert (type == QUANTOR_UNIVERSAL || type == QUANTOR_EXISTENTIAL);

  res = new (quantor, sizeof (*res));
  res->type = type;
  res->nesting = nesting;

#ifdef QUANTOR_LOG3
  LOG (quantor, 3, "NEW %s SCOPE %s",
       quantificationtype2str (type), scope2str (quantor, res));
#endif

  return res;
}

/*------------------------------------------------------------------------*/

static BinDB *
new_BinDB (Quantor * quantor)
{
  BinDB *res;
  size_t bytes;

  res = new (quantor, sizeof (*res));
  res->size = (1 << 10);
  res->count = 0;
  bytes = res->size * sizeof (res->table[0]);
  res->table = new (quantor, bytes);

  return res;
}

/*------------------------------------------------------------------------*/

static void
delete_BinDB (Quantor * quantor, BinDB * bindb)
{
  size_t size;

  size = bindb->size * sizeof (bindb->table[0]);
  delete (quantor, bindb->table, size);
  delete (quantor, bindb, sizeof (*bindb));
}

/*------------------------------------------------------------------------*/

static RHSDB *
new_RHSDB (Quantor * quantor)
{
  size_t bytes;
  RHSDB *res;

  res = new (quantor, sizeof (*res));
  res->size = 1;
  res->count = 0;
  bytes = res->size * sizeof (res->table[0]);
  res->table = new (quantor, bytes);

  return res;
}

/*------------------------------------------------------------------------*/

static void
delete_RHSDB (Quantor * quantor, RHSDB * rhsdb)
{
  size_t size;

  assert (!rhsdb->count);

  size = rhsdb->size * sizeof (rhsdb->table[0]);
  delete (quantor, rhsdb->table, size);
  delete (quantor, rhsdb, sizeof (*rhsdb));
}

/*------------------------------------------------------------------------*/

static int
is_constant (Lit * lit)
{
  return lit == QUANTOR_TRUE || lit == QUANTOR_FALSE;
}

/*------------------------------------------------------------------------*/

static int
lit2int (Quantor * quantor, Lit * lit)
{
  int idx, sign, res;

  (void) quantor;
  assert (lit != QUANTOR_UNASSIGNED);
  assert (!is_constant (lit));

  sign = QUANTOR_SIGN (lit) ? -1 : 1;
  idx = lit->var->idx;
  res = sign * idx;

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG6
/*------------------------------------------------------------------------*/


static void
log_rhs_aux (Quantor * quantor, RHS * rhs)
{
  unsigned i;

  for (i = 0; i < rhs->size; i++)
    fprintf (quantor->io.out, " %d", lit2int (quantor, rhs->literals[i]));

  fputc ('\n', quantor->io.out);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
log_rhs (Quantor * quantor, RHS * rhs)
{
#ifdef QUANTOR_LOG6
  if (quantor->opts.verbose < 6)
    return;

  fprintf (quantor->io.out, "%sNEW RHS %u :",
	   quantor_prefix (quantor), rhs->idx);

  log_rhs_aux (quantor, rhs);
#else
  (void) quantor;
  (void) rhs;
#endif
}

/*------------------------------------------------------------------------*/

static size_t
sizeof_RHS (int size)
{
  assert (size >= 2);
  return size * sizeof (Lit *) + (size_t) (&((RHS *) 0)->literals);
}

/*------------------------------------------------------------------------*/

static void
check_that_literals_are_sorted (Lit ** literals, unsigned size)
{
#ifdef NDEBUG
  Lit **p, **last_literal_ptr;

  assert (size > 0);
  last_literal_ptr = literals + size - 1;

  for (p = literals; p < last_literal_ptr; p++)
    assert (cmp_lit (p[0], p[1]) < 0);
#else
  (void) literals;
  (void) size;
#endif
}

/*------------------------------------------------------------------------*/

static Lit **
push_new_rhs (Quantor * quantor, Clause * clause, Lit * lhs)
{
  unsigned src, size;
  Lit *lit, **res;
#ifndef NDEBUG
  int found_lhs;
#endif

  reset_PtrStack (quantor, &quantor->new_rhs, 0);

  assert (clause->size >= 3);

  src = 0;
#ifndef NDEBUG
  found_lhs = 0;
#endif
  size = 0;

  while (src < clause->size)
    {
      lit = clause->row[src++].lit;
      if (lit == QUANTOR_NOT (lhs))
	{
#ifndef NDEBUG
	  found_lhs = 1;
#endif
	  continue;
	}

      push_PtrStack (quantor, &quantor->new_rhs, lit);
      size++;
    }

  assert (found_lhs = 1);

  res = (Lit **) quantor->new_rhs.start;
  check_that_literals_are_sorted (res, size);

  return res;
}

/*------------------------------------------------------------------------*/

static RHS *
new_RHS (Quantor * quantor, Lit ** literals, unsigned size)
{
  Lit **src, **dst, **end_of_src;
  size_t bytes;
  RHS *res;

  bytes = sizeof_RHS (size);
  res = new (quantor, bytes);
  res->size = size;

  end_of_src = literals + size;
  src = literals;
  dst = res->literals;
  while (src < end_of_src)
    *dst++ = *src++;

#ifdef QUANTOR_STATS1
#ifdef QUANTOR_LOG1
  res->idx = quantor->stats.rhs.new;
#endif
  inc_count_stats (&quantor->stats.rhs);
#endif
  log_rhs (quantor, res);

  return res;
}

/*------------------------------------------------------------------------*/

static Cell *
end_of_row (Clause * clause)
{
  return clause->row + clause->size;
}

/*------------------------------------------------------------------------*/

static unsigned
hash_lit (Quantor * quantor, Lit * lit)
{
  unsigned res;
  int tmp;

  if (lit->valid_hash)
    return lit->hash;

  tmp = lit2int (quantor, lit);
  res = tmp < 0 ? -tmp : tmp;
  res *= 2000000011;
  if (tmp < 0)
    res = (res + 1) * 1000000007;
#ifdef QUANTOR_LOG8
  LOG (quantor, 8, "HASHED LITERAL %d TO 0x%08x",
       lit2int (quantor, lit), res);
#endif
  lit->valid_hash = 1;
  lit->hash = res;

  return res;
}

/*------------------------------------------------------------------------*/

static unsigned
extend_literal_hash_value (Quantor * quantor, unsigned hash, Lit * lit)
{
  unsigned tmp;

  tmp = (hash >> 28);
  hash = (hash << 4) + hash_lit (quantor, lit);
  if (tmp)
    hash ^= tmp;

  return hash;
}

/*------------------------------------------------------------------------*/

static unsigned
hash_literals (Quantor * quantor, Lit ** literals, unsigned size)
{
  Lit **p, **end_of_literals;
  unsigned res;

  check_that_literals_are_sorted (literals, size);

  end_of_literals = literals + size;
  res = 0;

  for (p = literals; p < end_of_literals; p++)
    res = extend_literal_hash_value (quantor, res, *p);

  return res;
}

/*------------------------------------------------------------------------*/

static int
literals_match_rhs (Lit ** literals, unsigned size, RHS * rhs)
{
  Lit **p, **q, **end_of_literals;

  if (size != rhs->size)
    return 0;

  p = literals;
  q = rhs->literals;
  end_of_literals = literals + size;

  while (p < end_of_literals)
    if (*p++ != *q++)
      return 0;

  return 1;
}

/*------------------------------------------------------------------------*/

static RHS **
find_RHS (Quantor * quantor, Lit ** literals, unsigned size)
{
  RHS **p, *rhs;
  unsigned h;

  assert (quantor->opts.functions);

  h = hash_literals (quantor, literals, size);
  h &= quantor->rhsdb->size - 1;
  assert (h < quantor->rhsdb->size);

  for (p = quantor->rhsdb->table + h;
       (rhs = *p) && !literals_match_rhs (literals, size, rhs);
       p = &(*p)->next)
    ;

  return p;
}

/*------------------------------------------------------------------------*/

static int
contains_RHS (Quantor * quantor,
	      Lit ** literals, unsigned size, RHS ** res_ptr)
{
  RHS **p;

  p = find_RHS (quantor, literals, size);
  if (*p)
    {
      if (res_ptr)
	*res_ptr = *p;

      return 1;
    }

  return 0;
}

/*------------------------------------------------------------------------*/

static int
is_full_RHSDB (Quantor * quantor)
{
  return quantor->rhsdb->count >= quantor->rhsdb->size;
}

/*------------------------------------------------------------------------*/

static unsigned
hash_RHS (Quantor * quantor, RHS * rhs)
{
  unsigned res, i;

  res = 0;
  for (i = 0; i < rhs->size; i++)
    res = extend_literal_hash_value (quantor, res, rhs->literals[i]);

  return res;
}

/*------------------------------------------------------------------------*/

static void
delete_RHS (Quantor * quantor, RHS * rhs, int finally)
{
  size_t bytes;
  unsigned h;
  RHS **p;

  assert (!rhs->functions.first);

#ifdef QUANTOR_LOG6
  if (!finally)
    LOG (quantor, 6, "DELETE RHS %u", rhs->idx);
#else
  (void) finally;
#endif

  h = hash_RHS (quantor, rhs);
  h &= quantor->rhsdb->size - 1;
  assert (h < quantor->rhsdb->size);

  for (p = quantor->rhsdb->table + h; *p && *p != rhs; p = &(*p)->next)
    ;

  assert (*p == rhs);
  *p = rhs->next;

  assert (quantor->rhsdb->count > 0);
  quantor->rhsdb->count--;

  bytes = sizeof_RHS (rhs->size);
  delete (quantor, rhs, bytes);
}

/*------------------------------------------------------------------------*/

static void
enlarge_RHSDB (Quantor * quantor)
{
  RHS **old_table, **new_table, *p, *next;
  unsigned old_size, new_size, i, h;
  size_t bytes;

  old_table = quantor->rhsdb->table;
  old_size = quantor->rhsdb->size;
  new_size = old_size * 2;
  quantor->rhsdb->size = new_size;
  bytes = new_size * sizeof (new_table[0]);
  new_table = new (quantor, bytes);
  quantor->rhsdb->table = new_table;

  for (i = 0; i < old_size; i++)
    {
      for (p = old_table[i]; p; p = next)
	{
	  next = p->next;
	  h = hash_RHS (quantor, p);
	  h &= new_size - 1;
	  assert (h < new_size);
	  p->next = new_table[h];
	  new_table[h] = p;
	}
    }

  bytes = old_size * sizeof (old_table[0]);
  delete (quantor, old_table, bytes);
}

/*------------------------------------------------------------------------*/

static RHS *
insert_RHS (Quantor * quantor, Lit ** literals, unsigned size)
{
  RHS **p, *res;

  assert (quantor->opts.functions);

  if (is_full_RHSDB (quantor))
    enlarge_RHSDB (quantor);

  p = find_RHS (quantor, literals, size);
  assert (!*p);

  res = new_RHS (quantor, literals, size);
#ifdef QUANTOR_CHECK
  if (quantor->opts.check)
    {
      assert (hash_RHS (quantor, res) ==
	      hash_literals (quantor, literals, size));
      assert (literals_match_rhs (literals, size, res));
    }
#endif
  *p = res;
  quantor->rhsdb->count++;

  return res;
}

/*------------------------------------------------------------------------*/

static Quantor *
new_Quantor (void)
{
  Quantor *res;

  res = (Quantor *) malloc (sizeof (*res));
  memset (res, 0, sizeof (*res));

  res->io.in = stdin;
  res->io.in_name = "<stdin>";

  res->io.out = stdout;
  res->io.out_name = "<stdout>";

  res->stats.time = get_time ();
  res->recalc_sigs_count_down = 100;
  res->max_external_nesting = -1;

  return res;
}

/*------------------------------------------------------------------------*/

static void
quantor_setup (Quantor * quantor)
{
  if (!quantor->opts.equivalences)
    {
      quantor->opts.functions = 0;
      quantor->opts.hyper1res = 0;
      quantor->opts.binstrengthen = 0;
    }

  if (!quantor->opts.functions)
    quantor->opts.function_resolution = 0;

  if (!quantor->opts.forward_subsume)
    quantor->opts.forward_subsume_instead_recalc = 0;

  if (!quantor->opts.backward_processing)
    quantor->opts.forward_processing = 0;

  if (!quantor->opts.backward_processing)
    {
      quantor->opts.functions = 0;
      quantor->opts.hyper1res = 0;
    }

  if (!quantor->opts.functions && !quantor->opts.hyper1res)
    {
      quantor->opts.forward_processing = 0;
      quantor->opts.backward_processing = 0;
    }

  if (quantor->opts.equivalences)
    quantor->bindb = new_BinDB (quantor);

  if (quantor->opts.functions)
    quantor->rhsdb = new_RHSDB (quantor);

  if (quantor->opts.soft_exists_length < 0)
    quantor->opts.soft_exists_length = 0;

  if (quantor->opts.soft_exists_length)
    quantor->opts.literals_added_by_exists =
      new (quantor, sizeof (int) * (quantor->opts.soft_exists_length));
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_FAST_ALLOC
/*------------------------------------------------------------------------*/

static void
release_chunks (Quantor * quantor)
{
  Chunk *p, *next;

  for (p = quantor->allocated_chunks; p; p = next)
    {
      next = p->header.next;
      delete (quantor, p, p->header.bytes + CHUNK_DATA_OFFSET);
    }
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifndef NDEBUG
/*------------------------------------------------------------------------*/

static int
is_valid_alignment (size_t size)
{
  unsigned i;
  int found;

  found = 0;
  for (i = 2; !found && i < 32; i++)
    found = (size == (1u << i));

  return found;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
alignment (void *ptr, size_t size)
{
  size_t mask;
  int res;

  assert (is_valid_alignment (size));
  mask = size - 1;
  res = size - (mask & (Word) ptr);
  res &= mask;

  return res;
}

/*------------------------------------------------------------------------*/

static int
is_aligned (void *ptr, size_t size)
{
  return !alignment (ptr, size);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_FAST_ALLOC
/*------------------------------------------------------------------------*/

static void *
align (void *ptr, size_t size)
{
  char *res;

  res = ptr;
  res += alignment (res, size);

  return res;
}

/*------------------------------------------------------------------------*/

static void
populate_aux (Quantor * quantor, void *anchor_as_void_ptr,
	      int min_nelems, size_t size, size_t alignment)
{
  int aligned_start_idx, count, nelems;
  char *aligned_start, *last, *p;
  void **anchor;
  Chunk *chunk;
  size_t bytes;

  assert (size >= sizeof (void *));
  assert (!(alignment & 1));
  assert (size == alignment || !alignment);

  nelems = (QUANTOR_MIN_CHUNK_SIZE - alignment + size - 1) / size;
  if (nelems < min_nelems)
    nelems = min_nelems;

  assert (nelems > 0);

  bytes = size * nelems + alignment;
  assert (bytes >= QUANTOR_MIN_CHUNK_SIZE);
  chunk = new (quantor, bytes + CHUNK_DATA_OFFSET);
  chunk->header.bytes = bytes;
  chunk->header.next = quantor->allocated_chunks;
  chunk->header.size = size;
  quantor->allocated_chunks = chunk;

  if (alignment)
    {
      aligned_start_idx = !is_aligned (&chunk->data, alignment);
      nelems += !aligned_start_idx;
      aligned_start = align (&chunk->data, size);
    }
  else
    aligned_start = (char *) &chunk->data;

  chunk->header.nelems = nelems;

  last = ((char *) &chunk->data) + bytes - size;
  anchor = (void **) anchor_as_void_ptr;
  count = (last - aligned_start + size) / size;

  assert (count > 0);
  assert (count == nelems);

  for (p = aligned_start; p <= last; p += size)
    {
      *(void **) p = *anchor;
      *anchor = p;
      count--;
    }

  assert (!count);

  INCSTATS2 (quantor->stats.num_chunks);
}

/*------------------------------------------------------------------------*/

static void
populate (Quantor * quantor, void *anchor_as_void_ptr, size_t sz)
{
  populate_aux (quantor, anchor_as_void_ptr, 1000, sz, 0);
}

/*------------------------------------------------------------------------*/

static void
populate_aligned (Quantor * quantor, void *anchor_as_void_ptr, size_t sz)
{
  populate_aux (quantor, anchor_as_void_ptr, 1000, sz, sz);
}

/*------------------------------------------------------------------------*/

static void
put_back_on_free_list (Quantor * quantor,
		       void *anchor_as_void_ptr, void *elem_as_void_ptr)
{
  void **anchor, **elem;

  (void) quantor;
  anchor = (void **) anchor_as_void_ptr;
  elem = (void **) elem_as_void_ptr;

  *elem = *anchor;
  *anchor = elem;
}

/*------------------------------------------------------------------------*/
#endif /* QUANTOR_FAST_ALLOC */
/*------------------------------------------------------------------------*/

static void
init_lit (Quantor * quantor, Lit * lit, Var * v)
{
  (void) quantor;

  lit->var = v;
  lit->valid_ldsig = 0;
  lit->sigsum = (Signature) 0;

#ifdef QUANTOR_SIGREF
  /* TODO do we want to use buckets here as well ?
   */
  if (quantor->opts.sigref)
    lit->sigref = (unsigned char *) new (quantor, sizeof (Signature) * 8);
#endif
}

/*------------------------------------------------------------------------*/

static Lit *
new_literal_pair (Quantor * quantor, Var * v)
{
  size_t size;
  Lit *res;

  size = 2 * sizeof (Lit);
#ifdef QUANTOR_FAST_ALLOC
  if (!quantor->free_literal_pairs)
    populate_aligned (quantor, &quantor->free_literal_pairs, size);
  res = quantor->free_literal_pairs;
  quantor->free_literal_pairs = *(Lit **) res;
  memset (res, 0, size);
#else
  res = new (quantor, 2 * size);
  if (!is_aligned (res, size))
    {
      size_t offset = alignment (res, size);
      assert (res->delete_offset < size);
      res = (Lit *) (offset + (char *) res);
      res->delete_offset = offset;
    }
#endif
  assert (is_aligned (res, size));

  init_lit (quantor, res + 0, v);
  init_lit (quantor, res + 1, v);

  return res;
}

/*------------------------------------------------------------------------*/

static int
cmp_ints (int a, int b)
{
  return (a < b) ? -1 : (a > b) ? 1 : 0;
}

/*------------------------------------------------------------------------*/

static Lit *
var2lit (Var * v, int sign)
{
  assert (sign == 0 || sign == 1);
  return v->lits + sign;
}

/*------------------------------------------------------------------------*/

static int
cmp (Quantor * quantor, Var * u, Var * v)
{
  int res;

  (void) quantor;

  INCSTATS2 (quantor->stats.cmps);

  assert (u != v);

  res = cmp_ints (u->score, v->score);
  if (!res)
    res = cmp_ints (u->idx, v->idx);

  assert (res);

  return res;
}

/*------------------------------------------------------------------------*/

static int
cmp_order (Quantor * quantor, PtrStack * order, unsigned a, unsigned b)
{
  assert (a < count_PtrStack (order));
  assert (b < count_PtrStack (order));

  return cmp (quantor, order->start[a], order->start[b]);
}

/*------------------------------------------------------------------------*/

static int
left_child_pos (int a, int size)
{
  int res;

  assert (a >= 0);
  assert (a < size);

  res = 2 * (a + 1) - 1;
  if (res > size)
    res = size;

  return res;
}

/*------------------------------------------------------------------------*/

static int
right_child_pos (int a, int size)
{
  int res;

  assert (a >= 0);
  assert (a < size);

  res = 2 * (a + 1);
  if (res > size)
    res = size;

  return res;
}

/*------------------------------------------------------------------------*/

static int
parent_pos (int a, int size)
{
  int res;

  (void) size;
  assert (a > 0);
  assert (a < size);

  res = (a + 1) / 2 - 1;

  assert (left_child_pos (res, size) == a ||
	  right_child_pos (res, size) == a);

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static void
check_order_rec (Quantor * quantor, PtrStack * order, int pos, int size)
{
  int child;

  child = left_child_pos (pos, size);
  if (child < size)
    {
      assert (cmp_order (quantor, order, pos, child) < 0);
      check_order_rec (quantor, order, child, size);

      child = right_child_pos (pos, size);
      if (child < size)
	{
	  assert (cmp_order (quantor, order, pos, child) < 0);
	  check_order_rec (quantor, order, child, size);
	}
    }
}

/*------------------------------------------------------------------------*/

static void
check_order (Quantor * quantor, PtrStack * order)
{
#ifdef QUANTOR_STATS2
  double old_cmps = quantor->stats.cmps;
#endif
  int pos;
  void **p;
  Var *v;

  pos = 0;
  for (p = order->start; p < order->top; p++)
    {
      v = *p;
      assert (v->rank == pos);
      pos++;
    }

  assert (pos == (int)count_PtrStack (order));

  if (pos)
    check_order_rec (quantor, order, 0, pos);

#ifdef QUANTOR_STATS2
  quantor->stats.cmps = old_cmps;
#endif
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#if defined(QUANTOR_LOG4) || defined(QUANTOR_CHECK)
/*------------------------------------------------------------------------*/

static const char *
score2str (Quantor * quantor, int cost)
{
  if (cost == QUANTOR_OVERFLOW)
    strcpy (quantor->cost_buffer, "<OVERFLOW>");
  else if (cost == QUANTOR_UNDEFINED_SCORE)
    strcpy (quantor->cost_buffer, "<UNDEF>");
  else if (cost == QUANTOR_ASSIGNED_SCORE)
    strcpy (quantor->cost_buffer, "<ASSIGNED>");
  else if (cost == QUANTOR_NONREPRESENTATIVE_SCORE)
    strcpy (quantor->cost_buffer, "<NONREP>");
  else if (cost == QUANTOR_STICKY_SCORE)
    strcpy (quantor->cost_buffer, "<STICKY>");
  else
    sprintf (quantor->cost_buffer, "%d", cost);

  assert (strlen (quantor->cost_buffer) + 1 <= sizeof (quantor->cost_buffer));

  return quantor->cost_buffer;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
swap_order (Quantor * quantor, PtrStack * order, unsigned a, unsigned b)
{
  unsigned i[2];
  Var *u[2];

  (void) quantor;

  INCSTATS2 (quantor->stats.swaps);
  i[0] = a;
  i[1] = b;

  assert (i[0] < count_PtrStack (order));
  assert (i[1] < count_PtrStack (order));

  u[0] = order->start[i[0]];
  u[1] = order->start[i[1]];

  order->start[i[0]] = u[1];
  order->start[i[1]] = u[0];

  u[0]->rank = i[1];
  u[1]->rank = i[0];

#ifdef QUANTOR_LOG9
  assert (u[0]->scope);
  assert (u[0]->scope == u[1]->scope);
  assert (i[a < b] == (unsigned) u[a > b]->rank);
  assert (i[a > b] == (unsigned) u[a < b]->rank);
  LOG (quantor, 9, "ORDER[%d]=%d IN SCOPE %s WITH SCORE %s",
       i[a < b], u[a > b]->idx, scope2str (quantor, u[a > b]->scope),
       score2str (quantor, u[a > b]->score));
  LOG (quantor, 9, "ORDER[%d]=%d IN SCOPE %s WITH SCORE %s",
       i[a > b], u[a < b]->idx, scope2str (quantor, u[a < b]->scope),
       score2str (quantor, u[a < b]->score));
#endif
}

/*------------------------------------------------------------------------*/

static int
up (Quantor * quantor, PtrStack * order, int pos)
{
  int size = count_PtrStack (order), parent;

  INCSTATS2 (quantor->stats.ups);

  assert (pos >= 0);
  assert (pos < size);

  while (pos > 0)
    {
      parent = parent_pos (pos, size);
      if (cmp_order (quantor, order, parent, pos) <= 0)
	break;

      swap_order (quantor, order, parent, pos);
      pos = parent;
    }

  return pos;
}

/*------------------------------------------------------------------------*/

static void
down (Quantor * quantor, PtrStack * order, int pos)
{
  int child, left, right, size = count_PtrStack (order);

  INCSTATS2 (quantor->stats.downs);

  for (;;)
    {
      left = left_child_pos (pos, size);
      right = right_child_pos (pos, size);

      assert (left <= right);

      if (left >= size)
	break;

      if (right < size && cmp_order (quantor, order, left, right) > 0)
	child = right;
      else
	child = left;

      if (cmp_order (quantor, order, pos, child) > 0)
	{
	  swap_order (quantor, order, pos, child);
	  pos = child;
	}
      else
	break;
    }
}

/*------------------------------------------------------------------------*/

static void
fix_order (Quantor * quantor, PtrStack * order, int pos)
{
  pos = up (quantor, order, pos);
  down (quantor, order, pos);
#ifdef QUANTOR_CHECK
  if (quantor->opts.check >= 4)
    check_order (quantor, order);
#endif
}

/*------------------------------------------------------------------------*/

static void
add_to_order (Quantor * quantor, PtrStack * order, Var * v)
{
  assert (v->rank < 0);
  v->rank = count_PtrStack (order);
  push_PtrStack (quantor, order, v);
  fix_order (quantor, order, v->rank);
}

/*------------------------------------------------------------------------*/

static int
lit_does_not_occur (Lit * lit)
{
  int res;

  res = !lit->column.first;
  assert (res == !lit->column.len);
  assert (res == !lit->sum);

  return res;
}

/*------------------------------------------------------------------------*/

static int
var_does_not_occur (Var * v)
{
  if (!lit_does_not_occur (var2lit (v, 0)))
    return 0;

  return lit_does_not_occur (var2lit (v, 1));
}

/*------------------------------------------------------------------------*/

static int
is_unate (Quantor * quantor, Var * v)
{
  return is_linked (&quantor->unates, v, &v->unate_link);
}

/*------------------------------------------------------------------------*/

static int
is_proposed_unate (Quantor * quantor, Var * v)
{
  return is_linked (&quantor->proposed_unates, v, &v->proposed_unate_link);
}

/*------------------------------------------------------------------------*/

static int
is_proposed_zombie (Quantor * quantor, Var * v)
{
  int res;

  res = is_linked (&quantor->proposed_zombies, v, &v->proposed_zombie_link);
  assert (!res || var_does_not_occur (v));

  return res;
}

/*------------------------------------------------------------------------*/

static int
is_unit (Quantor * quantor, Var * v)
{
  return is_linked (&quantor->units, v, &v->unit_link);
}

/*------------------------------------------------------------------------*/

static int
is_dying_clause (Quantor * quantor, Clause * clause)
{
  int res;

  res = is_linked (&quantor->dying_clauses, clause, &clause->dying_link);
  assert (res == (clause->state == QUANTOR_DYING_CLAUSE));

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static void
check_row (Quantor * quantor, Clause * clause)
{
  Cell *c, *eor;
  unsigned size;
  Scope *scope;
  Var *v;

  size = 0;
  scope = 0;
  eor = end_of_row (clause);
  for (c = clause->row; c < eor; c++)
    {
      assert (c->clause == clause);
      v = c->lit->var;
      if (!scope || (v->scope && scope->nesting < v->scope->nesting))
	scope = v->scope;

      size++;
    }

  assert (clause->scope == scope);
  assert (!scope || scope->type == QUANTOR_EXISTENTIAL);

  if (!is_dying_clause (quantor, clause))
    for (c = clause->row; c < eor; c++)
      assert ((clause->size == 2) ==
	      is_linked (&c->lit->binary_clauses, c,
			 &c->binary_clauses_link));
}

/*------------------------------------------------------------------------*/

static void
check_rows (Quantor * quantor)
{
  unsigned num_clauses;
  Clause *c;

  num_clauses = 0;

  for (c = quantor->clauses.first; c; c = c->link.next)
    {
      check_row (quantor, c);
      num_clauses++;
    }

  assert (quantor->clauses.len == num_clauses);
}

/*------------------------------------------------------------------------*/

static void
check_column (Quantor * quantor, Lit * lit)
{
  unsigned sum, len;
  Cell *cell;

  (void) quantor;

  sum = len = 0;

  for (cell = lit->column.first; cell; cell = cell->column_link.next)
    {
      assert (cell->lit == lit);
      sum += cell->clause->size;
      len++;
    }

  assert (lit->column.len == len);
  assert (lit->sum == sum);
}

/*------------------------------------------------------------------------*/

static void
check_columns (Quantor * quantor)
{
  void **p;
  Var *v;

  for (p = quantor->vars.start; p < quantor->vars.top; p++)
    {
      if (!(v = *p))
	continue;

      check_column (quantor, var2lit (v, 0));
      check_column (quantor, var2lit (v, 1));
    }
}

/*------------------------------------------------------------------------*/

static void
check_var (Quantor * quantor, Var * v)
{
  Scope *scope;
  int count;

  scope = v->scope;

  assert (scope);
  assert (member (&scope->vars, v, &v->scope_link));

  count = is_proposed_unate (quantor, v);
  count += is_unate (quantor, v);
  count += is_unit (quantor, v);
  assert (count <= 1);

  count = v->zombie;
  count += is_proposed_zombie (quantor, v);
  assert (count <= 1);

  if (is_proposed_zombie (quantor, v) || v->zombie)
    assert (var_does_not_occur (v));
}

/*------------------------------------------------------------------------*/

static void
check_vars (Quantor * quantor)
{
  void **p;
  Var *v;

  for (p = quantor->vars.start; p < quantor->vars.top; p++)
    {
      if (!(v = *p))
	continue;

      check_var (quantor, v);
    }
}

/*------------------------------------------------------------------------*/

static void
check_scope (Quantor * quantor, Scope * scope)
{
  unsigned sum;
  Clause *p;

  assert (scope);
  assert (scope->vars.len == count_PtrStack (&scope->order));

  check_order (quantor, &scope->order);

  if (scope->type == QUANTOR_UNIVERSAL)
    assert (!scope->clauses.first);

  sum = 0;
  for (p = scope->clauses.first; p; p = p->scope_link.next)
    sum += p->size;

  assert (sum == scope->sum);
}

/*------------------------------------------------------------------------*/

static void
check_scopes (Quantor * quantor)
{
  Scope *scope;

  for (scope = quantor->scopes.first; scope; scope = scope->link.next)
    check_scope (quantor, scope);
}

/*------------------------------------------------------------------------*/

static void
check_cells (Quantor * quantor)
{
  check_rows (quantor);		/* has to preceed 'check_scopes' */
  check_scopes (quantor);
  check_columns (quantor);
}

/*------------------------------------------------------------------------*/

static void
check_equivalence_class (Quantor * quantor, EquivalenceClass * ec)
{
  unsigned num_eliminated;
  unsigned num_zombies;
  Var *v;

  (void) quantor;

  num_eliminated = 0;
  num_zombies = 0;

  for (v = ec->elems.first; v; v = v->eqclass_link.next)
    {
      assert (v->eqclass == ec);

      if (v->eliminated)
	num_eliminated++;

      if (v->zombie)
	num_zombies++;
    }

  assert (num_zombies == ec->num_zombies);
  assert (num_eliminated == ec->num_eliminated);
}

/*------------------------------------------------------------------------*/

static void
check_equivalence_classes (Quantor * quantor)
{
  EquivalenceClass *p;

  for (p = quantor->equivalence_classes.first; p; p = p->link.next)
    check_equivalence_class (quantor, p);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG7
/*------------------------------------------------------------------------*/

static void
log_new_scope_of_clause (Quantor * quantor, Clause * clause)
{
  LOG (quantor, 7,
       "CONNECTED CLAUSE %d OF SIZE %d TO SCOPE %s",
       clause->idx, clause->size, scope2str (quantor, clause->scope));
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
connect_clause_to_scope (Quantor * quantor, Clause * clause, Scope * scope)
{
  (void) quantor;

  if (clause->scope)
    {
      undlink (&clause->scope->clauses, clause, &clause->scope_link);
      assert (clause->scope->sum >= clause->size);
      clause->scope->sum -= clause->size;
    }

  clause->scope = scope;

  if (scope)
    {
      dlink (&scope->clauses, clause, &clause->scope_link);
      scope->sum += clause->size;
    }

#ifdef QUANTOR_LOG7
  log_new_scope_of_clause (quantor, clause);
#endif
}

/*------------------------------------------------------------------------*/

static void
add_to_scope (Quantor * quantor, Var * v, Scope * scope)
{
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "ADDING VARIABLE %d TO %s SCOPE %s", v->idx,
       quantificationtype2str (scope->type), scope2str (quantor, scope));
#endif
  dlink (&scope->vars, v, &v->scope_link);
  v->scope = scope;

  add_to_order (quantor, &scope->order, v);
}

/*------------------------------------------------------------------------*/

static void
init_default_scope (Quantor * quantor)
{
  Scope *default_scope;

  assert (!quantor->scopes.first);

  default_scope = new_Scope (quantor,
                             QUANTOR_EXISTENTIAL,
			     QUANTOR_DEFAULT_SCOPE_NESTING);

  dlink (&quantor->scopes, default_scope, &default_scope->link);
}

/*------------------------------------------------------------------------*/

static Scope *
default_scope (Quantor * quantor)
{
  return quantor->scopes.last;
}

/*------------------------------------------------------------------------*/

static int
is_assigned_var (Var * v)
{
  return v->assignment != QUANTOR_UNASSIGNED;
}

/*------------------------------------------------------------------------*/

static void
export_var (Quantor * quantor, Var * v)
{
  assert (!v->exported);

  v->exported = 1;
  if (quantor->max_idx < v->idx)
    quantor->max_idx = v->idx;

  assert (!is_assigned_var (v));
  quantor->stats.num_unassigned_exported++;
  quantor->stats.num_exported++;
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "VARIABLE %d MARKED FOR EXPORT", v->idx);
#endif
}

/*------------------------------------------------------------------------*/

static int
is_external (Quantor * quantor, Var * v)
{
  int res;

  assert (v->scope);
  assert (quantor->scopes.len >= 1);

  if (quantor->scopes.len == 1)
    {
      assert (v->scope == default_scope (quantor));
      assert (v->scope->type == QUANTOR_EXISTENTIAL);
      res = 1;
    }
  else if (v->scope->type == QUANTOR_EXISTENTIAL)
    {
      res = (v->scope->nesting <= quantor->max_external_nesting);
    }
  else
    res = 0;

  return res;
}

/*------------------------------------------------------------------------*/

static int
is_universal (Var * v)
{
  assert (v->scope);
  return v->scope->type == QUANTOR_UNIVERSAL;
}

/*------------------------------------------------------------------------*/

static int
is_existential (Var * v)
{
  assert (v->scope);
  return v->scope->type == QUANTOR_EXISTENTIAL;
}

/*------------------------------------------------------------------------*/

static Lit *
lit2const (Lit * lit)
{
  return QUANTOR_SIGN (lit) ? QUANTOR_FALSE : QUANTOR_TRUE;
}

/*------------------------------------------------------------------------*/

static void
log_var (Quantor * quantor, const char *action, const char *status, Lit * lit)
{
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "%s %s %s %d",
       action, quantificationtype2str (lit->var->scope->type),
       status, lit2int (quantor, lit));
#else
  (void) quantor;
  (void) action;
  (void) status;
  (void) lit;
#endif
}

/*------------------------------------------------------------------------*/

static void
propose_unate (Quantor * quantor, Lit * lit)
{
  Lit *assignment;
  Var *v;

  v = lit->var;

  assert (!is_unate (quantor, v));
  assert (!is_proposed_unate (quantor, v));

  if (is_unit (quantor, v))
    return;

  assert (lit_does_not_occur (QUANTOR_NOT (lit)));

  if (is_universal (v))
    lit = QUANTOR_NOT (lit);

  assignment = lit2const (lit);
  assert (v->unate_assignment == QUANTOR_UNASSIGNED);
  v->unate_assignment = assignment;

  dlink (&quantor->proposed_unates, v, &v->proposed_unate_link);

  log_var (quantor, "PROPOSED", "UNATE", lit);
}

/*------------------------------------------------------------------------*/

static void
unpropose_unate (Quantor * quantor, Var * v)
{
  Lit *lit;

  assert (v->unate_assignment != QUANTOR_UNASSIGNED);

  lit = var2lit (v, v->unate_assignment == QUANTOR_FALSE);

  assert (is_proposed_unate (quantor, v));

  log_var (quantor, "UNPROPOSED", "UNATE", lit);

  undlink (&quantor->proposed_unates, v, &v->proposed_unate_link);
  v->unate_assignment = QUANTOR_UNASSIGNED;
}

/*------------------------------------------------------------------------*/

static void
register_unate (Quantor * quantor, Var * v)
{
  assert (is_proposed_unate (quantor, v));
  assert (!is_unit (quantor, v));
  assert (!is_unate (quantor, v));
  assert (is_constant (v->unate_assignment));

  undlink (&quantor->proposed_unates, v, &v->proposed_unate_link);
  dlink (&quantor->unates, v, &v->unate_link);

  INCSTATS1 (quantor->stats.unates);

  log_var (quantor, "REGISTERED", "UNATE",
	   var2lit (v, v->unate_assignment == QUANTOR_FALSE));
}

/*------------------------------------------------------------------------*/

static void
unregister_unate (Quantor * quantor, Var * v)
{
  Lit *lit;

  assert (v->unate_assignment != QUANTOR_UNASSIGNED);

  lit = var2lit (v, v->unate_assignment == QUANTOR_FALSE);

  assert (is_unate (quantor, v));

  log_var (quantor, "UNREGISTERED", "UNATE", lit);

  undlink (&quantor->unates, v, &v->unate_link);
  v->unate_assignment = QUANTOR_UNASSIGNED;

#ifdef QUANTOR_STATS1
  assert (quantor->stats.unates > 0);
  quantor->stats.unates--;
#endif
}

/*------------------------------------------------------------------------*/

static void
unregister_unit (Quantor * quantor, Var * v)
{
  Lit *lit;

  assert (v->unit_assignment != QUANTOR_UNASSIGNED);

  lit = var2lit (v, v->unit_assignment == QUANTOR_FALSE);

  assert (is_unit (quantor, v));

  log_var (quantor, "UNREGISTERED", "UNIT", lit);

  undlink (&quantor->units, v, &v->unit_link);
  v->unit_assignment = QUANTOR_UNASSIGNED;
}

/*------------------------------------------------------------------------*/

static void
propose_zombie (Quantor * quantor, Var * v)
{
  assert (var_does_not_occur (v));
  assert (!v->zombie);
  assert (!is_proposed_zombie (quantor, v));

  if (is_proposed_unate (quantor, v))
    unpropose_unate (quantor, v);

  if (is_unit (quantor, v))
    unregister_unit (quantor, v);

  if (is_unate (quantor, v))
    unregister_unate (quantor, v);

  log_var (quantor, "PROPOSED", "ZOMBIE", var2lit (v, 0));
  dlink (&quantor->proposed_zombies, v, &v->proposed_zombie_link);
}

/*------------------------------------------------------------------------*/

static void
unpropose_zombie (Quantor * quantor, Var * v)
{
  assert (!v->zombie);
  assert (is_proposed_zombie (quantor, v));
  log_var (quantor, "UNPROPOSED", "ZOMBIE", var2lit (v, 0));
  undlink (&quantor->proposed_zombies, v, &v->proposed_zombie_link);
}

/*------------------------------------------------------------------------*/

static Var *
new_Var (Quantor * quantor, int idx, Scope * scope)
{
  Var *res;

#ifdef QUANTOR_FAST_ALLOC
  if (!quantor->free_vars)
    populate (quantor, &quantor->free_vars, sizeof (*res));

  res = quantor->free_vars;
  quantor->free_vars = *(Var **) res;
  memset (res, 0, sizeof (*res));
#else
  res = new (quantor, sizeof (*res));
#endif
  res->idx = idx;
  res->lits = new_literal_pair (quantor, res);
#ifdef QUANTOR_STATS1
  inc_count_stats (&quantor->stats.vars);
#endif
  res->rank = -1;
  res->score = QUANTOR_UNDEFINED_SCORE;
  res->assignment = QUANTOR_UNASSIGNED;
  res->unit_assignment = QUANTOR_UNASSIGNED;
  res->unate_assignment = QUANTOR_UNASSIGNED;

  if (!scope)
    scope = default_scope (quantor);
  res->scope = scope;
  add_to_scope (quantor, res, scope);

  if (is_external (quantor, res))
    export_var (quantor, res);

  propose_zombie (quantor, res);

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG5
/*------------------------------------------------------------------------*/

static void
log_add_EquivalenceClasse (Quantor * quantor,
			   EquivalenceClass * ec, Lit * lit)
{
  LOG (quantor, 5, "ADDING LITERAL %d TO EQUIVALENCE CLASS %u",
       lit2int (quantor, lit), ec->idx);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
is_unprocessed_equivalence_class (Quantor * quantor, EquivalenceClass * ec)
{
  return is_linked (&quantor->unprocessed_equivalence_classes, ec,
		    &ec->unprocessed_link);
}

/*------------------------------------------------------------------------*/

static void
mark_equivalence_class_as_unprocessed (Quantor * quantor,
				       EquivalenceClass * ec)
{
  if (!dlink_if_not_linked (&quantor->unprocessed_equivalence_classes,
			    ec, &ec->unprocessed_link))
    return;

#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "MARKING EQUIVALENCE CLASS %u AS UNPROCESSED", ec->idx);
#endif
}

/*------------------------------------------------------------------------*/

static void
new_EquivalenceClass (Quantor * quantor, Var * v)
{
  EquivalenceClass *res;

  assert (!v->eqclass);
  assert (!v->eqclass_sign);

  res = new (quantor, sizeof (*res));
  dlink (&quantor->equivalence_classes, res, &res->link);
  v->eqclass = res;
  v->eqclass_sign = 0;
  res->representative = v;

  dlink (&res->elems, v, &v->eqclass_link);

#ifdef QUANTOR_STATS1
#ifdef QUANTOR_LOG1
  res->idx = quantor->stats.eqclasses.new;
#endif
  inc_count_stats (&quantor->stats.eqclasses);
#endif
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "NEW EQUIVALENCE CLASS %u", res->idx);
  log_add_EquivalenceClasse (quantor, res, var2lit (v, v->eqclass_sign));
#endif

  mark_equivalence_class_as_unprocessed (quantor, res);

  assert (!v->zombie);

  if (v->eliminated)
    res->num_eliminated++;
}

/*------------------------------------------------------------------------*/

static int
cmp_variables_by_nesting_level (Var * a, Var * b)
{
  int res;

  assert (a != b);

  res = cmp_ints (a->scope->nesting, b->scope->nesting);

  if (!res)
    res = cmp_ints (a->idx, b->idx);

  assert (res);

  return res;
}

/*------------------------------------------------------------------------*/

static int
cmp_literals_by_nesting_level (Lit * a, Lit * b)
{
  int res;

  assert (a != b);
  assert (a != QUANTOR_NOT (b));

  res = cmp_variables_by_nesting_level (a->var, b->var);

  assert (res);

  return res;
}

/*------------------------------------------------------------------------*/

static void
log_new_equivalence_class_representative (Quantor * quantor,
					  EquivalenceClass * ec)
{
#ifdef QUANTOR_LOG5
  Var *repr;
  int lit;

  repr = ec->representative;
  lit = repr->idx;
  if (repr->eqclass_sign)
    lit = -lit;

  LOG (quantor, 5, "NEW REPRESENTATIVE %d FOR EQUIVALENCE CLASS %d",
       lit, ec->idx);
#else
  (void) quantor;
  (void) ec;
#endif
}

/*------------------------------------------------------------------------*/

static void
add_EquivalenceClass (Quantor * quantor, EquivalenceClass * ec, Lit * lit)
{
  Var *v;

  v = lit->var;

  assert (!v->eqclass);

  dlink (&ec->elems, v, &v->eqclass_link);
  v->eqclass = ec;
  v->eqclass_sign = QUANTOR_SIGN (lit);
#ifdef QUANTOR_LOG5
  log_add_EquivalenceClasse (quantor, ec, lit);
#endif
  if (cmp_variables_by_nesting_level (ec->representative, v) > 0)
    {
      ec->representative = v;
      log_new_equivalence_class_representative (quantor, ec);
    }

  mark_equivalence_class_as_unprocessed (quantor, ec);

  assert (!v->zombie);
  assert (!v->eliminated);
}

/*------------------------------------------------------------------------*/

static void
delete_EquivalenceClass (Quantor * quantor,
			 EquivalenceClass * eqclass, int finally)
{
  Var *p;

#ifdef QUANTOR_LOG5
  if (!finally)
    LOG (quantor, 5, "DELETING EQUIVALENCE CLASS %u", eqclass->idx);
#else
  (void) finally;
#endif
  assert (eqclass);

#ifdef QUANTOR_STATS1
  dec_count_stats (&quantor->stats.eqclasses);
#endif

  undlink_if_linked (&quantor->unprocessed_equivalence_classes,
		     eqclass, &eqclass->unprocessed_link);

  undlink_if_linked (&quantor->dying_equivalence_classes,
		     eqclass, &eqclass->dying_link);

  for (p = eqclass->elems.first; p; p = p->eqclass_link.next)
    {
      assert (p->eqclass == eqclass);
      p->eqclass = 0;
      p->eqclass_sign = 0;
    }

  undlink (&quantor->equivalence_classes, eqclass, &eqclass->link);

  delete (quantor, eqclass, sizeof (*eqclass));
}

/*------------------------------------------------------------------------*/

static int
is_dying_equivalence_class (Quantor * quantor, EquivalenceClass * ec)
{
  return is_linked (&quantor->dying_equivalence_classes, ec, &ec->dying_link);
}

/*------------------------------------------------------------------------*/

static void
kill_EquivalenceClass (Quantor * quantor, EquivalenceClass * eqclass)
{
  undlink_if_linked (&quantor->unprocessed_equivalence_classes,
		     eqclass, &eqclass->unprocessed_link);

  if (is_dying_equivalence_class (quantor, eqclass))
    return;

  dlink (&quantor->dying_equivalence_classes, eqclass, &eqclass->dying_link);
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "KILLING EQUIVALENCE CLASS %u", eqclass->idx);
#endif
}

/*------------------------------------------------------------------------*/

static int
cmp_equivalence_class_representative_candidates (Var * u, Var * v)
{
  return cmp_variables_by_nesting_level (u, v);
}

/*------------------------------------------------------------------------*/

static Var *
minimize_equivalence_class_representative (Quantor * quantor,
					   EquivalenceClass * ec)
{
  Var *p, *res;

  assert (ec->elems.first);
  assert (!is_unprocessed_equivalence_class (quantor, ec));

  res = 0;

  for (p = ec->elems.first; p; p = p->eqclass_link.next)
    {
      if (!res ||
	  cmp_equivalence_class_representative_candidates (res, p) > 0)
	res = p;
    }

  assert (res);

  if (ec->representative != res)
    {
      ec->representative = res;
      log_new_equivalence_class_representative (quantor, ec);
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
update_equivalence_class_representative (Quantor * quantor,
					 EquivalenceClass * ec)
{
  Var *res;

  res = ec->elems.first;

  assert (res);

  if (ec->representative == res)
    return;

  ec->representative = res;
  log_new_equivalence_class_representative (quantor, ec);
  mark_equivalence_class_as_unprocessed (quantor, ec);
}

/*------------------------------------------------------------------------*/

static void
remove_from_EquivalenceClass (Quantor * quantor, Var * v)
{
  EquivalenceClass *ec;

  ec = v->eqclass;
  assert (ec);

#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "REMOVING LITERAL %d FROM EQUIVALENCE CLASS %u",
       lit2int (quantor, var2lit (v, v->eqclass_sign)), ec->idx);
#endif
  undlink (&ec->elems, v, &v->eqclass_link);

  if (!ec->elems.first)
    kill_EquivalenceClass (quantor, ec);
  else if (ec->representative == v)
    update_equivalence_class_representative (quantor, ec);

  if (v->zombie)
    {
      assert (ec->num_zombies > 0);
      ec->num_zombies--;
    }

  if (v->eliminated)
    {
      assert (ec->num_eliminated > 0);
      ec->num_eliminated--;
    }

  v->eqclass = 0;
  v->eqclass_sign = 0;
}

/*------------------------------------------------------------------------*/

static void
merge_delete_EquivalenceClass (Quantor * quantor,
			       EquivalenceClass * a,
			       EquivalenceClass * b, unsigned sign)
{
  EquivalenceClass *tmp;
  Var *p, *next;
#ifndef NDEBUG
  unsigned len = a->elems.len + b->elems.len;
#endif
  assert (sign == 0 || sign == 1);

  assert (a != b);
  assert (a->representative != b->representative);

  if ((a->elems.len < b->elems.len && !a->processing) || b->processing)
    {
      tmp = a;
      a = b;
      b = tmp;
    }

  assert (a->elems.len);
  assert (!b->processing);

  for (p = b->elems.first; p; p = next)
    {
      next = p->eqclass_link.next;
      undlink (&b->elems, p, &p->eqclass_link);
      dlink (&a->elems, p, &p->eqclass_link);
      p->eqclass = a;
      p->eqclass_sign ^= sign;
#ifdef QUANTOR_LOG5
      LOG (quantor, 5, "MOVING LITERAL %d TO EQUIVALENCE CLASS %u",
	   lit2int (quantor, var2lit (p, p->eqclass_sign)), a->idx);
#endif
    }

  assert (a->elems.len == len);
  assert (!b->elems.first);

  a->num_zombies += b->num_zombies;
  a->num_eliminated += b->num_eliminated;

  b->num_zombies = 0;
  b->num_eliminated = 0;

  if (cmp_variables_by_nesting_level (b->representative,
				      a->representative) < 0)
    {
      a->representative = b->representative;
      log_new_equivalence_class_representative (quantor, a);
    }

  kill_EquivalenceClass (quantor, b);
  mark_equivalence_class_as_unprocessed (quantor, a);
}

/*------------------------------------------------------------------------*/

static void
delete_literal_pair (Quantor * quantor, Lit * lits)
{
#ifndef QUANTOR_FAST_ALLOC
  char *ptr = (char *) lits;
#endif
#ifdef QUANTOR_SIGREF
  if (quantor->opts.sigref)
    {
      delete (quantor, lits[0].sigref, sizeof (Signature) * 8);
      delete (quantor, lits[1].sigref, sizeof (Signature) * 8);
    }
#endif

#ifdef QUANTOR_FAST_ALLOC
  put_back_on_free_list (quantor, &quantor->free_literal_pairs, lits);
#else
  ptr -= lits->delete_offset;
  delete (quantor, ptr, sizeof (*lits) * 4);
#endif
}

/*------------------------------------------------------------------------*/

static Var *
gen_Var (Quantor * quantor, Scope * scope)
{
  void **p;
  Var *res;
  int idx;

  assert (quantor->no_more_external_vars);

  if (quantor->opts.recycle_indices &&
      count_IntStack (&quantor->free_var_indices))
    {
      idx = pop_IntStack (&quantor->free_var_indices);
      INCSTATS2 (quantor->stats.recycled_vars);
#ifdef QUANTOR_LOG5
      LOG (quantor, 5, "RECYCLING INDEX OF VARIABLE %d", idx);
#endif
    }
  else
    idx = count_PtrStack (&quantor->vars);

#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "NEW INTERNAL GENERIC VARIABLE %d", idx);
#endif
  res = new_Var (quantor, idx, scope);

  p = access_PtrStack (quantor, &quantor->vars, idx);
  assert (!*p);
  *p = res;

  return res;
}

/*------------------------------------------------------------------------*/

static Var *
int2var (Quantor * quantor, int lit, int *sign_ptr, Scope * scope)
{
  int idx, sign;
  Var *res;
  void **p;

  assert (lit);

  sign = (lit < 0);
  idx = sign ? -lit : lit;

  p = access_PtrStack (quantor, &quantor->vars, idx);
  if (!*p)
    {
      assert (!quantor->no_more_external_vars);
#ifdef QUANTOR_LOG5
      LOG (quantor, 5, "NEW %s VARIABLE %d",
	   quantificationtype2str (scope ? scope->type : QUANTOR_EXISTENTIAL),
	   idx);
#endif
      *p = new_Var (quantor, idx, scope);
    }

  res = *p;
  if (sign_ptr)
    *sign_ptr = sign;

  return res;
}

/*------------------------------------------------------------------------*/

static Lit *
int2lit (Quantor * quantor, int lit, Scope * scope)
{
  Lit *res;
  Var *v;
  int sign;

  v = int2var (quantor, lit, &sign, scope);
  res = v->lits + sign;

  assert (is_aligned (res, 2 * sizeof (Lit)) == (lit > 0));

  return res;
}

/*------------------------------------------------------------------------*/

static int assign (Quantor *, Var *, Lit *);
static void kill_Clause (Quantor *, Clause *);

/*------------------------------------------------------------------------*/

static void
new_Equivalence (Quantor * quantor, Clause * a, Clause * b)
{
  Lit *lhs, *rhs, *tmp_lit;
  Clause *tmp_clause;

  assert (a->size == 2);
  assert (b->size == 2);

  lhs = QUANTOR_NOT (a->row[0].lit);
  rhs = a->row[1].lit;

  assert (b->row[0].lit == lhs);
  assert (b->row[1].lit == QUANTOR_NOT (rhs));

  if (cmp_literals_by_nesting_level (lhs, rhs) < 0)
    {
      tmp_lit = lhs;
      lhs = rhs;
      rhs = tmp_lit;
    }

  if (QUANTOR_SIGN (lhs))
    {
      lhs = QUANTOR_NOT (lhs);
      rhs = QUANTOR_NOT (rhs);
    }

  if (lhs == a->row[0].lit || lhs == a->row[1].lit)
    {
      tmp_clause = a;
      a = b;
      b = tmp_clause;
    }

#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "NEW EQUIVALENCE %d = %d FROM CLAUSES %d AND %d",
       lit2int (quantor, lhs), lit2int (quantor, rhs), a->idx, b->idx);
#endif
  INCSTATS1 (quantor->stats.equivalences);

  assert (!QUANTOR_SIGN (lhs));
  assign (quantor, lhs->var, rhs);
}

/*------------------------------------------------------------------------*/

static void
delete_Function (Quantor * quantor, Function * function, int finally)
{
#ifdef QUANTOR_STATS1
  CountStats *type_stats;

  dec_count_stats (&quantor->stats.functions);
  type_stats = 0;
  switch (function->type)
    {
    default:
      assert (function->type == QUANTOR_OR_GATE);
      type_stats = &quantor->stats.or;
      break;
    }
  dec_count_stats (type_stats);
#endif
#ifdef QUANTOR_LOG6
  if (!finally)
    LOG (quantor, 6, "DELETE FUNCTION %d", function->idx);
#endif
  undlink_if_linked (&quantor->dying_functions,
		     function, &function->dying_link);

  undlink (&function->lhs->var->functions, function, &function->lhs_link);

  undlink (&function->rhs->functions, function, &function->rhs_link);
  if (!function->rhs->functions.first)
    delete_RHS (quantor, function->rhs, finally);

  undlink (&function->clause->functions, function, &function->clause_link);

  undlink (&quantor->functions, function, &function->link);
  delete (quantor, function, sizeof (*function));
}

/*------------------------------------------------------------------------*/

static void
register_invalidity (Quantor * quantor)
{
  if (quantor->invalid)
    return;
#ifdef QUANTOR_LOG2
  LOG (quantor, 2, "DETERMINED INVALIDITY");
#endif
  quantor->invalid = 1;
}

/*------------------------------------------------------------------------*/

static void
mark_to_be_reordered (Quantor * quantor, Var * v)
{
  Scope *scope = v->scope;

  assert (scope);

  if (!is_linked (&scope->reorder, v, &v->reorder_link))
    {
      dlink (&scope->reorder, v, &v->reorder_link);
#ifdef QUANTOR_LOG8
      LOG (quantor, 8, "MARKING VARIABLE %d OF SCOPE %s TO BE REORDERED",
	   v->idx, scope2str (quantor, scope));
#else
      (void) quantor;
#endif
    }
}

/*------------------------------------------------------------------------*/

static int
is_signed (Lit * lit)
{
  return QUANTOR_SIGN (lit);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG5
/*------------------------------------------------------------------------*/

static const char *
assignment2str (Quantor * quantor, Lit * lit)
{
  assert (lit != QUANTOR_UNASSIGNED);

  if (lit == QUANTOR_TRUE)
    return "TRUE";

  if (lit == QUANTOR_FALSE)
    return "FALSE";

  sprintf (quantor->assignment_buffer, "%d", lit2int (quantor, lit));

  return quantor->assignment_buffer;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
log_assign (Quantor * quantor, const char *msg, Var * v, Lit * assignment)
{
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "%s VARIABLE %d TO %s",
       msg, v->idx, assignment2str (quantor, assignment));
#else
  (void) quantor;
  (void) msg;
  (void) v;
  (void) assignment;
#endif
}

/*------------------------------------------------------------------------*/

static void
assign_aux (Quantor * quantor, Var * v, Lit * assignment)
{
  assert (is_constant (assignment));
  assert (!is_assigned_var (v));

  quantor->stats.num_assigned++;
  if (v->exported)
    {
      assert (quantor->stats.num_unassigned_exported > 0);
      quantor->stats.num_unassigned_exported--;
    }

  mark_to_be_reordered (quantor, v);
  v->assignment = assignment;
  log_assign (quantor, "ASSIGNING", v, assignment);
}

/*------------------------------------------------------------------------*/

static void
assign_non_constant (Quantor * quantor, Var * u, Lit * assignment)
{
  int sign;
  Var *v;

  assert (assignment != QUANTOR_UNASSIGNED);
  assert (!is_constant (assignment));

  log_assign (quantor, "ASSIGNING NON CONSTANT", u, assignment);

  v = assignment->var;
  assert (u != v);

  if (u->eqclass && v->eqclass)
    {
      sign = QUANTOR_SIGN (assignment) ^ u->eqclass_sign ^ v->eqclass_sign;
      merge_delete_EquivalenceClass (quantor, u->eqclass, v->eqclass, sign);
    }
  else if (u->eqclass && !v->eqclass)
    {
      if (u->eqclass_sign)
	assignment = QUANTOR_NOT (assignment);
      add_EquivalenceClass (quantor, u->eqclass, assignment);
    }
  else if (!u->eqclass && v->eqclass)
    {
      sign = QUANTOR_SIGN (assignment) ^ v->eqclass_sign;
      assignment = var2lit (u, sign);
      add_EquivalenceClass (quantor, v->eqclass, assignment);
    }
  else
    {
      assert (!u->eqclass && !v->eqclass);
      new_EquivalenceClass (quantor, u);
      add_EquivalenceClass (quantor, u->eqclass, assignment);
    }
}

/*------------------------------------------------------------------------*/

static Lit *
deref (Quantor * quantor, Lit * lit)
{
  Lit *assignment, *res;
  int sign;
  Var *v;

  (void) quantor;

  if (is_constant (lit) || lit == QUANTOR_UNASSIGNED)
    return lit;

  v = lit->var;
  sign = QUANTOR_SIGN (lit);

  if (v->eqclass)
    {
      sign ^= v->eqclass_sign;
      v = v->eqclass->representative;
      sign ^= v->eqclass_sign;
    }

  assignment = v->assignment;

  if (assignment == QUANTOR_UNASSIGNED)
    {
      res = var2lit (v, sign);
      assert (v->eqclass || res == lit);
    }
  else
    {
      assert (is_constant (assignment));
      res = sign ? QUANTOR_NOT (assignment) : assignment;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void update_clauses_after_assigning_literal (Quantor *, Lit *);
static void register_unit (Quantor *, Lit *);

/*------------------------------------------------------------------------*/

static int
assign_constant (Quantor * quantor, Var * v, Lit * assignment)
{
  unsigned count_non_zombie;
  EquivalenceClass *ec;
  Lit *lit, *tmp;
  Var *p, *next;
  int res;

  assert (is_constant (assignment));

  ec = v->eqclass;

  if (ec)
    {
      count_non_zombie = 0;
      for (p = ec->elems.first; p; p = p->eqclass_link.next)
	if (!p->zombie)
	  count_non_zombie++;

      if (count_non_zombie > 1)
	{
	  for (p = ec->elems.first; p; p = p->eqclass_link.next)
	    {
	      if (is_universal (p))
		{
#ifdef QUANTOR_LOG2
		  LOG (quantor, 2, 
     "CONFLICT FROM ASSIGNING EQUIVALENCE CLASS %d OF UNIVERSAL VARIABLE %d",
		       ec->idx, v->idx);
#endif
		  register_invalidity (quantor);
		  return 0;
		}
	    }
	}

#ifdef QUANTOR_LOG5
      if (v != ec->representative)
	LOG (quantor, 5, "DELAYING ASSIGNMENT OF %d TO %s",
	     v->idx, assignment2str (quantor, assignment));
#endif
      /* Normalize this assignment to the equivalence class representative.
       */
      if (v->eqclass_sign != ec->representative->eqclass_sign)
	assignment = QUANTOR_NOT (assignment);

      if (v != ec->representative)
	{
	  v = ec->representative;
#ifdef QUANTOR_LOG5
	  LOG (quantor, 5,
	       "ASSIGNING %s TO REPRESENTATIVE %d OF EQUIVALENCE CLASS %d",
	       assignment2str (quantor, assignment),
	       ec->representative->idx, ec->idx);
#endif
	}
    }

  res = 1;
  assign_aux (quantor, v, assignment);

  if (!ec)
    return res;

  if (v->eqclass_sign)
    assignment = QUANTOR_NOT (assignment);

#ifdef QUANTOR_LOG5
  LOG (quantor, 5,
       "ASSIGNING %s TO REMAINING ELEMENTS OF EQUIVALENCE CLASS %d",
       assignment2str (quantor, assignment), ec->idx);
#endif

  /* Assign all literals in the equivalence class.
   */
  for (p = ec->elems.first; p; p = p->eqclass_link.next)
    {
      tmp = p->eqclass_sign ? QUANTOR_NOT (assignment) : assignment;

      if (p != v && p->assignment == QUANTOR_UNASSIGNED)
	{
	  res++;
	  assign_aux (quantor, p, tmp);
	}

      assert (p->assignment == tmp);
    }

  /* Finally update all clauses with literals in the equivalence class and
   * remove the variables from the equivalence class.
   */
  for (p = ec->elems.first; !quantor->invalid && p; p = next)
    {
      next = p->eqclass_link.next;

      tmp = p->eqclass_sign ? QUANTOR_NOT (assignment) : assignment;
      assert (p->assignment == tmp);

      lit = var2lit (p, (tmp == QUANTOR_FALSE));
      update_clauses_after_assigning_literal (quantor, lit);

      remove_from_EquivalenceClass (quantor, p);
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
check_assigned (Quantor * quantor, Var * v, Lit * b)
{
#ifndef NDEBUG
  Lit *a, *s, *t;

  a = var2lit (v, 0);
  s = deref (quantor, a);
  t = deref (quantor, b);
  assert (s == t);
#else
  (void) quantor;
  (void) v;
  (void) b;
#endif
}

/*------------------------------------------------------------------------*/

static void
register_illegal_variable_assignment (Quantor * quantor, Var * a, Var * b)
{
#ifdef QUANTOR_LOG2
  LOG (quantor, 2,
       "ILLEGAL ASSIGNMENT OF VARIABLE %d TO VARIABLE %d", a->idx, b->idx);
#else
  (void) a;
  (void) b;
#endif
  register_invalidity (quantor);
}

/*------------------------------------------------------------------------*/

static int
conflict_from_illegal_assignment (Quantor * quantor, Var * a, Var * e)
{
  Var *tmp;

  (void) quantor;
  assert (a != e);

  if (is_existential (a) && is_existential (e))
    return 0;

  if (is_universal (a) && is_universal (e))
    return 1;

  if (is_universal (e))
    {
      tmp = a;
      a = e;
      e = tmp;
    }

  assert (is_universal (a));
  assert (is_existential (e));

  if (a->scope->nesting > e->scope->nesting)
    return 1;

  assert (a->scope != e->scope);

  return 0;
}

/*------------------------------------------------------------------------*/
/* Returns the number of variables assigned to constants.
 */
static int
assign (Quantor * quantor, Var * v, Lit * assignment)
{
  int res;

  if (is_constant (assignment))
    {
      res = assign_constant (quantor, v, assignment);
      if (res)
	check_assigned (quantor, v, assignment);
    }
  else if (conflict_from_illegal_assignment (quantor, v, assignment->var))
    {
      register_illegal_variable_assignment (quantor, v, assignment->var);
      res = 0;
    }
  else
    {
      /* The check for conflicts from illegal assignments is not fully
       * completed for this equivalence class.  It is just delayed until the
       * actual substitution takes place.
       */
      assign_non_constant (quantor, v, assignment);
      check_assigned (quantor, v, assignment);
      res = 0;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
register_conflicting_unit (Quantor * quantor, Lit * lit)
{
#ifdef QUANTOR_LOG2
  LOG (quantor, 2, "CONFLICTING NEW UNIT %d", lit2int (quantor, lit));
#endif
  register_invalidity (quantor);
}

/*------------------------------------------------------------------------*/

static void
register_unit (Quantor * quantor, Lit * lit)
{
  Lit *assignment;
  Var *v;

  v = lit->var;

  if (is_proposed_unate (quantor, v))
    unpropose_unate (quantor, v);

  if (is_unate (quantor, v))
    unregister_unate (quantor, v);

  INCSTATS1 (quantor->stats.units);

  assignment = lit2const (lit);

  if (is_universal (v))
    {
      assert (v->assignment == QUANTOR_UNASSIGNED);
#ifdef QUANTOR_LOG2
      LOG (quantor, 2, "CONFLICT FROM UNIVERSAL UNIT %d",
	   lit2int (quantor, lit));
#endif
      register_invalidity (quantor);
    }
  else if (is_assigned_var (v))
    {
      if (v->assignment != assignment)
	register_conflicting_unit (quantor, lit);
    }
  else if (is_unit (quantor, v))
    {
      assert (v->unit_assignment != QUANTOR_UNASSIGNED);

      if (v->unit_assignment != assignment)
	register_conflicting_unit (quantor, lit);
    }
  else
    {
      log_var (quantor, "REGISTERED", "UNIT", lit);
      dlink (&quantor->units, v, &v->unit_link);
      assert (v->unit_assignment == QUANTOR_UNASSIGNED);
      v->unit_assignment = assignment;
    }
}

/*------------------------------------------------------------------------*/

static void
print_clause (Quantor * quantor, Clause * clause, FILE * file)
{
  Cell *c, *eor;
  int lit;

  eor = end_of_row (clause);
  for (c = clause->row; c < eor; c++)
    {
      lit = lit2int (quantor, c->lit);
      fprintf (file, "%d ", lit);
    }

  fprintf (file, "0\n");
}

/*------------------------------------------------------------------------*/

static void
check_invariant (Quantor * quantor, int check)
{
#ifdef QUANTOR_CHECK
  if (quantor->opts.check < check)
    return;

  check_vars (quantor);
  check_cells (quantor);
  check_equivalence_classes (quantor);
#else
  (void) quantor;
  (void) check;
#endif
}

/*------------------------------------------------------------------------*/

static void
delete_Var (Quantor * quantor, Var * v, int finally)
{
  int idx;

  assert (!v->mark);
  assert (!v->dead);

  v->dead = 1;

#ifdef QUANTOR_STATS1
  dec_count_stats (&quantor->stats.vars);
#endif
  idx = v->idx;
#ifdef QUANTOR_LOG5
  if (!finally)
    LOG (quantor, 5, "GC VARIABLE %d", idx);
#else
  (void) finally;
#endif

  if (v->assignment != QUANTOR_UNASSIGNED)
    {
      assert (quantor->stats.num_assigned > 0);
      quantor->stats.num_assigned--;
    }

  if (v->eqclass)
    remove_from_EquivalenceClass (quantor, v);

  delete_literal_pair (quantor, v->lits);

  push_IntStack (quantor, &quantor->free_var_indices, idx);
  quantor->vars.start[idx] = 0;

#ifdef QUANTOR_FAST_ALLOC
  put_back_on_free_list (quantor, &quantor->free_vars, v);
#else
  delete (quantor, v, sizeof (*v));
#endif
}

/*------------------------------------------------------------------------*/

static void
remove_from_order (Quantor * quantor, PtrStack * order, Var * v)
{
  int pos, last;
  Var *tmp;

  last = count_PtrStack (order) - 1;
  assert (v->rank >= 0);
  assert (v->rank <= last);
  pos = v->rank;
  assert (order->start[pos] == v);
  if (pos < last)
    {
      swap_order (quantor, order, pos, last);
      assert (v->rank == last);
    }
  tmp = pop_PtrStack (quantor, order);
  assert (tmp == v);
  (void) tmp;
  v->rank = -1;

  if (pos < last)
    fix_order (quantor, order, pos);
}

/*------------------------------------------------------------------------*/

static void
remove_from_scope (Quantor * quantor, Var * v)
{
  Scope *scope = v->scope;

#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "REMOVING VARIABLE %d FROM %s SCOPE %s",
       v->idx, quantificationtype2str (scope->type),
       scope2str (quantor, scope));
#endif
  undlink_if_linked (&scope->reorder, v, &v->reorder_link);

  remove_from_order (quantor, &scope->order, v);

  undlink (&scope->vars, v, &v->scope_link);
  v->scope = 0;
}

/*------------------------------------------------------------------------*/

static void
delete_Scope (Quantor * quantor, Scope * scope)
{
  Var *p, *next;

  for (p = scope->vars.first; p; p = next)
    {
      next = p->scope_link.next;
      delete_Var (quantor, p, 1);
    }

  release_PtrStack (quantor, &scope->order);

  undlink (&quantor->scopes, scope, &scope->link);
  delete (quantor, scope, sizeof (*scope));
}

/*------------------------------------------------------------------------*/

static void
register_zombie (Quantor * quantor, Var * v)
{
  Scope *scope;

  unpropose_zombie (quantor, v);

  assert (var_does_not_occur (v));
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "REGISTERED ZOMBIE %d", v->idx);
#endif

  if (v->eqclass)
    {
      v->eqclass->num_zombies++;
      assert (v->eqclass->num_zombies <= v->eqclass->elems.len);
    }

  v->zombie = 1;

  if (v->exported)
    {
      dlink (&quantor->zombies, v, &v->zombie_link);
    }
  else
    {
      assert (!is_unit (quantor, v));

      if (is_unate (quantor, v))
	unregister_unate (quantor, v);

      scope = v->scope;
      (void) scope;
      remove_from_scope (quantor, v);
      delete_Var (quantor, v, 0);
    }
}

/*------------------------------------------------------------------------*/

static void
add_Cell (Quantor * quantor, Cell * cell, Clause * clause, Lit * lit)
{
  int was_proposed_zombie, was_proposed_unate;
  Lit *tmp;
  Var *v;

  assert (cell >= clause->row && cell < end_of_row (clause));

  v = lit->var;
  if (!clause->scope || clause->scope->nesting < v->scope->nesting)
    connect_clause_to_scope (quantor, clause, v->scope);

  was_proposed_unate = is_proposed_unate (quantor, v);
  was_proposed_zombie = is_proposed_zombie (quantor, v);

  if (was_proposed_zombie)
    unpropose_zombie (quantor, v);

  cell->lit = lit;
  dlink (&lit->column, cell, &cell->column_link);

  cell->clause = clause;
  lit->sum += clause->size;

  mark_to_be_reordered (quantor, lit->var);

  if (was_proposed_zombie && !was_proposed_unate)
    propose_unate (quantor, lit);

  tmp = lit2const (lit);
  if (is_universal (v))
    tmp = QUANTOR_NOT (tmp);

  if (was_proposed_unate && tmp != v->unate_assignment)
    unpropose_unate (quantor, v);

#ifdef QUANTOR_STATS1
  inc_count_stats (&quantor->stats.cells);
#endif
  quantor->stats.eliminated_cells++;
}

/*------------------------------------------------------------------------*/

static void
fix_sum_of_lits_in_clause (Quantor * quantor, Clause * clause, int delta)
{
  Cell *c, *eor;
  Lit *lit;

  eor = end_of_row (clause);
  for (c = clause->row; c < eor; c++)
    {
      lit = c->lit;
      assert (delta >= 0 || lit->sum >= (unsigned) -delta);
      lit->sum += delta;
      mark_to_be_reordered (quantor, lit->var);
    }
}

/*------------------------------------------------------------------------*/

static void
remove_Cell (Quantor * quantor, Cell * cell)
{
  Clause *clause;
  Lit *lit;
  Var *v;

  lit = cell->lit;
  undlink (&lit->column, cell, &cell->column_link);

  clause = cell->clause;
  assert (clause->state == QUANTOR_GARBAGE_CLAUSE);
  (void) clause;

  v = lit->var;

  mark_to_be_reordered (quantor, v);

  if (var_does_not_occur (v))
    propose_zombie (quantor, v);
  else if (!v->eliminated && !is_assigned_var (v) && lit_does_not_occur (lit))
    propose_unate (quantor, QUANTOR_NOT (lit));

#ifdef QUANTOR_STATS1
  dec_count_stats (&quantor->stats.cells);
#endif
  quantor->stats.eliminated_cells--;
}

/*------------------------------------------------------------------------*/

static int
lit2mark (Lit * lit)
{
  return is_signed (lit) ? -1 : 1;
}

/*------------------------------------------------------------------------*/

static void
mark_var (Quantor * quantor, Var * v, int mark)
{
  assert (!v->mark);
  assert (mark);

  v->mark = mark;
  push_PtrStack (quantor, &quantor->marked_vars, v);
}

/*------------------------------------------------------------------------*/

static void
unmark_vars (Quantor * quantor)
{
  void **p;
  Var *v;

  for (p = quantor->marked_vars.start; p < quantor->marked_vars.top; p++)
    {
      v = *p;
      assert (v->mark);
      v->mark = 0;
    }

  reset_PtrStack (quantor, &quantor->marked_vars, 0);
}

/*------------------------------------------------------------------------*/

static int
new_clause_is_trivial (Quantor * quantor, const char *msg)
{
  int res, mark;
  void **p;
  Lit *lit;
  Var *v;

  assert (!count_PtrStack (&quantor->marked_vars));

  res = 0;

  for (p = quantor->new_clause.start;
       !res && p < quantor->new_clause.top; p++)
    {
      lit = *p;
      lit = deref (quantor, lit);

      if (lit == QUANTOR_FALSE)
	continue;

      if (lit == QUANTOR_TRUE)
	{
	  res = 1;
	  break;
	}

      v = lit->var;

      mark = lit2mark (lit);
      if (v->mark == -mark)
	{
	  res = 1;
#ifdef QUANTOR_LOG7
	  LOG (quantor, 7,
	       "VARIABLE %d OCCURS POSITIVE AND NEGATIVE", v->idx);
#else
	  (void) msg;
#endif
	}
      else if (v->mark != mark)
	mark_var (quantor, v, mark);
    }

  unmark_vars (quantor);

  if (res)
    {
      INCSTATS2 (quantor->stats.trivial_clauses);
#ifdef QUANTOR_LOG6
      LOG (quantor, 6, "%s TRIVIAL CLAUSE", msg);
#endif
    }

  return res;
}

/*------------------------------------------------------------------------*/

static int
get_max_existential_nesting_of_new_clause (Quantor * quantor)
{
  int res, nesting;
  Scope *scope;
  void **p;
  Lit *lit;
  Var *v;

  res = -1;

  for (p = quantor->new_clause.start; p < quantor->new_clause.top; p++)
    {
      lit = *p;
      lit = deref (quantor, lit);

      if (lit == QUANTOR_FALSE)
	continue;

      assert (!is_constant (lit));

      v = lit->var;
      scope = v->scope;
      if (scope->type == QUANTOR_EXISTENTIAL)
	{
	  nesting = scope->nesting;
	  if (nesting > res)
	    res = nesting;
	}
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
simplify_new_clause (Quantor * quantor)
{
  int trivial, count, max_existential, mark;
  void **p, **q;
  Lit *lit;
  Var *v;

  /* All lits with a deeper nesting than the maximal exisential literal
   * in this clause can be removed.  This simplification is usually referred
   * to as 'forall reduction' and subsumes 'QBF resolution'.  Here is an
   * example (we write A instead of \forall and E instead of \exists):
   *
   *     E a A b E c A d E e [ (a | b | d ) & f ]
   * ==  E a A b E c A d [ (a | b | d ) & E e [ f ] ]
   * ==  E a A b E c [ A d [ a | b | d ] & A d E e [ f ] ]
   * ==  E a A b E c [ ( a | b ) & A d E e [ f ] ]
   * ==  E a A b [ ( a | b ) & E c A d E e [ f ] ]
   * ==  E a [ A b [ a | b ] & A b E c A d E e [ f ] ]
   * ==  E a [ a & A b E c A d E e [ f ] ]
   * ==  E a A b E c A d E e [ a & f ]
   *
   * NOTE: detection of trivial clauses and removing lits via forall
   * reduction in the same loop may be tricky!  A problematic example
   * is '\forall a [ a | !a ]'.
   */
  max_existential = get_max_existential_nesting_of_new_clause (quantor);

  count = 0;
  trivial = 0;
  q = quantor->new_clause.start;
  (void) trivial;

  for (p = quantor->new_clause.start; p < quantor->new_clause.top; p++)
    {
      lit = *p;
      lit = deref (quantor, lit);

      if (lit == QUANTOR_FALSE)
	{
	  INCSTATS2 (quantor->stats.false_lits);
#ifdef QUANTOR_LOG7
	  LOG (quantor, 7,
	       "SKIPPING FALSE LITERAL %d", lit2int (quantor, *p));
#endif
	}
      else
	{
	  v = lit->var;
	  mark = lit2mark (lit);

	  if (v->mark != mark)
	    {
	      if (v->scope->nesting > max_existential)
		{
		  assert (is_universal (v));
		  INCSTATS2 (quantor->stats.forall_reduced_lits);
#ifdef QUANTOR_LOG7
		  LOG (quantor, 7,
		       "FORALL REDUCE %d", lit2int (quantor, lit));
#endif
		}
	      else
		{
		  mark_var (quantor, v, mark);
		  *q++ = lit;
		  count++;
		}
	    }
	  else
	    {
#ifdef QUANTOR_LOG7
	      LOG (quantor, 7,
		   "MULTIPLE OCCURRENCE %d", lit2int (quantor, lit));
#endif
	      INCSTATS2 (quantor->stats.duplicated_lits);
	      assert (v->mark == mark);
	    }
	}
    }

  reset_PtrStack (quantor, &quantor->new_clause, count);
  unmark_vars (quantor);
}

/*------------------------------------------------------------------------*/

static void
log_clause (Quantor * quantor, const char *msg, Clause * clause)
{
#ifdef QUANTOR_LOG6
  if (quantor->opts.verbose < 6)
    return;

  fprintf (quantor->io.out, "%s%s CLAUSE %d : ", quantor_prefix (quantor),
	   msg, clause->idx);
  print_clause (quantor, clause, quantor->io.out);
#else
  (void) quantor;
  (void) msg;
  (void) clause;
#endif
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_SIGREF
/*------------------------------------------------------------------------*/

static void
sub_sig (Quantor * quantor, Lit * lit, Signature sig)
{
  int i;

  if (quantor->opts.sigref)
    {
      assert (lit->sigref);

      for (i = 0; i < sizeof (Signature) * 8; i++)
	{
	  if (sig & (((Signature) 1) << i))
	    {
	      if (lit->sigref[i] < 255)
		{
		  assert (lit->sigref[i] > 0);
		  lit->sigref[i] -= 1;
		  if (!lit->sigref[i])
		    lit->sigsum &= ~(((Signature) 1) << i);
		}
	    }
	}
    }
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifndef NDEBUG
/*------------------------------------------------------------------------*/

static int
is_dying_Function (Quantor * quantor, Function * function)
{
  return is_linked (&quantor->dying_functions,
		    function, &function->dying_link);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
is_unprocessed_clause (Quantor * quantor, Clause * clause)
{
  int res;

  (void) quantor;
  res = (clause->processing_type != QUANTOR_NO_PROCESSING);

  assert (res == is_linked (&quantor->unprocessed_clauses,
			    clause, &clause->unprocessed_clauses_link));

  return res;
}

/*------------------------------------------------------------------------*/

static void
remove_unprocessed_clauses (Quantor * quantor, Clause * clause)
{
  assert (is_unprocessed_clause (quantor, clause));
  undlink (&quantor->unprocessed_clauses,
	   clause, &clause->unprocessed_clauses_link);
  clause->processing_type = QUANTOR_NO_PROCESSING;
#ifdef QUANTOR_LOG7
  LOG (quantor, 7,
       "REMOVING CLAUSE %d FROM UNPROCESSED CLAUSES", clause->idx);
#endif
}

/*------------------------------------------------------------------------*/

static void
mark_clause_as_unprocessed (Quantor * quantor,
			    Clause * clause, ClauseProcessingType type)
{
  assert (quantor->opts.forward_processing ||
	  quantor->opts.backward_processing);

  assert (!is_unprocessed_clause (quantor, clause));

  assert (type != QUANTOR_NO_PROCESSING);
  clause->processing_type = type;
  dlink (&quantor->unprocessed_clauses,
	 clause, &clause->unprocessed_clauses_link);
#ifdef QUANTOR_LOG7
  LOG (quantor, 7, "MARKING CLAUSE %u AS UNPROCESSED CLAUSE", clause->idx);
#else
  (void) quantor;
#endif
}

/*------------------------------------------------------------------------*/

static void
kill_Function (Quantor * quantor, Function * function)
{
  assert (!is_dying_Function (quantor, function));
  dlink (&quantor->dying_functions, function, &function->dying_link);
#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "KILLING FUNCTION %d", function->idx);
#endif
}

/*------------------------------------------------------------------------*/

static void
kill_Clause (Quantor * quantor, Clause * clause)
{
  Cell *c, *eor;
  Function *f;

  if (is_dying_clause (quantor, clause))
    return;

  assert (clause->state == QUANTOR_ACTIVE_CLAUSE);
  clause->state = QUANTOR_DYING_CLAUSE;
  assert (quantor->stats.num_active_clauses > 0);
  quantor->stats.num_active_clauses--;

  if (quantor->opts.recalc_sigs != 0 && quantor->recalc_sigs_count_down > 0)
    quantor->recalc_sigs_count_down--;

#ifdef QUANTOR_SIGREF
  if (quantor->opts.sigref)
    {
      int i;
      for (i = 0; i < clause->size; i++)
	sub_sig (quantor, clause->row[i].lit, clause->sig);
    }
#endif

#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "KILLING CLAUSE %d", clause->idx);
#endif
  dlink (&quantor->dying_clauses, clause, &clause->dying_link);
  clause->state = QUANTOR_DYING_CLAUSE;

  if (is_unprocessed_clause (quantor, clause))
    remove_unprocessed_clauses (quantor, clause);

  for (f = clause->functions.first; f; f = f->clause_link.next)
    kill_Function (quantor, f);

  if (clause->size == 2)
    {
      eor = end_of_row (clause);
      for (c = clause->row; c < eor; c++)
	undlink (&c->lit->binary_clauses, c, &c->binary_clauses_link);
    }
}

/*------------------------------------------------------------------------*/

static void
calculate_ldsig (Quantor * quantor, Lit * lit)
{
  unsigned hash, combined, ldsig;

  assert (!lit->valid_ldsig);

  hash = hash_lit (quantor, lit);
  assert (sizeof (Signature) * 8 <= (1 << 8));
  /* TODO: XOR with upper 8 bits (hash >> 24) as well */
  combined = hash ^ (hash >> 16) ^ (hash >> 8);
  ldsig = combined & (sizeof (Signature) * 8 - 1);
  lit->ldsig = ldsig;
  lit->valid_ldsig = 1;
#ifdef QUANTOR_LOG8
  LOG (quantor, 8, "LDSIG LITERAL %d IS %d", lit2int (quantor, lit), ldsig);
#endif
}

/*------------------------------------------------------------------------*/

static Signature
sig_lit (Quantor * quantor, Lit * lit)
{
  Signature res;

  if (!lit->valid_ldsig)
    calculate_ldsig (quantor, lit);

  res = ((Signature) 1) << lit->ldsig;
  assert (res);

  return res;
}

/*------------------------------------------------------------------------*/

static Signature
sig_clause (Quantor * quantor, Clause * clause)
{
  Signature res;
  int i, size;

  res = (Signature) 0;
  size = clause->size;
  for (i = 0; i < size; i++)
    res |= sig_lit (quantor, clause->row[i].lit);

  return res;
}

/*------------------------------------------------------------------------*/

static void
add_sig (Quantor * quantor, Lit * lit, Signature sig)
{
#ifdef QUANTOR_SIGREF
  int i;
#endif

  (void) quantor;
  lit->sigsum |= sig;

#ifdef QUANTOR_SIGREF
  if (quantor->opts.sigref)
    {
      assert (lit->sigref);

      for (i = 0; i < sizeof (Signature) * 8; i++)
	{
	  if (sig & (((Signature) 1) << i))
	    {
	      if (lit->sigref[i] == 254)
		INCSTATS2 (sticky_sigref);

	      if (lit->sigref[i] < 255)
		lit->sigref[i] += 1;
	    }
	}
    }
#endif
}

/*------------------------------------------------------------------------*/

static int
reset_sig (Quantor * quantor, Lit * lit)
{
  int res;

  (void) quantor;
  if (lit->sigsum)
    {
      lit->sigsum = (Signature) 0;
#ifdef QUANTOR_SIGREF
      if (quantor->opts.sigref)
	{
	  assert (lit->sigref);
	  memset (lit->sigref, 0, sizeof (Signature) * 8);
	}
#endif

      res = 1;
    }
  else
    res = 0;

  return res;
}

/*------------------------------------------------------------------------*/

static void
clear_sigs (Quantor * quantor)
{
  unsigned count_vars;
  Scope *scope;
  Var *v;

  count_vars = 0;

  for (scope = quantor->scopes.first; scope; scope = scope->link.next)
    {
      for (v = scope->vars.first; v; v = v->scope_link.next)
	{
	  count_vars += reset_sig (quantor, v->lits + 1);
	  count_vars += reset_sig (quantor, v->lits + 0);
	}
    }

#ifdef QUANTOR_LOG3
  LOG (quantor, 3, "CLEARED SIGNATURE OF %u LITERALS", count_vars);
#endif
}

/*------------------------------------------------------------------------*/

static void
recalc_sigs (Quantor * quantor)
{
  int i, size, count_clauses, count_lits;
  Scope *scope;
  Clause *c;

#ifdef QUANTOR_SIGREF
  if (quantor->opts.sigref)
    return;
#endif

  quantor->recalc_sigs_count_down =
    quantor->stats.num_active_clauses * quantor->opts.recalc_sigs;

  INCSTATS2 (quantor->stats.num_recalc_sigs);

  clear_sigs (quantor);
  count_clauses = count_lits = 0;

  for (scope = quantor->scopes.first; scope; scope = scope->link.next)
    {
      for (c = scope->clauses.first; c; c = c->scope_link.next)
	{
	  if (!is_dying_clause (quantor, c))
	    {
	      count_clauses++;
	      size = c->size;

	      for (i = 0; i < size; i++)
		{
		  add_sig (quantor, c->row[i].lit, c->sig);
		  count_lits++;
		}
	    }
	}
    }

#ifdef QUANTOR_LOG3
  LOG (quantor, 3,
       "RECALCULATED SIGNATURES OF %d LITERAL OCCURENCES IN %d CLAUSES",
       count_lits, count_clauses);
#endif
}

/*------------------------------------------------------------------------*/

static void
initialize_clause_sig (Quantor * quantor, Clause * clause)
{
  Signature sig;
  int i, size;

  if (!quantor->recalc_sigs_count_down)
    recalc_sigs (quantor);

  sig = sig_clause (quantor, clause);
  clause->sig = sig;
#ifdef QUANTOR_LOG8
  if (quantor->opts.verbose >= 8)
    {
      fprintf (quantor->io.out, "%sSIG CLAUSE %d : ",
	       quantor_prefix (quantor), clause->idx);

      for (i = sizeof (Signature) * 8 - 1; i >= 0; i--)
	{
	  if (i != sizeof (Signature) * 8 - 1 && !((i + 1) % 8))
	    fputc ('.', quantor->io.out);

	  fputc ((sig & (((Signature) 1) << i)) ? '1' : '0', quantor->io.out);
	}

#ifdef QUANTOR_SIG64
      {
	fprintf (quantor->io.out, " = 0x%08x", (unsigned) (sig >> 32));
	fprintf (quantor->io.out, "%08x\n", (unsigned) sig);
      }
#else
      fprintf (quantor->io.out, " = 0x%08x\n", (unsigned) sig);
#endif
    }
#endif

  size = clause->size;
  for (i = 0; i < size; i++)
    add_sig (quantor, clause->row[i].lit, sig);
}

/*------------------------------------------------------------------------*/

static void
set_clause_idx (Quantor * quantor, Clause * clause)
{
  int idx, count;

  count = count_PtrStack (&quantor->idx2clause);

  if (quantor->opts.recycle_indices &&
      count_IntStack (&quantor->free_clause_indices))
    {
      idx = pop_IntStack (&quantor->free_clause_indices);
      INCSTATS2 (quantor->stats.recycled_clauses);
#ifdef QUANTOR_LOG6
      LOG (quantor, 6, "RECYCLING INDEX OF CLAUSE %d", idx);
#endif
      assert (idx < count);
      quantor->idx2clause.start[idx] = clause;
    }
  else
    {
      idx = count;
      push_PtrStack (quantor, &quantor->idx2clause, clause);
    }

  clause->idx = idx;
}

/*------------------------------------------------------------------------*/

static size_t
sizeof_Clause (int size)
{
  size_t res;

  res = (size_t) & (((Clause *) 0)->row[0]);
  res += sizeof (Cell) * size;

  return res;
}

/*------------------------------------------------------------------------*/

static Clause *
alloc_Clause (Quantor * quantor, int size)
{
  Clause *res;

  assert (size >= 0);
  res = new (quantor, sizeof_Clause (size));
  res->size = size;

#ifdef QUANTOR_STATS1
  inc_count_stats (&quantor->stats.clauses);
#endif
  res->state = QUANTOR_ALLOCATED_CLAUSE;

  return res;
}

/*------------------------------------------------------------------------*/

static int
cmp_int (int a, int b)
{
  return a - b;
}

/*------------------------------------------------------------------------*/

static int
cmp_var (Var * a, Var * b)
{
  int res;

  assert (a != b);

  res = cmp_int (a->idx, b->idx);

  return res;
}

/*------------------------------------------------------------------------*/

static int
cmp_lit (Lit * a, Lit * b)
{
  int res;

  assert (a != b);
  assert (QUANTOR_NOT (a) != b);

  res = cmp_var (a->var, b->var);

  return res;
}

/*------------------------------------------------------------------------*/

static int
litptrptrcmp4qsort (const void *p0, const void *p1)
{
  Lit *l0, *l1;
  int res;

  assert (p0 != p1);

  l0 = *(Lit **) p0;
  l1 = *(Lit **) p1;

  res = cmp_lit (l0, l1);

  return res;
}

/*------------------------------------------------------------------------*/
#ifndef NDEBUG
/*------------------------------------------------------------------------*/

static void
check_new_clause_is_sorted (Quantor * quantor)
{
  void **p;

  if (!count_PtrStack (&quantor->new_clause))
    return;

  for (p = quantor->new_clause.start; p < quantor->new_clause.top - 1; p++)
    assert (cmp_lit (p[0], p[1]) < 1);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
sort_new_clause (Quantor * quantor)
{
  void **start;
  size_t n;

  n = count_PtrStack (&quantor->new_clause);
  start = quantor->new_clause.start;

  qsort (start, n, sizeof (*start), litptrptrcmp4qsort);
#ifndef NDEBUG
  check_new_clause_is_sorted (quantor);
#endif
}

/*------------------------------------------------------------------------*/

static void
copy_new_clause (Quantor * quantor, const char *msg, Clause * clause)
{
  Cell *c, *eor;
  void **p;
  int i;

  assert (clause->size == count_PtrStack (&quantor->new_clause));

  i = 0;
  for (p = quantor->new_clause.start; p < quantor->new_clause.top; p++)
    add_Cell (quantor, clause->row + i++, clause, *p);

  assert (!clause->size ||
	  (clause->scope && clause->scope->sum >= clause->size));

  if (clause->size == 2)
    {
      eor = end_of_row (clause);
      for (c = clause->row; c < eor; c++)
	dlink (&c->lit->binary_clauses, c, &c->binary_clauses_link);
    }

  clause->state = QUANTOR_ACTIVE_CLAUSE;
  quantor->stats.num_active_clauses++;

  log_clause (quantor, msg, clause);
}

/*------------------------------------------------------------------------*/

static unsigned
hash_Clause (Quantor * quantor, Clause * clause)
{
  Cell *p, *eor;
  unsigned res;
  Lit *lit;

  res = 0;
  eor = end_of_row (clause);

  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      res = extend_literal_hash_value (quantor, res, lit);
    }
#ifdef QUANTOR_LOG8
  LOG (quantor, 8, "HASHED CLAUSE %d TO 0x%08x", clause->idx, res);
#endif
  return res;
}

/*------------------------------------------------------------------------*/

static int
eq_Clause (Quantor * quantor, Clause * a, Clause * b)
{
  Cell *p, *q, *aeor, *beor;
  Lit *al, *bl;
  int res;

  (void) quantor;
  (void) beor;
  res = 0;

  if (a->size == b->size)
    {
      res = 1;
      p = a->row;
      q = b->row;
      aeor = end_of_row (a);
      beor = end_of_row (b);

      while (res && p < aeor)
	{
	  assert (q < beor);
	  al = p++->lit;
	  bl = q++->lit;
	  res = (al == bl);
	}

      assert (!res || q == beor);
    }

  return res;
}

/*------------------------------------------------------------------------*/

static Clause **
findpos_BinDB (Quantor * quantor, BinDB * bindb, Clause * clause,
	       int contained)
{
  Clause **p;
  unsigned h;

  assert (quantor->opts.equivalences);

  h = hash_Clause (quantor, clause);
  h &= bindb->size - 1;
  assert (h < bindb->size);
  p = bindb->table + h;
  while (*p &&
	 (!contained || *p != clause) &&
	 (contained || !eq_Clause (quantor, *p, clause)))
    p = &(*p)->next_in_bindb;

  return p;
}

/*------------------------------------------------------------------------*/

static Clause *
find_binary_clause (Quantor * quantor, Lit * a, Lit * b)
{
  unsigned h;
  Clause *res;
  Lit *tmp;

  assert (a != b);
  assert (QUANTOR_NOT (a) != b);
  assert (quantor->opts.equivalences);

  if (cmp_lit (a, b) > 0)
    {
      tmp = a;
      a = b;
      b = tmp;
    }

  assert (cmp_lit (a, b) < 0);

  h = extend_literal_hash_value (quantor, 0, a);
  assert (h == hash_lit (quantor, a));
  h = extend_literal_hash_value (quantor, h, b);
  h &= quantor->bindb->size - 1;
  assert (h < quantor->bindb->size);

  for (res = quantor->bindb->table[h]; res; res = res->next_in_bindb)
    {
      assert (res->size == 2);

      if (res->row[0].lit != a)
	continue;

      if (res->row[1].lit != b)
	continue;

      return res;
    }

  return 0;
}

/*------------------------------------------------------------------------*/

static Clause *
find_implication (Quantor * quantor, Lit * cond, Lit * concl)
{
  return find_binary_clause (quantor, QUANTOR_NOT (cond), concl);
}

/*------------------------------------------------------------------------*/

static void
resize_BinDB (Quantor * quantor, BinDB * bindb)
{
  Clause **old_table, *p, *next;
  unsigned old_size, h, i;
  size_t bytes;

  assert (quantor->opts.equivalences);

  old_table = bindb->table;
  old_size = bindb->size;

  bindb->size *= 2;
  bytes = bindb->size * sizeof (bindb->table[0]);
  bindb->table = new (quantor, bytes);

  for (i = 0; i < old_size; i++)
    {
      for (p = old_table[i]; p; p = next)
	{
	  next = p->next_in_bindb;
	  h = hash_Clause (quantor, p);
	  h &= bindb->size - 1;
	  assert (h < bindb->size);
	  p->next_in_bindb = bindb->table[h];
	  bindb->table[h] = p;
	}
    }

  bytes = old_size * sizeof (old_table[0]);
  delete (quantor, old_table, bytes);
}

/*------------------------------------------------------------------------*/

static void
insert_BinDB (Quantor * quantor, BinDB * bindb, Clause * clause)
{
  Clause **p;

  assert (quantor->opts.equivalences);
  assert (!clause->next_in_bindb);

  if (bindb->size <= bindb->count)
    resize_BinDB (quantor, bindb);

  p = findpos_BinDB (quantor, bindb, clause, 0);
  clause->next_in_bindb = *p;
  *p = clause;

  bindb->count++;
}

/*------------------------------------------------------------------------*/

static void
remove_BinDB (Quantor * quantor, BinDB * bindb, Clause * clause)
{
  Clause **p;

  assert (quantor->opts.equivalences);
  assert (bindb->count > 0);

  p = findpos_BinDB (quantor, bindb, clause, 1);
  assert (*p == clause);
  *p = clause->next_in_bindb;
  clause->next_in_bindb = 0;

  bindb->count--;
}

/*------------------------------------------------------------------------*/

static Clause *
find_dual_binary_clause (Quantor * quantor, Clause * clause)
{
  Lit *a, *b;

  assert (clause->size == 2);

  a = clause->row[0].lit;
  b = clause->row[1].lit;

  a = QUANTOR_NOT (a);
  b = QUANTOR_NOT (b);

  return find_binary_clause (quantor, a, b);
}

/*------------------------------------------------------------------------*/

static void
extract_unit_from_binary_clause_aux (Quantor * quantor,
				     Clause * new_clause, Lit * a, Lit * b)
{
  Clause *old_clause;

  assert (quantor->opts.binstrengthen);

  if (!(old_clause = find_binary_clause (quantor, a, QUANTOR_NOT (b))))
    return;

  INCSTATS2 (quantor->stats.binary_self_subsuming_resolution);
  INCSTATS2 (quantor->stats.self_subsuming_resolution);
  INCSTATS2 (quantor->stats.self_subsuming_resolution_lits);
#ifdef QUANTOR_LOG6
  LOG (quantor, 6,
       "UNIT %d THROUGH RESOLUTION OF CLAUSE %u AND CLAUSE %u",
       lit2int (quantor, a), old_clause->idx, new_clause->idx);
#else
  (void) new_clause;
#endif
  register_unit (quantor, a);
}

/*------------------------------------------------------------------------*/

static void
extract_unit_from_binary_clause (Quantor * quantor, Clause * clause)
{
  Lit *a, *b;

  assert (clause->size == 2);

  if (!quantor->opts.binstrengthen)
    return;

  a = clause->row[0].lit;
  b = clause->row[1].lit;

  extract_unit_from_binary_clause_aux (quantor, clause, a, b);
  extract_unit_from_binary_clause_aux (quantor, clause, b, a);
}

/*------------------------------------------------------------------------*/

static void
register_binary_clause (Quantor * quantor, Clause * clause)
{
  Clause *dual_clause;

#ifdef QUANTOR_STATS2
  inc_count_stats (&quantor->stats.binary_clauses);
#endif
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "REGISTERED BINARY CLAUSE %d", clause->idx);
#endif
  if (!quantor->opts.equivalences)
    return;

  insert_BinDB (quantor, quantor->bindb, clause);

  dual_clause = find_dual_binary_clause (quantor, clause);
  if (dual_clause)
    new_Equivalence (quantor, clause, dual_clause);

  extract_unit_from_binary_clause (quantor, clause);
}

/*------------------------------------------------------------------------*/
/* Returns non zero iff a clause with signature 'a' may subsume a clause with
 * signature 'b'.  This is the case if all signature bits set in 'a' are
 * also set in 'b'.  Alternatively we check that there is no unset bit in
 * 'b' which is set in 'a'.
 */
static int
sig_subset (Signature a, Signature b)
{
  return !(a & ~b);
}

/*------------------------------------------------------------------------*/

static int
is_unprocessed_lit (Quantor * quantor, Lit * lit)
{
  return is_linked (&quantor->unprocessed_literals,
		    lit, &lit->unprocessed_link);
}

/*------------------------------------------------------------------------*/

static void
mark_lit_as_unprocessed (Quantor * quantor, Lit * lit)
{
  assert (!is_unprocessed_lit (quantor, lit));
  dlink (&quantor->unprocessed_literals, lit, &lit->unprocessed_link);
#ifdef QUANTOR_LOG7
  LOG (quantor, 7, "MARKING LITERAL %d AS UNPROCESSED ",
       lit2int (quantor, lit));
#endif
}

/*------------------------------------------------------------------------*/

static void
remove_unprocessed_lit (Quantor * quantor, Lit * lit)
{
  assert (is_unprocessed_lit (quantor, lit));
  undlink (&quantor->unprocessed_literals, lit, &lit->unprocessed_link);
#ifdef QUANTOR_LOG7
  LOG (quantor, 7, "REMOVING LITERAL %d FROM UNPROCESSED LITERALS",
       lit2int (quantor, lit));
#endif
}

/*------------------------------------------------------------------------*/

static void
mark_clauses_resolvable_to_lit_as_unprocessed (Quantor * quantor, Lit * lit)
{
  Clause *clause;
  Cell *p;

  for (p = lit->column.first; p; p = p->column_link.next)
    {
      clause = p->clause;

      if (!is_unprocessed_clause (quantor, clause))
	mark_clause_as_unprocessed (quantor, clause,
				    QUANTOR_FORWARD_PROCESSING);
    }
}

/*------------------------------------------------------------------------*/

static void
mark_clauses_resolvable_to_unprocessed_lits_as_unprocessed (Quantor * quantor)
{
  Lit *lit;

  while ((lit = quantor->unprocessed_literals.first))
    {
      remove_unprocessed_lit (quantor, lit);
      mark_clauses_resolvable_to_lit_as_unprocessed (quantor, lit);
    }
}

/*------------------------------------------------------------------------*/

static void
mark_resolvable_clauses_as_unprocessed (Quantor * quantor, Clause * clause)
{
  Cell *p, *eor;
  Lit *not_lit;

  assert (quantor->opts.forward_processing ||
	  quantor->opts.backward_processing);

  eor = end_of_row (clause);
  for (p = clause->row; p < eor; p++)
    {
      not_lit = QUANTOR_NOT (p->lit);
      if (!is_unprocessed_lit (quantor, not_lit))
	mark_lit_as_unprocessed (quantor, not_lit);
    }
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static int
can_not_check_unary_hyper_resolution_clause (Quantor * quantor,
					     Clause * clause)
{
  if (quantor->opts.forward_processing)
    assert (quantor->opts.backward_processing);
  else if (!quantor->opts.backward_processing)
    return 1;

  /* Otherwise we can not determine whether 'other_clause' is younger.
   */
  if (!quantor->opts.forward_processing && quantor->opts.recycle_indices)
    return 1;

  if (clause->size <= 1)
    return 1;

  if (is_dying_clause (quantor, clause))
    return 1;

  return 0;
}

/*------------------------------------------------------------------------*/

static void
check_no_more_external_unary_hyper_resolutions_clause (Quantor * quantor,
						       Clause * clause)
{
  Lit *first_lit, *pivot, *other_lit;
  Cell *p, *q, *r, *eor;
  Clause *other_clause;
  int pivot_idx;

  if (can_not_check_unary_hyper_resolution_clause (quantor, clause))
    return;

  first_lit = QUANTOR_NOT (clause->row[0].lit);
  eor = end_of_row (clause);

  for (p = first_lit->binary_clauses.first;
       p; p = p->binary_clauses_link.next)
    {
      other_clause = p->clause;
      assert (other_clause->size == 2);

      if (is_dying_clause (quantor, other_clause))
	continue;

      pivot_idx = (other_clause->row[0].lit == first_lit);
      assert (other_clause->row[!pivot_idx].lit == first_lit);
      pivot = other_clause->row[pivot_idx].lit;

      if (deref (quantor, pivot) == QUANTOR_TRUE)
	continue;

      if (is_unit (quantor, pivot->var))
	{
	  assert (pivot->var->unit_assignment == lit2const (pivot));
	  continue;
	}

      for (q = clause->row + 1; q < eor; q++)
	{
	  other_lit = QUANTOR_NOT (q->lit);
	  for (r = other_lit->binary_clauses.first;
	       r; r = r->binary_clauses_link.next)
	    {
	      other_clause = r->clause;
	      assert (other_clause->size == 2);

	      if (is_dying_clause (quantor, other_clause))
		continue;

	      if (!quantor->opts.forward_processing &&
		  other_clause->idx > clause->idx)
		continue;

	      pivot_idx = (other_clause->row[0].lit == other_lit);
	      assert (other_clause->row[!pivot_idx].lit == other_lit);
	      if (other_clause->row[pivot_idx].lit == pivot)
		break;
	    }

	  if (!r)
	    break;
	}

      assert (q < eor);
    }
}

/*------------------------------------------------------------------------*/

static void
check_no_more_external_unary_hyper_resolutions (Quantor * quantor)
{
  Clause *p;

  if (quantor->invalid)
    return;

  for (p = quantor->clauses.first; p; p = p->link.next)
    check_no_more_external_unary_hyper_resolutions_clause (quantor, p);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
external_hyper1res_pivot_is_unit (Quantor * quantor,
				  Lit * pivot, Lit * skip, Clause * clause)
{
  Cell *p, *eor;
  int skipped;
  Lit *lit;

  eor = end_of_row (clause);
  skipped = 0;
  (void) skipped;

  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      if (lit == pivot)
	return 0;

      lit = QUANTOR_NOT (lit);
      if (lit == pivot)
	return 0;

      if (lit == skip)
	{
	  skipped = 1;
	  continue;
	}

      if (!find_binary_clause (quantor, pivot, lit))
	return 0;
    }

  assert (skipped);

  return 1;
}

/*------------------------------------------------------------------------*/
/* Generate units from a clause through hyper resolution.  From the first
 * clause in
 *
 *  A  -1 -2 3
 *  B  1 4
 *  C  2 4
 *  D  -3 4
 *  E  1 -5
 *  F  2 -5
 *  G  -3 -5
 *  H  1 6
 * 
 * the units '4' and '-5' can be generated in two hyper resolution steps.
 *
 * Both unary hyper resolutions are 'external' to the clause, since the unit
 * is introduced through the binary clauses and is not part of the original
 * clause.  In particular, the units found through external unary hyper
 * resolution do not occur in the clause and the clause is not subsumed by
 * any of the produced units.
 *
 * The dual case is handled by 'self_subsuming_unary_hyper_resolution'.
 */
static void
external_unary_hyper_resolution (Quantor * quantor, Clause * clause)
{
  Signature dual_sig_sum_intersection, dual_sig;
  unsigned len, tmp_len, units, idx;
  Lit *lit, *tmp, *pivot;
  Clause *binary_clause;
  Cell *eor, *p;

  assert (clause->size >= 2);

  if (!quantor->opts.hyper1res)
    return;

  INCSTATS2 (quantor->stats.hyper1res.checks);

  /* First calculate the intersection of the signature sums of the literals
   * in the dual clause.  Compare with the comments for the similar filter
   * in 'extract_or_gates'.  In the example above we have
   *    
   *    -A  1 2 -3
   *
   *    sigsum(1) = 1 4 -5 6
   *    sigsum(2) = 2 4 -5
   *    sigsum(-3) = -3 4 -5
   *    -----------------------
   *    dual_sig_sum_intersection(-A) = 4 -5
   *
   * which leaves us two unit candidates '4' and '-5'.
   */
  eor = end_of_row (clause);
  dual_sig_sum_intersection = ~(Signature) 0;
  dual_sig = (Signature) 0;

  for (p = clause->row; dual_sig_sum_intersection && p < eor; p++)
    {
      lit = QUANTOR_NOT (p->lit);
      dual_sig |= sig_lit (quantor, lit);
      dual_sig_sum_intersection &= lit->sigsum;
    }

  units = 0;

  /* In the intersection there has to be at least one literal, otherwise
   * external unary hyper resolution is impossible.
   */
  if (dual_sig_sum_intersection)
    {
      /* Find the dual literal with smallest number of binary clauses.
       *
       * Since we assume external hyper resolution, we can pick an arbitrary
       * literal.  To minimize the number of pivot checks, we pick the one
       * with the smallest number of binary clauses, which also results in
       * the smalles number of pivot candidates.
       *
       * In the example above 'lit' would be '2' since it has only 2 binary
       * clauses while '1' has 3 and '-3' would be checked last.  If '1'
       * would have been choosen the set of pivot candidates would grow from
       * '4 -5' to '4 -5 6'.
       *
       * The pivot is defined to be the literal on which we resolve, and
       * which will be one of the extracted units.  Note, that we may
       * extract multiple units, as in the example above.
       */
      len = UINT_MAX;
      lit = 0;

      for (p = clause->row; len && p < eor; p++)
	{
	  tmp = QUANTOR_NOT (p->lit);
	  tmp_len = tmp->binary_clauses.len;

	  if (tmp_len < len)
	    {
	      len = tmp_len;
	      lit = tmp;
	    }
	}
      assert (lit);

      /* If the dual literal does not have any binary clauses, then external
       * hyper unary resolution can not work.
       */
      if (len > 0)
	{
	  /* Traverse all the binary clauses of the dual literal and try to
	   * find a pivot, which is then checked to have all required binary
	   * clauses, that will make it a unit.
	   */
	  for (p = lit->binary_clauses.first;
	       !quantor->invalid && p; p = p->binary_clauses_link.next)
	    {
	      INCSTATS2 (quantor->stats.hyper1respivot.checks);

	      binary_clause = p->clause;
	      assert (binary_clause->size == 2);
	      idx = (binary_clause->row[0].lit == lit);
	      pivot = binary_clause->row[idx].lit;
	      assert (binary_clause->row[!idx].lit == lit);

	      /* The pivot needs to have 'clause->size' binary clauses,
	       * which all can be resolved against 'clause'.
	       */
	      if (pivot->binary_clauses.len < clause->size)
		{
		INC_HITS_AND_CONTINUE:
		  INCSTATS2 (quantor->stats.hyper1respivot.hits);
		  continue;
		}

	      if (!sig_subset (dual_sig, pivot->sigsum))
		goto INC_HITS_AND_CONTINUE;

	      if (external_hyper1res_pivot_is_unit (quantor, pivot,
						    lit, clause))
		{
#ifdef QUANTOR_LOG5
		  LOG (quantor, 5,
		       "NEW UNIT %d THROUGH "
		       "EXTERNAL UNARY HYPER RESOLUTION",
		       lit2int (quantor, pivot));
#endif
		  register_unit (quantor, pivot);
		  INCSTATS2 (quantor->stats.hyper1res_units);
		  units++;
		}
	      else
		INCSTATS2 (quantor->stats.hyper1respivot.spurious);
	    }

	  if (!units)
	    INCSTATS2 (quantor->stats.hyper1res.spurious);
	}
      else
	INCSTATS2 (quantor->stats.hyper1res.hits);
    }
  else
    INCSTATS2 (quantor->stats.hyper1res.hits);

#ifdef QUANTOR_CHECK
  if (quantor->opts.check >= 3)
    check_no_more_external_unary_hyper_resolutions_clause (quantor, clause);
#endif
#ifdef QUANTOR_LOG6
  if (units || quantor->opts.verbose >= 8)
    LOG (quantor, 6,
	 "EXTERNAL UNARY HYPER RESOLUTION ON CLAUSE %u GENERATES %u UNITS",
	 clause->idx, units);
#endif
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static void
check_no_more_self_subsuming_unary_hyper_resolutions_clause (Quantor *
							     quantor,
							     Clause * clause)
{
  Lit *pivot, *lit, *other_lit;
  int pivot_still_valid, found;
  Cell *p, *q, *eor, *r;
  Clause *other_clause;

  if (can_not_check_unary_hyper_resolution_clause (quantor, clause))
    return;

  assert (clause->size > 1);

  eor = end_of_row (clause);

  for (p = clause->row; p < eor; p++)
    {
      pivot = p->lit;
      pivot_still_valid = 1;

      if (deref (quantor, pivot) == QUANTOR_TRUE)
	break;;

      if (is_unit (quantor, pivot->var))
	break;;

      for (q = clause->row; pivot_still_valid && q < eor; q++)
	{
	  if (p == q)
	    continue;

	  lit = QUANTOR_NOT (q->lit);
	  found = 0;

	  for (r = lit->binary_clauses.first;
	       !found && r; r = r->binary_clauses_link.next)
	    {
	      other_clause = r->clause;

	      if (is_dying_clause (quantor, other_clause))
		continue;

	      if (!quantor->opts.forward_processing &&
		  other_clause->idx > clause->idx)
		continue;

	      assert (other_clause->size == 2);
	      other_lit = other_clause->row[0].lit;
	      if (other_lit == lit)
		other_lit = other_clause->row[1].lit;

	      found = (other_lit == pivot);
	    }

	  pivot_still_valid = found;
	}

      assert (!pivot_still_valid);
    }
}

/*------------------------------------------------------------------------*/

static void
check_no_more_self_subsuming_unary_hyper_resolutions (Quantor * q)
{
  Clause *p;

  if (q->invalid)
    return;

  for (p = q->clauses.first; p; p = p->link.next)
    check_no_more_self_subsuming_unary_hyper_resolutions_clause (q, p);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
self_subsuming_hyper1res_pivot_is_unit (Quantor * quantor,
					Lit * pivot, Clause * clause)
{
  Cell *p, *eor;
  int found_pivot;
  Lit *lit;

  found_pivot = 0;
  (void) found_pivot;

  eor = end_of_row (clause);
  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      if (lit == pivot)
	{
	  found_pivot = 1;
	  continue;
	}

      lit = QUANTOR_NOT (lit);
      assert (lit != pivot);

      if (!find_binary_clause (quantor, pivot, lit))
	return 0;
    }

  assert (found_pivot);

  return 1;
}

/*------------------------------------------------------------------------*/
/* This is very similar to 'external_unary_hyper_resolution', except that
 * the unit is already part of the clause:
 *
 *   A  -1 2 3 4
 *   B  1 3
 *   C  -2 3
 *   D  3 -4
 *   E  -1 -2
 *   F  -1 -3
 *   G  -1 -4
 *
 * Hyper resolution on A-D results in the unit 3, while hyper resolution on
 * clause A with E-F results in the unit -1.  Since the second unit can also
 * be obtained by simple unit resolution, we can stop after one unit is
 * found.
 *
 */
static void
self_subsuming_unary_hyper_resolution (Quantor * quantor, Clause * clause)
{
  Signature tmp, dual_sig_sum_intersection, dual_sig;
  Lit *pivot, *not_pivot, *lit;
  Cell *p, *eor;
  int found_unit;

  if (!quantor->opts.hyper1res)
    return;

  found_unit = 0;

  /* Binary self subsuming hyper resolution is handled seperately in
   * 'extract_unit_from_binary_clause'.
   */
  assert (clause->size >= 2);
  if (clause->size == 2)
    return;

  INCSTATS2 (quantor->stats.hyper1res.checks);

  /* Similar to the calculation of the 'dual_sig_sum_intersection' in
   * 'external_unary_hyper_resolution', except that we include the
   * signatures of the literals of the clause before intersection.
   */
  eor = end_of_row (clause);
  dual_sig_sum_intersection = ~(Signature) 0;
  dual_sig = (Signature) 0;

  for (p = clause->row; dual_sig_sum_intersection && p < eor; p++)
    {
      lit = p->lit;
      tmp = sig_lit (quantor, lit);
      lit = QUANTOR_NOT (lit);
      tmp |= lit->sigsum;
      dual_sig_sum_intersection &= tmp;
      dual_sig |= sig_lit (quantor, lit);
    }

  if (dual_sig_sum_intersection)
    {
      for (p = clause->row; !quantor->invalid && p < eor; p++)
	{
	  pivot = p->lit;
	  not_pivot = QUANTOR_NOT (pivot);
	  INCSTATS2 (quantor->stats.hyper1respivot.checks);

	  /* For each of the other literals of the clause there has to
	   * be one binary clause to which the literal is resolvable.
	   * All these clauses contain the target unit.  If there are
	   * less than 'clause->size -1' binary clauses with 'pivot',
	   * then there are not enough clauses for self subsuming unary
	   * hyper resolution with 'pivot' as target unit.
	   */
	  if (pivot->binary_clauses.len < clause->size - 1)
	    {
	    INC_HITS_AND_CONTINUE_OR_BREAK:
	      /* Assume that 'pivot' can not be used as target unit.
	       */
	      INCSTATS2 (quantor->stats.hyper1respivot.hits);

	      /* Since 'pivot' is not the target unit, it needs to be
	       * resolvable to a binary clause which contains
	       * 'not_pivot'.  If no such clause exists we can stop the
	       * search.
	       */
	      if (!not_pivot->binary_clauses.len)
		break;

	      /* The binary clause against which we resolve 'pivot' will
	       * introduce a new literal, which has to be the target
	       * unit.  Since the target unit is already part of the
	       * clause, the signature of the clause and the signature
	       * sum of 'not_pivot' can not have an empty intersection.
	       */
	      if (!(clause->sig & not_pivot->sigsum))
		break;

	      /* Otherwise we have just excluded 'pivot' from being a
	       * target unit, but still need to check the other
	       * candidates.
	       */
	      continue;
	    }

	  tmp = sig_lit (quantor, pivot);
	  if (!sig_subset (tmp, dual_sig_sum_intersection))
	    goto INC_HITS_AND_CONTINUE_OR_BREAK;

	  /* The dual signature of the original clause should be contained
	   * in the signature sum of the pivot candidate, except for the
	   * signature bit of the negation of the pivot.
	   */
	  tmp = pivot->sigsum | sig_lit (quantor, QUANTOR_NOT (pivot));
	  if (!sig_subset (dual_sig, tmp))
	    goto INC_HITS_AND_CONTINUE_OR_BREAK;

	  if (self_subsuming_hyper1res_pivot_is_unit (quantor, pivot, clause))
	    {
#ifdef QUANTOR_LOG5
	      LOG (quantor, 5,
		   "NEW UNIT %d THROUGH "
		   "SELF SUBSUMING UNARY HYPER RESOLUTION",
		   lit2int (quantor, pivot));
#endif
	      register_unit (quantor, pivot);
	      INCSTATS2 (quantor->stats.hyper1res_units);
	      found_unit = 1;
	      break;
	    }

	  INCSTATS2 (quantor->stats.hyper1respivot.spurious);
	}

      if (!found_unit)
	INCSTATS2 (quantor->stats.hyper1res.spurious);
    }
  else
    INCSTATS2 (quantor->stats.hyper1res.hits);

#ifdef QUANTOR_CHECK
  if (quantor->opts.check >= 3)
    check_no_more_self_subsuming_unary_hyper_resolutions_clause (quantor,
								 clause);
#endif
#ifdef QUANTOR_LOG6
  if (found_unit)
    LOG (quantor, 6,
	 "SELF SUBSUMING UNARY HYPER RESOLUTION ON CLAUSE %u GENERATES UNIT",
	 clause->idx);
#ifdef QUANTOR_LOG8
  else
    LOG (quantor, 8,
	 "SELF SUBSUMING UNARY HYPER RESOLUTION ON CLAUSE %u UNSUCCESSFUL",
	 clause->idx);
#endif
#endif
}

/*------------------------------------------------------------------------*/

static void
process_clause_size (Quantor * quantor, Clause * clause)
{
  if (clause->size == 0)
    {
#ifdef QUANTOR_LOG2
      LOG (quantor, 2,
	   "PROCESSING SIZE OF CLAUSE %d FINDS EMPTY CLAUSE", clause->idx);
#endif
      register_invalidity (quantor);
    }
  else if (clause->size == 1)
    register_unit (quantor, clause->row[0].lit);
  else
    {
      if (clause->size == 2)
	{
	  register_binary_clause (quantor, clause);

	  if (quantor->opts.forward_processing)
	    mark_resolvable_clauses_as_unprocessed (quantor, clause);
	}

      if (quantor->opts.backward_processing)
	mark_clause_as_unprocessed (quantor,
				    clause, QUANTOR_BACKWARD_PROCESSING);
    }
}

/*------------------------------------------------------------------------*/

static void
check_clause_is_simplified (Quantor * quantor, Clause * clause)
{
#ifndef NDEBUG
  Cell *p, *eor;
  Lit *lit;

  eor = end_of_row (clause);

  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      assert (deref (quantor, lit) == lit);
      assert (!lit->mark);
      lit->mark = 1;
    }

  for (p = clause->row; p < eor; p++)
    p->lit->mark = 0;
#else
  (void) quantor;
  (void) clause;
#endif
}

/*------------------------------------------------------------------------*/

static Clause *
new_Clause (Quantor * quantor, const char *msg)
{
  Clause *res;
  int size;

  size = count_PtrStack (&quantor->new_clause);
  res = alloc_Clause (quantor, size);
  set_clause_idx (quantor, res);
  sort_new_clause (quantor);
  copy_new_clause (quantor, msg, res);
  check_clause_is_simplified (quantor, res);
  process_clause_size (quantor, res);
  initialize_clause_sig (quantor, res);
  dlink (&quantor->clauses, res, &res->link);

  return res;
}

/*------------------------------------------------------------------------*/

static Signature
sig_new_clause (Quantor * quantor)
{
  Signature res;
  void **p;

  res = (Signature) 0;

  for (p = quantor->new_clause.start; p < quantor->new_clause.top; p++)
    res |= sig_lit (quantor, *p);

  return res;
}

/*------------------------------------------------------------------------*/

static void
mark_new_clause (Quantor * quantor, int new_mark)
{
  void **p, ** end_of_new_clause;
  Lit * lit;

  end_of_new_clause = quantor->new_clause.top;
  for (p = quantor->new_clause.start; p < end_of_new_clause; p++)
    {
      lit = *p;
      lit->mark = new_mark;
    }
}

/*------------------------------------------------------------------------*/

static int
new_clause_subsumes (Quantor * quantor, Clause * other)
{
  unsigned count, size;
  Cell *q, *eor;
  int res;

  size = count_PtrStack (&quantor->new_clause);
  assert (size <= other->size);

  mark_new_clause (quantor, 1);

  count = 0;
  eor = end_of_row (other);
  for (q = other->row; q < eor; q++)
    if (q->lit->mark)
      count++;

  res = (count == size);

  mark_new_clause (quantor, 0);

  return res;
}

/*------------------------------------------------------------------------*/
/* THIS FUNCTION IS VERY MUCH RELATED TO 'forward_subsume_clause'.
 * IF YOU MAKE CHANGES HERE YOU PROBABLY NEED TO DO THEM THERE AS WELL!
 *
 * Return value is non zero iff the new clause already exists.
 */
static int
backward_subsume_from_new_clause (Quantor * quantor, const char *msg)
{
  int num_subsumed, new_clause_already_exists;
  void **p, **end_of_new_clause;
  Clause *clause;
  Lit *lit, *tmp;
  unsigned size;
  Signature sig;
  Cell *q;

  if (!quantor->opts.backward_subsume)
    return 0;

  size = count_PtrStack (&quantor->new_clause);
  if (!size)
    return 0;

  INCSTATS2 (quantor->stats.subsume.checks);

  sig = sig_new_clause (quantor);
  num_subsumed = INT_MAX;

  end_of_new_clause = quantor->new_clause.top;

  for (p = quantor->new_clause.start;
       num_subsumed && p < end_of_new_clause; p++)
    {
      lit = *p;

      /* If there is a literal in the new clause, that is not contained in
       * any of the old clauses, then no old clauses can be backward
       * subsumed by the new clause.
       */
      if (!sig_subset (sig, lit->sigsum))
	num_subsumed = 0;
    }

  new_clause_already_exists = 0;

  if (num_subsumed)
    {
      /* All clauses subsumed by the new clause will contain the same
       * literals.  So we can choose an arbitrary literal from which we
       * traverse all occurences to find an subsumed clause.
       */
      lit = 0;
      for (p = quantor->new_clause.start; p < end_of_new_clause; p++)
	{
	  tmp = *p;

	  /* Heuristically we choose the literal with least number of
	   * occurences (the column length).  This minimizes the number of
	   * iterations in the following loop.
	   */
	  if (!lit || lit->column.len > tmp->column.len)
	    lit = tmp;
	}
      assert (lit);

      num_subsumed = 0;
      for (q = lit->column.first; q; q = q->column_link.next)
	{
	  clause = q->clause;

	  if (is_dying_clause (quantor, clause))
	    continue;

	  /* If there is a literal which occurs in the new clause, but
	   * does not occur in the candidate old clause, this old clause can
	   * not be subsumed by the new clause.
	   */
	  if (!sig_subset (sig, clause->sig))
	    continue;

	  /* If the candidate clause is smaller than the new clause, then it
	   * can not be subsumed either.  If it is the same size it may
	   * still be subsumed, since it may contain the same set of
	   * literals. This case can then be omitted in
	   * 'forward_subsume_clause'.
	   */
	  if (clause->size < size)
	    continue;

	  if (!new_clause_subsumes (quantor, clause))
	    continue;

	  if (clause->size == size)
	    {
#ifdef QUANTOR_LOG6
	      LOG (quantor, 6, "%s CLAUSE ALREADY EXISTS AS CLAUSE %d",
		   msg, clause->idx);
#else
	      (void) msg;
#endif
	      INCSTATS2 (quantor->stats.already_exists);
	      new_clause_already_exists = 1;

	      /* Continue with the loop since the old clause may not have
	       * been traversed during forward subsumption and thus this new
	       * copy may actually be used for backward subsumption of
	       * clauses added since the already existing copy has been
	       * added.
	       */
	    }
	  else
	    {
#ifdef QUANTOR_LOG6
	      LOG (quantor, 6,
		   "CLAUSE %d IS BACKWARD SUBSUMED BY %s CLAUSE",
		   clause->idx, msg);
#endif
	      INCSTATS1 (quantor->stats.backward_subsumed);
	      kill_Clause (quantor, clause);
	    }

	  num_subsumed++;
	}

      if (!num_subsumed)
	INCSTATS2 (quantor->stats.subsume.spurious);
    }
  else
    INCSTATS2 (quantor->stats.subsume.hits);

  return new_clause_already_exists;
}

/*------------------------------------------------------------------------*/
#ifndef NSTRENGTHEN
/*------------------------------------------------------------------------*/

static int
new_clause_subsumed_except_for_oneiteral (Quantor * quantor, Clause * clause)
{
  int count, size, res;
  Cell * q, * eor;

  size = count_PtrStack (&quantor->new_clause);
  assert (size >= clause->size);

  mark_new_clause (quantor, 1);

  count = 0;
  eor = end_of_row (clause);
  for (q = clause->row; q < eor; q++)
    if (q->lit->mark)
      count++;

  res = (count + 1 == clause->size);

  mark_new_clause (quantor, 0);

  return res;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
forward_strengthen_new_clause (Quantor * quantor, const char *msg)
{
#ifndef NSTRENGTHEN
  void **p, **end_of_new_clause;
  Signature sig, tmp;
  Lit *lit, *not_lit;
  Clause * clause;
  unsigned size;
  Cell * q;

  if (!quantor->opts.forward_strengthen)
    return 0;

  size = count_PtrStack (&quantor->new_clause);
  if (!size)
    return 0;

  INCSTATS2 (quantor->stats.strengthen.checks);

  sig = sig_new_clause (quantor);

  end_of_new_clause = quantor->new_clause.top;
  for (p = quantor->new_clause.start; p < end_of_new_clause; p++)
    {
      lit = *p;
      not_lit = QUANTOR_NOT (lit);

      tmp = sig;
      tmp &= ~sig_lit (quantor, lit);
      tmp |= sig_lit (quantor, not_lit);

      for (q = not_lit->column.first; q; q = q->column_link.next)
	{
	  clause = q->clause;

	  if (is_dying_clause (quantor, clause))
	    continue;

	  if (clause->size > size)
	    continue;

	  if (!sig_subset (clause->sig, tmp))
	    continue;

	  if (new_clause_subsumed_except_for_oneiteral (quantor, clause))
	    {
	      while (++p < end_of_new_clause)
		p[-1] = p[0];

	      quantor->new_clause.top--;

	      INCSTATS1(quantor->stats.strengthend);
	      INCSTATS1(quantor->stats.forward_strengthend);
#ifdef QUANTOR_LOG6
	      LOG (quantor, 6,
		   "FW STRENGTHEND NEW CLAUSE BY CLAUSE %d AND LITERAL %d",
		   clause->idx, lit2int (quantor, not_lit));
#endif
	      return 1;
	    }
	}
    }
  INCSTATS2 (quantor->stats.strengthen.spurious);
#else
  (void) quantor;
  (void) msg;
#endif

  return 0;
}

/*------------------------------------------------------------------------*/

static Clause *
add_new_clause (Quantor * quantor, const char *msg)
{
  Clause *clause;
  int subsumed;

  clause = 0;

  if (!new_clause_is_trivial (quantor, msg))
    {
      simplify_new_clause (quantor);

      do
	{
	  subsumed = backward_subsume_from_new_clause (quantor, msg);
	}
      while (!subsumed && forward_strengthen_new_clause (quantor, msg));

      if (!subsumed)
	clause = new_Clause (quantor, msg);
    }

  reset_PtrStack (quantor, &quantor->new_clause, 0);
  check_invariant (quantor, 3);

  return clause;
}

/*------------------------------------------------------------------------*/

static void
add_external_clause (Quantor * quantor)
{
  Clause *clause;
  Lit *l;
  int *p;

  assert (!count_PtrStack (&quantor->new_clause));

  for (p = quantor->external_literals.start;
       p < quantor->external_literals.top; p++)
    {
      l = int2lit (quantor, *p, 0);
      push_PtrStack (quantor, &quantor->new_clause, l);
    }

  reset_IntStack (quantor, &quantor->external_literals, 0);

  clause = add_new_clause (quantor, "ORIGINAL");
  if (clause)
    clause->original = 1;
}

/*------------------------------------------------------------------------*/

static const char *
check_and_internalize_scope (Quantor * quantor, Scope * scope)
{
  const char *err;
  int *p, lit;
  Var *v;

  assert (!count_PtrStack (&quantor->new_scope));
  err = 0;

  for (p = quantor->external_scope.start;
       !err && p < quantor->external_scope.top; p++)
    {
      lit = *p;
      assert (lit > 0);
      if (*access_PtrStack (quantor, &quantor->vars, lit))
	err = "variable already quantified earlier";
      else
	{
	  v = int2var (quantor, lit, 0, scope);
	  push_PtrStack (quantor, &quantor->new_scope, v);
	}
    }

  reset_IntStack (quantor, &quantor->external_scope, 0);

  return err;
}

/*------------------------------------------------------------------------*/

static void
release_equivalence_classes (Quantor * quantor)
{
  EquivalenceClass *p;

  while ((p = quantor->equivalence_classes.first))
    delete_EquivalenceClass (quantor, p, 1);
}

/*------------------------------------------------------------------------*/

static void
release_scopes (Quantor * quantor)
{
  Scope *p, *prev;

  for (p = quantor->scopes.last; p; p = prev)
    {
      prev = p->link.prev;
      delete_Scope (quantor, p);
    }
}

/*------------------------------------------------------------------------*/

static void
register_proposed_zombies (Quantor * quantor)
{
  Var *v;

  while ((v = quantor->proposed_zombies.first))
    register_zombie (quantor, v);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG1
/*------------------------------------------------------------------------*/

static void
log_scopes (Quantor * quantor, int level)
{
  Scope *p, *next;
  double ratio;

  assert (level < 5);

  if (quantor->opts.verbose < level)
    return;

  if (quantor->scopes.first)
    {
      fputs (quantor_prefix (quantor), quantor->io.out);

      assert (quantor->zombies.len <= quantor->stats.num_exported);
      fprintf (quantor->io.out, "(%d)",
	       quantor->stats.num_exported - quantor->zombies.len);

      for (p = quantor->scopes.first; p; p = next)
	{
	  next = p->link.next;

	  if (!p->vars.len)
	    continue;

	  fputc (p->type == QUANTOR_EXISTENTIAL ? '<' : '[', quantor->io.out);
	  fprintf (quantor->io.out, "%d", p->vars.len);
	  if ((quantor->opts.verbose >= 5 ||
	       quantor->opts.log_clauses_of_scopes) &&
	      p->type == QUANTOR_EXISTENTIAL && p->clauses.len > 0)
	    {
	      ratio = p->sum / (double) p->clauses.len;
	      fprintf (quantor->io.out, "{%d/%d=%.2f}",
		       p->sum, p->clauses.len, ratio);
	    }
	  fputc (p->type == QUANTOR_EXISTENTIAL ? '>' : ']', quantor->io.out);
	}

      fputc ('\n', quantor->io.out);
    }
  else
    LOG (quantor, level, "NO SCOPES LEFT");
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static Scope *
add_Scope (Quantor * quantor)
{
  Scope *scope, *last_scope, *default_scope;
  int nesting;

  default_scope = quantor->scopes.last;
  assert (default_scope);
  last_scope = default_scope->link.prev;
  nesting = last_scope ? last_scope->nesting + 1 : 0;
  scope = new_Scope (quantor, quantor->external_type, nesting);
  link_before (&quantor->scopes, scope, &scope->link, default_scope);
  reset_PtrStack (quantor, &quantor->new_scope, 0);

  if (nesting - 1 == quantor->max_external_nesting &&
      quantor->external_type == QUANTOR_EXISTENTIAL)
    quantor->max_external_nesting = nesting;

  check_invariant (quantor, 2);

  return scope;
}

/*------------------------------------------------------------------------*/

static int
get_char (Quantor * quantor)
{
  int ch;

  if (quantor->parser.look_ahead != EOF)
    {
      ch = quantor->parser.look_ahead;
      quantor->parser.look_ahead = EOF;
    }
  else
    ch = fgetc (quantor->io.in);

  if (ch == '\n')
    quantor->parser.lineno++;

  return ch;
}

/*------------------------------------------------------------------------*/

static void
unget_char (Quantor * quantor, int ch)
{
  assert (quantor->parser.look_ahead == EOF);

  if (ch == '\n')
    {
      assert (quantor->parser.lineno > 1);
      quantor->parser.lineno--;
    }

  quantor->parser.look_ahead = ch;
}

/*------------------------------------------------------------------------*/

static int
read_unsigned (Quantor * quantor, int ch, int *res_ptr)
{
  int res, valid;

  res = 0;
  valid = 0;

  if (isdigit (ch))
    {
      valid = 1;
      res += ch - '0';		/* potentially non portable */
    }

  for (;;)
    {
      ch = get_char (quantor);
      if (!isdigit (ch))
	break;

      res = 10 * res + (ch - '0');	/* potentially non portable */
      valid = 1;
    }

  unget_char (quantor, ch);
  *res_ptr = res;

  return valid;
}

/*------------------------------------------------------------------------*/

static int
read_white_space (Quantor * quantor)
{
  int valid, ch;

  valid = 0;

  while ((ch = get_char (quantor)) == ' ' || ch == '\t')
    valid = 1;

  unget_char (quantor, ch);

  return valid;
}

/*------------------------------------------------------------------------*/

static int
is_first_occurence_of_var (Quantor * quantor, int lit)
{
  unsigned idx = (unsigned)((lit < 0) ? -lit : lit);

  if (idx >= count_PtrStack (&quantor->vars))
    return 1;

  return !quantor->vars.start[idx];
}

/*------------------------------------------------------------------------*/

static void
dealloc_Clause (Quantor * quantor, Clause * clause)
{
  delete (quantor, clause, sizeof_Clause (clause->size));
}

/*------------------------------------------------------------------------*/

static void
unregister_binary_clause (Quantor * quantor, Clause * c)
{
#ifdef QUANTOR_STATS2
  dec_count_stats (&quantor->stats.binary_clauses);
#endif
#ifdef QUANTOR_LOG7
  LOG (quantor, 7, "UNREGISTERED BINARY CLAUSE %d", c->idx);
#endif
  if (!quantor->opts.equivalences)
    return;

  remove_BinDB (quantor, quantor->bindb, c);
}

/*------------------------------------------------------------------------*/

static void
delete_Clause (Quantor * quantor, Clause * c)
{
  Cell *p, *eor;
  int idx, lit;

  assert (!c->mark);
  assert (c->state == QUANTOR_DYING_CLAUSE);
  c->state = QUANTOR_GARBAGE_CLAUSE;

  idx = c->idx;
#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "DELETE CLAUSE %d", idx);
#endif

  if (c->size == 2)
    unregister_binary_clause (quantor, c);

  connect_clause_to_scope (quantor, c, 0);

  fix_sum_of_lits_in_clause (quantor, c, -c->size);

  if (c->scope)
    {
      assert (c->scope->sum >= c->size);
      c->scope->sum -= c->size;
    }

  eor = end_of_row (c);

  if (quantor->save_dead_original_clauses && c->original)
    {
      for (p = c->row; p < eor; p++)
	{
	  lit = lit2int (quantor, p->lit);
	  push_IntStack (quantor, &quantor->dead_original_clauses, lit);
	}

      push_IntStack (quantor, &quantor->dead_original_clauses, 0);
    }

  for (p = c->row; p < eor; p++)
    remove_Cell (quantor, p);

  undlink_if_linked (&quantor->dying_clauses, c, &c->dying_link);
  undlink_if_linked (&quantor->unprocessed_clauses,
		     c, &c->unprocessed_clauses_link);
  undlink (&quantor->clauses, c, &c->link);

  push_IntStack (quantor, &quantor->free_clause_indices, idx);
  dealloc_Clause (quantor, c);
  quantor->idx2clause.start[idx] = 0;

#ifdef QUANTOR_LOG2
  if (!quantor->clauses.len)
    LOG (quantor, 2, "ALL CLAUSES DELETED");
#endif
#ifdef QUANTOR_STATS1
  dec_count_stats (&quantor->stats.clauses);
#endif
}

/*------------------------------------------------------------------------*/

static void
gc_clauses (Quantor * quantor)
{
  unsigned collected;
  Clause *clause;

  collected = 0;
  while ((clause = quantor->dying_clauses.first))
    {
      delete_Clause (quantor, clause);
      collected++;
    }
#ifdef QUANTOR_LOG4
  if (collected)
    LOG (quantor, 4, "GC %u CLAUSES", collected);
#endif
}

/*------------------------------------------------------------------------*/

static void
gc_equivalence_classes (Quantor * quantor)
{
  EquivalenceClass *ec;
  unsigned collected;

  collected = 0;

  while ((ec = quantor->dying_equivalence_classes.first))
    {
      delete_EquivalenceClass (quantor, ec, 1);
      collected++;
    }
#ifdef QUANTOR_LOG4
  if (collected)
    LOG (quantor, 4, "GC %u EQUIVALENCE CLASSES", collected);
#endif
}

/*------------------------------------------------------------------------*/

static void
gc_clauses_and_register_zombies (Quantor * quantor)
{
  gc_clauses (quantor);
  register_proposed_zombies (quantor);
  assert (!quantor->proposed_zombies.first);
}

/*------------------------------------------------------------------------*/

static void
gc_functions (Quantor * quantor)
{
  Function *function;
  while ((function = quantor->dying_functions.first))
    delete_Function (quantor, function, 0);
}

/*------------------------------------------------------------------------*/

static void
gc_core (Quantor * quantor)
{
  gc_functions (quantor);
  gc_clauses_and_register_zombies (quantor);
  gc_equivalence_classes (quantor);
}

/*------------------------------------------------------------------------*/

static void forward_subsume_core (Quantor *);

/*------------------------------------------------------------------------*/

static void
gc (Quantor * quantor)
{
  if (quantor->invalid)
    return;

  gc_core (quantor);
}

/*------------------------------------------------------------------------*/

static int
subsumes (Quantor * quantor, Clause * this, Clause * other)
{
  unsigned count;
  Cell *p, *eor;
  int res;

  (void ) quantor;
  assert (this->size <= other->size);

  eor = end_of_row (this);
  for (p = this->row; p < eor; p++)
    p->lit->mark = 1;

  count = 0;
  eor = end_of_row (other);
  for (p = other->row; p < eor; p++)
    if (p->lit->mark)
      count++;

  res = (count == this->size);

  eor = end_of_row (this);
  for (p = this->row; p < eor; p++)
    p->lit->mark = 0;

  return res;
}

/*------------------------------------------------------------------------*/
/* This is only called in 'forward_subsume' where we in essence traverse the
 * clauses in reverse chronological order using the same check as for the
 * original backward subsumption checks:  
 *
 *   check whether younger clauses which are traversed earlier are
 *   subsumed by this older 'clause' given as argument
 *
 * This is equivalent of all the literals in 'clause' being contained in
 * the subsumed younger clauses.  The number of subsumed younger clauses
 * is returned.
 *
 * THIS FUNCTION IS VERY MUCH RELATED TO 'backward_subsume_from_new_clause'.
 * IF YOU MAKE CHANGES HERE YOU PROBABLY NEED TO DO THEM THERE AS WELL!
 */
static int
forward_subsume_clause (Quantor * quantor, Clause * clause)
{
  Cell *p, *eor;
  Clause *other;
  Lit *lit;
  int res;

  assert (clause->size > 0);
  assert (clause->sig);
  assert (!is_dying_clause (quantor, clause));
  assert (clause->mark);

  INCSTATS2 (quantor->stats.subsume.checks);

  eor = end_of_row (clause);
  res = INT_MAX;

  for (p = clause->row; res && p < eor; p++)
    if (!sig_subset (clause->sig, p->lit->sigsum))
      res = 0;

  if (res)
    {
      lit = 0;
      for (p = clause->row; p < eor; p++)
	{
	  if (!lit || lit->column.len > p->lit->column.len)
	    lit = p->lit;
	}

      assert (lit);

      res = 0;
      for (p = lit->column.first; p; p = p->column_link.next)
	{
	  other = p->clause;

	  if (other->mark)	/* only consider 'younger' clauses */
	    continue;

	  assert (clause != other);

	  if (!sig_subset (clause->sig, other->sig))
	    continue;

	  if (is_dying_clause (quantor, other))
	    continue;

	  /* If 'backward_subsume' is enabled, there won't be two identical
	   * clauses in the clause data base. Therefore the size of the
	   * candidate subsumed clause 'other' has to be larger
	   * than the size of the subsuming clause.
	   */
	  if (quantor->opts.backward_subsume && other->size <= clause->size)
	    continue;

	  /* If 'backward_subsume' is disabled, the size of the candiate
	   * clause can be the same.
	   */
	  if (!quantor->opts.backward_subsume && other->size < clause->size)
	    continue;

	  /* This is the expensive check that we try to avoid by using
	   * signatures.  Non matching signatures only rule out possible
	   * subsumption.  Matching signatures still require a full
	   * subsumption test, before we can conclude that the other clause
	   * is allowed to be killed.
	   */
	  if (!subsumes (quantor, clause, other))
	    continue;

#ifdef QUANTOR_LOG6
	  LOG (quantor, 6,
	       "CLAUSE %d IS FORWARD SUBSUMED BY CLAUSE %d",
	       other->idx, clause->idx);
#endif
	  INCSTATS2 (quantor->stats.forward_subsumed);
	  kill_Clause (quantor, other);
	  res++;
	}

      if (!res)
	INCSTATS2 (quantor->stats.subsume.spurious);
    }
  else
    INCSTATS2 (quantor->stats.subsume.hits);

  for (p = clause->row; p < eor; p++)
    add_sig (quantor, p->lit, clause->sig);

  clause->mark = 0;

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static void
check_no_subsumptions_clause (Quantor * quantor, Clause * clause)
{
  PtrStack literals, clauses;
  Cell *p, *q, *eor;
  Clause *other;
  void **r;
  Lit *lit;

  memset (&literals, 0, sizeof (literals));
  memset (&clauses, 0, sizeof (clauses));

  eor = end_of_row (clause);
  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      for (q = lit->column.first; q; q = q->column_link.next)
	{
	  other = q->clause;

	  if (other == clause)
	    continue;

	  if (other->mark < 0)
	    continue;

	  if (other->size < clause->size)
	    continue;

	  if (is_dying_clause (quantor, other))
	    continue;

	  if (!other->mark)
	    push_PtrStack (quantor, &clauses, other);
	  other->mark++;
	}
    }

  for (r = clauses.start; r < clauses.top; r++)
    {
      other = *r;
      assert (other->mark > 0);
      assert (other->mark < (int)clause->size);
      other->mark = 0;
    }

  release_PtrStack (quantor, &literals);
  release_PtrStack (quantor, &clauses);

  clause->mark = -1;
}

/*------------------------------------------------------------------------*/

static void
clear_marks_of_clauses (Quantor * quantor)
{
  Clause *c;

  for (c = quantor->clauses.first; c; c = c->link.next)
    {
      assert (c->mark);
      c->mark = 0;
    }
}

/*------------------------------------------------------------------------*/

static void
check_no_backward_subsumptions (Quantor * quantor)
{
  Clause *c;

  for (c = quantor->clauses.last; c; c = c->link.prev)
    if (!is_dying_clause (quantor, c))
      check_no_subsumptions_clause (quantor, c);

  clear_marks_of_clauses (quantor);
}

/*------------------------------------------------------------------------*/

static void
check_no_forward_subsumptions (Quantor * quantor)
{
  Clause *c;

  for (c = quantor->clauses.first; c; c = c->link.next)
    if (!is_dying_clause (quantor, c))
      check_no_subsumptions_clause (quantor, c);

  clear_marks_of_clauses (quantor);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
clause_is_part_of_function (Quantor * quantor, Lit * lhs, Clause * clause)
{
  Cell *p, *eor;
  int found_lhs;
  Lit *rhs;

  found_lhs = 0;
  eor = end_of_row (clause);
  (void) found_lhs;

  for (p = clause->row; p < eor; p++)
    {
      rhs = p->lit;
      if (lhs == QUANTOR_NOT (rhs))
	{
	  assert (!found_lhs);
	  found_lhs = 1;
	  continue;
	}

      assert (lhs != rhs);

      if (!find_implication (quantor, rhs, lhs))
	return 0;
    }

  assert (found_lhs);

  return 1;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG6
/*------------------------------------------------------------------------*/

const char *
FunctionType2Str (FunctionType type)
{
  switch (type)
    {
    case QUANTOR_ITE_GATE:
      return "ITE";
    case QUANTOR_XOR_GATE:
      return "XOR";
    default:
      assert (type == QUANTOR_OR_GATE);
      return "OR";
    }
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
log_function (Quantor * quantor, Function * f)
{
#ifdef QUANTOR_LOG6
  LOG (quantor, 6,
       "NEW %s GATE %u FUNCTION %u SIZE %d LHS %d",
       FunctionType2Str (f->type), f->type_idx, f->idx,
       f->rhs->size, lit2int (quantor, f->lhs));
#ifdef QUANTOR_LOG7
  if (quantor->opts.verbose >= 7)
    {
      fprintf (quantor->io.out, "%sNEW FUNCTION %u : %d =",
	       quantor_prefix (quantor), f->idx, lit2int (quantor, f->lhs));

      log_rhs_aux (quantor, f->rhs);
    }
#endif
  LOG (quantor, 6, "CONNECTED FUNCTION %u TO RHS %u", f->idx, f->rhs->idx);
#else
  (void) quantor;
  (void) f;
#endif
}

/*------------------------------------------------------------------------*/

static int
make_functions_with_same_rhs_consistent (Quantor * quantor,
					 FunctionType type,
					 Lit * lhs, RHS * rhs)
{
  Lit *dereferenced_lhs, *dereferenced_other_lhs;
  Lit *other_lhs, *normalized_other_lhs;
  Function *other_function;
  Var *lhs_var;

  (void) type;

  other_function = rhs->functions.first;
  assert (other_function);
  other_lhs = other_function->lhs;

  dereferenced_lhs = deref (quantor, lhs);
  dereferenced_other_lhs = deref (quantor, other_lhs);

  if (dereferenced_lhs == QUANTOR_NOT (dereferenced_other_lhs))
    {
#ifdef QUANTOR_LOG2
      LOG (quantor, 2, "CONFLICTING NEW LHS %d AND %d FOR RHS %u",
	   lit2int (quantor, lhs), lit2int (quantor, other_lhs), rhs->idx);
#endif
      register_invalidity (quantor);
      return 0;
    }
  else if (dereferenced_lhs != dereferenced_other_lhs)
    {
#ifdef QUANTOR_LOG5
      LOG (quantor, 5, "RHS %u WITH NEW ADDITIONAL LITERAL", rhs->idx);
#endif
      INCSTATS2 (quantor->stats.equivalences_from_functions);
      INCSTATS1 (quantor->stats.equivalences);

      lhs_var = lhs->var;
      normalized_other_lhs = other_lhs;
      if (is_signed (lhs))
	normalized_other_lhs = QUANTOR_NOT (normalized_other_lhs);
      assign (quantor, lhs_var, normalized_other_lhs);
    }

  return 1;
}

/*------------------------------------------------------------------------*/

static Function *
new_Function (Quantor * quantor,
	      FunctionType type, Lit * lhs, Lit ** literals, unsigned size)
{
#ifdef QUANTOR_STATS1
  CountStats *type_stats;
#endif
  Function *res;
  RHS *rhs;

  if (contains_RHS (quantor, literals, size, &rhs))
    {
      if (!make_functions_with_same_rhs_consistent (quantor, type, lhs, rhs))
	return 0;
    }
  else
    rhs = insert_RHS (quantor, literals, size);

  res = new (quantor, sizeof (*res));
  res->type = type;
  res->lhs = lhs;
  dlink (&lhs->var->functions, res, &res->lhs_link);
#ifdef QUANTOR_STATS1
#ifdef QUANTOR_LOG1
  res->idx = quantor->stats.functions.new;
#endif
  inc_count_stats (&quantor->stats.functions);
#endif
#ifdef QUANTOR_STATS2
  quantor->stats.args_functions += size;
#endif

  res->rhs = rhs;
  dlink (&rhs->functions, res, &res->rhs_link);

  dlink (&quantor->functions, res, &res->link);

#ifdef QUANTOR_STATS1
  type_stats = 0;
#endif
  switch (type)
    {
    default:
      assert (type == QUANTOR_OR_GATE);
#ifdef QUANTOR_STATS1
#ifdef QUANTOR_LOG1
      res->type_idx = quantor->stats.or.new;
#endif
      type_stats = &quantor->stats.or;
#endif
      break;
    }
#ifdef QUANTOR_STATS1
  inc_count_stats (type_stats);
#endif
  log_function (quantor, res);

  return res;
}

/*------------------------------------------------------------------------*/

static void
connect_clause_to_function (Quantor * quantor, Clause * clause, Function * f)
{
  f->clause = clause;
  dlink (&clause->functions, f, &f->clause_link);
#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "CONNECTED CLAUSE %u TO FUNCTION %u",
       f->clause->idx, f->idx);
#else
  (void) quantor;
#endif
}

/*------------------------------------------------------------------------*/

static void
extract_or_gates (Quantor * quantor, Clause * clause)
{
  Signature tmp, dual_clause_sig, dual_sigsum_intersection;
  unsigned lhs_checks, or_extractions, size;
  Lit *not_lit, **literals;
  Cell *p, *eor;
  Function *f;

  if (clause->size <= 2)
    return;

  if (!quantor->opts.functions)
    return;

  assert (quantor->opts.equivalences);

  INCSTATS2 (quantor->stats.or_extractions.checks);

  eor = end_of_row (clause);

  /* Calculate the intersection of the signature sums of the literals in the
   * dual clause in order to filter out clauses, which do not have any
   * candidate LHS.  The signature of the LHS candidate has to be in this
   * intersection.  If the intersection becomes empty, we can stop.  As an
   * example consider
   *    
   *   A    1 2 3 4             (== clause)
   *   B    -1 -2
   *   C    -2 -3
   *   D    -2 -4
   * 
   * from which the function '-2 = (1 | 3 | 4)' can be extracted.  The dual
   * clause '-A' is '-1 -2 -3 -4'.  For simplicity assume we have exact
   * signatures, which means 'Signature' is actually the power set over
   * '{1,2,3,4,-1,-2,-3,-4}'.
   *
   *   sig(A) = 1 2 3 4         (we omit '{' and '}')
   *   sig(B) = -1 -2 
   *   sig(C) = -2 -3
   *   sig(D) = -2 -4
   *   sig(-A) = -1 -2 -3 -4
   *
   *   sigsum(1) = 1 2 3 4
   *   sigsum(2) = 1 2 3 4
   *   sigsum(3) = 1 2 3 4
   *   sigsum(4) = 1 2 3 4
   *
   *   sigsum(-1) = -1 -2
   *   sigsum(-2) = -1 -2 -3 -4
   *   sigsum(-3) = -2 -3
   *   sigsum(-4) = -2 -4
   *   ----------------------
   *   dual_sig_sum_intersection(-A) =
   *    sigsum(-1) & sigsum(-2) & sigsum(-3) & sigsum(-4)
   *    -2
   *
   *  So '-2' is the only candidate.  If for instance clause D is missing
   *  then the 'dual_sig_sum_intersection(-A)' becomes empty and we would
   *  know immediately that no function can be extracted.
   */
  dual_sigsum_intersection = ~(Signature) 0;
  dual_clause_sig = 0;
  for (p = clause->row; dual_sigsum_intersection && p < eor; p++)
    {
      not_lit = QUANTOR_NOT (p->lit);

      /* If any of the dual literals does not have any binary clauses, then
       * we are done.
       */
      if (not_lit->binary_clauses.len)
	{
	  assert (deref (quantor, not_lit) == not_lit);
	  dual_clause_sig |= sig_lit (quantor, not_lit);
	  dual_sigsum_intersection &= not_lit->sigsum;
	}
      else
	dual_sigsum_intersection = 0;
    }

  lhs_checks = 0;
  or_extractions = 0;

  if (dual_sigsum_intersection)
    {
      for (p = clause->row; !quantor->invalid && p < eor; p++)
	{
	  INCSTATS2 (quantor->stats.or_lhs.checks);

	  not_lit = QUANTOR_NOT (p->lit);
	  tmp = sig_lit (quantor, not_lit);

	  /* As discussed above, the signature of any LHS candidate has to
	   * lie in the intersection of the signatures of the dual literals.
	   * In the example above only 'not_lit = -2' fullfils this test.
	   */
	  if (!sig_subset (tmp, dual_sigsum_intersection))
	    {
	    INC_HITS_AND_CONTINUE:
	      INCSTATS2 (quantor->stats.or_lhs.hits);
	      continue;
	    }

	  /* The candidate LHS needs to have 'clause->size - 1'
	   * binary clauses, which can be resolved to the clause.
	   */
	  if (not_lit->binary_clauses.len < clause->size - 1)
	    goto INC_HITS_AND_CONTINUE;

	  /* The sum of the signature of the binary clauses needs to cover
	   * the signature of the dual clause.
	   */
	  if (!sig_subset (dual_clause_sig, not_lit->sigsum))
	    goto INC_HITS_AND_CONTINUE;

	  /* If all the filters above fail, then we explicitly check the
	   * existence of the binary clauses.
	   */
	  if (clause_is_part_of_function (quantor, not_lit, clause))
	    {
	      literals = push_new_rhs (quantor, clause, not_lit);
	      assert (clause->size > 0);
	      size = clause->size - 1;
	      f = new_Function (quantor, QUANTOR_OR_GATE,
				not_lit, literals, size);
	      if (f)
		{
		  connect_clause_to_function (quantor, clause, f);
		  or_extractions++;
		}
	      else
		assert (quantor->invalid);
	    }
	  else
	    INCSTATS2 (quantor->stats.or_lhs.spurious);

	  lhs_checks++;
	}

      if (!or_extractions)
	INCSTATS2 (quantor->stats.or_extractions.spurious);
    }
  else
    INCSTATS2 (quantor->stats.or_extractions.hits);

#ifdef QUANTOR_LOG6
  if (or_extractions || quantor->opts.verbose >= 8)
    LOG (quantor, 6,
	 "EXTRACTED %u OR GATES FROM %u LHS CANDIDATES FOR CLAUSE %u",
	 or_extractions, lhs_checks, clause->idx);
#endif
}

/*------------------------------------------------------------------------*/

static void
process_clause (Quantor * quantor, Clause * clause)
{
#ifdef QUANTOR_LOG8
  LOG (quantor, 8, "PROCESSING CLAUSE %u", clause->idx);
#endif
  assert (clause->size >= 2);
  assert (!is_dying_clause (quantor, clause));
  assert (is_unprocessed_clause (quantor, clause));

  INCSTATS2 (quantor->stats.processed);

#if 0
  if (getenv ("ONLYCOUNT"))
    {
      remove_unprocessed_clauses (quantor, clause);
      return;
    }
#endif

  if (clause->processing_type == QUANTOR_FORWARD_PROCESSING)
    {
      INCSTATS2 (quantor->stats.forward_processed);
    }
  else
    {
      assert (clause->processing_type == QUANTOR_BACKWARD_PROCESSING);
      INCSTATS2 (quantor->stats.backward_processed);
    }

  external_unary_hyper_resolution (quantor, clause);

  if (quantor->invalid || quantor->units.first)
    return;

  self_subsuming_unary_hyper_resolution (quantor, clause);

  if (quantor->invalid || quantor->units.first)
    return;

  remove_unprocessed_clauses (quantor, clause);

  extract_or_gates (quantor, clause);
}

/*------------------------------------------------------------------------*/

static void
copy_clause_except (Quantor * quantor, Clause * clause, Lit * lit)
{
  Cell *p, *eor;

  assert (!count_PtrStack (&quantor->new_clause));

  eor = end_of_row (clause);
  for (p = clause->row; p < eor; p++)
    if (p->lit != lit)
      push_PtrStack (quantor, &quantor->new_clause, p->lit);

  add_new_clause (quantor, "RESOLVED");
}

/*------------------------------------------------------------------------*/

static void
unlink_unit_or_unate (Quantor * quantor, Var * v)
{
  if (is_unit (quantor, v))
    {
      undlink (&quantor->units, v, &v->unit_link);
    }
  else
    {
      assert (is_unate (quantor, v));
      undlink (&quantor->unates, v, &v->unate_link);
    }
}

/*------------------------------------------------------------------------*/

static void
kill_clauses_with_lit (Quantor * quantor, Lit * lit)
{
  Cell *p, *next;

  for (p = lit->column.first; p; p = next)
    {
      next = p->column_link.next;
#ifdef QUANTOR_LOG7
      LOG (quantor, 7,
	   "CLAUSE %d CONTAINS %d", p->clause->idx, lit2int (quantor, lit));
#endif
      kill_Clause (quantor, p->clause);
    }
}

/*------------------------------------------------------------------------*/

static void
remove_lit_in_clauses (Quantor * quantor, Lit * lit)
{
  Cell *p, *next;
  Clause *clause;

  for (p = lit->column.first; !quantor->invalid && p; p = next)
    {
      next = p->column_link.next;
      clause = p->clause;
#ifdef QUANTOR_LOG7
      LOG (quantor, 7,
	   "CLAUSE %d CONTAINS %d", p->clause->idx, lit2int (quantor, lit));
#endif
      kill_Clause (quantor, clause);
      copy_clause_except (quantor, clause, lit);
    }
}

/*------------------------------------------------------------------------*/

static void
update_clauses_after_assigning_literal (Quantor * quantor, Lit * lit)
{
  kill_clauses_with_lit (quantor, lit);
  remove_lit_in_clauses (quantor, QUANTOR_NOT (lit));
}

/*------------------------------------------------------------------------*/

static void
resolve_unit_or_unate (Quantor * quantor, Var * v)
{
  Lit *lit, *assignment;
  int saved_is_unit;

  saved_is_unit = is_unit (quantor, v);
  (void) saved_is_unit;

  if (is_unit (quantor, v))
    {
      assert (!is_unate (quantor, v));
      assignment = v->unit_assignment;
      v->unit_assignment = QUANTOR_UNASSIGNED;
    }
  else
    {
      assert (is_unate (quantor, v));
      assert (!is_unit (quantor, v));
      assignment = v->unate_assignment;
      v->unate_assignment = QUANTOR_UNASSIGNED;
    }

  unlink_unit_or_unate (quantor, v);
  assert (is_constant (assignment));

  assign (quantor, v, assignment);

  lit = var2lit (v, assignment == QUANTOR_FALSE);
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "RESOLVING %s %s %d",
       quantificationtype2str (v->scope->type),
       saved_is_unit ? "UNIT" : "UNATE", lit2int (quantor, lit));
#endif
  update_clauses_after_assigning_literal (quantor, lit);

  check_invariant (quantor, 2);
}

/*------------------------------------------------------------------------*/

static Var *
next_unit_to_resolve (Quantor * quantor)
{
  assert (!quantor->invalid);
  return quantor->units.first;
}

/*------------------------------------------------------------------------*/

static void
register_proposed_unates (Quantor * quantor)
{
  Var *v;

  while ((v = quantor->proposed_unates.first))
    register_unate (quantor, v);
}

/*------------------------------------------------------------------------*/

static Var *
next_unate_to_resolve (Quantor * quantor)
{
  assert (!quantor->invalid);
  register_proposed_unates (quantor);
  return quantor->unates.first;
}

/*------------------------------------------------------------------------*/

static Var *
next_unit_or_unate_to_resolve (Quantor * quantor)
{
  Var *res;

  assert (!quantor->invalid);

  res = next_unit_to_resolve (quantor);
  if (!res)
    res = next_unate_to_resolve (quantor);

  return res;
}

/*------------------------------------------------------------------------*/

static EquivalenceClass *
next_unprocessed_equivalence_class (Quantor * quantor)
{
  assert (!quantor->invalid);
  return quantor->unprocessed_equivalence_classes.first;
}

/*------------------------------------------------------------------------*/

static Clause *
next_unprocessed_clause (Quantor * quantor)
{
  assert (!quantor->invalid);

  if (quantor->unprocessed_literals.first)
    mark_clauses_resolvable_to_unprocessed_lits_as_unprocessed (quantor);

  return quantor->unprocessed_clauses.first;
}

/*------------------------------------------------------------------------*/

static void
unit_resolution (Quantor * quantor)
{
  Var *v;
#ifdef QUANTOR_LOG3
  int count = 0;
#endif
  gc (quantor);

  while (!quantor->invalid && (v = next_unit_or_unate_to_resolve (quantor)))
    {
      if (is_assigned_var (v))
	{
	  unlink_unit_or_unate (quantor, v);
	  continue;
	}

#ifdef QUANTOR_LOG4
      if (count)
	{
	  LOG (quantor, 4, "SCOPES AFTER %d UNIT RESOLUTION STEPS:", count);
	  log_scopes (quantor, 4);
	}
      count++;
#endif
      resolve_unit_or_unate (quantor, v);
      gc (quantor);
    }

#ifdef QUANTOR_LOG3
  if (count > 0)
    {
#ifdef QUANTOR_LOG4
      LOG (quantor, 4, "SCOPES AFTER UNIT RESOLUTION PHASE (%d STEPS):",
	   count);
#endif
      log_scopes (quantor, 3);
    }
#endif
  check_invariant (quantor, 2);
}

/*------------------------------------------------------------------------*/

static void
substitute_clause (Quantor * quantor, Clause * clause, Lit * lhs, Lit * rhs)
{
#ifdef QUANTOR_LOG6
  char buffer[30];
#endif
  const char *msg;
  Cell *p, *eor;
  int found;
  Lit *lit;

  assert (!count_PtrStack (&quantor->new_clause));
#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "SUBSTITUTING %d BY %d IN CLAUSE %u",
       lit2int (quantor, lhs), lit2int (quantor, rhs), clause->idx);
#endif

  eor = end_of_row (clause);

  found = 0;
  (void) found;
  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;

      if (lit == lhs)
	{
	  lit = rhs;
	  found = 1;
	}

      push_PtrStack (quantor, &quantor->new_clause, lit);
    }
  assert (found);

  kill_Clause (quantor, clause);
#ifdef QUANTOR_LOG6
  sprintf (buffer, "SUBSTITUTED(%u)", clause->idx);
  msg = buffer;
#else
  msg = "SUBSTITUTED";
#endif
  add_new_clause (quantor, msg);
}

/*------------------------------------------------------------------------*/

static void
substitute_one_phase (Quantor * quantor, Lit * lhs, Lit * rhs)
{
  Cell *p, *next;

  for (p = lhs->column.first; !quantor->invalid && p; p = next)
    {
      next = p->column_link.next;
      substitute_clause (quantor, p->clause, lhs, rhs);
    }

}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static void
check_literal_has_only_dying_clauses (Quantor * quantor, Lit * lit)
{
  Cell *p;

  for (p = lit->column.first; p; p = p->column_link.next)
    assert (is_dying_clause (quantor, p->clause));
}

/*------------------------------------------------------------------------*/

static void
check_variable_has_only_dying_clauses (Quantor * quantor, Var * v)
{
  check_literal_has_only_dying_clauses (quantor, var2lit (v, 0));
  check_literal_has_only_dying_clauses (quantor, var2lit (v, 1));
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
check_substitute_post_condition (Quantor * quantor, Lit * lhs)
{
#ifdef QUANTOR_CHECK
  if (!quantor->opts.check)
    return;

  if (quantor->invalid)
    return;

  check_variable_has_only_dying_clauses (quantor, lhs->var);
#else
  (void) quantor;
  (void) lhs;
#endif
}

/*------------------------------------------------------------------------*/

static void
substitute (Quantor * quantor, Lit * lhs, Lit * rhs)
{
  INCSTATS2 (quantor->stats.substitutions);
#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "SUBSTITUTING LITERAL %d BY LITERAL %d",
       lit2int (quantor, lhs), lit2int (quantor, rhs));
#endif
  rhs = deref (quantor, rhs);
  assert (deref (quantor, lhs) == rhs);

  /* We have to check this here again, since it is to costly to check it
   * everytime, when a variable is added to a equivalence class.
   */
  if (conflict_from_illegal_assignment (quantor, lhs->var, rhs->var))
    {
      register_illegal_variable_assignment (quantor, lhs->var, rhs->var);
      return;
    }

  substitute_one_phase (quantor, lhs, rhs);

  if (quantor->invalid)
    return;

  rhs = deref (quantor, rhs);
  assert (deref (quantor, lhs) == rhs);

  substitute_one_phase (quantor, QUANTOR_NOT (lhs), QUANTOR_NOT (rhs));

  check_substitute_post_condition (quantor, lhs);
}


/*------------------------------------------------------------------------*/

static void
process_equivalence_class (Quantor * quantor, EquivalenceClass * ec)
{
  Var *u, *v, *next;
  Lit *rhs, *lhs;
  int count;

#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "PROCESSING EQUIVALENCE CLASS %d OF SIZE %d",
       ec->idx, ec->elems.len);
#endif
  assert (!ec->processing);
  ec->processing = 1;

  count = 0;

RESTART_PROCESSING_EQUIVALENCE_CLASS:

  undlink (&quantor->unprocessed_equivalence_classes,
	   ec, &ec->unprocessed_link);

  v = minimize_equivalence_class_representative (quantor, ec);
  rhs = var2lit (v, v->eqclass_sign);

  for (u = ec->elems.first; u; u = next)
    {
      next = u->eqclass_link.next;

      if (u->dead)
	continue;

      if (u == v)
	continue;

      lhs = var2lit (u, u->eqclass_sign);
      assert (lhs != rhs);
      assert (lhs != QUANTOR_NOT (rhs));

      count++;
      substitute (quantor, lhs, rhs);

      if (quantor->invalid)
	break;

      if (is_unprocessed_equivalence_class (quantor, ec))
	{
#ifdef QUANTOR_LOG5
	  LOG (quantor, 5, "RESTARTING PROCESSING EQUIVALENCE CLASS %d",
	       ec->idx);
#endif
	  goto RESTART_PROCESSING_EQUIVALENCE_CLASS;
	}
    }
  assert (ec->processing);
  ec->processing = 0;
#ifdef QUANTOR_LOG5
  LOG (quantor, 5,
       "PROCESSED EQUIVALENCE CLASS %d AND SUBSTITUTED %d VARIABLES",
       ec->idx, count);
#endif
  gc (quantor);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

static void
check_bcp_post_condition_clause (Quantor * quantor, Clause * clause)
{
  Cell *p, *eor;
  Lit *lit;

  eor = end_of_row (clause);

  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      assert (deref (quantor, lit) == lit);
    }
}

/*------------------------------------------------------------------------*/

static void
check_bcp_post_condition_clauses (Quantor * quantor)
{
  Clause *c;

  for (c = quantor->clauses.first; c; c = c->link.next)
    check_bcp_post_condition_clause (quantor, c);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
check_bcp_post_condition (Quantor * quantor)
{
  if (quantor->invalid)
    return;

  assert (!quantor->dying_clauses.first);

#ifdef QUANTOR_CHECK
  if (quantor->invalid)
    return;

  assert (!quantor->units.first);
  assert (!quantor->unates.first);
  assert (!quantor->unprocessed_equivalence_classes.first);
  assert (!quantor->unprocessed_clauses.first);

  if (quantor->opts.check <= 2)
    return;

  check_bcp_post_condition_clauses (quantor);
  check_no_more_external_unary_hyper_resolutions (quantor);
  check_no_more_self_subsuming_unary_hyper_resolutions (quantor);
#endif
}

/*------------------------------------------------------------------------*/

static void
bcp (Quantor * quantor)
{
  EquivalenceClass *ec;
  Clause *clause;
  int round;

  gc (quantor);

  round = 0;

  while (!quantor->invalid &&
	 (next_unit_or_unate_to_resolve (quantor) ||
	  next_unprocessed_equivalence_class (quantor) ||
	  next_unprocessed_clause (quantor)))
    {
      if (!round)
	{
#ifdef QUANTOR_STATS1
	  INCSTATS1 (quantor->stats.bcps);
#ifdef QUANTOR_LOG4
	  LOG (quantor, 4, "STARTING BCP %0.f", quantor->stats.bcps);
#endif
#endif
	}

      round++;
#ifdef QUANTOR_STATS2
      quantor->stats.bcp_rounds++;
#ifdef QUANTOR_LOG4
      LOG (quantor, 4, "BCP %0.f ROUND %d", quantor->stats.bcps, round);
#endif
#endif
      if (next_unit_or_unate_to_resolve (quantor))
	{
	  unit_resolution (quantor);
	  continue;
	}

      if ((ec = next_unprocessed_equivalence_class (quantor)))
	{
	  process_equivalence_class (quantor, ec);
	  continue;
	}

      if ((clause = next_unprocessed_clause (quantor)))
	process_clause (quantor, clause);
    }

#if defined(QUANTOR_STATS1) && defined(QUANTOR_LOG4)
  if (round)
    LOG (quantor, 4, "FINISHED BCP %0.f IN %d ROUNDS",
	 quantor->stats.bcps, round);
#endif

  check_bcp_post_condition (quantor);
}

/*------------------------------------------------------------------------*/

static void
forward_subsume_core (Quantor * quantor)
{
  int num_subsumed_clauses;
  Clause *c, *prev;

#ifdef QUANTOR_CHECK
  if (quantor->opts.check >= 2)
    check_no_backward_subsumptions (quantor);
#endif

  clear_sigs (quantor);

  for (c = quantor->clauses.first; c; c = c->link.next)
    c->mark = 1;

  num_subsumed_clauses = 0;
  for (c = quantor->clauses.last; c; c = prev)
    {
      prev = c->link.prev;
      num_subsumed_clauses += forward_subsume_clause (quantor, c);
    }

#ifdef QUANTOR_LOG3
  LOG (quantor, 3, "FORWARD SUBSUMED %d CLAUSES", num_subsumed_clauses);
#endif
  gc_core (quantor);
#ifndef NDEBUG
  for (c = quantor->clauses.first; c; c = c->link.next)
    assert (!c->mark);
#endif
#ifdef QUANTOR_CHECK
  if (quantor->opts.check >= 2)
    check_no_forward_subsumptions (quantor);
#endif
}

/*------------------------------------------------------------------------*/
/* Since forward subsumption is too expensive to be performed on-the-fly.
 * In certain intervals we go over all clauses again and check for forward
 * subsumed clauses by, essentially, performing backward subsumption.
 */
static void
forward_subsume (Quantor * quantor)
{
  if (!quantor->opts.forward_subsume)
    return;

  if (quantor->invalid)
    return;

  gc_core (quantor);
  forward_subsume_core (quantor);
  bcp (quantor);
}

/*------------------------------------------------------------------------*/

static void
register_propositional_original_problem (Quantor * quantor)
{
  quantor->original_problem_is_propositional = 1;
  quantor->save_dead_original_clauses = quantor->opts.resolve_exported;
}

/*------------------------------------------------------------------------*/

static int
scope_or_clause_not_closed (Quantor * quantor)
{
  if (count_IntStack (&quantor->external_literals))
    return 1;

  if (count_IntStack (&quantor->external_scope))
    return 1;

  if (quantor->external_type != QUANTOR_UNDEFINED_TYPE)
    return 1;

  return 0;
}

/*------------------------------------------------------------------------*/

static QuantorQuantificationType
char2qtype (char ch)
{
  assert (ch == 'a' || ch == 'e');
  return (ch == 'a') ?
   QUANTOR_UNIVERSAL_VARIABLE_TYPE : QUANTOR_EXISTENTIAL_VARIABLE_TYPE;
}

/*------------------------------------------------------------------------*/

const char *
quantor_scope (Quantor * quantor, QuantorQuantificationType type)
{
  const char * res;

  assert (type == QUANTOR_UNIVERSAL_VARIABLE_TYPE ||
	  type == QUANTOR_EXISTENTIAL_VARIABLE_TYPE);

  res = 0;

  if (quantor->stats.external_clauses || scope_or_clause_not_closed (quantor))
    {
      if (quantor->external_type == QUANTOR_UNDEFINED_TYPE)
	res = "misplaced quantifier prefix";
      else
	res = "'0' missing";
    }
  else if (type == QUANTOR_UNIVERSAL_VARIABLE_TYPE)
    quantor->external_type = QUANTOR_UNIVERSAL;
  else
    quantor->external_type = QUANTOR_EXISTENTIAL;

  return res;
}

/*------------------------------------------------------------------------*/

const char *
quantor_add (Quantor * quantor, int lit)
{
  const char * res;
  Scope * scope;

  if (!quantor->scopes.last)
    init_default_scope (quantor);

  res = 0;

  if (lit)
    {
      if (quantor->external_type != QUANTOR_UNDEFINED_TYPE && lit < 0)
	res = "negative quantified variable";
      else
	{
	  if (is_first_occurence_of_var (quantor, lit))
	    quantor->stats.external_vars++;

	  if (quantor->external_type == QUANTOR_UNDEFINED_TYPE)
	    {
	      push_IntStack (quantor, &quantor->external_literals, lit);
	      quantor->stats.external_lits++;
	    }
	  else
	    push_IntStack (quantor, &quantor->external_scope, lit);
	}
    }
  else if (quantor->external_type == QUANTOR_UNDEFINED_TYPE)
    {
      add_external_clause (quantor);
      quantor->stats.external_clauses++;
    }
  else
    {
      scope = add_Scope (quantor);

      if (!(res = check_and_internalize_scope (quantor, scope)))
	{
	  quantor->stats.external_scopes++;
	  quantor->external_type = QUANTOR_UNDEFINED_TYPE;
	}
    }

  return res;
}

/*------------------------------------------------------------------------*/

static int
parse (Quantor * quantor)
{
  Parser *parser = &quantor->parser;
  int ch, sign, lit, max_idx;
  double time;

  assert (!parser->parsing);
  parser->parsing = 1;

  init_default_scope (quantor);

  time = get_time ();
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "PARSING %s", quantor->io.in_name);
#endif
  assert (!count_IntStack (&quantor->external_scope));
  assert (!count_IntStack (&quantor->external_literals));
  assert (!parser->err);

  assert (!quantor->stats.external_clauses);
  assert (!quantor->stats.external_scopes);
  assert (!quantor->stats.external_lits);
  assert (!quantor->stats.external_vars);

  parser->lineno = 1;
  parser->look_ahead = EOF;
  quantor->stats.specified_clauses = -1;
  quantor->stats.specified_vars = -1;
  max_idx = 0;

  assert (quantor->external_type == QUANTOR_UNDEFINED_TYPE);

  while (!parser->err && (ch = get_char (quantor)) != EOF)
    {
      if (ch == 'c')
	{
	  while ((ch = get_char (quantor)) != EOF && ch != '\n')
	    ;
	}
      else if (ch == 'p')
	{
	  if (quantor->stats.specified_vars >= 0)
	    parser->err = "multiple 'p' found";
	  else if (scope_or_clause_not_closed (quantor) ||
		   quantor->stats.external_clauses ||
		   quantor->stats.external_scopes)
	    parser->err = "misplaced 'p'";
	  else if (!read_white_space (quantor) ||
		   (get_char (quantor) != 'c') ||
		   (get_char (quantor) != 'n') ||
		   (get_char (quantor) != 'f') ||
		   !read_white_space (quantor) ||
		   !read_unsigned (quantor, ' ',
				   &quantor->stats.specified_vars) ||
		   !read_white_space (quantor) ||
		   !read_unsigned (quantor, ' ',
				   &quantor->stats.specified_clauses))
	    parser->err = "malformend 'p cnf ...'";
	}
      else if (ch == '-' || isdigit (ch))
	{
	  if (ch == '-')
	    {
	      sign = -1;
	      ch = get_char (quantor);
	    }
	  else
	    sign = 1;

	  if (!read_unsigned (quantor, ch, &lit))
	    parser->err = "invalid literal";
	  else
	    {
	      if (lit > max_idx)
		max_idx = lit;

	      lit *= sign;
	      parser->err = quantor_add (quantor, lit);
	    }
	}
      else if (ch == 'a' || ch == 'e')
	parser->err = quantor_scope (quantor, char2qtype (ch));
      else if (ch != ' ' && ch != '\t' && ch != '\n')
	parser->err = "invalid character";
    }

  if (!parser->err && scope_or_clause_not_closed (quantor))
    parser->err = "'0' missing";

  if (!parser->err && quantor->stats.specified_vars < 0)
    parser->err = "'p cnf ...' missing";

  if (!parser->err && quantor->stats.specified_vars < max_idx)
    parser->err = "more variables found than specified";

  if (!parser->err &&
      quantor->stats.specified_clauses != quantor->stats.external_clauses)
    parser->err = "specified and actual number of clauses differ";

  if (!parser->err)
    {
      /* TODO factor out this code */

      register_proposed_zombies (quantor);
      register_proposed_unates (quantor);
      quantor->no_more_external_vars = 1;
#ifdef QUANTOR_LOG4
      LOG (quantor, 4, "EXPORTED %d VARIABLES", quantor->stats.num_exported);
#endif
#ifdef QUANTOR_LOG4
      LOG (quantor, 4, "SCOPES AFTER PARSING:");
#endif
#ifdef QUANTOR_LOG1
      log_scopes (quantor, 1);
#endif
    }

  if (!quantor->stats.external_scopes)
    {
#ifdef QUANTOR_LOG1
      LOG (quantor, 1, "INPUT IS PROPOSITIONAL (NO QUANTIFIERS)");
#endif
      register_propositional_original_problem (quantor);
    }

  if (!parser->err && 
      quantor->stats.external_scopes &&
      default_scope (quantor)->vars.len)
    parser->err = "unquantified variable in QBF instance";

  time = get_time () - time;
  time = (time < 0) ? 0 : time;
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "PARSED %s", quantor->io.in_name);
  LOG (quantor, 1, "  %d PARSED VARIABLES", quantor->stats.external_vars);
  LOG (quantor, 1, "  %d PARSED CLAUSES", quantor->stats.external_clauses);
  LOG (quantor, 1, "  %d PARSED LITERALS", quantor->stats.external_lits);
  LOG (quantor, 1, "  %d PARSED SCOPES", quantor->stats.external_scopes);
  LOG (quantor, 1, "PARSE TIME %.2f SECONDS", time);
#endif

  assert (parser->parsing);
  parser->parsing = 0;

  return !parser->err;
}

/*------------------------------------------------------------------------*/

static int
is_empty_Scope (Scope * scope)
{
  return !scope->vars.first;
}

/*------------------------------------------------------------------------*/

static int
is_propositional (Quantor * quantor)
{
  Scope *p;

  if (quantor->invalid)
    return 1;

  for (p = quantor->scopes.first; p; p = p->link.next)
    if (!is_empty_Scope (p) && p->type == QUANTOR_UNIVERSAL)
      return 0;

  return 1;
}

/*------------------------------------------------------------------------*/

static int
var_can_not_be_eliminated (Quantor * quantor, Var * var)
{
  if (var->eliminated)
    return 1;

  if (var->zombie)
    return 1;

  if (var->exported && !quantor->opts.resolve_exported)
    return 1;

  assert (!is_assigned_var (var));

  return 0;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_DOUBLE_PRECISION_INT
/*------------------------------------------------------------------------*/

static int
dpi2int (DPI dpi)
{
  if (dpi <= INT_MIN || dpi > QUANTOR_OVERFLOW)
    return QUANTOR_OVERFLOW;
  else
    return (int) dpi;
}

/*------------------------------------------------------------------------*/

static int
mult_with_overflow (int a, int b)
{
  if (!a || !b)
    return 0;

  if (a >= QUANTOR_OVERFLOW || b >= QUANTOR_OVERFLOW)
    return QUANTOR_OVERFLOW;

  return dpi2int (((DPI) a) * ((DPI) b));
}

/*------------------------------------------------------------------------*/

static int
add_with_overflow (int a, int b)
{
  if (a >= QUANTOR_OVERFLOW || b >= QUANTOR_OVERFLOW)
    return QUANTOR_OVERFLOW;

  return dpi2int (((DPI) a) + ((DPI) b));
}

/*------------------------------------------------------------------------*/
#else /* explicit saturating arithmetic without double precision int */
/*------------------------------------------------------------------------*/

static int
ldfloor2 (int n)
{
  if (n & 2)
    return 1;
  else
    return 0;
}

/*------------------------------------------------------------------------*/

static int
ldfloor4 (int n)
{
  if (n & 0xc)
    return ldfloor2 (n >> 2) + 2;
  else
    return ldfloor2 (n);
}

/*------------------------------------------------------------------------*/

static int
ldfloor8 (int n)
{
  if (n & 0xf0)
    return ldfloor4 (n >> 4) + 4;
  else
    return ldfloor4 (n);
}

/*------------------------------------------------------------------------*/

static int
ldfloor16 (int n)
{
  if (n & 0xff00)
    return ldfloor8 (n >> 8) + 8;
  else
    return ldfloor8 (n);
}

/*------------------------------------------------------------------------*/

static int
ldfloor (int n)
{
  assert (n >= 0);

  if (n & 0xffff0000)
    return ldfloor16 (n >> 16) + 16;
  else
    return ldfloor16 (n);
}

/*------------------------------------------------------------------------*/
/* Returns the number of bits necessary to represent the given number.
 */
static int
num_bits (int n)
{
  assert (n >= 0);

  return ldfloor (n) + 1;
}

/*------------------------------------------------------------------------*/
/* We work with a special saturating arithmetic by adding a new value
 * 'QUANTOR_OVERFLOW' which is a number slightly smaller than 'INT_MAX'.  The
 * arithmetic is 'saturating' in the sense that adding or multiplying any
 * value with 'QUANTOR_OVERFLOW' results in 'QUANTOR_OVERFLOW' as well.  In
 * addition we make sure that any real overflow during addition or
 * multiplication is detected and results in 'QUANTOR_OVERFLOW' to be returned.
 */
static int
mult_with_overflow (int a, int b)
{
  int res, sign, tmp;

  if (a == 0 || b == 0)
    return 0;

  if (a >= QUANTOR_OVERFLOW || b >= QUANTOR_OVERFLOW)
    return QUANTOR_OVERFLOW;

  if (a > b)
    {
      tmp = a;
      a = b;
      b = tmp;
    }

  if (b < 0)
    {
      assert (a < 0);

      if (a == INT_MIN || b == INT_MIN)
	return QUANTOR_OVERFLOW;

      a = -a;
      b = -b;
      sign = 1;
    }
  else if (a < 0)
    {
      assert (b >= 0);

      if (a == INT_MIN)
	return QUANTOR_OVERFLOW;

      sign = -1;
      a = -a;
    }
  else
    {
      assert (a > 0 && b > 0);
      sign = 1;
    }

  if (a > b)
    {
      tmp = a;
      a = b;
      b = tmp;
    }

  res = a * b;

  if (a == 1 || (a == 2 && num_bits (b) <= 30 && res > 0) ||
      (a > 2 && (num_bits (a) + num_bits (b) <= 31)))
    {
      assert (res > 0);

      if (sign == -1)
	res = -res;
    }
  else
    res = QUANTOR_OVERFLOW;

  if (res >= QUANTOR_OVERFLOW)
    res = QUANTOR_OVERFLOW;

  return res;
}

/*------------------------------------------------------------------------*/

static int
add_with_overflow (int a, int b)
{
  int sign, tmp, res;

  if (a >= QUANTOR_OVERFLOW || b >= QUANTOR_OVERFLOW)
    return QUANTOR_OVERFLOW;
  else if ((a <= 0 && b >= 0) || (a >= 0 && b <= 0))
    return a + b;

  if (a < 0 && b < 0)
    {
      if (a == INT_MIN || b == INT_MIN)
	return QUANTOR_OVERFLOW;
      a = -a;
      b = -b;
      sign = -1;
    }
  else
    sign = 1;

  if (a < b)
    {
      tmp = a;
      a = b;
      b = tmp;
    }

  res = a + b;

  if (res < a || (sign > 0 && res >= QUANTOR_OVERFLOW))
    return QUANTOR_OVERFLOW;

  res *= sign;

  assert (res < 0 || res < QUANTOR_OVERFLOW);

  return res;
}

/*------------------------------------------------------------------------*/
#endif /* QUANTOR_HAVE_DOUBLE_PRECISION_INT */
/*------------------------------------------------------------------------*/

static int
is_representative (Var * v)
{
  EquivalenceClass *ec;
  int res;

  ec = v->eqclass;
  assert (ec);
  assert (ec->representative);
  res = (ec->representative == v);

  return res;
}

/*------------------------------------------------------------------------*/
/* This function computes an upper bound on the number of literals added if
 * 'v' is eliminated by generating all resolvents on 'v' and removing all
 * clauses in which 'v' occurs.  It is only an upper bound since some of the
 * resolvents could be further simplified or may lead to the simplification
 * of other clauses.  
 *
 * Let 's(l)' denote the sum of the number of literals of clauses in which
 * 'l' occurs and 'o(l)' the number of occurences of 'l'.  As an abreviation
 * use 'd(l) = s(l) - o(l)'.
 *
 * In the resulting clauses, obtained from resolving on all clauses in
 * which 'v' resp. '-v' occurs, we have 'o(-v)' copies of the 'd(v)'
 * literals from the clauses in which 'v' occured positively and 'o(v)'
 * copies of the 'd(-v)' literals in which 'v' occured negatively.
 *
 *                     s(v)               s(-v)      
 *                   literals            literals
 *              .-------^-------.   .-------^-------.
 *              +---------------+   +-----------+---+
 *              |           | v |   |           |-v |
 * o(v) clauses |           | : | x |           | : |  o(-v) clauses
 *              |           | v |   |           |-v |
 *              +-----------+---+   +-----------+---+
 *              `-----v-----'       `-----v-----'
 *           d(v) = s(v) - o(v)   d(-v) = s(-v) - o(-v)
 *                literals            literals
 */
static int
exists_resolve_all_score (Quantor * quantor, Var * v)
{
  int pos_delta, neg_delta, pos_lits, neg_lits, added, removed, res;
  Lit *pos, *neg;

  (void) quantor;

  pos = var2lit (v, 0);
  neg = var2lit (v, 1);

  /* The sum of the number of literals in the clauses in which 'v' occurs
   * positively resp. negatively without counting 'v' resp. '-v'.
   */
  pos_delta = pos->sum - pos->column.len;	/* d(v) */
  neg_delta = neg->sum - neg->column.len;	/* d(-v) */

  /* o(-v) * d(v)
   */
  pos_lits = mult_with_overflow (pos_delta, neg->column.len);

  /* o(v) * d(-v)
   */
  neg_lits = mult_with_overflow (neg_delta, pos->column.len);

  /* o(-v) * d(v) + o(v) * d(-v)
   */
  added = add_with_overflow (pos_lits, neg_lits);

  /* s(v) + s(-v)
   */
  removed = add_with_overflow (pos->sum, neg->sum);

  /* o(-v) * d(v) + o(v) * d(-v) - (s(v) + s(-v))
   */
  res = add_with_overflow (added, -removed);

  /* Mark as being resolved by simple resolution of all clauses with 'v'.
   */
  v->cheapest_function_to_substitute = 0;

  return res;
}

/*------------------------------------------------------------------------*/
/* In function resolution we replace the variable 'v' by substituting the
 * RHS of a function with LHS 'l = v' or 'l = -v'.  This follows from
 *     
 *     exists x . (x = rhs & rest)  ==  rest [x := rhs]    (*)
 *
 * if 'x' does not occur in 'rhs'.  Assume we have extracted the following
 * function of size 'n':
 *   
 *     l = l1 l2 ... ln
 *
 * WE USE JUXTAPOSITION TO DENOTE DISJUNCTION!
 *
 * Then the set of clauses in which 'v' or '-v' occurs can be partitioned in
 * to four classes:
 *
 *   C1 -l                l -l1     l D1
 *    :         C -l       :         :
 *   Cm -l                l -ln     l Dk
 *                      
 *     P         Q         R         S
 *
 * The 'n+1' clauses in 'Q' and 'R' have been used to extract the function.
 * In 'P' we collect the 'm' remaining clauses in which '-l' occurs and in
 * 'S' the remaining 'k' clauses in which 'l' occurs.  
 *
 * Substitution now results in replacing 'l' in 'S' by 'C' and '-l' by '-C'
 * in 'P'.  The first substitution simply results in:
 *   
 *   (C D1) & ... & (C Dk)     (**)
 *   
 * The latter substitution destroys the CNF, which can be fixed by
 * applying the distributivity law as follows (i = 1..m):
 *
 *    Ci -C 
 *    == 
 *    Ci (-l1 & ... & -ln) 
 *    ==
 *    (Ci -l1) & ... & (Ci -ln)    (***)
 *
 * First note, that these 'n * m' clauses are exactly the resolvents in the
 * resolution of the clauses in 'P' with the clauses in 'R' (or short 'P'
 * with 'R').  Of course, the same applies to the 'k' clauses obtained
 * substituting 'l' by 'C' in 'S'.  Here we simply resolve 'Q' with 'S'.
 *
 * The resolution of 'Q' with 'R' will only produce trivial clauses.  Thus,
 * using function resolution following (*) first avoids generating the
 * trivial clauses 'Q x R' and also does not need to add the clauses
 * obtained by resolving 'P' with 'S'.  It will never produce more literals
 * than ordinary elimination by resolution.
 *
 * Finally it is interesting to note that all of the potential resolvents
 * between 'P' and 'S', which actually are not generated, are subsumed by
 * the CNF obtained by function resolution:
 *
 * Resolving 'Ci -l' with 'l Dj' results in 'Ci Dj' which can be obtained
 * in one hyper resolution step over the clauses
 *
 *    (Ci -l1) & ... & (Ci -ln) & (C Dj)
 *
 * and thus does not need to be added.
 *
 * To summarize, function resolution produces less clauses and literals and
 * can be seen as ordinary variable elimination by resolution.
 *
 * In addition to the terminology defined for'exists_resolve_all_score' we
 * use 's(P)' to denote the sum of literals in 'P' and 'o(P) = |P| = m' to
 * denote the number of clauses in 'P' etc. and obtain:
 *   
 *   s(R) = 2*n             o(R) = n
 *   s(Q) = n+1             o(Q) = 1
 *   s(P) = s(-l) - (n+1)   o(P) = m = o(-l) - 1
 *   s(S) = s(l) - 2*n      o(S) = k = o(l) - n
 *
 * The number of added literals by function resolution is the number of
 * literals in 'P x R' in (***)
 *   
 *   n * s(P) = n * (s(-l) - (n+1))
 *
 * plus the number of literals in 'Q x S' in (**)
 *   
 *   s(S) + k * (n - 1) = s(l) - 2*n + (o(l) - n) * (n - 1)
 *
 * The number of removed literals is the same as above.
 */
static int
exists_function_resolution_score_aux (Quantor * quantor, Function * f)
{
  int PR, QS, added, removed, res, PR1, n, QS1, QS2, ol;
  Lit *not_l, *l;

  (void) quantor;

  l = f->lhs;
  ol = l->column.len;
  not_l = QUANTOR_NOT (l);
  n = f->rhs->size;

  /* s(-l) - (n+1)
   */
  PR1 = add_with_overflow (not_l->sum, 0 - (n + 1));

  /* n * (s(-l) - (n+1))
   */
  PR = mult_with_overflow (n, PR1);

  /* s(l) - 2*n
   */
  QS1 = add_with_overflow (l->sum, 0 - 2 * n);

  /* (o(l) - n) * (n - 1)
   */
  QS2 = mult_with_overflow (ol - n, n - 1);

  /* s(l) - 2*n + (o(l) - n) * (n - 1)
   */
  QS = add_with_overflow (QS1, QS2);

  added = add_with_overflow (PR, QS);
  removed = add_with_overflow (l->sum, not_l->sum);
  res = add_with_overflow (added, 0 - removed);

  return res;
}

/*------------------------------------------------------------------------*/

static int
exists_function_resolution_score (Quantor * quantor, Var * v)
{
  Function *p;
  int res, tmp;

  assert (v->functions.first);
  res = QUANTOR_OVERFLOW;

  for (p = v->functions.first; p; p = p->lhs_link.next)
    {
      tmp = exists_function_resolution_score_aux (quantor, p);
      if (tmp < res)
	{
	  v->cheapest_function_to_substitute = p;
	  res = tmp;
	}
    }

  return res;
}

/*------------------------------------------------------------------------*/

static int
exists_score (Quantor * quantor, Var * v)
{
  int res;

  if (is_assigned_var (v))
    return QUANTOR_ASSIGNED_SCORE;

  if (v->eqclass && !is_representative (v))
    return QUANTOR_NONREPRESENTATIVE_SCORE;

  if (var_can_not_be_eliminated (quantor, v))
    return QUANTOR_STICKY_SCORE;

  if (quantor->opts.function_resolution && v->functions.first)
    res = exists_function_resolution_score (quantor, v);
  else
    res = exists_resolve_all_score (quantor, v);

  return res;
}

/*------------------------------------------------------------------------*/

static int
forall_score (Quantor * quantor, Var * v)
{
  int res, sum, len;
  Lit *neg, *pos;

  (void) quantor;

  assert (!is_assigned_var (v));
  assert (!var_can_not_be_eliminated (quantor, v));

  pos = var2lit (v, 0);
  neg = var2lit (v, 1);
  len = add_with_overflow (0 - pos->column.len, 0 - neg->column.len);
  sum = add_with_overflow (0 - pos->sum, 0 - neg->sum);
  res = add_with_overflow (len, sum);

  return res;
}

/*------------------------------------------------------------------------*/

static int
reorder_var (Quantor * quantor, Var * v)
{
  int old_score, new_score, old_order;
  Scope *scope;

  INCSTATS2 (quantor->stats.reordered);

  old_score = v->score;
  scope = v->scope;
  (void) old_score;

  assert (scope);
  assert (is_linked (&scope->reorder.first, v, &v->reorder_link));

  if (scope->type == QUANTOR_EXISTENTIAL)
    new_score = exists_score (quantor, v);
  else
    new_score = forall_score (quantor, v);

  undlink (&scope->reorder.first, v, &v->reorder_link);

  if (old_score == new_score)
    return 0;

  v->score = new_score;
#ifdef QUANTOR_LOG8
  LOG (quantor, 8,
       "UPDATED VARIABLE %d WITH NEW SCORE %s",
       v->idx, score2str (quantor, new_score));
#endif

  old_order = v->rank;
  (void) old_order;
  fix_order (quantor, &scope->order, v->rank);

#ifdef QUANTOR_LOG8
  if (old_order != v->rank)
    LOG (quantor, 8, "REORDERED VARIABLE %d FROM %d TO %d",
	 v->idx, old_order, v->rank);
#endif

  return 1;
}

/*------------------------------------------------------------------------*/

static unsigned
reorder_scope (Quantor * quantor, Scope * scope)
{
  unsigned count;
  Var *v;

  count = 0;
  while ((v = scope->reorder.first))
    count += reorder_var (quantor, v);

#ifdef QUANTOR_LOG5
  if (count)
    LOG (quantor, 7, "REORDERED %d VARIABLES IN SCOPE %s",
	 count, scope2str (quantor, scope));
#endif

  return count;
}

/*------------------------------------------------------------------------*/

static Scope *
prev_non_empty_scope (Scope * scope)
{
  Scope *res;

  for (res = scope ? scope->link.prev : 0;
       res && is_empty_Scope (res); res = res->link.prev)
    ;

  return res;
}

/*------------------------------------------------------------------------*/

static Scope *
next_non_empty_scope (Scope * scope)
{
  Scope *res;

  for (res = scope ? scope->link.next : 0;
       res && is_empty_Scope (res); res = res->link.next)
    ;

  return res;
}

/*------------------------------------------------------------------------*/

static Scope *
last_non_empty_existential_scope (Quantor * quantor)
{
  Scope *res;

  res = quantor->scopes.last;
  if (res && is_empty_Scope (res))
    res = prev_non_empty_scope (res);

  assert (!res || res->type == QUANTOR_EXISTENTIAL);

  return res;
}

/*------------------------------------------------------------------------*/

static Scope *
first_non_empty_innermost_existential_scope (Quantor * quantor)
{
  Scope *res, *prev;

  res = last_non_empty_existential_scope (quantor);
  prev = prev_non_empty_scope (res);

  while (prev && prev->type == QUANTOR_EXISTENTIAL)
    {
      res = prev;
      prev = prev_non_empty_scope (res);
    }

  return res;
}

/*------------------------------------------------------------------------*/

static Scope *
first_non_empty_scope (Quantor * quantor)
{
  Scope *res;

  for (res = quantor->scopes.first;
       res && is_empty_Scope (res); res = res->link.next)
    ;

  return res;
}

/*------------------------------------------------------------------------*/

static int
literals_per_clause_limit_exceeded (Quantor * quantor, Var * existential_var)
{
  int sum, res, len;
  Scope *scope;
  double ratio;

  assert (is_existential (existential_var));
  scope = first_non_empty_innermost_existential_scope (quantor);
  sum = existential_var->score;
  len = 0;
  while (scope)
    {
      sum += scope->sum;
      len += scope->clauses.len;
      scope = scope->link.next;
    }
  assert (len > 0);
  ratio = sum / (double) len;
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "NEW RATIO AFTER EXISTS WOULD BE %.1f", ratio);
#endif
  res = (ratio >= quantor->opts.literals_per_clause_limit);

  return res;
}

/*------------------------------------------------------------------------*/
#ifndef NDEBUG
/*------------------------------------------------------------------------*/

static void
check_basically_simplified (Quantor * quantor)
{
  assert (!quantor->units.first);
  assert (!quantor->unates.first);
  assert (!quantor->dying_clauses.first);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static Var *
cheapest_existential (Quantor * quantor, int *cost_ptr, const char *msg)
{
  Var *res, *tmp;
  Scope *p;

  res = 0;
  for (p = quantor->scopes.last; p; p = p->link.prev)
    {
      if (is_empty_Scope (p))
	continue;

      if (p->type == QUANTOR_UNIVERSAL)
	break;

      reorder_scope (quantor, p);
      assert (count_PtrStack (&p->order) > 0);
      tmp = p->order.start[0];
      if (!res || res->score > tmp->score)
	res = tmp;
    }

  assert (res);
#ifdef QUANTOR_LOG4
  LOG (quantor, 4,
       "CHEAPEST %s VARIABLE %d HAS EXPECTED COST OF %s",
       msg, res->idx, score2str (quantor, res->score));
#else
  (void) msg;
#endif
  *cost_ptr = res->score;

  return res;
}

/*------------------------------------------------------------------------*/

static Var *
cheapest_universal (Quantor * quantor, int *cost_ptr)
{
  int cost, min_cost, removed, added;
  Scope *p, *existential_scope;
  Var *res, *tmp;

  existential_scope = first_non_empty_innermost_existential_scope (quantor);

  added = 0;
  for (p = existential_scope; p; p = p->link.next)
    added += p->sum;

  res = 0;
  min_cost = INT_MAX;

  for (p = existential_scope->link.prev; p; p = p->link.prev)
    {
      if (is_empty_Scope (p))
	continue;

      if (p->type == QUANTOR_EXISTENTIAL)
	break;

      reorder_scope (quantor, p);
      assert (count_PtrStack (&p->order) > 0);
      tmp = p->order.start[0];
      removed = tmp->score;
      assert (removed == QUANTOR_OVERFLOW || removed < 0);
      cost = add_with_overflow (removed, added);
      if (!res || min_cost > cost)
	{
	  res = tmp;
	  min_cost = cost;
	}
    }

  assert (res);
#ifdef QUANTOR_LOG4
  LOG (quantor, 4,
       "CHEAPEST INNER UNIVERSAL VARIABLE %d HAS EXPECTED COST OF %s",
       res->idx, score2str (quantor, min_cost));
#endif
  *cost_ptr = min_cost;

  return res;
}

/*------------------------------------------------------------------------*/

static int
reached_hard_exists_limit (Quantor * quantor, int cost)
{
  if (cost < quantor->opts.hard_exists_limit)
    return 0;

#ifdef QUANTOR_LOG4
  LOG (quantor, 4, "COST %d REACHED HARD EXISTS LIMIT %d",
       cost, quantor->opts.hard_exists_limit);
#endif

  return 1;
}

/*------------------------------------------------------------------------*/

static int
reached_soft_exists_limit (Quantor * quantor, int cost)
{
  int i, res, len, *literals;
  double sum, avg;

  sum = cost;
  len = quantor->opts.soft_exists_length;
  literals = quantor->opts.literals_added_by_exists;

  for (i = 0; i < len; i++)
    sum += literals[i];

  assert (len >= 0);
  avg = sum / (double) (len + 1);

#ifdef QUANTOR_LOG4
  LOG (quantor, 4, "AVERAGE LAST %d EXIST COSTS IS %.2f", len, avg);
#endif

  if (avg >= (double) quantor->opts.soft_exists_limit)
    {
#ifdef QUANTOR_LOG4
      LOG (quantor, 4, "SOFT EXIST LIMIT %d REACHED",
	   quantor->opts.soft_exists_limit);
#endif
      res = 1;
    }
  else
    res = 0;

  return res;
}

/*------------------------------------------------------------------------*/

static void
save_actual_exists_costs (Quantor * quantor, int actual_cost)
{
  int i, len, *literals;

  len = quantor->opts.soft_exists_length;
  if (len <= 0)
    return;

  literals = quantor->opts.literals_added_by_exists;

  for (i = 1; i < len; i++)
    literals[i - 1] = literals[i];

  literals[len - 1] = actual_cost;
}

/*------------------------------------------------------------------------*/

static int
reached_exists_limit (Quantor * quantor, int cost)
{
  if (reached_hard_exists_limit (quantor, cost))
    return 1;

  if (reached_soft_exists_limit (quantor, cost))
    return 1;

  return 0;
}

/*------------------------------------------------------------------------*/

static Var *
pivot (Quantor * quantor, int *expected_cost_ptr)
{
  int existential_cost, universal_cost, choose_universal;
  Var *res, *existential_var, *universal_var;
  double biased_cost;

#ifndef NDEBUG
  check_basically_simplified (quantor);
#endif
  if (!quantor->opts.exists && !quantor->opts.forall)
    return 0;

  existential_cost = universal_cost = 0;
  existential_var = universal_var = 0;

  if (quantor->opts.exists)
    existential_var =
      cheapest_existential (quantor, &existential_cost, "INNER EXISTENTIAL");

  if (quantor->opts.exists && !quantor->opts.forall)
    {
#ifdef QUANTOR_LOG5
      LOG (quantor, 5, "CHOOSING EXIST SINCE FORALL IS DISABLED");
#endif
      choose_universal = 0;
    }
  else if (quantor->opts.exists &&
	   !reached_exists_limit (quantor, existential_cost))
    {
      INCSTATS2 (quantor->stats.smaller_exists_limit);
#ifdef QUANTOR_LOG5
      LOG (quantor, 5, "EXISTENTIAL COST IS BELOW EXISTS LIMIT");
#endif
      choose_universal = 0;
    }
  else
    {
      assert (quantor->opts.forall);
      universal_var = cheapest_universal (quantor, &universal_cost);
      if (!quantor->opts.exists)
	{
	  choose_universal = 1;
	}
      else if (quantor->opts.exists &&
	       literals_per_clause_limit_exceeded (quantor, existential_var))
	{
#ifdef QUANTOR_LOG5
	  LOG (quantor, 5, "EXISTS WOULD EXCEED LITERALS PER CLAUSE RATIO");
#endif
	  INCSTATS2 (quantor->stats.literals_per_clause_limit_exceeded);
	  quantor->opts.literals_per_clause_limit *=
	    quantor->opts.literals_per_clause_factor;
	  choose_universal = 1;
	}
      else
	{
	  biased_cost = quantor->opts.forall_bias * existential_cost;
	  choose_universal = (universal_cost <= biased_cost);
#ifdef QUANTOR_LOG5
	  LOG (quantor, 5,
	       "UNIVERSAL COST %d %s BIASED EXISTENTIAL COST %.0f",
	       universal_cost, choose_universal ? "<=" : ">", biased_cost);
#endif
	}
    }

  if (choose_universal)
    {
      assert (universal_var);
      *expected_cost_ptr = universal_cost;
      res = universal_var;
    }
  else
    {
      assert (existential_var);
      *expected_cost_ptr = existential_cost;
      res = existential_var;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
add_literals_of_clause_except (Quantor * quantor, Clause * c, Var * v)
{
  Lit *pos, *neg, *lit;
  Cell *p, *eor;
#ifndef NDEBUG
  Lit *found = 0;
#endif

  pos = v->lits;
  neg = pos + 1;

  eor = end_of_row (c);
  for (p = c->row; p < eor; p++)
    {
      lit = p->lit;

      if (lit == pos || lit == neg)
	{
#ifndef NDEBUG
	  assert (!found);
	  found = lit;
#endif
	}
      else
	push_PtrStack (quantor, &quantor->new_clause, lit);
    }

  assert (found);
}

/*------------------------------------------------------------------------*/

static void
resolve (Quantor * quantor, Var * v, Clause * c, Clause * d)
{
#ifdef QUANTOR_LOG6
  LOG (quantor, 6, "RESOLVING CLAUSE %d AND CLAUSE %d ON %d",
       c->idx, d->idx, v->idx);
#endif
  assert (!count_PtrStack (&quantor->new_clause));
  add_literals_of_clause_except (quantor, c, v);
  add_literals_of_clause_except (quantor, d, v);
  add_new_clause (quantor, "RESOLVED");
}

/*------------------------------------------------------------------------*/

static void
resolve_all_clauses_with (Quantor * quantor, Var * v)
{
  Lit *neg, *pos;
  Cell *p, *q;

  pos = var2lit (v, 0);
  neg = var2lit (v, 1);

  for (p = pos->column.first; p; p = p->column_link.next)
    for (q = neg->column.first; q; q = q->column_link.next)
      resolve (quantor, v, p->clause, q->clause);
}

/*------------------------------------------------------------------------*/

static void
remove_all_clauses_with (Quantor * quantor, Lit * lit)
{
  Cell *p, *next;

  for (p = lit->column.first; p; p = next)
    {
      next = p->column_link.next;
      kill_Clause (quantor, p->clause);
    }
}

/*------------------------------------------------------------------------*/

static void
mark_clauses_of_substituted_function (Quantor * quantor,
				      Function * f, int new_mark)
{
  Clause *clause, *binary_clause;
  Lit *lit, *not_lit, *lhs;
  Cell *p, *eor;
  int found_lhs;

  lhs = f->lhs;
  clause = f->clause;
  assert (clause->part_of_substituted_function != new_mark);
  clause->part_of_substituted_function = new_mark;

  found_lhs = 0;
  (void) found_lhs;
  eor = end_of_row (clause);
  for (p = clause->row; p < eor; p++)
    {
      lit = p->lit;
      not_lit = QUANTOR_NOT (lit);
      if (not_lit == lhs)
	{
	  found_lhs = 1;
	  continue;
	}

      binary_clause = find_binary_clause (quantor, lhs, not_lit);
      assert (binary_clause);
      assert (binary_clause->part_of_substituted_function == !new_mark);
      binary_clause->part_of_substituted_function = new_mark;
    }

  assert (found_lhs);
}

/*------------------------------------------------------------------------*/

static void
substitute_cheapest_function (Quantor * quantor, Var * v)
{
  Lit *neg, *pos;
  Clause *a, *b;
  Function *f;
  Cell *p, *q;

  assert (quantor->opts.function_resolution);
  assert (v->functions.first);

  INCSTATS2 (quantor->stats.exists_function_resolution);

  f = v->cheapest_function_to_substitute;
  assert (f);
  assert (f->lhs->var == v);
  mark_clauses_of_substituted_function (quantor, f, 1);

  pos = var2lit (v, 0);
  neg = var2lit (v, 1);

  for (p = pos->column.first; p; p = p->column_link.next)
    for (q = neg->column.first; q; q = q->column_link.next)
      {
	a = p->clause;
	b = q->clause;

	if (a->part_of_substituted_function ==
	    b->part_of_substituted_function)
	  continue;

	resolve (quantor, v, a, b);
      }

  mark_clauses_of_substituted_function (quantor, f, 0);
}

/*------------------------------------------------------------------------*/
/* To eliminate an existential variable we remove all clauses in which the
 * variable occurs (positively or negatively) and add all possible
 * resolvents of this variable.  This is the same elimination procedure as
 * in the original resolution based Davis and Putnam procedure.
 */
static void
exists (Quantor * quantor, Var * v)
{
  assert (is_existential (v));

  INCSTATS1 (quantor->stats.exists);

  if (v->cheapest_function_to_substitute)
    substitute_cheapest_function (quantor, v);
  else
    resolve_all_clauses_with (quantor, v);

  remove_all_clauses_with (quantor, var2lit (v, 1));
  remove_all_clauses_with (quantor, var2lit (v, 0));

  gc (quantor);

#if 0
  {
    static int count = 0;
    char buffer[200];
    sprintf (buffer, "/tmp/quantor-after-exists-%d.cnf", count++);
    FILE * file = fopen (buffer, "w");
    quantor_print (quantor, file);
    fclose (file);
  }
#endif
}

/*------------------------------------------------------------------------*/

static void
map_var (Quantor * quantor, Var * v)
{
  assert (!is_assigned_var (v));
  assert (!v->mapped);
  v->mapped = gen_Var (quantor, default_scope (quantor));
#ifdef QUANTOR_LOG5
  LOG (quantor, 5, "MAPPED VARIABLE %d TO GENERIC VARIABLE %d",
       v->idx, v->mapped->idx);
#endif
}

/*------------------------------------------------------------------------*/

static Lit *
map_lit (Lit * lit)
{
  Var *target;
  Lit *res;

  target = lit->var->mapped;
  if (target)
    {
      res = target->lits;
      if (!is_signed (lit))
	res = QUANTOR_NOT (res);
    }
  else
    res = lit;

  return res;
}

/*------------------------------------------------------------------------*/
/* There are three cases:                     
 *                                             {l0, l1, l2}
 *
 *   1. variable does not occur in clause:     v, -v not in {l0, l1, l2}
 *
 *     keep old clause                         {l0, l1, l2}
 *     map clause as a whole                   {l0', l1', l2'}
 *
 *   2. lit in clause:                         l0 = v
 *     
 *     kill old clause                         --------
 *     copy clause without lit                 {l1, l2}
 *     
 *   3. not_lit in clause:                     l0 = -v
 *     
 *     kill old clause                         ----------
 *     map clause without not_lit              {l1', l2'}
 *
 * Note, that 'mapping' really means mapping all literals (l) of the clause
 * to their 'mapped' counterpart (l').  Copying keeps the old literals!
 */
static void
forall_clause (Quantor * quantor, Clause * clause, Var * v)
{
  Lit *lit, *not_lit, *old_lit, *new_lit;
  int found_lit, found_not_lit;
  Clause *added_clause;
  Cell *p, *eor;
#ifndef NDEBUG
  int size = clause->size;
  int count = size;
#endif

  not_lit = v->lits;
  lit = QUANTOR_NOT (not_lit);
  found_not_lit = found_lit = 0;

  eor = end_of_row (clause);

  for (p = clause->row; !found_lit && !found_not_lit && p < eor; p++)
    {
      old_lit = p->lit;
      if (lit == old_lit)
	found_lit = 1;		/* case 2. */
      else if (not_lit == old_lit)
	found_not_lit = 1;	/* case 3. */
    }

  assert (!(found_not_lit && found_lit));

  if (found_not_lit || found_lit)	/* case 2. or 3. */
    {
      kill_Clause (quantor, clause);
#ifndef NDEBUG
      count -= size;
#endif
    }

  if (found_lit)		/* case 2. */
    {
      assert (!found_not_lit);
      assert (!count_PtrStack (&quantor->new_clause));

      for (p = clause->row; p < eor; p++)
	{
	  old_lit = p->lit;
	  if (old_lit != lit)
	    push_PtrStack (quantor, &quantor->new_clause, old_lit);
	}

      added_clause = add_new_clause (quantor, "FORALL");
      (void) added_clause;
#ifndef NDEBUG
      count += added_clause ? added_clause->size : 0;
#endif
    }

  if (!found_lit)		/* case 1. or 3. */
    {
      assert (!count_PtrStack (&quantor->new_clause));
      for (p = clause->row; p < eor; p++)
	{
	  old_lit = p->lit;
	  if (old_lit != not_lit)
	    {
	      new_lit = map_lit (old_lit);
	      push_PtrStack (quantor, &quantor->new_clause, new_lit);
	    }
	}
      if (found_not_lit)
	added_clause = add_new_clause (quantor, "FORALL MAPPED REDUCED");
      else
	added_clause = add_new_clause (quantor, "FORALL MAPPED");
#ifndef NDEBUG
      count += added_clause ? added_clause->size : 0;
#endif
    }

#ifndef NDEBUG
  if (found_not_lit || found_lit)
    assert (count <= size - 1);
  else
    assert (count <= 2 * size);
#endif
}

/*------------------------------------------------------------------------*/

static void
unmap_var (Quantor * quantor, Var * v)
{
  (void) quantor;

  assert (v->mapped);
  v->mapped = 0;
}

/*------------------------------------------------------------------------*/
/* Implementation of the scope reduction optimization enabled by
 * 'reduce_scope = 1'.   This is particular useful for problems of the
 * following form:
 *
 *   exists u, v. forall x, y. exists a, b, c, d.
 *
 *     w(u,v) & 		// can be ignored
 *     f(u,a) & g(v,b) &	// outer clauses
 *     p(x,a) & q(a,c) &	// connect a to c
 *     s(y,b) & t(b,d)          // connect b to d
 *
 *   ==
 *
 *   exists u, v.
 *
 *     w(u,v) &
 *    
 *     (forall x. exists a, c. f(u,a) & p(x,a) & q(a,c)) &
 *
 *     (forall y. exists b, d. g(v,b) & s(y,b) & t(b,d))
 *
 *   ==
 *
 *   exists u, v, a, c, a', c'.
 *
 *     w(u,v) &
 *    
 *     (f(u,a) & p(x,a) & q(a,c))) [0/x] &
 *     (f(u,a') & p(x,a') & q(a',c'))) [1/x] &
 *
 *     (forall y. exists b, d. g(v,b) & s(y,b) & t(b,d))
 *
 *   ==
 *
 *   exists u, v, a, b, c, d, a', b', c', d'.
 *
 *     w(u,v) &
 *    
 *     (f(u,a) & p(x,a) & q(a,c))) [0/x] &
 *     (f(u,a') & p(x,a') & q(a',c'))) [1/x] &
 *
 *     (g(v,b) & s(y,b) & t(b,d))) [0/y] &
 *     (g(v,b') & s(y,b') & t(b',d'))) [1/y]
 *
 * Note that expanding x and y without scope reduction would result in
 * drippling the number of copies of 'a, b, c, d' from 4 to 12.  Also many
 * more clauses would have been 'mapped'.
 *
 * In the next function we mark all variables that recursively share a
 * clause with 'quantified_var' ignoring variables with lower nesting, with
 * the 'quantified_var' as an exception.  These marked variables and the
 * clauses connecting them are the only clauses that need to be copied in
 * forall expansion.  The connecting clauses are marked as well.
 */
static void
mark_to_be_mapped (Quantor * quantor, Var * quantified_var)
{
  unsigned num_mapped_vars, num_mapped_clauses;
  Cell *p, *q, *eor;
  Scope *first_scope;
  Var *var, *tmp;
  PtrStack stack;
  Clause *c;
  int sign;

  assert (!quantified_var->to_be_mapped);
  num_mapped_vars = num_mapped_clauses = 0;
  first_scope = first_non_empty_innermost_existential_scope (quantor);

  init_PtrStack (quantor, &stack);
  push_PtrStack (quantor, &stack, quantified_var);

  while (count_PtrStack (&stack))
    {
      var = pop_PtrStack (quantor, &stack);

      if (var != quantified_var && var->scope->nesting < first_scope->nesting)
	continue;

      if (var->to_be_mapped)
	continue;

      var->to_be_mapped = 1;
      num_mapped_vars++;

      for (sign = 0; sign <= 1; sign++)
	{
	  for (p = var->lits[sign].column.first; p; p = p->column_link.next)
	    {
	      c = p->clause;
	      if (c->to_be_mapped)
		continue;

	      c->to_be_mapped = 1;
	      num_mapped_clauses++;

	      eor = end_of_row (c);
	      for (q = c->row; q < eor; q++)
		{
		  tmp = q->lit->var;
		  if (tmp->to_be_mapped)
		    continue;

		  push_PtrStack (quantor, &stack, tmp);
		}
	    }
	}
    }
  release_PtrStack (quantor, &stack);

  assert (quantified_var->to_be_mapped);
  quantified_var->to_be_mapped = 0;
  num_mapped_vars--;

#ifdef QUANTOR_LOG4
  LOG (quantor, 4, "MARKED %u VARIABLES AND %u CLAUSES TO BE MAPPED",
       num_mapped_vars, num_mapped_clauses);
#endif
}

/*------------------------------------------------------------------------*/
/* Compare with the comments to 'mark_to_be_mapped'.  Note, that this
 * optimizations can be disabled by setting 'reduce_scope = 0'.
 */
static void
collect_mapped_vars (Quantor * quantor, PtrStack * mapped_vars)
{
  Scope *scope, *first_scope, *last_scope, *end;
  unsigned num_candidates, num_collected;
  Var *v;

  first_scope = first_non_empty_innermost_existential_scope (quantor);
  last_scope = last_non_empty_existential_scope (quantor);
  end = last_scope->link.next;
  assert (last_scope->nesting >= first_scope->nesting);

  num_candidates = num_collected = 0;

  for (scope = first_scope; scope != end; scope = scope->link.next)
    {
      for (v = scope->vars.first; v; v = v->scope_link.next)
	{
	  num_candidates++;

	  if (quantor->opts.reduce_scope && !v->to_be_mapped)
	    continue;

	  num_collected++;
	  v->to_be_mapped = 0;
	  push_PtrStack (quantor, mapped_vars, v);
	}
    }
#ifdef QUANTOR_LOG4
  LOG (quantor, 4, "COLLECTED %u VARIABLES OUT OF %u (%.1f%%)",
       num_collected, num_candidates,
       num_candidates ? 100.0 * num_collected / (double) num_candidates : 0);
#endif
}

/*------------------------------------------------------------------------*/
/* See 'collect_mapped_vars' and 'mark_to_be_mapped'.
 */
static void
collect_mapped_clauses (Quantor * quantor, PtrStack * mapped_clauses)
{
  Scope *scope, *first_scope, *last_scope, *end;
  unsigned num_candidates, num_collected;
  Clause *c;

  first_scope = first_non_empty_innermost_existential_scope (quantor);
  last_scope = last_non_empty_existential_scope (quantor);
  end = last_scope->link.next;
  assert (last_scope->nesting >= first_scope->nesting);

  num_candidates = num_collected = 0;

  for (scope = first_scope; scope != end; scope = scope->link.next)
    {
      for (c = scope->clauses.first; c; c = c->scope_link.next)
	{
	  num_candidates++;
	  if (quantor->opts.reduce_scope && !c->to_be_mapped)
	    continue;

	  num_collected++;
	  c->to_be_mapped = 0;
	  push_PtrStack (quantor, mapped_clauses, c);
	}
    }

#ifdef QUANTOR_LOG4
  LOG (quantor, 4, "COLLECTED %u CLAUSES OUT OF %u (%.1f%%)",
       num_collected, num_candidates,
       num_candidates ? 100.0 * num_collected / (double) num_candidates : 0);
#endif
}

/*------------------------------------------------------------------------*/
/* This is an expensive elimination procedure for one universally quantified
 * var.  Here is the logic:
 *
 *   exists a, b. forall x, y. exists c, d. 
 *
 *     f(a,b) & 
 *     g (a,b,c,d,x,y)
 *
 *   ==
 *
 *   exists a, b. forall y. exists c, d, c', d'. 
 *
 *     f(a,b) &
 *     g(a,b,c, d, 0,y) &
 *     g(a,b,c',d',1,y)
 */
static void
forall (Quantor * quantor, Var * quantified_var)
{
  PtrStack mapped_clauses, mapped_vars;
  void **p;

  forward_subsume (quantor);

  assert (is_universal (quantified_var));
#ifndef NDEBUG
  quantor->entered_forall = quantified_var;
#endif
  init_PtrStack (quantor, &mapped_vars);
  init_PtrStack (quantor, &mapped_clauses);

  mark_to_be_mapped (quantor, quantified_var);

  collect_mapped_vars (quantor, &mapped_vars);
  for (p = mapped_vars.start; p < mapped_vars.top; p++)
    map_var (quantor, *p);

  collect_mapped_clauses (quantor, &mapped_clauses);
  for (p = mapped_clauses.start; p < mapped_clauses.top; p++)
    forall_clause (quantor, *p, quantified_var);

  release_PtrStack (quantor, &mapped_clauses);

  for (p = mapped_vars.start; p < mapped_vars.top; p++)
    unmap_var (quantor, *p);

  release_PtrStack (quantor, &mapped_vars);

  INCSTATS1 (quantor->stats.foralls);
  bcp (quantor);
#ifndef NDEBUG
  quantor->entered_forall = 0;
#endif
}

/*------------------------------------------------------------------------*/

static void
eliminate (Quantor * quantor, Var * v, int expected_cost)
{
  int delta, was_existential;
#ifdef QUANTOR_LOG4
  int idx = v->idx;
  const char *type = is_existential (v) ? "EXISTS" : "FORALL";
  LOG (quantor, 4, "%s %d EXPECTS COST %s",
       type, v->idx, score2str (quantor, expected_cost));
#else
  (void) expected_cost;
#endif
  assert (v->scope);
  assert (!v->eliminated);

  quantor->stats.eliminated_cells = 0;

  v->eliminated = 1;
  if (v->eqclass)
    v->eqclass->num_eliminated++;

  if ((was_existential = is_existential (v)))
    exists (quantor, v);
  else
    forall (quantor, v);

  /************************************/
  /* NOTE: 'v' may now be invalid !!! */
  /************************************/

  delta = quantor->stats.eliminated_cells;
  assert (quantor->invalid || delta <= expected_cost);
#ifdef QUANTOR_LOG4
  LOG (quantor, 4,
       "%s %d ADDED %d LITERALS (EXPECTED %s)",
       type, idx, delta, score2str (quantor, expected_cost));
#endif
  if (was_existential)
    save_actual_exists_costs (quantor, delta);

#ifdef QUANTOR_LOG4
  LOG (quantor, 4, "SCOPES AFTER ELIMINATION OF VARIABLE %d:", idx);
#endif
#ifdef QUANTOR_LOG3
  log_scopes (quantor, 3);
#endif
  check_invariant (quantor, 1);
}

/*------------------------------------------------------------------------*/

static int
is_trivial (Quantor * quantor, QuantorResult * res_ptr)
{
  QuantorResult res;

  if (quantor->invalid)
    res = QUANTOR_RESULT_UNSATISFIABLE;
  else if (!quantor->clauses.len)
    res = QUANTOR_RESULT_SATISFIABLE;
  else
    res = QUANTOR_RESULT_UNKNOWN;

  if (res_ptr)
    *res_ptr = res;

  return res != QUANTOR_RESULT_UNKNOWN;
}

/*------------------------------------------------------------------------*/

static int
timeout (Quantor * quantor)
{
  double delta;
  int res;

  if (quantor->opts.time_limit < 0)
    return 0;

  delta = get_time () - quantor->stats.time;
  res = (delta >= quantor->opts.time_limit);

  return res;
}

/*------------------------------------------------------------------------*/

static int
spaceout (Quantor * quantor)
{
  double mb;
  int res;

  if (quantor->opts.space_limit < 0)
    return 0;

  mb = quantor->stats.bytes;
  mb /= (1 << 20);
  res = (mb >= quantor->opts.space_limit);

  return res;
}

/*------------------------------------------------------------------------*/

static QuantorResult
limit_reached (Quantor * quantor)
{
  if (timeout (quantor))
    return QUANTOR_RESULT_TIMEOUT;

  if (spaceout (quantor))
    return QUANTOR_RESULT_SPACEOUT;

  return QUANTOR_RESULT_UNKNOWN;
}

/*------------------------------------------------------------------------*/

static QuantorResult trivial_truth (Quantor *);
static QuantorResult trivial_falsity (Quantor *);

/*------------------------------------------------------------------------*/

static QuantorResult
trivial_truth_and_falsity (Quantor * quantor)
{
  QuantorResult res;

  res = trivial_truth (quantor);
  assert (res != QUANTOR_RESULT_UNSATISFIABLE);
  if (res != QUANTOR_RESULT_UNKNOWN)
    return res;

  res = trivial_falsity (quantor);
  assert (res != QUANTOR_RESULT_SATISFIABLE);

  return res;
}

/*------------------------------------------------------------------------*/

static QuantorResult
inner_quantified_variables_elimination_loop (Quantor * quantor)
{
  QuantorResult res;
  Var *var;
  int cost;

  for (;;)
    {
      bcp (quantor);

      if (is_trivial (quantor, &res))
	return res;

      if (is_propositional (quantor))
	return QUANTOR_RESULT_UNKNOWN;

      if ((res = limit_reached (quantor)) != QUANTOR_RESULT_UNKNOWN)
	return res;

      var = pivot (quantor, &cost);
      if (!var)
	return QUANTOR_RESULT_UNKNOWN;

      if (is_universal (var))
	{
	  res = trivial_truth_and_falsity (quantor);
	  if (res != QUANTOR_RESULT_UNKNOWN)
	    return res;
	}

      eliminate (quantor, var, cost);
    }
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG2
/*------------------------------------------------------------------------*/

#define MAX_WORST 1e12

/*------------------------------------------------------------------------*/

static double
worst_case_number_vars_after_expansion_rec (Quantor * quantor, Scope * scope)
{
  Scope *next;
  double res;
  unsigned i;

  if (!scope)
    return 0;

  next = scope->link.next;
  res = worst_case_number_vars_after_expansion_rec (quantor, next);

  if (scope->type == QUANTOR_EXISTENTIAL)
    {
      res += scope->vars.len;
    }
  else
    {
      for (i = 0; res < MAX_WORST && i < scope->vars.len; i++)
	res *= 2.0;
    }

  if (res >= MAX_WORST)
    res = MAX_WORST;

  return res;
}

/*------------------------------------------------------------------------*/

static double
worst_case_number_vars_after_expansion (Quantor * quantor)
{
  Scope *first = quantor->scopes.first;
  return worst_case_number_vars_after_expansion_rec (quantor, first);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static QuantorResult
eliminate_inner_quantified_variables (Quantor * quantor)
{
  QuantorResult res;
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS1
  double rel_num_vars, num_vars;
#endif
  double worst = worst_case_number_vars_after_expansion (quantor);
  LOG (quantor, 2, "STARTING ELIMINATION OF QUANTIFIED VARIABLES");
  if (worst < MAX_WORST)
    LOG (quantor, 2, "EXPECTING AT MOST %.0f EXPANDED VARIABLES", worst);
  else
    LOG (quantor, 2, "CAN NOT PRINT EXPECTED NUMBER OF EXPANDED VARIABLES");
#endif
  forward_subsume (quantor);
  res = inner_quantified_variables_elimination_loop (quantor);
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS1
  num_vars = quantor->stats.vars.num;
  LOG (quantor, 2, "EXPANDED TO %.0f VARIABLES", num_vars);

  if (worst < MAX_WORST)
    {
      rel_num_vars = (worst > 0) ? 100.0 * num_vars / worst : 0;

      LOG (quantor, 2,
	   "EXPANDED VARIABLES ARE %.1f%% OF WORST CASE %.0f",
	   rel_num_vars, worst);
    }
  else
#endif
    LOG (quantor, 2, "CAN NOT PRINT EXPECTED NUMBER OF EXPANDED VARIABLES");

  LOG (quantor, 2, "FINISHED ELIMINATION OF QUANTIFIED VARIABLES");
#endif
  return res;
}

/*------------------------------------------------------------------------*/

static QuantorResult
eliminate_propositional_variables (Quantor * quantor)
{
#ifdef QUANTOR_LOG2
  int count;
#endif
  int became_trivial, cost;
  QuantorResult res;
  Var *v;

  assert (is_propositional (quantor));

  forward_subsume (quantor);

  if (is_trivial (quantor, &res))
    return res;

#ifndef NDEBUG
  check_basically_simplified (quantor);
#endif

  if (!quantor->opts.exists)
    {
#ifdef QUANTOR_LOG2
      LOG (quantor, 2, "EXISTS OF PROPOSITIONAL VARIABLES DISABLED");
#endif
      return QUANTOR_RESULT_UNKNOWN;
    }

#ifdef QUANTOR_LOG2
  count = 0;
  LOG (quantor, 2, "STARTING ELIMINATION OF PROPOSITIONAL VARIABLES");
#endif

  res = QUANTOR_RESULT_UNKNOWN;

  do
    {
      if ((res = limit_reached (quantor)) != QUANTOR_RESULT_UNKNOWN)
	break;

      v = cheapest_existential (quantor, &cost, "PROPOSITIONAL");
      if (v->exported && !quantor->opts.resolve_exported)
	{
#ifdef QUANTOR_LOG5
	  LOG (quantor, 5, "NO MORE UNEXPORTED PROPOSITIONAL VARIABLES");
#endif
	  break;
	}

      if (reached_exists_limit (quantor, cost))
	{
#ifdef QUANTOR_LOG5
	  LOG (quantor, 5, "EXISTENTIAL COST IS ABOVE EXISTS LIMIT");
#endif
	  break;
	}

      eliminate (quantor, v, cost);
      bcp (quantor);
#ifdef QUANTOR_LOG2
      count++;
#endif
    }
  while (!is_trivial (quantor, 0));

  if (res == QUANTOR_RESULT_UNKNOWN)
    became_trivial = is_trivial (quantor, &res);
  else
    became_trivial = 0;

#ifdef QUANTOR_LOG2
  LOG (quantor, 2,
       "ELIMINATED %d PROPOSITIONAL VARIABLES BY RESOLUTION", count);
  if (became_trivial)
    LOG (quantor, 2, "TRIVIAL AFTER PROPOSITIONAL ELIMINATION");
#endif

  return res;
}

/*------------------------------------------------------------------------*/
#if defined(QUANTOR_LOG1) && defined(QUANTOR_STATS1)
/*------------------------------------------------------------------------*/

static int
count_remaining_propositional_vars (Quantor * quantor)
{
  void **p;
  Var *v;
  int res;

  res = 0;

  for (p = quantor->vars.start; p < quantor->vars.top; p++)
    {
      v = *p;
      if (!v)
	continue;

      if (var_does_not_occur (v))
	continue;

      if (v->assignment == QUANTOR_UNASSIGNED || !is_constant (v->assignment))
	res++;
    }

  return res;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
equivalence_class_can_be_assigned_trivially (EquivalenceClass * ec)
{
  return !ec->num_eliminated && ec->num_zombies == ec->elems.len;
}

/*------------------------------------------------------------------------*/

static void
assign_not_eliminated_zombies (Quantor * quantor)
{
  Var *v, *next;
  int assigned;

#ifdef QUANTOR_LOG3
  LOG (quantor, 3, "ASSIGNING NOT ELIMINATED ZOMBIES");
#endif
  assigned = 0;

  for (v = quantor->zombies.first; v; v = next)
    {
      next = v->zombie_link.next;

      assert (v->exported);
      assert (v->zombie);
      assert (var_does_not_occur (v));

      if (is_assigned_var (v) || v->eliminated)
	continue;

      if (v->eqclass &&
	  !equivalence_class_can_be_assigned_trivially (v->eqclass))
	continue;

      assign (quantor, v, QUANTOR_FALSE);
      assigned++;
    }
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "ASSIGNED %d NOT ELIMINATED ZOMBIES", assigned);
#endif
}

/*------------------------------------------------------------------------*/

static QuantorResult
propositional_simplification (Quantor * quantor)
{
  QuantorResult res;
#if defined(QUANTOR_LOG1) && defined(QUANTOR_STATS1)
  int old_num_vars, delta_num_vars, new_num_vars;
  int old_num_clauses, delta_num_clauses;
  int old_num_cells, delta_num_cells;
  double old_time, delta_time;

  old_time = get_time ();
  old_num_vars = count_remaining_propositional_vars (quantor);
  old_num_cells = quantor->stats.cells.num;
  old_num_clauses = quantor->stats.clauses.num;
#endif

  bcp (quantor);

  if (is_trivial (quantor, &res))
    return res;

  res = eliminate_propositional_variables (quantor);

  if (res != QUANTOR_RESULT_UNKNOWN)
    return res;

  assert (!is_trivial (quantor, 0));

  forward_subsume (quantor);
  assert (!is_trivial (quantor, 0));

#ifdef QUANTOR_CHECK
  if (quantor->opts.check == 1)
    {
      check_no_backward_subsumptions (quantor);
      check_no_forward_subsumptions (quantor);
    }
#endif
#if defined(QUANTOR_LOG1) && defined(QUANTOR_STATS1)
  new_num_vars = count_remaining_propositional_vars (quantor);
  delta_num_vars = old_num_vars - new_num_vars;
  delta_num_clauses = old_num_clauses - quantor->stats.clauses.num;
  delta_num_cells = old_num_cells - quantor->stats.cells.num;

  LOG (quantor, 1, "PROPOSITIONAL SIMPLIFICATION");
  LOG (quantor, 1, "  %.1f%% (%d = %d - %d) VARIABLE REDUCTION",
       old_num_vars ?
       100.0 * (delta_num_vars / (double) old_num_vars) : 0,
       delta_num_vars, old_num_vars, new_num_vars);
  LOG (quantor, 1, "  %.1f%% (%d = %d - %d) CLAUSE REDUCTION",
       old_num_clauses ?
       100.0 * (delta_num_clauses / (double) old_num_clauses) : 0,
       delta_num_clauses, old_num_clauses, quantor->clauses.len);
  LOG (quantor, 1, "  %.1f%% (%d = %d - %d) LITERAL REDUCTION",
       old_num_cells ?
       100.0 * (delta_num_cells / (double) old_num_cells) : 0,
       delta_num_cells, old_num_cells, quantor->stats.cells.num);
  delta_time = get_time () - old_time;
  delta_time = (delta_time >= 0) ? delta_time : 0;
  LOG (quantor, 1, "PROPOSITIONAL SIMPLIFICATION TOOK %.2f SECONDS",
       delta_time);
#endif

  return res;
}

/*------------------------------------------------------------------------*/

static int
max_occuring_idx (Quantor * quantor)
{
  void **p;
  Var *v;
  int res;

  res = 0;
  for (p = quantor->vars.start; p < quantor->vars.top; p++)
    {
      v = *p;
      if (!v)
	continue;

      assert (v->idx > res);

      res = v->idx;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
print_header (Quantor * quantor, FILE * file)
{
  int max_idx;

  max_idx = max_occuring_idx (quantor);
  fprintf (file, "p cnf %d %d\n", max_idx, quantor->clauses.len);
}

/*------------------------------------------------------------------------*/

static void
print_Scope (Quantor * quantor,
	     Scope * prev, Scope * scope, Scope * next, FILE * file)
{
  Var *p;

  (void) quantor;

  if (!prev || prev->type != scope->type)
    fprintf (file, "%c ", scope->type == QUANTOR_UNIVERSAL ? 'a' : 'e');

  for (p = scope->vars.first; p; p = p->scope_link.next)
    fprintf (file, "%d ", p->idx);

  if (!next || next->type != scope->type)
    fputs ("0\n", file);
}

/*------------------------------------------------------------------------*/

static void
print_scopes (Quantor * quantor, FILE * file)
{
  Scope *prev, *this, *next;

  prev = 0;
  this = first_non_empty_scope (quantor);
  next = next_non_empty_scope (this);

  while (this)
    {
      print_Scope (quantor, prev, this, next, file);
      prev = this;
      this = next;
      next = next_non_empty_scope (this);
    }
}

/*------------------------------------------------------------------------*/

static void
print_clauses (Quantor * quantor, FILE * file)
{
  Clause *c;

  for (c = quantor->clauses.first; c; c = c->link.next)
    print_clause (quantor, c, file);
}

/*------------------------------------------------------------------------*/

void
quantor_print (Quantor * quantor, FILE * file)
{
  if (!quantor->invalid)
    {
      print_header (quantor, file);
      if (!is_propositional (quantor))
	print_scopes (quantor, file);
      print_clauses (quantor, file);
    }
  else
    fprintf (file, "p cnf 0 1\n0\n");

  fflush (file);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/

void
quantor_debug_print (Quantor * quantor)
{
  quantor_print (quantor, stdout);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifndef QUANTOR_QBF_EVALUATION_FORMAT
/*------------------------------------------------------------------------*/

static void
print_line_buffer (Quantor * quantor)
{
  fputc ('v', quantor->io.out);
  fputs (quantor->line_buffer, quantor->io.out);
  fputc ('\n', quantor->io.out);
  quantor->line_buffer_len = 0;
  quantor->line_buffer[0] = 0;
}

/*------------------------------------------------------------------------*/

static void
add_to_line_buffer (Quantor * quantor, unsigned i)
{
  unsigned len, old_line_buffer_len, new_line_buffer_len;

  sprintf (quantor->num_buffer, " %d", i);
  len = strlen (quantor->num_buffer);
  assert (len + 1 <= sizeof (quantor->num_buffer));
  old_line_buffer_len = quantor->line_buffer_len;

  if (old_line_buffer_len)
    {
      new_line_buffer_len = old_line_buffer_len + len;
      if (new_line_buffer_len + 1 > sizeof (quantor->line_buffer))
	{
	  print_line_buffer (quantor);
	  goto ADD_TO_BEGINNING_OF_LINE_BUFFER;
	}
      else
	{
	  strcpy (quantor->line_buffer + old_line_buffer_len,
		  quantor->num_buffer);
	  quantor->line_buffer_len = new_line_buffer_len;
	}
    }
  else
    {
    ADD_TO_BEGINNING_OF_LINE_BUFFER:
      strcpy (quantor->line_buffer, quantor->num_buffer);
      quantor->line_buffer_len = len;
    }
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

int
quantor_deref (Quantor * quantor, int idx)
{
  Lit * assignment;
  Var * v;

  assert (idx > 0);
  assert (idx <= quantor->max_idx);

  v = quantor->vars.start[idx];

  if (!v)
    return -1;

  if (!v->exported)
    return -1;

  assignment = v->assignment;

  if (assignment == QUANTOR_FALSE)
    return 0;

  if (assignment == QUANTOR_TRUE)
    return 1;

  return -1;
}

/*------------------------------------------------------------------------*/

static int
val2lit (int idx, int val)
{
  if (val == 0)
    return -idx;

  if (val == 1)
    return idx;

  return 0;
}

/*------------------------------------------------------------------------*/

static void
print_assignment (Quantor * quantor)
{
  int lit, val;
  void **p;
  Var *v;

#ifndef QUANTOR_QBF_EVALUATION_FORMAT
  assert (!quantor->line_buffer_len);
  assert (!quantor->line_buffer[0]);
#endif

  for (p = quantor->vars.start; p < quantor->vars.top; p++)
    {
      v = *p;
      if (!v || !v->exported)
	continue;

      val = quantor_deref (quantor, v->idx);
      if (!(lit = val2lit (v->idx, val)))
	continue;

#ifdef QUANTOR_QBF_EVALUATION_FORMAT
      fprintf (quantor->io.out, "V %d\n", lit);
#else
      add_to_line_buffer (quantor, lit);
#endif
    }

#ifndef QUANTOR_QBF_EVALUATION_FORMAT
  add_to_line_buffer (quantor, 0);
  if (quantor->line_buffer_len)
    print_line_buffer (quantor);
#endif
}

/*------------------------------------------------------------------------*/

const int *
quantor_assignment (Quantor * quantor)
{
  int i, val, lit;

  reset_IntStack (quantor, &quantor->external_assignment, 0);

  for (i = 1; i <= quantor->max_idx; i++)
    {
      val = quantor_deref (quantor, i);
      lit = val2lit (i, val);
      if (!lit)
	continue;

      push_IntStack (quantor, &quantor->external_assignment, lit);
    }

  push_IntStack (quantor, &quantor->external_assignment, 0);

  return quantor->external_assignment.start;
}

/*------------------------------------------------------------------------*/

static void
SatSolver_init (Quantor * quantor, SatSolver * solver, SatSolverAPI * api)
{
  solver->api = api;
  solver->quantor = quantor;
}

/*------------------------------------------------------------------------*/

static void
SatSolver_release (SatSolver * solver)
{
  release_PtrStack (solver->quantor, &solver->assignment);
  release_IntStack (solver->quantor, &solver->idx2pidx);
}

/*------------------------------------------------------------------------*/

static int
SatSolver_lit2plit (SatSolver * solver, int lit)
{
  int sign, plit, idx, pidx, *p;

  sign = (lit < 0) ? -1 : 1;
  idx = (sign < 0) ? -lit : lit;
  p = access_IntStack (solver->quantor, &solver->idx2pidx, idx);
  if (!*p)
    {
      pidx = ++solver->max_pidx;
      *p = pidx;
#ifdef QUANTOR_LOG5
      LOG (solver->quantor, 5,
	   "MAPPING IDX=%d TO PIDX=%d FOR \"%s\"",
	   idx, pidx, solver->api->name);
#endif
    }

  pidx = *p;
  plit = pidx * sign;

  return plit;
}

/*------------------------------------------------------------------------*/

static int
SatSolver_copy (SatSolver * solver, LitIt * it)
{
  int plit, lit, count_clauses, count_literals, ok;
  Quantor *quantor = solver->quantor;
  double time, delta;
#ifndef NDEBUG
  int pidx;
#endif

  time = get_time ();
  count_clauses = count_literals = 0;
  ok = 1;

  while (ok && !it->api->done (it, &lit))
    {
      if (lit)
	{
	  plit = SatSolver_lit2plit (solver, lit);
	  count_literals++;
	}
      else
	{
	  plit = 0;
	  count_clauses++;
	}
#ifndef NDEBUG
      pidx = (plit < 0) ? -plit : plit;
      assert (0 <= pidx);
      assert (pidx <= solver->max_pidx);
#endif
      ok = solver->api->add (solver, plit);
    }

  if (quantor->opts.verbose)
    {
      delta = get_time () - time;
      delta = (delta < 0) ? 0 : delta;
#ifdef QUANTOR_LOG1
      LOG (quantor, 1, ok ? "COPIED" : "COPIED ONLY");
      LOG (quantor, 1, "  %d CLAUSES", count_clauses);
      LOG (quantor, 1, "  %d LITERALS", count_literals);
      LOG (quantor, 1, "  %d MAX VARIABLE INDEX", solver->max_pidx);
      LOG (quantor, 1, "IN %.2f SECONDS", delta);
      if (!ok)
	LOG (quantor, 1, "COPYING FAILED");
#endif
    }

  return ok;
}

/*------------------------------------------------------------------------*/

static void
SatSolver_init_assignment (SatSolver * solver)
{
  int pidx;

  assert (!solver->assignment_initialized);

  /* We need to start at the invalid index 'pidx == 0'.
   */
  for (pidx = 0; pidx <= solver->max_pidx; pidx++)
    push_PtrStack (solver->quantor, &solver->assignment, QUANTOR_UNASSIGNED);
}

/*------------------------------------------------------------------------*/
#if defined(QUANTOR_HAVE_LIMMAT) || \
    defined(QUANTOR_HAVE_FUNEX)
/*------------------------------------------------------------------------*/

static void
SatSolver_set_assignment (SatSolver * solver, const int *assignment)
{
  int plit, pidx;
  const int *p;
  Lit *tmp;

  assert (assignment);
  assert (!solver->assignment_initialized);

  for (p = assignment; (plit = *p); p++)
    {
      tmp = (plit < 0) ? QUANTOR_FALSE : QUANTOR_TRUE;
      pidx = (plit < 0) ? -plit : plit;
      assert (pidx <= solver->max_pidx);
      assert (solver->assignment.start[pidx] == QUANTOR_UNASSIGNED);
      solver->assignment.start[pidx] = tmp;
    }

  solver->assignment_initialized = 1;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static Lit *
SatSolver_deref (SatSolver * solver, int lit)
{
  int pidx, plit;
  Lit *res;
#ifndef NDEBUG
  Var *v = int2var (solver->quantor, lit, 0, 0);
  assert (v->exported);
#endif
  if (!solver->assignment_initialized)
    solver->api->assignment (solver);

  assert (lit > 0);
  plit = SatSolver_lit2plit (solver, lit);
  assert (plit > 0);
  pidx = plit;
  assert (pidx <= (int) solver->max_pidx);
  assert (pidx < (int) count_PtrStack (&solver->assignment));
  res = solver->assignment.start[pidx];
  assert (res != QUANTOR_UNASSIGNED);
#ifdef QUANTOR_LOG5
  LOG (solver->quantor, 5,
       "SAT SOLVER \"%s\" ASSIGNS LIT(%d)=PLIT(%d)=%s",
       solver->api->name, lit, plit, assignment2str (solver->quantor, res));
#endif

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG1
/*------------------------------------------------------------------------*/

static const char *
res2str (QuantorResult res)
{
  switch (res)
    {
    case QUANTOR_RESULT_UNSATISFIABLE:
      return "UNSATISFIABLE";
    case QUANTOR_RESULT_SATISFIABLE:
      return "SATISFIABLE";
    case QUANTOR_RESULT_TIMEOUT:
      return "TIMEOUT";
    case QUANTOR_RESULT_SPACEOUT:
      return "SPACEOUT";
    default:
      assert (res == QUANTOR_RESULT_UNKNOWN);
      return "UNKNOWN";
    }
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static QuantorResult
SatSolver_run (SatSolver * solver)
{
  Quantor *quantor = solver->quantor;
  QuantorResult res;

  INCSTATS2 (quantor->stats.sat_solver);
  res = quantor->sat_api->run (solver);
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "SAT SOLVER \"%s\" RETURNS %s",
       solver->api->name, res2str (res));
#endif

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_FUNEX
/*------------------------------------------------------------------------*/

#include "funex.h"

/*------------------------------------------------------------------------*/

typedef struct SatSolverFunEx SatSolverFunEx;

struct SatSolverFunEx
{
  SatSolver super;
  FunEx *funex;
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *SatSolverFunEx_get_api (void);

/*------------------------------------------------------------------------*/

static SatSolver *
SatSolverFunEx_new (Quantor * quantor)
{
  double time_limit, time_sofar;
  SatSolverFunEx *this;

  this = new (quantor, sizeof (*this));
  SatSolver_init (quantor, &this->super, SatSolverFunEx_get_api ());
  this->funex = funex_init ();
  if (quantor->opts.verbose)
    {
      fprintf (quantor->io.out,
	       FUNEX_PREFIX "FunEx SAT Solver Version %s\n",
	       funex_version ());
      fprintf (quantor->io.out, FUNEX_PREFIX "%s\n", funex_id);
    }

  funex_set_log_file (this->funex, quantor->io.out);
  funex_set_verbose (this->funex, quantor->opts.verbose);
  if (quantor->opts.verbose)
    funex_set_report_interval (this->funex, 1000);

  if (quantor->opts.time_limit >= 0)
    {
      time_sofar = get_time () - quantor->stats.time;
      time_limit = quantor->opts.time_limit - time_sofar;
      time_limit = (time_limit < 0) ? 0 : time_limit;
      funex_set_time_limit (this->funex, time_limit);
#ifdef QUANTOR_LOG2
      LOG (quantor, 2, "SAT SOLVER TIME LIMIT %.1f seconds", time_limit);
#endif
    }

  /* TODO: space limit for funex */

  return &this->super;
}

/*------------------------------------------------------------------------*/

static void
SatSolverFunEx_delete (SatSolver * solver)
{
  SatSolverFunEx *this = (SatSolverFunEx *) solver;
  Quantor *quantor = solver->quantor;
#ifdef QUANTOR_STATS1
  size_t bytes;
#endif

  if (quantor->opts.verbose)
    fputs (funex_stats (this->funex), quantor->io.out);

#ifdef QUANTOR_STATS1
  quantor->stats.sat_solver_time += funex_seconds (this->funex);
  bytes = funex_bytes (this->funex);
  if (bytes > quantor->stats.sat_solver_bytes)
    quantor->stats.sat_solver_bytes = bytes;
#endif
  funex_release (this->funex);
  SatSolver_release (solver);
  delete (quantor, this, sizeof (*this));
}

/*------------------------------------------------------------------------*/

static int
SatSolverFunEx_add (SatSolver * solver, int lit)
{
  SatSolverFunEx *this = (SatSolverFunEx *) solver;
  funex_add_literal (this->funex, lit);
  return 1;
}

/*------------------------------------------------------------------------*/

static QuantorResult
SatSolverFunEx_run (SatSolver * solver)
{
  SatSolverFunEx *this = (SatSolverFunEx *) solver;
  QuantorResult res;
  int funex_res;

  funex_res = funex_sat (this->funex);

  switch (funex_res)
    {
    case FUNEX_SATISFIABLE:
      res = QUANTOR_RESULT_SATISFIABLE;
      break;
    case FUNEX_UNSATISFIABLE:
      res = QUANTOR_RESULT_UNSATISFIABLE;
      break;
    case FUNEX_TIMEOUT:
      res = QUANTOR_RESULT_TIMEOUT;
      break;
    case FUNEX_SPACEOUT:
      res = QUANTOR_RESULT_SPACEOUT;
      break;
    default:
      res = QUANTOR_RESULT_UNKNOWN;
      break;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
SatSolverFunEx_assignment (SatSolver * solver)
{
  SatSolverFunEx *this = (SatSolverFunEx *) solver;
  const int *assignment;

  assignment = funex_assignment (this->funex);
  assert (assignment);
  SatSolver_init_assignment (solver);
  SatSolver_set_assignment (solver, assignment);
}

/*------------------------------------------------------------------------*/

static SatSolverAPI funex_api = {
  "funex",
  SatSolverFunEx_new,
  SatSolverFunEx_delete,
  SatSolverFunEx_add,
  SatSolverFunEx_run,
  SatSolverFunEx_assignment
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *
SatSolverFunEx_get_api (void)
{
  return &funex_api;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_LIMMAT
/*------------------------------------------------------------------------*/

#include "limmat.h"

/*------------------------------------------------------------------------*/

typedef struct SatSolverLimmat SatSolverLimmat;

struct SatSolverLimmat
{
  SatSolver super;
  Limmat *limmat;
  IntStack clause;
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *SatSolverLimmat_get_api (void);

/*------------------------------------------------------------------------*/

static SatSolver *
SatSolverLimmat_new (Quantor * quantor)
{
  SatSolverLimmat *this;

  this = new (quantor, sizeof (*this));
  SatSolver_init (quantor, &this->super, SatSolverLimmat_get_api ());
  this->limmat = new_Limmat (0);
  if (quantor->opts.verbose)
    {
      set_log_Limmat (this->limmat, quantor->io.out);

      fprintf (quantor->io.out,
	       LIMMAT_PREFIX "Limmat SAT Solver Version %s\n",
	       version_Limmat ());

      fprintf (quantor->io.out, LIMMAT_PREFIX "%s\n", id_Limmat ());
    }

  /* TODO: probably need to have a standard time out and space out limit */

  return &this->super;
}

/*------------------------------------------------------------------------*/

static void
SatSolverLimmat_delete (SatSolver * solver)
{
  SatSolverLimmat *this = (SatSolverLimmat *) solver;
  Quantor *quantor = solver->quantor;
#ifdef QUANTOR_STATS1
  size_t bytes;
#endif

  if (quantor->opts.verbose)
    stats_Limmat (this->limmat, quantor->io.out);

#ifdef QUANTOR_STATS1
  quantor->stats.sat_solver_time += time_Limmat (this->limmat);
  bytes = bytes_Limmat (this->limmat);
  if (bytes > quantor->stats.sat_solver_bytes)
    quantor->stats.sat_solver_bytes = bytes;
#endif
  delete_Limmat (this->limmat);
  release_IntStack (quantor, &this->clause);
  SatSolver_release (solver);
  delete (quantor, this, sizeof (*this));
}

/*------------------------------------------------------------------------*/

static int
SatSolverLimmat_add (SatSolver * solver, int lit)
{
  SatSolverLimmat *this = (SatSolverLimmat *) solver;

  push_IntStack (solver->quantor, &this->clause, lit);

  if (!lit)
    {
      add_Limmat (this->limmat, this->clause.start);
      reset_IntStack (solver->quantor, &this->clause, 0);
    }

  return 1;
}

/*------------------------------------------------------------------------*/

static QuantorResult
SatSolverLimmat_run (SatSolver * solver)
{
  SatSolverLimmat *this = (SatSolverLimmat *) solver;
  QuantorResult res;
  int limmat_res;

  limmat_res = sat_Limmat (this->limmat, -1);

  if (limmat_res < 0)
    res = QUANTOR_RESULT_UNKNOWN;
  else if (limmat_res)
    res = QUANTOR_RESULT_SATISFIABLE;
  else
    res = QUANTOR_RESULT_UNSATISFIABLE;

  return res;
}

/*------------------------------------------------------------------------*/

static void
SatSolverLimmat_assignment (SatSolver * solver)
{
  SatSolverLimmat *this = (SatSolverLimmat *) solver;
  const int *assignment;

  assignment = assignment_Limmat (this->limmat);
  assert (assignment);
  SatSolver_init_assignment (solver);
  SatSolver_set_assignment (solver, assignment);
}

/*------------------------------------------------------------------------*/

static SatSolverAPI limmat_api = {
  "limmat",
  SatSolverLimmat_new,
  SatSolverLimmat_delete,
  SatSolverLimmat_add,
  SatSolverLimmat_run,
  SatSolverLimmat_assignment
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *
SatSolverLimmat_get_api (void)
{
  return &limmat_api;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_NANOSAT
/*------------------------------------------------------------------------*/

#include "nanosat.h"

/*------------------------------------------------------------------------*/

static SatSolverAPI *SatSolverNanosat_get_api (void);

/*------------------------------------------------------------------------*/

static SatSolver *
SatSolverNanosat_new (Quantor * quantor)
{
  double time_limit, time_sofar;
  SatSolver *solver;

  solver = new (quantor, sizeof (*solver));
  SatSolver_init (quantor, solver, SatSolverNanosat_get_api ());
  nanosat_init ();

  if (quantor->opts.verbose)
    {
      fprintf (quantor->io.out,
	       NANOSAT_PREFIX "Nanosat SAT Solver Version %s\n",
	       nanosat_version ());
      fprintf (quantor->io.out, NANOSAT_PREFIX "%s\n", nanosat_id ());
    }

  nanosat_set_output_file (quantor->io.out);

  if (quantor->opts.time_limit >= 0)
    {
      time_sofar = get_time () - quantor->stats.time;
      time_limit = quantor->opts.time_limit - time_sofar;
      time_limit = (time_limit < 0) ? 0 : time_limit;
      nanosat_set_time_limit (time_limit);
#ifdef QUANTOR_LOG2
      LOG (quantor, 2, "SAT SOLVER TIME LIMIT %.1f seconds", time_limit);
#endif
    }

  /* TODO: space limit for nanosat */

  return solver;
}

/*------------------------------------------------------------------------*/

static void
SatSolverNanosat_delete (SatSolver * solver)
{
  Quantor *quantor = solver->quantor;
#ifdef QUANTOR_STATS1
  size_t bytes;
#endif

  if (quantor->opts.verbose)
    nanosat_stats (quantor->io.out);

#ifdef QUANTOR_STATS1
  quantor->stats.sat_solver_time += nanosat_seconds ();
  bytes = nanosat_bytes ();
  if (bytes > quantor->stats.sat_solver_bytes)
    quantor->stats.sat_solver_bytes = bytes;
#endif

  nanosat_release ();
  SatSolver_release (solver);
  delete (quantor, solver, sizeof (*solver));
}

/*------------------------------------------------------------------------*/

static int
SatSolverNanosat_add (SatSolver * solver, int lit)
{
  (void) solver;
  return nanosat_add (lit);
}

/*------------------------------------------------------------------------*/

static QuantorResult
SatSolverNanosat_run (SatSolver * solver)
{
  QuantorResult res;
  int nanosat_res;

  (void) solver;
  nanosat_res = nanosat_sat ();

  switch (nanosat_res)
    {
    case NANOSAT_SATISFIABLE:
      res = QUANTOR_RESULT_SATISFIABLE;
      break;
    case NANOSAT_UNSATISFIABLE:
      res = QUANTOR_RESULT_UNSATISFIABLE;
      break;
    case NANOSAT_TIME_OUT:
      res = QUANTOR_RESULT_TIMEOUT;
      break;
    case NANOSAT_SPACE_OUT:
      res = QUANTOR_RESULT_SPACEOUT;
      break;
    default:
      res = QUANTOR_RESULT_UNKNOWN;
      break;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
SatSolverNanosat_assignment (SatSolver * solver)
{
  int pidx, tmp;

  SatSolver_init_assignment (solver);

  assert (!solver->assignment_initialized);
  for (pidx = 1; pidx <= solver->max_pidx; pidx++)
    {
      assert (solver->assignment.start[pidx] == QUANTOR_UNASSIGNED);
      tmp = nanosat_deref (pidx);
      solver->assignment.start[pidx] = tmp ? QUANTOR_TRUE : QUANTOR_FALSE;
    }
  solver->assignment_initialized = 1;
}

/*------------------------------------------------------------------------*/

static SatSolverAPI nanosat_api = {
  "nanosat",
  SatSolverNanosat_new,
  SatSolverNanosat_delete,
  SatSolverNanosat_add,
  SatSolverNanosat_run,
  SatSolverNanosat_assignment
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *
SatSolverNanosat_get_api (void)
{
  return &nanosat_api;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_PICOSAT
/*------------------------------------------------------------------------*/

#include "picosat.h"

/*------------------------------------------------------------------------*/

typedef struct SatSolverPicosat SatSolverPicosat;

struct SatSolverPicosat
{
  SatSolver super;
  PicoSAT *picosat;
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *SatSolverPicosat_get_api (void);

/*------------------------------------------------------------------------*/

static SatSolver *
SatSolverPicosat_new (Quantor * quantor)
{
  SatSolverPicosat *this;

  this = new (quantor, sizeof (*this));
  SatSolver_init (quantor, &this->super, SatSolverPicosat_get_api ());

  if (quantor->opts.verbose >= 2)
    fprintf (quantor->io.out, "c PicoSAT Version %s\n", picosat_version ());

  this->picosat = picosat_init ();

  picosat_set_output (this->picosat, quantor->io.out);
  if (quantor->opts.verbose >= 2)
    picosat_set_verbosity (this->picosat, 1);

  /* TODO: time limit for picosat */

  /* TODO: space limit for picosat */

  return &this->super;
}

/*------------------------------------------------------------------------*/

static void
SatSolverPicosat_delete (SatSolver * solver)
{
  Quantor *quantor = solver->quantor;
  SatSolverPicosat * this = (SatSolverPicosat*) solver;
#ifdef QUANTOR_STATS1
  size_t bytes;
#endif
  if (quantor->opts.verbose)
    picosat_stats (this->picosat);

#ifdef QUANTOR_STATS1
  quantor->stats.sat_solver_time += picosat_seconds (this->picosat);

  bytes = picosat_max_bytes_allocated (this->picosat);
  if (bytes > quantor->stats.sat_solver_bytes)
    quantor->stats.sat_solver_bytes = bytes;
#endif
  picosat_reset (this->picosat);
  SatSolver_release (solver);
  delete (quantor, this, sizeof (*this));
}

/*------------------------------------------------------------------------*/

static int
SatSolverPicosat_add (SatSolver * solver, int lit)
{
  SatSolverPicosat * this = (SatSolverPicosat*) solver;
  picosat_add (this->picosat, lit);
  return 1;
}

/*------------------------------------------------------------------------*/

static QuantorResult
SatSolverPicosat_run (SatSolver * solver)
{
  SatSolverPicosat * this = (SatSolverPicosat*) solver;
  QuantorResult res;
  int picosat_res;

  (void) solver;
  picosat_res = picosat_sat (this->picosat, -1);

  switch (picosat_res)
    {
    case PICOSAT_SATISFIABLE:
      res = QUANTOR_RESULT_SATISFIABLE;
      break;
    case PICOSAT_UNSATISFIABLE:
      res = QUANTOR_RESULT_UNSATISFIABLE;
      break;
    default:
      res = QUANTOR_RESULT_UNKNOWN;
      break;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
SatSolverPicosat_assignment (SatSolver * solver)
{
  SatSolverPicosat * this = (SatSolverPicosat*) solver;
  int pidx, tmp;

  SatSolver_init_assignment (solver);

  assert (!solver->assignment_initialized);
  for (pidx = 1; pidx <= solver->max_pidx; pidx++)
    {
      assert (solver->assignment.start[pidx] == QUANTOR_UNASSIGNED);
      tmp = picosat_deref (this->picosat, pidx);
      solver->assignment.start[pidx] = tmp > 0 ? QUANTOR_TRUE : QUANTOR_FALSE;
    }
  solver->assignment_initialized = 1;
}

/*------------------------------------------------------------------------*/

static SatSolverAPI picosat_api = {
  "picosat",
  SatSolverPicosat_new,
  SatSolverPicosat_delete,
  SatSolverPicosat_add,
  SatSolverPicosat_run,
  SatSolverPicosat_assignment
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *
SatSolverPicosat_get_api (void)
{
  return &picosat_api;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_BOOLEFORCE
/*------------------------------------------------------------------------*/

#include "booleforce.h"

/*------------------------------------------------------------------------*/

static SatSolverAPI *SatSolverBooleforce_get_api (void);

/*------------------------------------------------------------------------*/

static SatSolver *
SatSolverBooleforce_new (Quantor * quantor)
{
  double time_limit, time_sofar;
  SatSolver *solver;

  solver = new (quantor, sizeof (*solver));
  SatSolver_init (quantor, solver, SatSolverBooleforce_get_api ());

  booleforce_init ();
  booleforce_set_output (quantor->io.out, quantor->io.out_name);
  booleforce_set_verbose (quantor->opts.verbose >= 2 ?
                          2 : quantor->opts.verbose);

  if (quantor->opts.time_limit >= 0)
    {
      time_sofar = get_time () - quantor->stats.time;
      time_limit = quantor->opts.time_limit - time_sofar;
      time_limit = (time_limit < 0) ? 0 : time_limit;
      booleforce_set_time_limit (time_limit);
#ifdef QUANTOR_LOG2
      LOG (quantor, 2, "SAT SOLVER TIME LIMIT %.1f seconds", time_limit);
#endif
    }

  if (quantor->opts.verbose)
    booleforce_options ();

  /* TODO: space limit for booleforce */

  return solver;
}

/*------------------------------------------------------------------------*/

static void
SatSolverBooleforce_delete (SatSolver * solver)
{
  Quantor *quantor = solver->quantor;
#ifdef QUANTOR_STATS1
  size_t bytes;
#endif

  if (quantor->opts.verbose)
    booleforce_stats (quantor->io.out);

#ifdef QUANTOR_STATS1
  quantor->stats.sat_solver_time += booleforce_seconds ();
  bytes = booleforce_max_bytes ();
  if (bytes > quantor->stats.sat_solver_bytes)
    quantor->stats.sat_solver_bytes = bytes;
#endif

  booleforce_reset ();
  SatSolver_release (solver);
  delete (quantor, solver, sizeof (*solver));
}

/*------------------------------------------------------------------------*/

static int
SatSolverBooleforce_add (SatSolver * solver, int lit)
{
  (void) solver;
  booleforce_add (lit);
  return 1;
}

/*------------------------------------------------------------------------*/

static QuantorResult
SatSolverBooleforce_run (SatSolver * solver)
{
  QuantorResult res;
  int booleforce_res;

  (void) solver;
  booleforce_res = booleforce_sat ();

  switch (booleforce_res)
    {
    case BOOLEFORCE_SATISFIABLE:
      res = QUANTOR_RESULT_SATISFIABLE;
      break;
    case BOOLEFORCE_UNSATISFIABLE:
      res = QUANTOR_RESULT_UNSATISFIABLE;
      break;
    default:
      res = QUANTOR_RESULT_UNKNOWN;
      break;
    }

  return res;
}

/*------------------------------------------------------------------------*/

static void
SatSolverBooleforce_assignment (SatSolver * solver)
{
  int pidx, tmp;

  SatSolver_init_assignment (solver);

  assert (!solver->assignment_initialized);
  for (pidx = 1; pidx <= solver->max_pidx; pidx++)
    {
      assert (solver->assignment.start[pidx] == QUANTOR_UNASSIGNED);
      tmp = booleforce_deref (pidx);
      solver->assignment.start[pidx] = tmp > 0 ? QUANTOR_TRUE : QUANTOR_FALSE;
    }
  solver->assignment_initialized = 1;
}

/*------------------------------------------------------------------------*/

static SatSolverAPI booleforce_api = {
  "booleforce",
  SatSolverBooleforce_new,
  SatSolverBooleforce_delete,
  SatSolverBooleforce_add,
  SatSolverBooleforce_run,
  SatSolverBooleforce_assignment
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *
SatSolverBooleforce_get_api (void)
{
  return &booleforce_api;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#ifdef QUANTOR_HAVE_COMPSAT
/*------------------------------------------------------------------------*/

#include "compsat.h"

/*------------------------------------------------------------------------*/

typedef struct SatSolverCompsat SatSolverCompsat;

struct SatSolverCompsat
{
  SatSolver super;
  CompSat * compsat;
};

/*------------------------------------------------------------------------*/


static SatSolverAPI * SatSolverCompsat_get_api (void);

/*------------------------------------------------------------------------*/

static SatSolver *
SatSolverCompsat_new (Quantor * quantor)
{
  SatSolverCompsat * this;

  this = new (quantor, sizeof (*this));
  SatSolver_init (quantor, &this->super, SatSolverCompsat_get_api());
  this->compsat = compsat_new ();
  compsat_set_log_file (this->compsat, quantor->io.out);
  compsat_set_out_file (this->compsat, quantor->io.out);
  if (quantor->opts.verbose)
    {
      compsat_set_verbose_level (this->compsat, 1);

      fprintf (quantor->io.out,
	       "%sCompSat SAT Solver Version %s\n",
	       compsat_prefix (this->compsat), compsat_version ());

      fprintf (quantor->io.out, "%s%s\n",
	       compsat_prefix(this->compsat), compsat_id ());
    }

  /* TODO: time limit for compsat */
  /* TODO: space limit for compsat */

  return &this->super;
}

/*------------------------------------------------------------------------*/

static void
SatSolverCompsat_delete (SatSolver * solver)
{
  SatSolverCompsat * this = (SatSolverCompsat *) solver;
  Quantor * quantor = solver->quantor;
#ifdef QUANTOR_STATS1
  size_t bytes;
#endif

  if (quantor->opts.verbose)
    compsat_stats (this->compsat, quantor->io.out);

#ifdef QUANTOR_STATS1
  quantor->stats.sat_solver_time += compsat_seconds (this->compsat);
  bytes = compsat_bytes (this->compsat);
  if (bytes > quantor->stats.sat_solver_bytes)
    quantor->stats.sat_solver_bytes = bytes;
#endif
  compsat_delete (this->compsat);
  SatSolver_release (solver);
  delete (quantor, this, sizeof (*this));
}

/*------------------------------------------------------------------------*/

static int
SatSolverCompsat_add (SatSolver * solver, int lit)
{
  SatSolverCompsat * this = (SatSolverCompsat*) solver;
  compsat_add_literal (this->compsat, lit);

  return 1;
}

/*------------------------------------------------------------------------*/

static QuantorResult
SatSolverCompsat_run (SatSolver * solver)
{
  SatSolverCompsat * this = (SatSolverCompsat*) solver;
  QuantorResult res;
  int compsat_res;

  compsat_res = compsat_sat (this->compsat);

  if (compsat_res == COMPSAT_SATISFIABLE)
    res = QUANTOR_RESULT_SATISFIABLE;
  else if (compsat_res == COMPSAT_UNSATISFIABLE)
    res = QUANTOR_RESULT_UNSATISFIABLE;
  else if (compsat_res == COMPSAT_TIME_OUT)
    res = QUANTOR_RESULT_TIMEOUT;
  else if (compsat_res == COMPSAT_SPACE_OUT)
    res = QUANTOR_RESULT_SPACEOUT;
  else
    res = QUANTOR_RESULT_UNKNOWN;

  return res;
}

/*------------------------------------------------------------------------*/

static void
SatSolverCompsat_assignment (SatSolver * solver)
{
  SatSolverCompsat * this = (SatSolverCompsat*) solver;
  int pidx, tmp;

  SatSolver_init_assignment (solver);
  assert (!solver->assignment_initialized);
  for (pidx = 1; pidx <= solver->max_pidx; pidx++)
    {
      assert (solver->assignment.start[pidx] == QUANTOR_UNASSIGNED);
      tmp = compsat_deref (this->compsat, pidx);
      solver->assignment.start[pidx] = tmp ? QUANTOR_TRUE : QUANTOR_FALSE;
    }
  solver->assignment_initialized = 1;
}

/*------------------------------------------------------------------------*/

static SatSolverAPI compsat_api = {
  "compsat",
  SatSolverCompsat_new,
  SatSolverCompsat_delete,
  SatSolverCompsat_add,
  SatSolverCompsat_run,
  SatSolverCompsat_assignment
};

/*------------------------------------------------------------------------*/

static SatSolverAPI *
SatSolverCompsat_get_api (void)
{
  return &compsat_api;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static SatSolverAPI *sat_apis[] = {
#ifdef QUANTOR_HAVE_PICOSAT
  &picosat_api,
#endif
#ifdef QUANTOR_HAVE_BOOLEFORCE
  &booleforce_api,
#endif
#ifdef QUANTOR_HAVE_COMPSAT
  &compsat_api,
#endif
#ifdef QUANTOR_HAVE_NANOSAT
  &nanosat_api,
#endif
#ifdef QUANTOR_HAVE_FUNEX
  &funex_api,
#endif
#ifdef QUANTOR_HAVE_LIMMAT
  &limmat_api,
#endif
  0				/* sentinel */
};

/*------------------------------------------------------------------------*/

static int
AllClausesLitIt_done (LitIt * super, int *next_int_lit_ptr)
{
  AllClausesLitIt *it;
  int int_lit;
  Lit *lit;
  Cell *eor;

  it = (AllClausesLitIt *) super;
  if (!it->current_clause)
    return 1;

  eor = end_of_row (it->current_clause);

NEXT_CELL:
  if (it->current_cell == eor)
    {
      lit = 0;
      int_lit = 0;
      if ((it->current_clause = it->current_clause->link.next))
	it->current_cell = it->current_clause->row;
    }
  else
    {
      lit = it->current_cell->lit;
      it->current_cell++;
      if (it->existential_literals_only && !is_existential (lit->var))
	goto NEXT_CELL;

      int_lit = lit2int (it->quantor, lit);
      assert (int_lit);
    }

  assert (!lit || !is_assigned_var (lit->var));
  *next_int_lit_ptr = int_lit;

  return 0;
}


/*------------------------------------------------------------------------*/

static void
AllClausesLitIt_delete (LitIt * super)
{
  AllClausesLitIt *it = (AllClausesLitIt *) super;
  delete (it->quantor, it, sizeof (*it));
}

/*------------------------------------------------------------------------*/

static LitItAPI all_clauses_lit_it_api = {
  AllClausesLitIt_done,
  AllClausesLitIt_delete
};

/*------------------------------------------------------------------------*/

static LitIt *
AllClausesLitIt_new (Quantor * quantor, int existential_literals_only)
{
  AllClausesLitIt *this;

  this = (AllClausesLitIt *) new (quantor, sizeof (*this));
  this->super.api = &all_clauses_lit_it_api;
  this->quantor = quantor;
  this->existential_literals_only = existential_literals_only;
  this->current_clause = quantor->clauses.first;
  if (this->current_clause)
    this->current_cell = this->current_clause->row;

  return &this->super;
}

/*------------------------------------------------------------------------*/

static int
IntStackLitIt_done (LitIt * super, int *next_int_lit_ptr)
{
  IntStackLitIt *it = (IntStackLitIt *) super;
  int lit;

  if (it->current >= it->stack->top)
    return 1;

  lit = *it->current++;
  *next_int_lit_ptr = lit;

  return 0;
}

/*------------------------------------------------------------------------*/

static void
IntStackLitIt_delete (LitIt * super)
{
  IntStackLitIt *it = (IntStackLitIt *) super;
  delete (it->quantor, it, sizeof (*it));
}

/*------------------------------------------------------------------------*/

static LitItAPI int_stack_lit_it_api = {
  IntStackLitIt_done,
  IntStackLitIt_delete
};

/*------------------------------------------------------------------------*/

static LitIt *
IntStackLitIt_new (Quantor * quantor, IntStack * stack)
{
  IntStackLitIt *this;

  this = (IntStackLitIt *) new (quantor, sizeof (*this));
  this->super.api = &int_stack_lit_it_api;
  this->quantor = quantor;
  this->stack = stack;
  this->current = stack->start;

  return &this->super;
}

/*------------------------------------------------------------------------*/

static int
AssignedLitIt_done (LitIt * super, int *next_int_lit_ptr)
{
  AssignedLitIt *it = (AssignedLitIt *) super;
  int lit, done, other;
  Lit *tmp;
  Var *v;

  if (count_IntStack (&it->clause))
    {
      lit = pop_IntStack (&it->clause);
      done = 0;
    }
  else
    {
      lit = 0;

      while (it->idx <= it->quantor->max_idx)
	{
	  v = it->quantor->vars.start[it->idx++];
	  if (!v)
	    continue;

	  tmp = v->assignment;

	  if (tmp == QUANTOR_UNASSIGNED)
	    continue;

	  tmp = deref (it->quantor, tmp);

	  if (tmp == QUANTOR_TRUE)
	    {
	      push_IntStack (it->quantor, &it->clause, 0);
	      lit = v->idx;
	      break;
	    }

	  if (tmp == QUANTOR_FALSE)
	    {
	      push_IntStack (it->quantor, &it->clause, 0);
	      lit = -v->idx;
	      break;
	    }

	  assert (!is_constant (tmp));
	  lit = v->idx;
	  other = lit2int (it->quantor, tmp);

	  push_IntStack (it->quantor, &it->clause, 0);
	  push_IntStack (it->quantor, &it->clause, other);
	  push_IntStack (it->quantor, &it->clause, -lit);
	  push_IntStack (it->quantor, &it->clause, 0);
	  push_IntStack (it->quantor, &it->clause, -other);

	  /* and lit is given back immediately */

	  break;
	}

      done = !lit;
    }

  if (done)
    return 1;

  *next_int_lit_ptr = lit;
  return 0;
}

/*------------------------------------------------------------------------*/

static void
AssignedLitIt_delete (LitIt * super)
{
  AssignedLitIt *this = (AssignedLitIt *) super;
  release_IntStack (this->quantor, &this->clause);
  delete (this->quantor, this, sizeof (*this));
}

/*------------------------------------------------------------------------*/

static LitItAPI assigned_lit_it_api = {
  AssignedLitIt_done,
  AssignedLitIt_delete
};

/*------------------------------------------------------------------------*/

static LitIt *
AssignedLitIt_new (Quantor * quantor)
{
  AssignedLitIt *this;
  this = (AssignedLitIt *) new (quantor, sizeof (*this));
  this->super.api = &assigned_lit_it_api;
  this->quantor = quantor;
  this->idx = 1;
  return &this->super;
}

/*------------------------------------------------------------------------*/

static void
propagate_sat_solver_assignment (Quantor * quantor, SatSolver * solver)
{
  int i, propagated;
  Lit *tmp;
  Var *v;

#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "PROPAGATING ASSIGNMENTS FROM SAT SOLVER");
#endif
  propagated = 0;

  for (i = 1; i <= quantor->max_idx; i++)
    {
      v = quantor->vars.start[i];
      if (!v || !v->exported || v->eliminated || v->zombie)
	continue;

      tmp = deref (quantor, var2lit (v, 0));
      if (is_constant (tmp))
	continue;

      assert (tmp != QUANTOR_UNASSIGNED);

      tmp = SatSolver_deref (solver, v->idx);
      assign (quantor, v, tmp);

      propagated++;
    }
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "PROPAGATED %d ASSIGNMENTS FROM SAT SOLVER", propagated);
#endif
}

/*------------------------------------------------------------------------*/

static QuantorResult
sat (Quantor * quantor)
{
  SatSolver *solver;
  QuantorResult res;
  LitIt *it;
  int ok;

  assert (quantor->sat_api);
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "CALLING EXTERNAL SAT SOLVER ON REMAINING CLAUSES");
#endif
  solver = quantor->sat_api->new (quantor);
  it = AllClausesLitIt_new (quantor, 0);
  ok = SatSolver_copy (solver, it);
  it->api->delete (it);

  if (ok)
    res = SatSolver_run (solver);
  else
    res = QUANTOR_RESULT_UNKNOWN;

  if (res == QUANTOR_RESULT_SATISFIABLE)
    {
      propagate_sat_solver_assignment (quantor, solver);
      assign_not_eliminated_zombies (quantor);
    }

  quantor->sat_api->delete (solver);

#ifdef QUANTOR_STATS2
  assert (!quantor->stats.sat_remaining_time);
  quantor->stats.sat_remaining_time = quantor->stats.sat_solver_time;
#endif

  return res;
}

/*------------------------------------------------------------------------*/

static QuantorResult
trivial_truth (Quantor * quantor)
{
  SatSolver *solver;
  QuantorResult res;
#ifdef QUANTOR_STATS2
  double time;
#endif
  LitIt *it;
  int ok;

  if (!quantor->opts.trivial_truth)
    return QUANTOR_RESULT_UNKNOWN;

  INCSTATS2 (quantor->stats.trivial_truth);
  assert (quantor->sat_api);
#ifdef QUANTOR_STATS2
  time = quantor->stats.sat_solver_time;
#endif
#ifdef QUANTOR_LOG2
  LOG (quantor, 2, "CALLING SAT SOLVER FOR TRIVIAL TRUTH");
#endif
  solver = quantor->sat_api->new (quantor);
  it = AllClausesLitIt_new (quantor, 1);
  ok = SatSolver_copy (solver, it);
  it->api->delete (it);
  if (ok)
    res = SatSolver_run (solver);
  else
    res = QUANTOR_RESULT_UNKNOWN;

  if (res == QUANTOR_RESULT_SATISFIABLE)
    {
#ifdef QUANTOR_LOG1
      LOG (quantor, 1, "DETECTED TRIVIAL TRUTH");
#endif
      propagate_sat_solver_assignment (quantor, solver);
      assign_not_eliminated_zombies (quantor);
    }
  else
    {
#ifdef QUANTOR_LOG1
      LOG (quantor, 1, "TRIVIAL TRUTH CHECK FAILED");
#endif
      res = QUANTOR_RESULT_UNKNOWN;
    }

  quantor->sat_api->delete (solver);

#ifdef QUANTOR_STATS2
  quantor->stats.sat_trivial_truth_time +=
    quantor->stats.sat_solver_time - time;
#endif

  assert (res == QUANTOR_RESULT_UNKNOWN || res == QUANTOR_RESULT_SATISFIABLE);

  return res;
}

/*------------------------------------------------------------------------*/

static QuantorResult
trivial_falsity (Quantor * quantor)
{
  if (!quantor->opts.trivial_falsity)
    return QUANTOR_RESULT_UNKNOWN;

  INCSTATS2 (quantor->stats.trivial_falsity);
  return QUANTOR_RESULT_UNKNOWN;
}

/*------------------------------------------------------------------------*/

static QuantorResult
lift_assignment_to_original_clauses (Quantor * quantor)
{
  SatSolver *solver;
  QuantorResult res;
#ifdef QUANTOR_STATS2
  double time;
#endif
  LitIt *it;
  int i, ok;
  Lit *tmp;
  Var *v;

  assert (quantor->sat_api);
#ifdef QUANTOR_STATS2
  time = quantor->stats.sat_solver_time;
#endif
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "CALLING EXTERNAL SAT SOLVER ON ORIGINAL CLAUSES");
#endif
  solver = quantor->sat_api->new (quantor);
  it = IntStackLitIt_new (quantor, &quantor->dead_original_clauses);
  ok = SatSolver_copy (solver, it);
  it->api->delete (it);
  it = AssignedLitIt_new (quantor);
  SatSolver_copy (solver, it);
  it->api->delete (it);

  if (ok)
    {
      res = SatSolver_run (solver);
      assert (res != QUANTOR_RESULT_UNSATISFIABLE);
      if (res == QUANTOR_RESULT_SATISFIABLE)
	{
	  for (i = 1; i <= quantor->max_idx; i++)
	    {
	      v = quantor->vars.start[i];
	      if (!v)
		continue;

	      assert (v->exported);
	      tmp = deref (quantor, v->lits);
	      if (is_constant (tmp))
		continue;

	      tmp = SatSolver_deref (solver, v->idx);
	      assign (quantor, v, tmp);
	    }
	}
    }
  else
    res = QUANTOR_RESULT_UNKNOWN;

  solver->api->delete (solver);
#ifdef QUANTOR_STATS2
  assert (!quantor->stats.sat_original_time);
  quantor->stats.sat_original_time = quantor->stats.sat_solver_time - time;
#endif

  return res;
}

/*------------------------------------------------------------------------*/

static void
delete_Opt (Quantor * quantor, Opt * opt)
{
  delete_string (quantor, opt->env);
  delete_string (quantor, opt->opt);
  delete (quantor, opt, sizeof (*opt));
}

/*------------------------------------------------------------------------*/

static void
release_opts (Quantor * quantor)
{
  while (count_PtrStack (&quantor->opts.setup))
    delete_Opt (quantor, pop_PtrStack (quantor, &quantor->opts.setup));

  release_PtrStack (quantor, &quantor->opts.setup);
}

/*------------------------------------------------------------------------*/

static void
release_clauses (Quantor * quantor)
{
  Clause *c, *next;

  for (c = quantor->clauses.first; c; c = next)
    {
      next = c->link.next;
      dealloc_Clause (quantor, c);
    }

  release_PtrStack (quantor, &quantor->idx2clause);
}

/*------------------------------------------------------------------------*/

static void
release_functions (Quantor * quantor)
{
  Function *f;

  while ((f = quantor->functions.first))
    delete_Function (quantor, f, 1);
}

/*------------------------------------------------------------------------*/

static const char *
quantor_putenv (Quantor * quantor, const char *str)
{
  char *tmp, *name, *new_val, *old_val;
  const char *res;
  unsigned len;

  res = 0;
  assert (str);
  len = strlen (str);
  tmp = new (quantor, len + 1);

  strcpy (tmp, str);
  name = strtok (tmp, "=");

  if (name && strlen (name) < len)
    {
      new_val = strtok (0, "");

      if (new_val)
	{
	  name = copy_string (quantor, name);
	  push_PtrStack (quantor, &quantor->environment, name);

	  old_val = getenv (name);
	  if (old_val)
	    old_val = copy_string (quantor, old_val);
	  push_PtrStack (quantor, &quantor->environment, old_val);

#ifndef __MINGW32__
	  setenv (name, new_val, 1);
#endif
	}
      else
	res = "value missing after '='";
    }
  else
    res = "'=' missing";

  delete (quantor, tmp, len + 1);

  return res;
}

/*------------------------------------------------------------------------*/

static void
release_environment (Quantor * quantor)
{
  void **p, **start;
  char *name, *val;

  if (count_PtrStack (&quantor->environment))
    {
      start = quantor->environment.start;
      p = quantor->environment.top - 1;
      while (p >= start)
	{
	  val = *p--;
	  assert (p >= start);
	  name = *p--;
	  assert (name);

	  if (val)
	    {
#ifndef __MINGW32__
	      setenv (name, val, 1);
#endif
	      delete_string (quantor, val);
	    }
#ifndef __MINGW32__
	  else
	    unsetenv (name);
#endif
	  delete_string (quantor, name);
	}
    }

  release_PtrStack (quantor, &quantor->environment);
}

/*------------------------------------------------------------------------*/

static void
delete_Quantor (Quantor * quantor)
{
  release_functions (quantor);
  release_equivalence_classes (quantor);
  release_scopes (quantor);
  release_IntStack (quantor, &quantor->external_literals);
  release_IntStack (quantor, &quantor->external_scope);
  release_PtrStack (quantor, &quantor->new_clause);
  release_PtrStack (quantor, &quantor->new_rhs);
  release_clauses (quantor);
  release_IntStack (quantor, &quantor->dead_original_clauses);

  if (quantor->opts.equivalences)
    delete_BinDB (quantor, quantor->bindb);
  else
    assert (!quantor->bindb);

  if (quantor->opts.functions)
    delete_RHSDB (quantor, quantor->rhsdb);
  else
    assert (!quantor->rhsdb);

  if (quantor->opts.literals_added_by_exists)
    delete (quantor, quantor->opts.literals_added_by_exists,
	    sizeof (int) * (quantor->opts.soft_exists_length));

  release_PtrStack (quantor, &quantor->new_scope);
  release_PtrStack (quantor, &quantor->vars);
  release_IntStack (quantor, &quantor->free_var_indices);
  release_IntStack (quantor, &quantor->free_clause_indices);
  release_PtrStack (quantor, &quantor->marked_vars);
  release_IntStack (quantor, &quantor->external_assignment);
  release_opts (quantor);
  release_environment (quantor);
  delete_string (quantor, quantor->prefix);
#ifdef QUANTOR_FAST_ALLOC
  release_chunks (quantor);
#endif

  if (quantor->io.close_out)
    fclose (quantor->io.out);

  if (quantor->io.close_in)
    fclose (quantor->io.in);

  if (quantor->io.pclose_in)
    pclose (quantor->io.in);

  assert (getenv ("LEAK") || !quantor->stats.bytes);

  free (quantor);
}

/*------------------------------------------------------------------------*/


static const char *
match (const char *str, const char *pattern)
{
  const char *p, *q;

  for (p = str, q = pattern; *q && (*p == *q); p++, q++)
    ;

  return *q ? ((const char *) 0) : p;
}

/*------------------------------------------------------------------------*/
#if defined(QUANTOR_LOG2) && defined(QUANTOR_STATS1)
/*------------------------------------------------------------------------*/

static const char *
get_sat_solver_name (Quantor * quantor)
{
  const char *res;

  res = "<none>";

  if (quantor->sat_api)
    res = quantor->sat_api->name;

  assert (res);

  return res;
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
init_opts (Quantor * quantor)
{
#include "options.c"

  if (!quantor->sat_api && sat_apis[0]->name)
    quantor->sat_api = sat_apis[0];
}

/*------------------------------------------------------------------------*/
/* [0-9][0-9]*
 */
static int
is_pos_int_str (const char *str)
{
  const char *p;

  p = str;

  if (!isdigit (*p))
    return 0;

  p++;

  while (isdigit (*p))
    p++;

  if (!*p)
    return 1;

  return 0;
}

/*------------------------------------------------------------------------*/
/* -?[0-9][0-9]*
 */
static int
is_int_str (const char *str)
{
  const char *p;

  p = str;

  if (*p == '-')
    p++;

  return is_pos_int_str (p);
}

/*------------------------------------------------------------------------*/
/* -?[0-9][0-9]*\(\.[0-9][0-9]*\)
 */
static int
is_float_str (const char *str)
{
  const char *p;

  p = str;

  if (*p == '-')
    p++;

  if (!isdigit (*p))
    return 0;

  p++;

  while (isdigit (*p))
    p++;

  if (!*p)
    return 1;

  if (*p == '.')
    p++;

  if (!isdigit (*p))
    return 0;

  p++;
  while (isdigit (*p))
    p++;

  if (!*p)
    return 1;

  return 0;
}

/*------------------------------------------------------------------------*/

static int
setenv_opt (Quantor * quantor, Opt * opt)
{
  const char *arg;
  int res;

  arg = getenv (opt->env);
  if (!arg)
    return 1;

  if (opt->is_iopt)
    {
      if (is_int_str (arg))
	{
	  *opt->data.as_iopt.ptr = atoi (arg);
	  res = 1;
	}
      else
	{
	  fprintf (quantor->io.out,
		   "*** quantor: %s=%s but expected <int>\n", opt->env, arg);
	  res = 0;
	}
    }
  else
    {
      if (is_float_str (arg))
	{
	  *opt->data.as_dopt.ptr = atof (arg);
	  res = 1;
	}
      else
	{
	  fprintf (quantor->io.out,
		   "*** quantor: %s=%s but expected <float>\n",
		   opt->env, arg);
	  res = 0;
	}
    }

  return res;
}

/*------------------------------------------------------------------------*/

static int
init_env (Quantor * quantor)
{
  void **p;
  int res;

  res = 1;
  for (p = quantor->opts.setup.start; res && p < quantor->opts.setup.top; p++)
    res = setenv_opt (quantor, *p);

  return res;
}

/*------------------------------------------------------------------------*/

static int
is_registered_env (Quantor * quantor, const char *env_assigned)
{
  char *env, *end;
  size_t len;
  void **p;
  Opt *o;
  int res;

  end = strchr (env_assigned, '=');
  assert (end);
  len = end - env_assigned;
  env = new (quantor, len + 1);
  strncpy (env, env_assigned, len);
  env[len] = 0;
  res = 0;
  for (p = quantor->opts.setup.start;
       !res && p < quantor->opts.setup.top; p++)
    {
      o = *p;
      if (!strcmp (o->env, env))
	res = 1;
    }
  delete (quantor, env, len + 1);

  return res;
}

/*------------------------------------------------------------------------*/

static const char *
get_invalid_env (Quantor * quantor)
{
  char **p;

  for (p = environ; *p; p++)
    {
      if (!match (*p, "QUANTOR_"))
	continue;

      if (!is_registered_env (quantor, *p))
	return *p;
    }

  return 0;
}

/*------------------------------------------------------------------------*/
/* Pretty print a floating point option by stripping trailing zeroes.
 */
static void
print_double (double d, FILE * file)
{
  char buffer[20], *p;

  sprintf (buffer, "%f", d);
  p = buffer + strlen (buffer) - 1;
  while (p > buffer && *p == '0' && p[-1] != '.')
    *p-- = 0;

  fputs (buffer, file);
}

/*------------------------------------------------------------------------*/

static void
usage_Opt (Opt * opt, FILE * file)
{
  fprintf (file, "%s", opt->opt);
  if (opt->is_iopt)
    fprintf (file, "%d", opt->data.as_iopt.def);
  else
    print_double (opt->data.as_dopt.def, file);
  fputc ('\n', file);
}

/*------------------------------------------------------------------------*/

static void
usage_opts (Quantor * quantor, FILE * file)
{
  void **p;

  for (p = quantor->opts.setup.start; p < quantor->opts.setup.top; p++)
    usage_Opt (*p, file);
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG2
/*------------------------------------------------------------------------*/

static void
log_Opt (Quantor * quantor, Opt * opt)
{
  fputs (quantor_prefix (quantor), quantor->io.out);
  fputs (opt->env, quantor->io.out);
  fputc ('=', quantor->io.out);
  if (opt->is_iopt)
    fprintf (quantor->io.out, "%d", *opt->data.as_iopt.ptr);
  else
    print_double (*opt->data.as_dopt.ptr, quantor->io.out);
  fputc ('\n', quantor->io.out);
}

/*------------------------------------------------------------------------*/

static void
log_opts (Quantor * quantor)
{
  void **p;

  for (p = quantor->opts.setup.start; p < quantor->opts.setup.top; p++)
    log_Opt (quantor, *p);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static int
match_opt (Quantor * quantor, Opt * opt, const char *arg, int *err_ptr)
{
  const char *val_str;
  int res, err;

  res = 0;
  err = 0;

  if ((val_str = match (arg, opt->opt)))
    {
      if (opt->is_iopt)
	{
	  if (is_int_str (val_str))
	    *opt->data.as_iopt.ptr = atoi (val_str);
	  else
	    err = 1;
	}
      else
	{
	  if (is_float_str (val_str))
	    *opt->data.as_dopt.ptr = atof (val_str);
	  else
	    err = 1;
	}

      res = 1;
    }

  if (err)
    fprintf (quantor->io.out,
	     "*** quantor: invalid argument to '%s' (try '-h')\n", opt->opt);

  *err_ptr = err;

  return res;
}

/*------------------------------------------------------------------------*/

static int
match_opts (Quantor * quantor, const char *arg, int *err_ptr)
{
  void **p;
  int res;

  res = 0;
  for (p = quantor->opts.setup.start;
       !res && p < quantor->opts.setup.top; p++)
    res = match_opt (quantor, *p, arg, err_ptr);

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_LOG1
/*------------------------------------------------------------------------*/

static void
log_line (Quantor * quantor, int level)
{
  int i;

  if (quantor->opts.verbose < level)
    return;

  fputs (quantor_prefix (quantor), quantor->io.out);

  for (i = 0; i < 70; i++)
    fputc ('-', quantor->io.out);

  fputc ('\n', quantor->io.out);
}

/*------------------------------------------------------------------------*/

static void
banner (Quantor * quantor)
{
  log_line (quantor, 1);
  LOG (quantor, 1, "QBF Solver Version %s", quantor_version ());
  LOG (quantor, 1, "%s", quantor_copyright ());
  LOG (quantor, 1, "%s", quantor_id ());
  log_line (quantor, 1);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

#define USAGE \
"usage: quantor [<option> ...] [ <in-file> ]\n" \
"\n" \
"where <option> is one of the following\n" \
"\n" \
"  -h | --help               print this command line summary\n" \
"  --version                 print version information\n" \
"  --copyright               print copyright information\n" \
"  --configuration           print configuration options\n" \
"  --id                      print RCS id string\n" \
"  -v[<inc>]                 increase verbosity level (default <inc>=1)\n" \
"  -q                        parse only\n" \
"  -p                        pretty print only\n" \
"  -n                        do not print assignment\n" \
"  -d                        dump SAT problem after simplification\n" \
"  -s                        quit after simplification\n" \
"  -o <out-file>             set output file\n" \
"  --<solver>                specify SAT solver for propositional part\n" \
"  --list-sat-solvers        list valid SAT solver names\n" \
"  --undocumented            list undocumented command line options\n" \
"  -D<name>=<val>            set environment variable <name> to <val>\n" \
"\n" \
"Default input file is <stdin> and is overwritten with <in-file> if\n" \
"specified.  The input is expected to be in QDIMACS format.  Default\n" \
"output file is <stdout> and can be overwritten with '-o <out-file>'.\n"

/*------------------------------------------------------------------------*/
#if defined(QUANTOR_LOG1) && defined(QUANTOR_STATS1)
/*------------------------------------------------------------------------*/

static const char *log_indent_str = "                    ";
    /*                                12345678901234567890 */

static const char *
log_indent (int level)
{
  assert (level > 0);
  assert (level < 20);
  assert (strlen (log_indent_str) == 20);
  return log_indent_str + 22 - (2 * level);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#if defined(QUANTOR_LOG2) && defined(QUANTOR_STATS2)
/*------------------------------------------------------------------------*/

static void
log_cache_stats (Quantor * quantor,
		 int level, const char *str, CacheStats * stats)
{
  double miss_rate, misses, spurious_miss_rate;

  assert (level >= 2);

  LOG (quantor, level, "%s%s=%.0f", log_indent (level), str, stats->checks);

  miss_rate = stats->checks > 0 ? 100.0 * stats->hits / stats->checks : 0;
  LOG (quantor, level,
       "%sHITS=%.0f HITRATE=%.1f%%",
       log_indent (level + 1), stats->hits, miss_rate);

  misses = stats->checks - stats->hits;
  spurious_miss_rate = misses > 0 ? 100.0 * stats->spurious / misses : 0;
  LOG (quantor, level,
       "%sMISSES=%.0f SPURIOUS=%.0f SPURIOUSMISSRATE=%.1f%%",
       log_indent (level + 1), misses, stats->spurious, spurious_miss_rate);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/
#if defined(QUANTOR_LOG1) && defined(QUANTOR_STATS1)
/*------------------------------------------------------------------------*/

static void
log_count_stats (Quantor * quantor, int level,
		 const char *str, CountStats * stats)
{
  LOG (quantor, level, "%s%s %u", log_indent (level), str, stats->num);
#ifdef QUANTOR_LOG2
  LOG (quantor, level + 1, "%s  MAX=%u NEW=%.0f GC=%.0f",
       log_indent (level), stats->max, stats->new, stats->gc);
#endif
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

void
quantor_stats (Quantor * quantor, FILE * file)
{
  FILE * saved_out;
  Stats *stats;
  double delta;

  saved_out = quantor->io.out;
  quantor->io.out = file;

  stats = &quantor->stats;

  delta = get_time () - stats->time;
  delta = (delta >= 0) ? delta : 0;

#ifdef QUANTOR_STATS1
#ifdef QUANTOR_LOG1
  log_count_stats (quantor, 1, "VARIABLES", &stats->vars);
#endif
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS2
  LOG (quantor, 2,
       "  REC=%.0f ASS=%u", stats->recycled_vars, stats->num_assigned);
#endif
#endif
#ifdef QUANTOR_LOG1
  log_count_stats (quantor, 1, "CLAUSES", &quantor->stats.clauses);
#endif
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  REC=%.0f TRIV=%.0f",
       stats->recycled_clauses, stats->trivial_clauses);
  log_count_stats (quantor, 2, "BINARY CLAUSES", &stats->binary_clauses);
#endif
#endif
#ifdef QUANTOR_LOG1
  log_count_stats (quantor, 1, "LITERALS", &stats->cells);
#endif
#if defined(QUANTOR_LOG2) && defined(QUANTOR_STATS2)
  LOG (quantor, 2,
       "  DUP=%.0f FALSE=%.0f FRED=%.0f CONS=%.0f",
       stats->duplicated_lits,
       stats->false_lits, stats->forall_reduced_lits,
       stats->self_subsuming_resolution_lits);
#endif
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "EQUIVALENCES %.0f", stats->equivalences);
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  FROMFUN=%.0f", stats->equivalences_from_functions);
#endif
#endif
#ifdef QUANTOR_LOG2
  log_count_stats (quantor, 2, "RHS", &stats->rhs);
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  AVGFUN=%.2f",
       (stats->functions.new > 0) ?
       stats->functions.new / stats->rhs.new : 0);
  log_count_stats (quantor, 1, "FUNCTIONS", &stats->functions);
  LOG (quantor, 2, "  AVGSIZE=%.1f",
       stats->functions.new > 0 ?
       (stats->args_functions / stats->functions.new) : 0);
  log_count_stats (quantor, 2, "OR", &stats->or);
  log_cache_stats (quantor, 2, "EXTRACTIONS", &stats->or_extractions);
  log_cache_stats (quantor, 2, "LHSCHECKS", &stats->or_lhs);
  LOG (quantor, 2, "    EXTRACTIONS/LHSCHECKS=%.2f",
       (stats->or_extractions.checks > 0) ?
       stats->or_lhs.checks / stats->or_extractions.checks : 0);
#endif
#endif
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "HYPER1RES %u", stats->hyper1res_units);
  log_cache_stats (quantor, 2, "CHECKS", &stats->hyper1res);
  log_cache_stats (quantor, 2, "PIVOTS", &stats->hyper1respivot);
  LOG (quantor, 2, "  PIVOTS/CHECKS=%.2f",
       (stats->hyper1res.checks > 0) ?
       stats->hyper1respivot.checks / stats->hyper1res.checks : 0);
#endif
#endif
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "SELFSUBRES %.0f", stats->self_subsuming_resolution);
  LOG (quantor, 2, "  BINARY=%.0f", stats->binary_self_subsuming_resolution);
#endif
  LOG (quantor, 2, "BCPS %.0f", stats->bcps);
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  ROUNDS=%.0f AVG=%.1f", stats->bcp_rounds,
       (stats->bcps > 0) ? stats->bcp_rounds / stats->bcps : 0);
  LOG (quantor, 2, "UNITS %.0f", stats->units);
  LOG (quantor, 2, "UNATES %.0f", stats->unates);
  LOG (quantor, 2, "SUBSTITUTIONS %.0f", stats->substitutions);
  LOG (quantor, 2, "PROCESSED %.0f", stats->processed);
  LOG (quantor, 2, "  BW=%.0f FW=%.0f",
       stats->backward_processed, stats->forward_processed);
  LOG (quantor, 2, "TRIVIAL TRUTH CALLS %.0f", stats->trivial_truth);
#endif
#ifdef QUANTOR_STATS1
  LOG (quantor, 2, "FORALL %.0f", stats->foralls);
#endif
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  RATIOEXCEEDED=%.0f",
       stats->literals_per_clause_limit_exceeded);
#endif
  LOG (quantor, 2, "EXISTS %.0f", stats->exists);
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  SMALLCOST=%.0f FUNSUB=%.0f",
       stats->smaller_exists_limit, stats->exists_function_resolution);
  LOG (quantor, 2, "SUBSUMED %.0f",
       stats->forward_subsumed + stats->backward_subsumed +
       stats->already_exists);
  LOG (quantor, 2, "  FW=%.0f BW=%.0f SAME=%.0f",
       stats->forward_subsumed, stats->backward_subsumed,
       stats->already_exists);
  log_cache_stats (quantor, 2, "SUBSUMECHECKS", &stats->subsume);
#ifndef NSTRENGTHEN
  LOG (quantor, 2, "STRENGTHEN %.0f",
       stats->forward_strengthend + stats->backward_strengthend);
  LOG (quantor, 2, "  FW=%.0f BW=%.0f",
       stats->forward_strengthend, stats->backward_strengthend);
  log_cache_stats (quantor, 2, "STRENGTHENCHECKS", &stats->strengthen);
#endif
  LOG (quantor, 2, "REORDERED %.0f", stats->reordered);
  LOG (quantor, 2, "  UP=%.0f DOWN=%.0f SWAP=%.0f CMP=%.0f",
       stats->ups, stats->downs, stats->swaps, stats->cmps);
#endif
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "RECALCSIGS=%.0f", stats->num_recalc_sigs);
#ifdef QUANTOR_SIGREF
  LOG (quantor, 2, "  STICKYSIGREF=%.0f", stats->sticky_sigref);
#endif
#endif
#endif
#endif /* ifdef QUANTOR_STATS1 */
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "SECONDS %.2f", delta);
#endif
#ifdef QUANTOR_STATS1
#ifdef QUANTOR_LOG2
  LOG (quantor, 2, "  ELIMINATION=%.2f", delta - stats->sat_solver_time);
  LOG (quantor, 2, "  SATSOLVER=%s", get_sat_solver_name (quantor));
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "    CALLS=%d SAT=%.2f LIFT=%.2f",
       stats->sat_solver, stats->sat_solver_time, stats->sat_original_time);
#endif
#endif
#endif
#ifdef QUANTOR_LOG1
  LOG (quantor, 1, "MB %.1f", stats->max_bytes / (double) (1 << 20));
#endif
#ifdef QUANTOR_LOG2
#ifdef QUANTOR_STATS1
  LOG (quantor, 2, "  SAT=%.1f",
       stats->sat_solver_bytes / (double) (1 << 20));
#endif
#ifdef QUANTOR_STATS2
  LOG (quantor, 2, "  CHUNKS=%u", stats->num_chunks);
#endif
#endif
  quantor->io.out = saved_out;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_CHECK
/*------------------------------------------------------------------------*/
#ifndef QUANTOR_HAVE_DOUBLE_PRECISION_INT
/*------------------------------------------------------------------------*/

static void
check_ldfloor (void)
{
  assert (ldfloor (0) == 0);
  assert (ldfloor (1) == 0);
  assert (ldfloor (2) == 1);
  assert (ldfloor (3) == 1);
  assert (ldfloor (4) == 2);
  assert (ldfloor (5) == 2);
  assert (ldfloor (6) == 2);
  assert (ldfloor (7) == 2);
  assert (ldfloor (8) == 3);
  assert (ldfloor (9) == 3);
  assert (ldfloor (15) == 3);
  assert (ldfloor (16) == 4);
  assert (ldfloor (17) == 4);
  assert (ldfloor (31) == 4);
  assert (ldfloor (32) == 5);
  assert (ldfloor (33) == 5);
  assert (ldfloor ((1 << 14) - 1) == 13);
  assert (ldfloor ((1 << 14) + 0) == 14);
  assert (ldfloor ((1 << 14) + 1) == 14);
  assert (ldfloor ((1 << 15) - 1) == 14);
  assert (ldfloor ((1 << 15) + 0) == 15);
  assert (ldfloor ((1 << 15) + 1) == 15);
  assert (ldfloor ((1 << 16) - 1) == 15);
  assert (ldfloor ((1 << 16) + 0) == 16);
  assert (ldfloor ((1 << 16) + 1) == 16);
  assert (ldfloor ((1 << 30) - 1) == 29);
  assert (ldfloor ((1 << 30) + 0) == 30);
  assert (ldfloor ((1 << 30) + 1) == 30);
  assert (ldfloor (INT_MAX) == 30);
}

/*------------------------------------------------------------------------*/

static void
check_num_bits (void)
{
  assert (num_bits (0) == 1);
  assert (num_bits (1) == 1);
  assert (num_bits (2) == 2);
  assert (num_bits (3) == 2);
  assert (num_bits (15) == 4);
  assert (num_bits (17) == 5);
  assert (num_bits ((1 << 17) - 1) == 17);
  assert (num_bits ((1 << 17) + 1) == 18);
  assert (num_bits (INT_MAX) == 31);
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static void
check_mult_with_overflow (void)
{
  assert (mult_with_overflow (0, 7) == 0);
  assert (mult_with_overflow (1, 7) == 7);
  assert (mult_with_overflow (-1, 7) == -7);
  assert (mult_with_overflow (0, INT_MIN) == 0);
  assert (mult_with_overflow (0, INT_MAX) == 0);
  assert (mult_with_overflow (0, QUANTOR_OVERFLOW) == 0);
  assert (mult_with_overflow (INT_MIN, 0) == 0);
  assert (mult_with_overflow (INT_MAX, 0) == 0);
  assert (mult_with_overflow (QUANTOR_OVERFLOW, 0) == 0);
  assert (mult_with_overflow (-3, 0) == 0);
  assert (mult_with_overflow (-3, 1) == -3);
  assert (mult_with_overflow (-3, -1) == 3);
  assert (mult_with_overflow (3, QUANTOR_OVERFLOW) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (QUANTOR_OVERFLOW, -5) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (17, 19) == 17 * 19);
  assert (mult_with_overflow (-17, 19) == -(17 * 19));
  assert (mult_with_overflow (17, -19) == -(17 * 19));
  assert (mult_with_overflow (-17, -19) == 17 * 19);
  assert (mult_with_overflow (32767, 32767) == 32767 * 32767);
  assert (mult_with_overflow (-32767, 32767) == -(32767 * 32767));
  assert (mult_with_overflow (32767, -32767) == -(32767 * 32767));
  assert (mult_with_overflow (-32767, -32767) == 32767 * 32767);
  assert (mult_with_overflow (65536, 32768) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (32767, 65539) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (-32767, 65539) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (32767, -65539) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (-32767, -65539) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (65535, 32767) == 65535 * 32767);
  assert (mult_with_overflow (65535, -32767) == -(65535 * 32767));
  assert (mult_with_overflow (-65535, 32767) == -(65535 * 32767));
  assert (mult_with_overflow (-65535, -32767) == 65535 * 32767);
  assert (mult_with_overflow (QUANTOR_OVERFLOW / 2, 2) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (QUANTOR_OVERFLOW / 2 - 1, 2) ==
	  QUANTOR_OVERFLOW - 2);
  assert (mult_with_overflow (-2, QUANTOR_OVERFLOW / 2) == -QUANTOR_OVERFLOW);
  assert (mult_with_overflow (-2, QUANTOR_OVERFLOW / 2 - 1) ==
	  -QUANTOR_OVERFLOW + 2);
  assert (mult_with_overflow (((QUANTOR_OVERFLOW - 1) / 2), 2) ==
	  (((QUANTOR_OVERFLOW - 1)) / 2) * 2);
  assert (mult_with_overflow (((QUANTOR_OVERFLOW + 1) / 2), 2) ==
	  QUANTOR_OVERFLOW);
  assert (mult_with_overflow (INT_MIN, 2) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (-2, INT_MIN) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (INT_MIN, -1) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (-QUANTOR_OVERFLOW, -1) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (INT_MIN / 2, 2) == QUANTOR_OVERFLOW);
  assert (mult_with_overflow (INT_MIN / 2 + 1, 2) == INT_MIN + 2);
}

/*------------------------------------------------------------------------*/

static void
check_add_with_overflow (void)
{
  assert (add_with_overflow (17, 23) == 40);
  assert (add_with_overflow (-17, -23) == -40);
  assert (add_with_overflow (17, -23) == -6);
  assert (add_with_overflow (-17, 23) == 6);
  assert (add_with_overflow (QUANTOR_OVERFLOW, 5) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (7, QUANTOR_OVERFLOW) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW, 0) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW, INT_MIN) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (INT_MAX, INT_MIN) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (0, INT_MAX) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (0, QUANTOR_OVERFLOW) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW, QUANTOR_OVERFLOW) ==
	  QUANTOR_OVERFLOW);
  assert (add_with_overflow (INT_MAX, INT_MIN) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (INT_MIN, INT_MIN) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (INT_MIN, INT_MAX) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (INT_MAX, INT_MAX) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (INT_MIN + 2, -1) == INT_MIN + 1);
  assert (add_with_overflow (-1, INT_MIN + 1) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW - 10, 9) ==
	  QUANTOR_OVERFLOW - 1);
  assert (add_with_overflow (9, QUANTOR_OVERFLOW - 10) ==
	  QUANTOR_OVERFLOW - 1);
  assert (add_with_overflow (10, QUANTOR_OVERFLOW - 10) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (10, QUANTOR_OVERFLOW - 9) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (11, QUANTOR_OVERFLOW - 9) == QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW / 2 - 1, QUANTOR_OVERFLOW / 2)
	  == QUANTOR_OVERFLOW - 1);
  assert (add_with_overflow (QUANTOR_OVERFLOW / 2, QUANTOR_OVERFLOW / 2) ==
	  QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW / 2 + 1, QUANTOR_OVERFLOW / 2)
	  == QUANTOR_OVERFLOW);
  assert (add_with_overflow (QUANTOR_OVERFLOW / 2, QUANTOR_OVERFLOW / 2 + 1)
	  == QUANTOR_OVERFLOW);
}

/*------------------------------------------------------------------------*/

static void
check_member (void)
{
  VarAnchor anchor[2];
  Var var[6];

  memset (&anchor, 0, sizeof (anchor));
  memset (&var, 0, sizeof (var));

  dlink (&anchor[0], &var[0], &var[0].unit_link);
  dlink (&anchor[0], &var[1], &var[1].unit_link);
  dlink (&anchor[0], &var[2], &var[2].unit_link);

  dlink (&anchor[1], &var[3], &var[3].unit_link);
  dlink (&anchor[1], &var[4], &var[4].unit_link);
  dlink (&anchor[1], &var[5], &var[5].unit_link);

  assert (!member (&anchor[1], &var[1], &var[1].unit_link));
  assert (!member (&anchor[0], &var[4], &var[4].unit_link));
}

/*------------------------------------------------------------------------*/

static void
check_overflow_score (void)
{
  const char *tmp;
  Quantor quantor;

  tmp = score2str (&quantor, QUANTOR_OVERFLOW);
  assert (tmp && !strcmp (tmp, "<OVERFLOW>"));
}

/*------------------------------------------------------------------------*/

static void
white (void)
{
  check_member ();
  check_overflow_score ();
#ifndef QUANTOR_HAVE_DOUBLE_PRECISION_INT
  check_ldfloor ();
  check_num_bits ();
#endif
  check_mult_with_overflow ();
  check_add_with_overflow ();
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

static QuantorResult
quantor_simplify (Quantor * quantor)
{
  QuantorResult res = QUANTOR_RESULT_UNKNOWN;

  if (!quantor->original_problem_is_propositional)
    res = eliminate_inner_quantified_variables (quantor);

  if (res == QUANTOR_RESULT_UNKNOWN && is_propositional (quantor))
    res = propositional_simplification (quantor);

  if (res != QUANTOR_RESULT_UNSATISFIABLE)
    assign_not_eliminated_zombies (quantor);

  return res;
}

/*------------------------------------------------------------------------*/

static QuantorResult
quantor_sat_after_simplification (Quantor * quantor, QuantorResult res)
{
  if (res == QUANTOR_RESULT_UNKNOWN && !quantor->sat_api)
    {
      report (quantor, "(no sat solver available)");
    }
  else
    {
      if (res == QUANTOR_RESULT_UNKNOWN && is_propositional (quantor))
	res = sat (quantor);

      if (res == QUANTOR_RESULT_SATISFIABLE &&
	  quantor->stats.num_unassigned_exported > 0 &&
	  quantor->save_dead_original_clauses)
	{
	  res = lift_assignment_to_original_clauses (quantor);

	  if (res != QUANTOR_RESULT_SATISFIABLE)
	    report (quantor, "(failed to lift assignment)");
	}

    }

  return res;
}

/*------------------------------------------------------------------------*/
#ifdef QUANTOR_QBF_EVALUATION_FORMAT
/*------------------------------------------------------------------------*/

static void
quantor_print_solution (Quantor * quantor, 
                        QuantorResult res, int do_print_assignment)
{
  int normalized;

  if (res == QUANTOR_RESULT_UNSATISFIABLE)
    normalized = 0;
  else if (res == QUANTOR_RESULT_SATISFIABLE)
    normalized = 1;
  else
    normalized = -1;

  fprintf (quantor->io.out, "s cnf %d %d %d\n",
	   normalized,
	   quantor->stats.specified_vars, quantor->stats.specified_clauses);

  if (do_print_assignment &&
      res == QUANTOR_RESULT_SATISFIABLE && quantor->stats.num_exported > 0)
    print_assignment (quantor);
}

/*------------------------------------------------------------------------*/
#else
/*------------------------------------------------------------------------*/

static void
quantor_print_solution (Quantor * quantor, 
                        QuantorResult res, int do_print_assignment)
{
  if (res == QUANTOR_RESULT_SATISFIABLE)
    {
      fprintf (quantor->io.out, "s %s\n",
	       quantor->original_problem_is_propositional ?
	       "SATISFIABLE" : "TRUE");

      if (do_print_assignment)
	{
	  if (quantor->stats.num_exported > 0)
	    {
	      if (quantor->stats.num_unassigned_exported > 0)
		report (quantor,
			"(assignment to exported "
			"and not eliminated variables follows)");
	      else
		report (quantor,
		        "(assignment to exported variables follows)");

	      print_assignment (quantor);
	    }
	  else
	    report (quantor, "(no variables exported)");
	}
    }
  else if (res == QUANTOR_RESULT_UNSATISFIABLE)
    {
      fprintf (quantor->io.out, "s %s\n",
	       quantor->original_problem_is_propositional ?
	       "UNSATISFIABLE" : "FALSE");
    }
  else if (res == QUANTOR_RESULT_TIMEOUT)
    fputs ("c TIMEOUT\n", quantor->io.out);
  else if (res == QUANTOR_RESULT_SPACEOUT)
    fputs ("c SPACEOUT\n", quantor->io.out);
  else
    {
      assert (res == QUANTOR_RESULT_UNKNOWN);
      fputs ("c UNKNOWN\n", quantor->io.out);
    }
}

/*------------------------------------------------------------------------*/
#endif
/*------------------------------------------------------------------------*/

#define GUNZIP "gunzip -c %s"

int
quantor_main (int argc, char **argv)
{
  int pretty_print_only, dump_sat, quit_after_simplification;
  int do_not_print_assignment, stats_printed;
  int i, err, done, fast_exit;
  SatSolverAPI **sat_api_ptr;
  const char *err_str;
  QuantorResult res;
  Quantor *quantor;
  FILE *file;

  err = 0;
  done = 0;
  dump_sat = 0;
  pretty_print_only = 0;
  do_not_print_assignment = 0;
  quit_after_simplification = 0;
  fast_exit = 0;

  quantor = new_Quantor ();

  /* 0. Phase: register environment variables and '--undocumented' command
   * line options defined in 'options.sh'.
   */
  init_opts (quantor);

  /* 1. Phase: handle output first to be able to write errors in parsing 
   * command line options or environment variable assignments to a LOG file.
   * This is necessary to get full coverage in the test suite.
   */
  for (i = 1; !err && i < argc; i++)
    {
      if (!strcmp (argv[i], "-o"))
	{
	  if (i + 1 >= argc)
	    {
	      fprintf (quantor->io.out,
		       "*** quantor: "
		       "argument to '-o' missing (try '-h')\n");
	      err = 1;
	    }
	  else if (!(file = fopen (argv[++i], "w")))
	    {
	      fprintf (quantor->io.out,
		       "*** quantor: could not write to '%s'\n", argv[i]);
	      err = 1;
	    }
	  else
	    {
	      if (quantor->io.close_out)
		fclose (quantor->io.out);
	      quantor->io.out = file;
	      quantor->io.close_out = 1;
	    }
	}
    }

  /* 2. Phase:  Now assign environment variables specified with
   * '-D<name>=<val>' in order to treat them the same way as externally
   * defined environment variable.  In particular, allow command line
   * options to overwrite environment variables.
   */
  for (i = 1; !err && i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] == 'D')
	{
	  err_str = quantor_putenv (quantor, argv[i] + 2);
	  if (err_str)
	    {
	      fprintf (quantor->io.out,
		       "*** quantor: %s in '%s'\n", err_str, argv[i]);
	      err = 1;
	    }
	}
    }

  /* 3. Phase: Set options found in environment variables with the idea that
   * command line options may overwrite these (e.g. see match_opts).
   */
  if (!err)
    init_env (quantor);

  /* 4. Phase: handle regular command line options.
   */
  for (i = 1; !err && !done && i < argc; i++)
    {
      if (!strcmp (argv[i], "-h") || !strcmp (argv[i], "--help"))
	{
	  fprintf (quantor->io.out, USAGE);
	  done = 1;
	}
      else if (match_opts (quantor, argv[i], &err))
	{
	  /* Do nothing! These are the '--undocumented' options defined in
	   * 'options.sh', which can also be set by environment variables.
	   */
	}
      else if (!strcmp (argv[i], "--undocumented"))
	{
	  usage_opts (quantor, quantor->io.out);
	  done = 1;
	}
      else if (!strcmp (argv[i], "--version"))
	{
	  fprintf (quantor->io.out, "%s\n", quantor_version ());
	  done = 1;
	}
      else if (!strcmp (argv[i], "--configuration"))
	{
	  fprintf (quantor->io.out, "%s", QUANTOR_COMPILE_TIME_OPTIONS);
	  done = 1;
	}
      else if (!strcmp (argv[i], "--id"))
	{
	  fprintf (quantor->io.out, "%s\n", quantor_id ());
	  done = 1;
	}
      else if (!strcmp (argv[i], "--copyright"))
	{
	  fprintf (quantor->io.out, "%s\n", quantor_copyright ());
	  done = 1;
	}
      else if (!strcmp (argv[i], "--white"))
	{
#ifdef QUANTOR_CHECK
	  /* TODO: if there are more white box test case, you should
	   * consider to connect them in the same way as the api test cases.
	   */
	  white ();
#endif
	}
      else if (argv[i][0] == '-' &&
	       argv[i][1] == '-' &&
	       argv[i][2] == 'a' && argv[i][3] == 'p' && argv[i][4] == 'i')
	{
#ifdef QUANTOR_CHECK
	  extern void checkapi (FILE *, int);
	  checkapi (quantor->io.out, atoi (argv[i] + 5));
#endif
	}
      else if (!strcmp (argv[i], "-v"))
	{
	  quantor->opts.verbose++;
	}
      else if (argv[i][0] == '-' && argv[i][1] == 'v')
	{
	  quantor->opts.verbose += atoi (argv[i] + 2);
	}
      else if (!strcmp (argv[i], "-p"))
	{
	  pretty_print_only = 1;
	}
      else if (!strcmp (argv[i], "-s"))
	{
	  quit_after_simplification = 1;
	}
      else if (!strcmp (argv[i], "-n"))
	{
	  do_not_print_assignment = 1;
	}
      else if (!strcmp (argv[i], "-d"))
	{
	  quit_after_simplification = 1;
	  dump_sat = 1;
	}
      else if (!strcmp (argv[i], "-q"))
	{
	  done = 1;
	}
      else if (!strcmp (argv[i], "--fast-exit"))
	{
	  fast_exit = 1;
	}
      else if (!strcmp (argv[i], "--list-sat-solvers"))
	{
	  for (sat_api_ptr = sat_apis; *sat_api_ptr; sat_api_ptr++)
	    fprintf (quantor->io.out, "%s\n", (*sat_api_ptr)->name);
	  done = 1;
	}
      else if (argv[i][0] == '-' && argv[i][1] == 'D')
	{
	  /* skip, since it has been handled above */
	}
      else if (argv[i][0] == '-' && argv[i][1] == '-')
	{
	  for (sat_api_ptr = sat_apis; *sat_api_ptr; sat_api_ptr++)
	    {
	      if (!strcmp ((*sat_api_ptr)->name, argv[i] + 2))
		break;
	    }

	  if (!*sat_api_ptr)
	    goto INVALID_OPTION;

	  quantor->sat_api = *sat_api_ptr;
	}
      else if (!strcmp (argv[i], "-o"))
	{
	  if (i + 1 >= argc)
	    {
	      fprintf (quantor->io.out,
		       "*** quantor: "
		       "argument to '-o' missing (try '-h')\n");
	      err = 1;
	    }
	  else if (!(file = fopen (argv[++i], "w")))
	    {
	      fprintf (quantor->io.out,
		       "*** quantor: could not write to '%s'\n", argv[i]);
	      err = 1;
	    }
	  else
	    {
	      if (quantor->io.close_out)
		fclose (quantor->io.out);
	      quantor->io.out = file;
	      quantor->io.out_name = argv[i];
	      quantor->io.close_out = 1;
	    }
	}
      else if (argv[i][0] == '-')
	{
	INVALID_OPTION:
	  fprintf (quantor->io.out,
		   "*** quantor: invalid option '%s' (try '-h')\n", argv[i]);
	  err = 1;
	}
      else if (strstr (argv[i], ".gz") &&
	       strlen (strstr (argv[i], ".gz")) == 3)
	{
	  int len = strlen (argv[i]) + strlen (GUNZIP);
	  char * cmd = new (quantor, len);

	  sprintf (cmd, GUNZIP, argv[i]);
	  file = popen (cmd, "r");
	  delete (quantor, cmd, len);

	  if (file)
	    {
	      quantor->io.in = file;
	      quantor->io.in_name = argv[i];
	      quantor->io.pclose_in = 1;
	    }
	  else
	    {
	      fprintf (quantor->io.out,
		       "*** quantor: could not gunzip '%s'\n", argv[i]);
	      err = 1;
	    }
	}
      else if (!(file = fopen (argv[i], "r")))
	{
	  fprintf (quantor->io.out,
		   "*** quantor: could not read '%s'\n", argv[i]);
	  err = 1;
	}
      else
	{
	  if (quantor->io.close_in)
	    fclose (quantor->io.in);

	  quantor->io.in = file;
	  quantor->io.in_name = argv[i];
	  quantor->io.close_in = 1;
	}
    }

  /* 5. Phase: check for invalid environment variables starting with the
   * prefix "QUANTOR_" which are not defined in 'options.sh' and registered 
   * in 'init_opts'.
   */
  err_str = get_invalid_env (quantor);
  if (err_str)
    {
      fprintf (quantor->io.out,
	       "*** quantor: invalid environment '%s'\n", err_str);
      err = 1;
    }

  /* Now we are done with parsing command line options and environment
   * variables and setting options accordingly.
   */
  quantor_setup (quantor);

  res = QUANTOR_RESULT_UNKNOWN;
  stats_printed = 0;

  if (!done && !err)
    {
#ifdef QUANTOR_LOG1
      banner (quantor);
#endif
#ifdef QUANTOR_LOG2
      if (quantor->opts.verbose >= 2)
	log_opts (quantor);
#endif
      if ((err = !parse (quantor)))
	{
	  assert (quantor->parser.err);
	  fprintf (quantor->io.out, "%s:%d: %s\n",
		   quantor->io.in_name, quantor->parser.lineno,
		   quantor->parser.err);
	}
      else if (pretty_print_only)
	{
	  quantor_print (quantor, quantor->io.out);
	}
      else
	{
	  res = quantor_simplify (quantor);
	  if (dump_sat)
	    quantor_print (quantor, quantor->io.out);
	  else if (!quit_after_simplification)
	    {
	      res = quantor_sat_after_simplification (quantor, res);
	      if (!done && !err) 
		{
		  quantor_stats (quantor, quantor->io.out);
		  stats_printed = 1;
		}

	      quantor_print_solution (quantor, res, !do_not_print_assignment);
	    }
	}
    }

  if (!done && !err && !stats_printed)
    quantor_stats (quantor, quantor->io.out);

  if (!fast_exit)
    delete_Quantor (quantor);

  return res;
}

/*------------------------------------------------------------------------*/

Quantor *
quantor_new (void)
{
  Quantor *res;

  res = new_Quantor ();
  init_opts (res);
  init_env (res);
  quantor_setup (res);

  return res;
}

/*------------------------------------------------------------------------*/

void
quantor_set_log (Quantor * quantor, FILE * LOG)
{
  if (quantor->io.close_out)
    fclose (quantor->io.out);

  quantor->io.out = LOG;
  quantor->io.close_out = 0;	/* we assume 'LOG' is closed by user */
}

/*------------------------------------------------------------------------*/

void
quantor_delete (Quantor * quantor)
{
  delete_Quantor (quantor);
}

/*------------------------------------------------------------------------*/

const char *
quantor_copyright (void)
{
  return QUANTOR_COPYRIGHT;
}

/*------------------------------------------------------------------------*/

const char *
quantor_version (void)
{
  return QUANTOR_VERSION;
}

/*------------------------------------------------------------------------*/

const char *
quantor_id (void)
{
  return QUANTOR_ID;
}

/*------------------------------------------------------------------------*/

QuantorResult
quantor_sat (Quantor * quantor)
{
  QuantorResult res;

  res = quantor_simplify (quantor);
  res = quantor_sat_after_simplification (quantor, res);

  return res;
}

