/*% $Id: polka.h,v 1.5 2004/02/27 10:34:10 bjeannet Exp $ */

/* This header file defines types, operations and variables that are global
   to the library. */

#ifndef __POLKA_H__
#define __POLKA_H__

#include "config.h"

/* Boolean type */
#ifndef HAS_BOOL
typedef enum bool {
  false = 0,
  true  = 1
} bool;
#endif

/* Complete lattice: bot$<$true,false$<$top */
typedef enum tbool {
  tbool_bottom = 0,
  tbool_true   = 1, 
  tbool_false  = 2,
  tbool_top    = 3 
} tbool;
tbool tbool_union(const tbool a, const tbool b);
tbool tbool_inter(const tbool a, const tbool b);
#define tbool_of_bool(a) ((a!=false) ? tbool_true : tbool_false)
#define tbool_of_int(a) ((a) ? tbool_true : tbool_false)

/* To check coherence at run-time */
extern const int polka_integer;

/* Initialization and finalization */
void polka_initialize(bool strict, int maxnbdims, int maxnbrows);
void polka_finalize(void);
void polka_set_widening_affine(void);
void polka_set_widening_linear(void);


/* Globals sets by initialization */
extern bool polka_strict;
extern int polka_dec;

extern bool polka_widening_affine;

extern int polka_maxnbdims;
extern int polka_maxnbrows;
extern int polka_maxcolumns;

/* Do not change ! */
static const int polka_cst = 1;
static const int polka_eps = 2;

/* To insert or remove dimensions */
typedef struct dimsup_t {
  int pos;
  int nbdims;
} dimsup_t;
/* To perform parallel assignements or substitutions */
typedef struct equation_t {
  int var;
  struct pkint_t* expr;
} equation_t;

int dimsup_of_multi(const dimsup_t* tab, int size);
int check_add_dimsup_of_multi(int nbcols, const dimsup_t* tab, int size);
int check_remove_dimsup_of_multi(int nbcols, const dimsup_t* tab, int size);

void translate_equations(equation_t * eqn, int size, int offset);
int cmp_equations(const equation_t* eqna, const equation_t* eqnb);
void sort_equations(equation_t * eqn, int size);

#endif
