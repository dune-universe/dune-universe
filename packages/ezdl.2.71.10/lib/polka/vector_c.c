/*% $Id: vector.c,v 1.5 2004/02/27 10:30:43 bjeannet Exp $ */

/*% Operations on vectors */

#include <stdio.h>
#include <assert.h>
#include "vector.h"
#include "internal.h"

/* %======================================================================== */
/* \section[Basic operations]
   {Basic operations: creation, destruction, copying and printing} */
/* %======================================================================== */

/* Internal allocation function: the elements are not initialized. */
pkint_t* _vector_alloc_int(const int size){
  assert(size>0);
  return (pkint_t*)malloc(size*sizeof(pkint_t));
}

/* Standard allocation function, with initialization of the elements. */
pkint_t* vector_alloc(const int size)
{
  int i;
  pkint_t* q;
  q = _vector_alloc_int(size);
  for(i=0; i<size; i++){
    pkint_init(q[i]);
  }
  return q;
}

/* Deallocation function. */
void vector_free(pkint_t* const q, const int size)
{
  int i;
  for(i=0; i<size; i++) pkint_clear(q[i]);
  free(q);
}

/* Set all elements to zero. */
void vector_clear(pkint_t* const q, const int size)
{
  int i;
  for (i=0; i<size; i++) pkint_set_ui(q[i],0);
}

/* Duplicate the vector. */
pkint_t* vector_copy(const pkint_t* const q, const int size)
{
  int i;
  pkint_t* q2 = _vector_alloc_int(size);
  for (i=0; i<size; i++) pkint_init_set(q2[i],q[i]);
  return q2;
}

/* Raw printing function. */
void vector_print(const pkint_t* const q, const int size)
{
  int i;
  printf("vector %d: ", size);
  for (i=0; i<size; i++){
    pkint_print(q[i]); printf(" ");
  }
  printf("\n");
}


/* %======================================================================== */
/* \section{Normalization} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Searching the minimum non-zero coefficient} */
/* %------------------------------------------------------------------------ */

/* The following functions search the index and the absolute value of the
   minimal non-zero coefficient of a vector. It returns its results with
   pointers \verb-index- and \verb-min-. If all coefficients are zero, then
   \verb-index- is set to \verb-size- and \verb-*min- to 0.

   This function uses \verb-pk_vector_tmp[0]-. */

static void
vector_min_notzero(const pkint_t* const v, const int size,
		   int* const index, pkint_t* const min)
{
  int i;
  pkint_set_ui(*min,0);

  /* search the first non-zero coefficient
     and stores the index and the coeff in *index and *min */
  i = 0;
  while (i<size){
    if (pkint_sgn(v[i])){
      *index = i;
      pkint_abs(*min,v[i]);
      break;
    }
    i++;
  }
  i++;
  /* search now the minimum */
  while (i<size) {
    if (pkint_sgn(v[i])){
      pkint_abs(pk_vector_tmp[0],v[i]);
      if (pkint_cmp(*min,pk_vector_tmp[0]) > 0){
	*index = i;
	pkint_set(*min,pk_vector_tmp[0]);
      }
    }
    i++;
  }
}

/* %------------------------------------------------------------------------ */
/* \subsection{Pgcd computation} */
/* %------------------------------------------------------------------------ */

/* This function computes the pgcd of a vector. The vector is lost, so
   you have to make a copy before.

   This function uses \verb-pk_vector_tmp[0]-. */

static void vector_gcd(pkint_t* const v, const int size, pkint_t* const p_gcd)
{
  int not_all_zero;
  do {
    int i,index=-1;
    vector_min_notzero(v,size,&index,p_gcd);
    if (pkint_sgn(*p_gcd)==0) break;
    not_all_zero = false;
    for (i=0; i<size; i++)
      if (i!=index){
	pkint_mod(v[i],v[i],*p_gcd);
	not_all_zero = not_all_zero || pkint_sgn(v[i]);
      }
  } while (not_all_zero);
}

/* %------------------------------------------------------------------------ */
/* \subsection{Main functions} */
/* %------------------------------------------------------------------------ */

/* The function \verb-vector_normalize- normalizes the vector considered as
   a contraint or a generator, and \verb-vector_normalize_expr- does the
   same thing for the vector considered as an affine expression.

   These function use \verb-pk_vector_tmp[0..1]-. */

void vector_normalize(pkint_t* const r, const int size)
{
  int i;
  /*  computation of the pgcd */
  for (i=1; i<size; i++)
    pkint_set(pk_vector_pkintp[i],r[i]);
  vector_gcd(&pk_vector_pkintp[1], size-1, &pk_vector_tmp[1]);
  /* possible division */
  if (pkint_cmp_ui(pk_vector_tmp[1],1)>0){
    for (i=1; i<size; i++)
      pkint_divexact(r[i],r[i],pk_vector_tmp[1]);
  }
}

void vector_normalize_expr(pkint_t* const r, const int size)
{
  int i;
  for (i=0; i<size; i++) pkint_set(pk_vector_pkintp[i],r[i]);
  vector_gcd(pk_vector_pkintp,size,&pk_vector_tmp[1]);
  if (pkint_cmp_ui(pk_vector_tmp[1],1)>0){
    for (i=0; i<size; i++)
      pkint_divexact(r[i],r[i],pk_vector_tmp[1]);
  }
}


/* %======================================================================== */
/* \section{Comparison and hashing functions} */
/* %======================================================================== */

/* Comparison function, for vectors considered as contraints.

The used order is the lexicographic order, with the exception that the
constant (and possibly epsilon) coefficient is considered last. As a
consequence, the equations or lines are classified before the
inequalities or rays when vectors are rows of a sorted matrix.

The meaning of the returned result \verb-res- is: \\
\begin{tabular}{rcl}
\verb-res<0- &:& \verb-q1- is smaller than \verb-q2-; \\
\verb-res==0- &:& they are equal; \\
\verb-res>0- &:& \verb-q1- is greater than \verb-q2-; \\
\verb-abs(res)==1- &:& in addition they are parallel.
\end{tabular} \\
For two parallel inequalities (equal coefficients apart for the $\xi$ and
$\epsilon$ dimensions), the defined order $\leq$ corresponds with the
entailment of constraints (if either \verb-polka_strict- is true or not): if
$\vec{a}\leq\vec{b}$, then $\vec{a}\{\geq,>\} 0 \Rightarrow \vec{b}\{\geq,>\}
0$.

This function uses \verb-pk_vector_tmp[0..3]-.
*/

int vector_compare(const pkint_t* const q1, const pkint_t* const q2,
		   const int size)
{
  int i;
  int res=1;
  int s1,s2;
  /* bidirectional/unidirectional ? */
  res = pkint_cmp(q1[0],q2[0]);
  if (res){
    return (res>0) ? 2 : (-2);
  }
  else {
    /* normal non-constant coefficients */
    /* compute pgcd q1 */
    for (i=polka_dec; i<size; i++)
      pkint_set(pk_vector_pkintp[i],q1[i]);
    vector_gcd(&pk_vector_pkintp[polka_dec],size-polka_dec,&pk_vector_tmp[1]);
    /* compute pgcd q2 */
    for (i=polka_dec; i<size; i++)
      pkint_set(pk_vector_pkintp[i],q2[i]);
    vector_gcd(&pk_vector_pkintp[polka_dec],size-polka_dec,&pk_vector_tmp[2]);
    /* special cases */
    s1 = pkint_sgn(pk_vector_tmp[1]);
    s2 = pkint_sgn(pk_vector_tmp[2]);
    if (s1==0 && s2==0){
      pkint_set_ui(pk_vector_tmp[1],1);
      pkint_set_ui(pk_vector_tmp[2],1);
      goto nextcompare;
    }
    if (s1==0)  pkint_set_ui(pk_vector_tmp[1],1);
    if (s2==0)  pkint_set_ui(pk_vector_tmp[2],1);
    /* comparison */
    for(i=polka_dec; i<size; i++){
      pkint_divexact(pk_vector_tmp[0],q1[i],pk_vector_tmp[1]);
      pkint_divexact(pk_vector_tmp[3],q2[i],pk_vector_tmp[2]);
      res = pkint_cmp(pk_vector_tmp[0],pk_vector_tmp[3]);
      if (res){
	return (res>0) ? 2 : (-2);
      }
    }
  }
 nextcompare:
  if (polka_cst<size){
    pkint_mul(pk_vector_tmp[0],q1[polka_cst],pk_vector_tmp[2]);
    pkint_mul(pk_vector_tmp[3],q2[polka_cst],pk_vector_tmp[1]);
    res = pkint_cmp(pk_vector_tmp[0],pk_vector_tmp[3]);
    if (res==0 && polka_strict){
      pkint_mul(pk_vector_tmp[0],q1[polka_eps],pk_vector_tmp[2]);
      pkint_mul(pk_vector_tmp[3],q2[polka_eps],pk_vector_tmp[1]);
      res = pkint_cmp(pk_vector_tmp[0],pk_vector_tmp[3]);
    }
    if (res){
      res = (res>0) ? 1 : (-1);
    }
    return res;
  }
  else
    return 0;
}

/* Comparison function for expressions. */

int vector_compare_expr(const pkint_t* const q1, const pkint_t* const q2,
		   const int size)
{
  int i;
  int res=1;
  int s1,s2;

  s1 = pkint_sgn(q1[0]);
  s2 = pkint_sgn(q2[0]);
  if (s1==0 && s2==0){
    pkint_set_ui(pk_vector_tmp[1],1);
    pkint_set_ui(pk_vector_tmp[2],1);
  }
  else if (s1==0) return 2;
  else if (s2==0) return (-2);
  else {
    pkint_set(pk_vector_tmp[1],q1[0]);
    pkint_set(pk_vector_tmp[2],q2[0]);
  }
  /* normal non-constant coefficients */
  for (i=2; i<size; i++) {
    pkint_mul(pk_vector_tmp[0],q1[i],pk_vector_tmp[2]);
    pkint_mul(pk_vector_tmp[3],q2[i],pk_vector_tmp[1]);
    res = pkint_cmp(pk_vector_tmp[0],pk_vector_tmp[3]);
    if (res){
      return (res>0) ? 2 : (-2);
    }
  }
  /* constant coefficient */
  if (polka_cst<size){
    pkint_mul(pk_vector_tmp[0],q1[polka_cst],pk_vector_tmp[2]);
    pkint_mul(pk_vector_tmp[3],q2[polka_cst],pk_vector_tmp[1]);
    res = pkint_cmp(pk_vector_tmp[0],pk_vector_tmp[3]);
    if (res){
      res = (res>0) ? 1 : (-1);
    }
    return res;
  }
  else
    return 0;
}

/* Hashing function.
It is compatible with the notion of equality.
*/

unsigned int vector_hash(const pkint_t* const q, const int size)
{
  if (size==0)
    return 0;
  else {
    int i;
    unsigned int dec = 0;
    unsigned int res = size << 8;
    for (i=0; i<size; i += (size+7)/8){
      res += pkint_get_ui(q[i]) << dec;
      dec++;
    }
    return res;
  }
}

/* %======================================================================== */
/* \section{Combination} */
/* %======================================================================== */

/* \verb-vector_combine- computes a combination \verb-q3- of \verb-q1- and
   \verb-q2- such that \verb-q3[k]=0-.  The first coefficient is never
   considered for computations, except when \verb-k==0-.

   This function uses \verb-pk_vector_tmp[0..4]-. */

void vector_combine(const pkint_t* const q1, const pkint_t* const q2,
		    pkint_t* const q3, const int k, const int size)
{
  int j;
  pkint_gcd(pk_vector_tmp[0],q1[k],q2[k]);
  pkint_divexact(pk_vector_tmp[1],q1[k],pk_vector_tmp[0]);
  pkint_divexact(pk_vector_tmp[2],q2[k],pk_vector_tmp[0]);
  for (j=1;j<size;j++){
    if (j!=k){
      pkint_mul(pk_vector_tmp[3],pk_vector_tmp[2],q1[j]);
      pkint_mul(pk_vector_tmp[4],pk_vector_tmp[1],q2[j]);
      pkint_sub(q3[j],pk_vector_tmp[3],pk_vector_tmp[4]);
    }
  }
  pkint_set_ui(q3[k],0);
  vector_normalize(q3,size);
}

/* %======================================================================== */
/* \section{Algebraic operations} */
/* %======================================================================== */

/* Scalar product.

Compute the scalar product of \verb-q1- and \verb-q2- considered as vectors
of length \verb-size-. The first coefficients are never considered.

This function uses \verb-pk_vector_tmp[0]-. */

void vector_product(pkint_t* const prod,
		    const pkint_t* const q1, const pkint_t* const q2, const int size)
{
  int j;
  pkint_set_ui(*prod,0);
  for (j=1; j<size; j++){
    pkint_mul(pk_vector_tmp[0],q1[j],q2[j]);
    pkint_add(*prod,*prod,pk_vector_tmp[0]);
  }
}

/* Same as previous function, but in case where \verb-polka_strict- is
   true, the $\epsilon$ coefficients are not taken into account. */

void vector_product_strict(pkint_t* const prod,
		    const pkint_t* const q1, const pkint_t* const q2, const int size)
{
  int j;
  if (polka_cst<size){
    pkint_mul(*prod,q1[polka_cst],q2[polka_cst]);
  }
  else {
    pkint_set_ui(*prod,0);
    return;
  }
  for (j=polka_dec; j<size; j++){
    pkint_mul(pk_vector_tmp[0],q1[j],q2[j]);
    pkint_add(*prod,*prod,pk_vector_tmp[0]);
  }
}

/* Addition, substraction and scaling of affine expression.

\verb-vector_add_expr- and \verb-vector_sub_expr- respectively adds and
substracts \verb-q1- and \verb-q2- considered as affine expressions. The
result is normalized and put in \verb-q3-.

These functions uses \verb-pk_vector_tmp[1..2]-. */

void vector_add_expr(pkint_t* const q3,
		     const pkint_t* const q1, const pkint_t* const q2,
		     const int size)
{
  int j;
  pkint_mul(q3[0],q1[0],q2[0]);
  for (j=1; j<size; j++){
    pkint_mul(pk_vector_tmp[1],q1[j],q2[0]);
    pkint_mul(pk_vector_tmp[2],q2[j],q1[0]);
    pkint_add(q3[j],pk_vector_tmp[1],pk_vector_tmp[2]);
  }
  vector_normalize_expr(q3,size);
}

void vector_sub_expr(pkint_t* const q3,
		     const pkint_t* const q1, const pkint_t* const q2,
		     const int size)
{
  int j;
  pkint_mul(q3[0],q1[0],q2[0]);
  for (j=1; j<size; j++){
    pkint_mul(pk_vector_tmp[1],q1[j],q2[0]);
    pkint_mul(pk_vector_tmp[2],q2[j],q1[0]);
    pkint_sub(q3[j],pk_vector_tmp[1],pk_vector_tmp[2]);
  }
  vector_normalize_expr(q3,size);
}

void vector_scale_expr(pkint_t* const q2,
		       const pkint_t num, const pkint_t den,
		       const pkint_t* const q1, const int size)
{
  int j;
  pkint_gcd(pk_vector_tmp[1],num,q1[0]);
  if (pkint_cmp_ui(pk_vector_tmp[1],1)>0){
    pkint_divexact(pk_vector_tmp[2],q1[0],pk_vector_tmp[1]);
    pkint_mul(q2[0],pk_vector_tmp[2],den);
    pkint_divexact(pk_vector_tmp[2],num,pk_vector_tmp[1]);
  }
  else {
    pkint_mul(q2[0],den,q1[0]);
    pkint_set(pk_vector_tmp[2],num);
  }
  if (pkint_cmp_ui(pk_vector_tmp[2],1)!=0){
    for (j=1; j<size; j++){
      pkint_mul(q2[j],q1[j],pk_vector_tmp[2]);
    }
  }
  else {
    for (j=1; j<size; j++){
      pkint_set(q2[j],q1[j]);
    }
  }
  vector_normalize_expr(q2,size);
}


/* %======================================================================== */
/* \section{Change of dimensions} */
/* %======================================================================== */

/*
If \verb|dimsup| is positive, adds \verb|dimsup| dimensions at the end of
\verb|q1| and puts the result in \verb|q2|; If \verb|dimsup| is negative,
deletes the \verb|-dimsup| last dimensions of \verb|q1| and puts the
result in \verb|q2|. \verb|q2| is supposed to have a sufficient size.
*/

void vector_add_dimensions(pkint_t* q2, const pkint_t* q1, int size, int dimsup)
{
  int j;

  if (dimsup > 0){
    for (j=0; j < size; j++)
      pkint_set(q2[j],q1[j]);
    for (j=size; j<size+dimsup; j++)
      pkint_set_ui(q2[j],0);
  }
  else {
    for (j=0; j < size+dimsup; j++)
      pkint_set(q2[j],q1[j]);
  }
}

/*
Fills the vector \verb|q2| by inserting new columns with null values in
vector \verb|q1|, according to the array \verb|tab| of size \verb|multi|.

For each element \verb|dimsup| of that array, \verb|dimsup.nbdims|
dimensions are inserted starting from rank \verb|dimsup.pos| (of the
initial vector). The coefficient \verb|q1[polka_dec+dimsup.pos]| is
pushed on the right. For instance, if the array \verb|tab| is
\verb|[(0,2);(1,1);(2,3)]|,
\begin{itemize}
\item the vector
\verb|[d,b,    a_0,  a_1,      a_2,a_3,...,a_{d-1}]|
\item becomes
\verb|[d,b,0,0,a_0,0,a_1,0,0,0,a_2,a_3,...,a_{d-1}]|.
\end{itemize}
If the strict option is set, \verb|[d,b,s,a_0,...,a_{d-1}]| becomes
\verb|[d,b,s,0,0,a_0,0,a_1,0,0,0,,a_2,a_3,...,a_{d-1}]|.

The array \verb|tab| is supposed to be sorted in ascending order
w.r.t. \verb|tab[i].pos|.  \verb|q2| is supposed to have a sufficient
size.
*/

void vector_add_dimensions_multi(pkint_t* q2, const pkint_t* q1, int size,
				 const dimsup_t* tab, int multi)
{
  int m,j,n,offset;

  j = 0;        /* Index for q1 */
  offset = 0;   /* Offset for q2; q2[offset+j] matches q1[j] */
  /* Iterate on ranges of new dimensions */
  for (m=0; m<multi; m++){
    while (j < polka_dec + tab[m].pos){
      pkint_set(q2[offset+j],q1[j]);
      j++;
    }
    for (n=0; n<tab[m].nbdims; n++)
      pkint_set_ui(q2[offset+j+n],0);

    offset += n;
  }
  while (j < size){
    pkint_set(q2[offset+j],q1[j]);
    j++;
  }
}

/*
Fills the vector \verb|q2| by deleting some columns in
vector \verb|q1|, according to the array \verb|tab| of size \verb|multi|.

For each element \verb|dimsup| of that array, \verb|dimsup.nbdims|
dimensions are deleted starting from rank \verb|dimsup.pos| (of the
initial vector). For instance, if the array \verb|tab| is
\verb|[(0,2);(3,1);(5,3)]|,
\begin{itemize}
\item the vector
\verb|[d,b,a_0,a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8,...,a_{d-1}]|
\item becomes
\verb|[d,b,        a_2,    a_4,            a_8, ...,a_{d-1}|].
\end{itemize}
The array \verb|tab| is supposed to be sorted in ascending order
w.r.t. \verb|tab[i].pos|. \verb|q2| is supposed to have a sufficient
size.
*/

void vector_remove_dimensions_multi(pkint_t* q2, const pkint_t* q1, int size,
				    const dimsup_t* tab, int multi)
{
  int m,j,offset;

  j = 0;
  offset = 0;
  for (m=0; m<multi; m++){
    while (offset+j < polka_dec + tab[m].pos){
      pkint_set(q2[j],q1[offset+j]);
      j++;
    }
    offset += tab[m].nbdims;
  }
  while (offset+j < size){
    pkint_set(q2[j],q1[offset+j]);
    j++;
  }
}

/*
\verb|dimsup| is supposed to be positive or null. Add
\verb|dimsup| dimensions to \verb|q| and apply the
permutation \verb|permutation|. The result is stored in \verb|newq|. The array
\verb|permutation| is supposed to be of size \verb|size+dimsup-polka_dec|.

The permutation \verb|permutation| defines a permutation (i.e., a
bijection) from \verb|[0..(size+dimsup-polka_dec)-1| to itself. BE
CAUTIOUS: value \verb|0| in the permutation means columns
\verb|polka_dec|.
*/

void vector_add_permute_dimensions
(pkint_t* newq, const pkint_t* q, const int size,
 const int dimsup, const int * const permut)
{
  int j,newj;
  for (j=0; j<polka_dec; j++){
    pkint_set(newq[j],q[j]);
  }
  for (j=polka_dec; j<size; j++){
    newj = permut[j-polka_dec]+polka_dec;
    pkint_set(newq[newj],q[j]);
  }
  for (j=size; j<size+dimsup; j++){
    newj = permut[j-polka_dec]+polka_dec;
    pkint_set_ui(newq[newj],0);
  }
}

/*
\verb|dimsup| is supposed to be strictly positive. Apply the permutation
\verb|permutation| to \verb|q|, then delete the last
\verb|dimsup| dimensions. The result is stored in \verb|newq|. The array
\verb|permutation| is supposed to of size \verb|size-dimsup-polka_dec|.

The permutation \verb|permutation| defines a permutation (i.e., a
bijection) from \verb|[0..(size-polka_dec)-1| to itself. BE CAUTIOUS: value
\verb|0| in the permutation means columns \verb|polka_dec|.
*/

void vector_permute_remove_dimensions
(pkint_t* newq, const pkint_t* q, const int size,
 const int dimsup, const int * const permut)
{
  int j,newj;

  for (j=0; j<polka_dec; j++){
    pkint_set(newq[j],q[j]);
  }
  for (j=polka_dec; j<size; j++){
    newj = permut[j-polka_dec]+polka_dec;
    /* If the new rank fits in the result, store it,
       otherwise forget it */
    if (newj < size-dimsup){
      pkint_set(newq[newj],q[j]);
    }
  }
}

/* %======================================================================== */
/* \section{Predicates} */
/* %======================================================================== */

/* The function tests if the given vector projected on the
   non-$\epsilon$ coefficients is null. */

bool vector_is_null_strict(const pkint_t* const q, const int size)
{
  int i;
  bool res = true;

  if (size>polka_cst){
    res = pkint_sgn(q[polka_cst])==0;
    if (res){
      for (i=polka_dec; i<size; i++){
	if (pkint_sgn(q[i])!=0){
	  res = false;
	  break;
	}
      }
    }
  }
  return res;
}

/* The function tests if the given vector represents a positivity
   constraint. */

bool vector_is_positivity_constraint(const pkint_t* const q, const int size)
{
  if (size < polka_dec){
    return false;
  }
  else {
    int i;
    bool res;

    res = pkint_sgn(q[0])>0;
    if (res){
      int s = pkint_sgn(q[polka_cst]);
      if (s>0){
	/* Tests if it could be the positivity constraint */
	res = polka_strict ? pkint_sgn(q[polka_eps])==0 : true;
      }
      if (res){
	for (i=polka_dec; i<size; i++){
	  if (pkint_sgn(q[i]) != 0){
	    res = false;
	    break;
	  }
	}
      }
    }
    return res;
  }
}
/* The function tests if the given vector represents a strictness
   constraint. */

bool vector_is_strictness_constraint(const pkint_t* const q, const int size)
{
  if (size < polka_dec){
    return false;
  }
  else {
    int i;
    bool res;

    res = polka_strict && pkint_sgn(q[0])>0;
    if (res){
      int s = pkint_sgn(q[polka_eps]);
      if (s>0){
	/* Tests if it could be the strictness constraint */
	res = pkint_sgn(q[polka_cst])==0;
      }
      if (res){
	for (i=polka_dec; i<size; i++){
	  if (pkint_sgn(q[i]) != 0){
	    res = false;
	    break;
	  }
	}
      }
    }
    return res;
  }
}

/* The function tests if the given vector represents a positivity
   or a strictness constraint. */

bool vector_is_dummy_constraint(const pkint_t* const q, const int size)
{
  if (size < polka_dec){
    return false;
  }
  else {
    int i;
    bool res;

    res = pkint_sgn(q[0])>0;
    if (res){
      int s = pkint_sgn(q[polka_cst]);
      if (s>0){
	/* Tests if it could be the positivity constraint */
	res = polka_strict ? pkint_sgn(q[polka_eps])==0 : true;
      }
      else if (s==0){
	/* Tests if it could be the strictness constraint */
	res = polka_strict && pkint_sgn(q[polka_eps])>0;
      }
      if (res){
	for (i=polka_dec; i<size; i++){
	  if (pkint_sgn(q[i]) != 0){
	    res = false;
	    break;
	  }
	}
      }
    }
    return res;
  }
}
