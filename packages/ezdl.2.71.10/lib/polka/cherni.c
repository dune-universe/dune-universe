/*% $Id: cherni.c,v 1.5 2003/12/12 14:02:55 bjeannet Exp $ */

/*% Conversion from one representation to the dual one. */

#include <assert.h>

#include "cherni.h"
#include "internal.h"
#include "caml/fail.h"

/* %======================================================================== */
/* \section{Computation of saturation matrix} */
/* %======================================================================== */

/* Bits of \verb-satline- are index by the constraints. \verb-satline- are
   supoosed to be filled with zeros. */


void cherni_buildsatline(const matrix_t* con, const pkint_t* tab, 
			 bitstring_t* satline)
{
  bitindex_t jx = bitindex_init(0);
  while (jx.index<con->nbrows){
    vector_product(&pk_cherni_prod,con->p[jx.index],tab,con->nbcolumns);
    if (pkint_sgn(pk_cherni_prod)) bitstring_set(satline,jx);
    bitindex_inc(&jx);
  }
}

/* %======================================================================== */
/* \section{Conversion algorithm} */
/* %======================================================================== */

/*
\verb-con- is the constraints matrix, \verb-start- indicates the number
of constraints supposed to be already taken in account, \verb-ray- is
the frames matrix, with the \verb-start- first constraints taken in
account, \verb-sat- is the saturation matrix initialized according to
\verb-con- and \verb-ray-, and \verb-nbline- indicates the number of
lines in \verb-ray-. One suppose that the saturation matrix has a
maximum size of \verb|ray->polka_maxrows x bitindex_size(con->nbrows)|.
For the saturation matrix, we suppose that Unused bits of existing rows
(from \verb-0- to \verb-start-) are set to zero. Each time a row is
added, this hypothese must remain true.
 
The saturation matrix is organised as follows:
\begin{itemize}
\item the rows are indexed by generators,
\item the columns by constraints.
\end{itemize}
 
The result is given by \verb-ray-, \verb-sat- and \verb-pnbline-. The
status coefficient of rows of \verb-ray- are set properly at the end of
the function.
*/

int cherni_conversion(matrix_t* con, const int start, 
                      matrix_t* ray, satmat_t* satc, int nbline)

 {       
  int i,j,l,w;
  int is_inequality, index_non_zero;
  int equal_bound,sup_bound,inf_bound,bound;
  int nbcommonconstraints;
  bitindex_t k;
  bitstring_t m,aux;
  bool redundant;
  const int nbcols = con->nbcolumns;
  const int satnbcols = bitindex_size(con->nbrows);
  int nbrows = ray->nbrows;

  /* ================= Code ================== */
  k = bitindex_init(start);
  while (k.index < con->nbrows){
    /* Iteration sur les contraintes */
    is_inequality = pkint_sgn(con->p[k.index][0]);
    
    /* Scalar product and index: */
/*  
    \par\begin{small}
    We compute for the new considered constraint its scalar products
    with each frame and put them into the first coefficient of the
    frames. Moreover we set \verb-index_non_zero- to the index of the
    first frame that does not saturate the constraint. 
    \end{small}\par
*/

    index_non_zero = nbrows;
    for (i=0; i<nbrows; i++){
      vector_product(&ray->p[i][0],ray->p[i],con->p[k.index],nbcols);
      if (index_non_zero == nbrows && pkint_sgn(ray->p[i][0])!=0){
	index_non_zero = i;
      }
    }
    
    if (index_non_zero < nbline){ /* If a line doesn't satisfy the constraint */
      /* Line becomes ray: */
      /* We transform the first line that does not saturate the constraint
	 into a ray and computes a new pointed cone. */

      /* remove it of lines and put it at index nbline */
      nbline--;
      if (index_non_zero != nbline)
	matrix_exch_rows(ray,index_non_zero,nbline);
      /* compute new lineality space */
      for (i=index_non_zero; i<nbline; i++)
	if (pkint_sgn(ray->p[i][0]) != 0)
	  matrix_combine_rows(ray,i,nbline,i,0);
      
      /* orient the new ray */
      if (pkint_sgn(ray->p[nbline][0]) < 0)
	for (j=0; j<nbcols; j++)
	  pkint_neg(ray->p[nbline][j],ray->p[nbline][j]);
      
      /* compute the new pointed cone */
      for (i=nbline+1; i<nbrows; i++)
	if (pkint_sgn(ray->p[i][0]))
	  matrix_combine_rows(ray,i,nbline,i,0);
      
      /* For the saturation matrix, we only add a column, */
      /* so new bits are initialized to zero (see above) */
      if (is_inequality){ 
	/* rays saturates all apart the last inequality */
	satmat_set(satc,nbline,k);
      } else {
	/* one remove the ray */
	nbrows --; ray->nbrows --; satc->nbrows--;
	matrix_exch_rows(ray, nbline, nbrows);
	satmat_exch_rows(satc, nbline, nbrows);
      }

     bitindex_inc(&k);
    } 

    else { /* if all lines saturates the constraint */
      /* Sort the rays: */
/*      
	\par\begin{small}
	Rays are sorted as follows:
	 \begin{itemize}\setlength{\itemsep}{0pt}
	 \item \verb-nbline- $\leq i <$ \verb-equal_bound-: 
	 saturate the constraint;
	 \item \verb-equal_bound- $\leq i <$ \verb-sup_bound-: verify it;
	 \item \verb-sup_bound- $\leq i <$ \verb-nbrows-: do not verify it.
	 \end{itemize}
	 \end{small}\par
*/
 
      equal_bound=nbline;
      sup_bound=nbline;
      inf_bound=nbrows;
      while (inf_bound>sup_bound) {
	int s = pkint_sgn(ray->p[sup_bound][0]);
	if (s==0){
	  matrix_exch_rows(ray, sup_bound, equal_bound);
	  satmat_exch_rows(satc, sup_bound, equal_bound);
	  equal_bound++;
	  sup_bound++;
	} else if (s<0) {
	  inf_bound--;
	  matrix_exch_rows(ray, sup_bound, inf_bound);
	  satmat_exch_rows(satc, sup_bound, inf_bound);
	} else {
	  sup_bound++;
	}
      }
      if (is_inequality && sup_bound == nbrows){ 
        /* all rays satisfy the constraint:redundancy */
        con->nbrows--;
        matrix_exch_rows(con, k.index, con->nbrows);
      }
      else {
        if (sup_bound==nbline){ /* no ray satisfies the constraint */
          nbrows = ray->nbrows = satc->nbrows = nbline; 
        }
        else { /* some rays do not satisfy the constraint */
	  /* Compute the new cones by combining adjacent constraints: */
	  bound = nbrows;
	  for (i=equal_bound; i<sup_bound; i++){
	    for(j=sup_bound; j<bound; j++){
	      /* For each pair R+,R-, */
	      /* compute the set of constraints saturated by both of them,
		 including equalities */
	      nbcommonconstraints=0; 
	      for (w=0; w<k.word; w++) {
		aux = pk_cherni_bitstringp[w] = satc->p[i][w] | satc->p[j][w];
		for (m=bitstring_msb; m!=0; m>>=1) 
		  if ((aux & m)==0) nbcommonconstraints++;
	      }
	      aux = pk_cherni_bitstringp[k.word] = satc->p[i][k.word] | satc->p[j][k.word];
	      for (m=bitstring_msb; m!=k.bit; m>>=1){
		if ((aux & m)==0) nbcommonconstraints++;
	      }
	      if (nbcommonconstraints+nbline>=nbcols-3){ /* possibly adjacent */
		/* Does exist another ray saturating the same constraints ? */
		redundant=false;
		for (l=nbline; l<bound; l++){
		  if ((l!=i)&&(l!=j)){
		    for (w=0; w<=k.word; w++){
		      if (satc->p[l][w] & ~(pk_cherni_bitstringp[w])) 
			break;
		    }
		    if (w>k.word){ 
		      redundant=true; break;
		    }
		  }
		}
		if (!redundant){ /* if not */
		  if (nbrows==matrix_get_maxrows(ray)){ 
		    failwith("Chernikova: out of table space\n");
		    printf("Chernikova: out of table space\n"); 
          fprintf(stderr, "Chernikova: out of table space\n"); 
          exit(1);
		  }
		  /* Compute the new ray and put it at end */
		  matrix_combine_rows(ray,j,i,nbrows,0);
		  /* New row in saturation matrix */
		  for (w=0; w<=k.word; w++) satc->p[nbrows][w] = pk_cherni_bitstringp[w];
		  for (w=k.word+1; w<satnbcols; w++) satc->p[nbrows][w] = 0;
		  nbrows ++; ray->nbrows ++; satc->nbrows ++;
		}
	      }
	    }
	  }
	  /* Remove non extremal rays by exchanging added rays */
	  /* with those that don t verify the constraint */
	  {
	    int l;
	    if (is_inequality){
	      j = sup_bound;
	      for (l=equal_bound; l<sup_bound; l++) satmat_set(satc,l,k);
	    }
	    else {
	      j = equal_bound;
	    }
	    i = nbrows;
	    while ((j<bound)&&(i>bound)) {
	      i--;
	      matrix_exch_rows(ray,i,j);
	      satmat_exch_rows(satc,i,j);
	      j++;
	    }
	    nbrows = (j==bound) ? i : j;
	    ray->nbrows = satc->nbrows = nbrows;
	  }
        }
        bitindex_inc(&k);
      }
    }
  }
  
  /* status coefficient */
  for (i=0; i<nbline; i++){
    pkint_set_si(ray->p[i][0],0);
  }
  for (i = nbline; i<nbrows; i++){
    pkint_set_si(ray->p[i][0],1);
  }
  ray->nbrows = satc->nbrows = nbrows;
  return nbline;
}

/* %======================================================================== */
/* \section{Simplification of origin representation} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Gauss pivot} */
/* %------------------------------------------------------------------------ */

/*
The function finds a minimal system for equalities, and returns the rank $r$ of
this system, equations of which are numbered from $0$ to $r-1$. Redundant (now
null) equations are put between row $r$ and row $nbeq$.  The function also
fills the array \verb-pk_cherni_intp- which indicates the ``main'' coefficent
of an equation, in this case the left-most non-zero one.
*/

int gauss(matrix_t* con, int nbeq)
{
  int i,j,k;
  pkint_t** p = con->p;
  int rank = 0;
  int s = 0;
  for (j = con->nbcolumns-1; j>=1; j--){
    /* find the first i such that p[i][j] is non zero */
    for (i=rank; i<nbeq; i++) {
      s = pkint_sgn(p[i][j]);
      if (s) break;
    }
    if (i<nbeq) { /* was one found ? */
      if (i>rank) { /* put it in rank */
        matrix_exch_rows(con,i,rank);
      }
      if (s<0) {  /* normalize with positive coefficient */
        for (k=1; k<con->nbcolumns; k++)
          pkint_neg(p[rank][k],p[rank][k]);
      }
      pkint_set_ui(p[rank][0],0);
      for (k=i+1; k<nbeq; k++) {
        if (pkint_sgn(p[k][j]))
          matrix_combine_rows(con,k,rank,k,j);
      }
      pk_cherni_intp[rank] = j;
      rank++;
    }
  }
  return rank;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Backsubstitution} */
/* %------------------------------------------------------------------------ */

/* This function backsubstitute the coefficients according to the system of
   equations and the array \verb-pk_cherni_intp- properly set by
   \verb-gauss-. */


void backsubstitute(matrix_t* con, int rank)
{
  int i,j,k;
  for (k=rank-1; k>=0; k--) {
    j = pk_cherni_intp[k];
    for (i=0; i<k; i++) {
      if (pkint_sgn(con->p[i][j]))
        matrix_combine_rows(con,i,k,i,j);
    }
    for (i=k+1; i<con->nbrows; i++) {
      if (pkint_sgn(con->p[i][j]))
        matrix_combine_rows(con,i,k,i,j);
    }
  }
}

/* %------------------------------------------------------------------------ */
/* \subsection{Simplifying constraints} */
/* %------------------------------------------------------------------------ */

/*
We suppose that we just obtained \verb-ray- and \verb-satc- from \verb-con-
with the chernikova algorithms. As a consequence the system of rays is
minimal. \verb-satf- is the transposed matrix of \verb-satc-, i.e. rows are
indexed by constraints. \verb-con- is supposed to be non empty.

We have still to simplify \verb-con- by detecting new equality constraints,
removing redundant inequalities, and obtaining a minimal system of
equalities. This is performed by gauss elimination.
*/

int cherni_simplify(matrix_t* con, matrix_t* ray, satmat_t* satf, const int nbline)
{
  int i,j,nb,nbj;
  int nbeq,rank;
  int w;
  bitstring_t m;

  bool redundant, is_equality;

  const int nbcols = con->nbcolumns;
  const bitindex_t nbrays = bitindex_init(ray->nbrows);
  int nbcons = con->nbrows;

  /* find the first inequality */
  for (nbeq=0; nbeq < nbcons; nbeq ++){
    if (pkint_sgn(con->p[nbeq][0])) break;
  }

  /* For each constraint, 
     - put it with equalities if it detected as one
     - or set the status word to the number of rays that saturates the constraint,
  */
  for (i=nbeq; i < nbcons; i++){
    is_equality = (pkint_sgn(con->p[i][0])==0);
    if (!is_equality){
      is_equality = true;
      for (w=0; w<satf->nbcolumns; w++){
        if (satf->p[i][w]){
          is_equality = false; break;
        }
      }
    }
    if (is_equality){
      /* we have an equality */
      pkint_set_ui(con->p[i][0],0);
      matrix_exch_rows(con, i,nbeq);
      satmat_exch_rows(satf,i,nbeq);
      nbeq++;
    }
    else {
      /* we count the number of zero bits */
      nb = 0;
      for (w=0; w<nbrays.word; w++){
        for (m=bitstring_msb; m!=0; m>>=1) 
          if ((satf->p[i][w] & m)==0) nb++;
      }
      for (m=bitstring_msb; m!=nbrays.bit; m>>=1)
        if ((satf->p[i][nbrays.word] & m)==0) nb++;
      pkint_set_si(con->p[i][0],nb);
    }
  }
  /* remove redundant equalities and update nbeq */
  rank = gauss(con, nbeq); /* gauss pivot to simplify equalities */

  /* remove redundants equations, located between rank and nbeq */
  if (rank<nbeq) {
    i = nbcons;
    j = rank;
    while( j < nbeq && i > nbeq ) {
      i--;
      matrix_exch_rows(con, j,i);
      satmat_exch_rows(satf,j,i);
      j++;
    }
    nbcons += rank - nbeq;
    nbeq = rank;
  }
  /* remove trivially redundants inequalities (nb < nbcols-nbeq-2) */
  i = nbeq;
  while (i < nbcons){
    nb = pkint_get_si(con->p[i][0]);
    if (nb < nbcols-nbeq-2){ /* redundant constraint */
      nbcons--;
      matrix_exch_rows(con, i,nbcons);
      satmat_exch_rows(satf,i,nbcons);
    }
    else
      i++;
  }    
  /* remove others redundants inequalities */
  i=nbeq;
  while (i < nbcons){
    nb = pkint_get_si(con->p[i][0]);
    redundant = false;
    j = nbeq;
    while (j < nbcons){
      nbj = pkint_get_si(con->p[j][0]);
      if (nbj > nb){
        /* does j saturates a strictly overset ? */
        redundant = true;
        for (w=0; w<satf->nbcolumns; w++){
          if( ~(satf->p[i][w]) & satf->p[j][w] ){
            redundant = false; break;
          }
        }
        if (redundant) 
          break;
        else
          j++;
      }
      else if (nbj == nb && j != i){
          /* is j mutually redundant with i ? */
          is_equality = true;
          for (w=0; w<satf->nbcolumns; w++){
            if( satf->p[i][w] != satf->p[j][w] ){
              is_equality = false; break;
            }
          }
          if (is_equality){
            /* yes: we can remove j */
            nbcons--;
            matrix_exch_rows(con, j,nbcons);
            satmat_exch_rows(satf,j,nbcons);
          }
          else
            j++;
      }
      else
        j++;
    }
    if (redundant){
      nbcons--;
      matrix_exch_rows(con, i,nbcons);
      satmat_exch_rows(satf,i,nbcons);
    }
    else
      i++;
  }
  /* setting status coefficient */
  for (i=nbeq; i<nbcons; i++){
    pkint_set_ui(con->p[i][0],1);
  }
  con->nbrows = satf->nbrows = nbcons;

  /* back substitution of remaining constraints */
  backsubstitute(con, nbeq);
  
  return nbeq;
}

/* %======================================================================== */
/* \section{Checking functions} */
/* %======================================================================== */

/* These functions perform coherence checks about minimized representation of
   polyhedra.

   \verb-cherni_checksatmat- recomputes the saturation matrix of \verb-C- and
   \verb-F- and checks the equality with \verb-satC-. */

bool cherni_checksatmat(const bool con_to_ray,
                        const matrix_t* C, const matrix_t* F, const satmat_t* satC)
{
  int i,s1,s2;
  bitindex_t j;

  for (i=0; i<F->nbrows; i++){
    for (j = bitindex_init(0); j.index  < C->nbrows; bitindex_inc(&j)){
      vector_product(&pk_cherni_prod,F->p[i],C->p[j.index],F->nbcolumns);
      s1 = pkint_sgn(pk_cherni_prod);
      s2 = satmat_get(satC,i,j);
      if (s1<0 || (s1!=0 && s2==0) || (s1==0 && s2!=0)){
        printf("cherni_checksatmat con_to_ray=%d: ray %d, con %d\n",
               con_to_ray,i,j.index);
        printf("Constraints\n"); matrix_print(C); 
        printf("Frames\n"); matrix_print(F); 
        satmat_print(satC);
        return false;
      }
    }
  }
  return true;
}

/* This function checks the saturation numbers of constraints and rays. */

bool cherni_checksat(const bool con_to_ray,
                     const matrix_t* C, int nbequations,
                     const matrix_t* F, int nblines,
                     const satmat_t* satC)
{
  int i,k,nb,rank;
  bitindex_t j;
  matrix_t* mat;
  
  bool res = true;
  const int nbcols = C->nbcolumns;

  /* saturation des rayons */
  mat = matrix_alloc(C->nbrows,nbcols,false);
  for (i=0; i<F->nbrows; i++){
    nb = 0;
    for (j = bitindex_init(0); j.index < C->nbrows; bitindex_inc(&j)){
      if (satmat_get(satC,i,j)==0){
        for (k=0;k<nbcols;k++){
          pkint_set(mat->p[nb][k],C->p[j.index][k]);
        }
        nb++;
      }
    }
    rank = gauss(mat,nb);
    if (!( (pkint_sgn(F->p[i][0])==0 && nb == C->nbrows)
           || (rank==nbcols-2-nblines && nb >= rank))){
      printf("cherni_checksat con_to_ray=%d: wrong ray %d; expected = %d, effective = (%d<=%d)\n",
             con_to_ray,i,nbcols-2-nblines,rank,nb);
      res = false;
    }
  }
  matrix_free(mat);

  /* saturation des contraintes */
  mat = matrix_alloc(F->nbrows,nbcols,false);
  for (j = bitindex_init(0); j.index < C->nbrows; bitindex_inc(&j)){
    nb = 0;
    for (i=0; i<F->nbrows; i++){
      if (satmat_get(satC,i,j)==0){
        for (k=0;k<nbcols;k++){
          pkint_set(mat->p[nb][k],F->p[i][k]);
        }
        nb++;
      }
    }
    rank = gauss(mat,nb);
    if (!( (pkint_sgn(C->p[j.index][0])==0 && nb == F->nbrows)
           || (rank==nbcols-2-nbequations && nb >= rank))){
      printf("cherni_checksat con_to_ray=%d: wrong con %d; expected = %d, effective = (%d<=%d)\n",
             con_to_ray, j.index, nbcols-2-nbequations, rank,nb);
      res = false;
    }
  }
  matrix_free(mat);
  
  if (res==false){
    printf("Constraints\n"); matrix_print(C); 
    printf("Frames\n"); matrix_print(F); 
    satmat_print(satC);
  }
  return res;
}
/* %======================================================================== */
/* \section{Minimization} */
/* %======================================================================== */

void cherni_minimize(const bool con_to_ray, matrix_t** p_C, 
                     matrix_t** p_F, satmat_t** p_satF,
                     int* p_nbeq, int* p_nbline)
{
  int i;
  bool special;

  matrix_t* C = *p_C;

  /* sorting of con */
  if (!matrix_is_sorted(C)) matrix_sort_rows(C);

  /* initialization of sat and frame */
  pk_cherni_bigray->nbrows = pk_cherni_bigsatc->nbrows = C->nbcolumns-1;
  pk_cherni_bigray->nbcolumns = C->nbcolumns;
  pk_cherni_bigsatc->nbcolumns = bitindex_size(C->nbrows);
  for (i=0; i<C->nbcolumns-1; i++){
    bitstring_clear(pk_cherni_bigsatc->p[i],pk_cherni_bigsatc->nbcolumns);
    vector_clear(pk_cherni_bigray->p[i],pk_cherni_bigray->nbcolumns);
    pkint_set_ui(pk_cherni_bigray->p[i][i+1],1);
  }
  /* conversion */
  *p_nbline = cherni_conversion(C, 0, pk_cherni_bigray, pk_cherni_bigsatc, C->nbcolumns-1);

  /* special case ? */
  /* We have to test
     - In non-strict mode, that $\xi$ can be strictly positive.
     - In strict mode, that both $\xi$ and $\epsilon$ can be strictly
       positive. Because $\xi\geq\epsilon$, we just need to check that
       $\epslion$ can be strictly positive. */

  special = true;
  for (i = *p_nbline; i<pk_cherni_bigray->nbrows; i++){
    if (pkint_sgn(pk_cherni_bigray->p[i][polka_dec-1])>0){ 
      special = false; 
      break; 
    }
  }
  if (special){
    if (con_to_ray){ /* this means we have an empty polyhedron */
      matrix_free(C);
      *p_C = 0;
      *p_F = 0;
      *p_satF = 0;
      *p_nbeq = *p_nbline = 0;
    }
    else { /* this means that the polyhedron is included in $\xi <= 0$ or $\epsilon<=0$ */
      fprintf(stderr,"cherni.o: cherni_minimize ray_to_con: matrix of rays was not valid\n");
      abort();
    } 
  }
  else {
    *p_satF = satmat_transpose(pk_cherni_bigsatc,C->nbrows);
    *p_nbeq = cherni_simplify(C,pk_cherni_bigray,*p_satF,*p_nbline);
    *p_F = matrix_copy(pk_cherni_bigray);
  }
}

/* %======================================================================== */
/* \section{Adding of a matrix of constraints to a minimal representation} */
/* %======================================================================== */

/* 
When \verb-con_to_ray=true-, this function takes a minimized polyhedron,
given by \verb-Cdep, Fdep, satCdep-, and a matrix of constraints \verb-Caut-,
and delivers the polyhedron which constraints are the union of \verb-Cdep-
and \verb-Caut- in a minimized form in \verb-*p_Cres-, \verb-*p_Fdep-,
\verb-*p_satCres-. If \verb-con_to_ray=false-, the function does the dual
operations, i.e. union of frames. In this case, you have to exchange the
meaning of parameters.

\verb-Cdep- and \verb-Caut- are assumed to be sorted.
*/

void cherni_add_and_minimize(bool con_to_ray,
                         const matrix_t* Cdep, const matrix_t* Fdep,
                         const satmat_t* satCdep, int nblines,
                         const matrix_t* Caut,
                         matrix_t** p_C, matrix_t** p_F, satmat_t** p_satF,
                         int* p_nbeq, int* p_nbline)
{
  int i,j;
  int kdep,kaut;
  bool special;
  matrix_t* C;
  
  const int nbcols = Cdep->nbcolumns;

  if (Cdep->nbrows + Caut->nbrows + 1 >= polka_maxnbrows){
    fprintf(stderr,"too much constraints in cherni_add_and_minimize\n");
    exit(-2);
  }
  /* constraint matrix */
  C = _matrix_alloc_int(Cdep->nbrows + Caut->nbrows, nbcols, false);

  /* filling first with Cdep */
  for (i=0; i<Cdep->nbrows; i++){
    for (j=0; j<nbcols; j++){ pkint_init_set(C->p[i][j], Cdep->p[i][j]); }
  }
  /* filling with constraints of Caut which do not already exist in Cdep */
  kdep = kaut = 0;
  i = Cdep->nbrows;
  while (kdep < Cdep->nbrows && kaut < Caut->nbrows){
    int s = vector_compare(Cdep->p[kdep],Caut->p[kaut],nbcols);
    if (s==0){
      kdep++; kaut++;
    }
    else if (s<0){
      kdep++;
    }
    else {
      for (j=0; j<nbcols; j++){
        pkint_init_set(C->p[i][j], Caut->p[kaut][j]);
      }
      i++; kaut++;
    }
  }
  while (kaut < Caut->nbrows){
    for (j=0; j<nbcols; j++){
      pkint_init_set(C->p[i][j], Caut->p[kaut][j]);
    }
    i++; kaut++;
  }
  C->nbrows = i;
  while (i<C->_maxrows){  /* filling the last rows with zeroes */
    for (j=0; j<nbcols; j++) pkint_init_set_ui(C->p[i][j],0);
    i++;
  }
  
  /* frame and saturation matrix */
  pk_cherni_bigray->nbrows = pk_cherni_bigsatc->nbrows = Fdep->nbrows;
  pk_cherni_bigray->nbcolumns = nbcols;
  pk_cherni_bigsatc->nbcolumns = bitindex_size(C->nbrows);
  for (i=0; i<Fdep->nbrows; i++){
    for (j=0; j<nbcols; j++)
      pkint_set(pk_cherni_bigray->p[i][j],Fdep->p[i][j]);

    for (j=0; j<satCdep->nbcolumns; j++)
      pk_cherni_bigsatc->p[i][j] = satCdep->p[i][j];
    for (j=satCdep->nbcolumns; j<pk_cherni_bigsatc->nbcolumns; j++)
      pk_cherni_bigsatc->p[i][j] = 0;
  }
  /* conversion */
  *p_nbline = cherni_conversion(C,Cdep->nbrows,pk_cherni_bigray,pk_cherni_bigsatc,nblines);

  /* special case ? */
  /* We have to test
     - In non-strict mode, that $\xi$ can be strictly positive.
     - In strict mode, that both $\xi$ and $\epsilon$ can be strictly
       positive. Because $\xi\geq\epsilon$, we just need to check that
       $\epsilon$ can be strictly positive. */
  
  special = true;
  for (i = *p_nbline; i<pk_cherni_bigray->nbrows; i++){
    if (pkint_sgn(pk_cherni_bigray->p[i][polka_dec-1])>0){ 
      special = false; 
      break; 
    }
  }
  if (special){
    if (con_to_ray){ /* this means we have an empty polyhedron */
      matrix_free(C);
      *p_C = *p_F = 0;
      *p_satF = 0;
      *p_nbeq = *p_nbline = 0;
    }
    else { /* this means that the polyhedron is included in $\xi <= 0$ or $\epsilon<=0$ */
      fprintf(stderr,"cherni.o: cherni_add_and_minimize ray_to_con: matrix of rays was not valid\n");
      matrix_print(Cdep);
      matrix_print(Fdep);
      matrix_print(Caut);
      matrix_print(C);
      matrix_print(pk_cherni_bigray);
      printf("nbeq = %d\tnbline = %d\n",*p_nbeq,*p_nbline);
      abort();
    } 
  }
  else {
    *p_satF = satmat_transpose(pk_cherni_bigsatc,C->nbrows);
    *p_nbeq = cherni_simplify(C,pk_cherni_bigray,*p_satF,*p_nbline);
    *p_C = C;
    *p_F = matrix_copy(pk_cherni_bigray);
    return;
  }
}

/* %======================================================================== */
/* \subsection{Adding of dimensions} */
/* %======================================================================== */

/* 
The standard case of this function is the addition of new dimensions
at the end of matrices and embeding of a polyhedron. To do the dual
operations, i.e.  projection, one have to exchange the meaning
parameters.
*/

void cherni_add_dimensions(const matrix_t* C, const matrix_t* F, 
                           satmat_t** p_satC, const satmat_t* satF, int dimsup,
                           matrix_t** p_Cres, matrix_t** p_Fres, 
                           satmat_t** p_satCres)
{
  int i,j;
  matrix_t* nC = 0;
  matrix_t* nF = 0;
  satmat_t* nsatC = 0;

  if (dimsup <= 0){
    fprintf(stderr,
            "cherni_add_dimensions : inconsistent dimension = %d\n",dimsup);
    abort();
  }
  
  if (C || F){
    /* etendre simplement les contraintes */
    if (C){
      nC = matrix_add_dimensions(C,dimsup);
    }
    /* etendre les rayons et ajouter les nouveaux generateurs */
    if (F){
      nF = matrix_alloc(F->nbrows+dimsup, F->nbcolumns+dimsup,false);
      /* addition of new lines at the beginning of the matrix */
      for (i=0; i<dimsup; i++){
        pkint_set_ui(nF->p[i][nF->nbcolumns-1-i],1);
      }
      /* extension of actual generators */
      for (i=0; i<F->nbrows; i++)
 	vector_add_dimensions(nF->p[dimsup+i],F->p[i],F->nbcolumns,dimsup);
      nF->_sorted = matrix_is_sorted(F) &&
        (vector_compare(nF->p[dimsup-1], nF->p[dimsup], nF->nbcolumns)<=0);
    }
    /* New saturation matrix: we can work with \verb|*p_satC| */
    if (!(*p_satC) && satF){
      *p_satC = satmat_transpose(satF,F->nbrows);
    }
    if (*p_satC){
      const satmat_t* satC = *p_satC;
      nsatC = satmat_alloc(satC->nbrows+dimsup, satC->nbcolumns);
      /* the first rows, corresponding to new lines, are already zero */
      for (i=0; i<satC->nbrows; i++){
        for (j=0; j<satC->nbcolumns; j++)
          nsatC->p[dimsup+i][j] = satC->p[i][j];
      }
    }
  }
  *p_Cres = nC;
  *p_Fres = nF;
  *p_satCres = nsatC;
}


/* 
The standard case of this function is the addition of new dimensions and
embeding of a polyhedron. To do the dual operations, i.e.  projection, one
have to exchange the meaning parameters.

For each element \verb|x| of the array of type \verb|dimsup_t|,
\verb|x.nbdims| dimensions are inserted at standard position
\verb|x.pos|, with 0 $<=$ \verb|x.pos| $<=$ \verb|polka_maxcolumns|.
For instance, on a vector, with \verb|polka_strict=true| and \verb|polka_dec=3|:

\verb|add_dimensions_multi [1 1 -1 2 3] [(0,2) (1,1) (2,3)] = [1 1 -1 0 0 2 0 3 0 0 0]|
*/

void cherni_add_dimensions_multi
(const matrix_t* C, const matrix_t* F, satmat_t** p_satC, const satmat_t* satF, 
 const dimsup_t* tab, int size,
 matrix_t** p_Cres, matrix_t** p_Fres, satmat_t** p_satCres,
 int* p_dimsup)
{
  int m,n,i,j,offset;
  int nbcolumns;
  matrix_t* nC = 0;
  matrix_t* nF = 0;
  satmat_t* nsatC = 0;
  int dimsup;

  if (C || F){
    nbcolumns = C ? C->nbcolumns : F->nbcolumns;
    
    /* Checks the input */
    dimsup = check_add_dimsup_of_multi(nbcolumns,tab,size);
    /* etendre simplement les contraintes */
    if (C){
      nC = matrix_add_dimensions_multi(C,tab,size);
    }
    /* etendre les rayons et ajouter les nouveaux generateurs */
    if (F){
      nF = matrix_alloc(F->nbrows+dimsup, F->nbcolumns+dimsup,false);
      /* addition of new lines at the beginning of the matrix */
     i = 0;
      offset = 0;
      for (m=0; m<size; m++){
	n=0;
	while (n<tab[m].nbdims){
	  pkint_set_ui(nF->p[i][polka_dec+offset+tab[m].pos+n],1);
	  i++; n++;
	}
	offset += n;
      }
      /* extension of actual generators */
      for (i=0; i<F->nbrows; i++)
 	vector_add_dimensions_multi(nF->p[dimsup+i],F->p[i],nbcolumns,tab,size);
    }
    /* New saturation matrix: we can work with \verb|*p_satC| */
    if (!(*p_satC) && satF){
      *p_satC = satmat_transpose(satF,F->nbrows);
    }
    if (*p_satC){
      const satmat_t* satC = *p_satC;
      nsatC = satmat_alloc(satC->nbrows+dimsup, satC->nbcolumns);
      /* the first rows, corresponding to new lines, are already zero */
      for (i=0; i<satC->nbrows; i++){
        for (j=0; j<satC->nbcolumns; j++)
          nsatC->p[dimsup+i][j] = satC->p[i][j];
      }
    }
  }
  else {
    dimsup = dimsup_of_multi(tab,size);
  }
  *p_Cres = nC;
  *p_Fres = nF;
  *p_satCres = nsatC;
  *p_dimsup = dimsup;
}

/* 
The standard case of this function is the addition of new dimensions and
embeding of a polyhedron. To do the dual operations, i.e.  projection, one
have to exchange the meaning parameters.
*/

void cherni_add_permute_dimensions
(const matrix_t* C, const matrix_t* F, satmat_t** p_satC, const satmat_t* satF, 
 const int dimsup, const int * const permutation,
 matrix_t** p_Cres, matrix_t** p_Fres, satmat_t** p_satCres)
{
  int i,j;
  int nbcolumns;
  matrix_t* nC = 0;
  matrix_t* nF = 0;
  satmat_t* nsatC = 0;

  if (C || F){
    nbcolumns = C ? C->nbcolumns : F->nbcolumns;
    
    /* etendre simplement les contraintes */
    if (C){
      nC = matrix_add_permute_dimensions(C,dimsup,permutation);
    }
    /* etendre les rayons et ajouter les nouveaux generateurs  */
    if (F){
      nF = matrix_alloc(F->nbrows+dimsup, F->nbcolumns+dimsup,false);
      /* addition of new lines at the beginning of the matrix */
      for (i=0; i<dimsup; i++){
	int col = nF->nbcolumns-1-i;
        pkint_set_ui(nF->p[i][permutation[col-polka_dec]+polka_dec],1);
      }
      /* extension and permutation of actual generators */
      for (i=0; i<F->nbrows; i++)
 	vector_add_permute_dimensions(nF->p[dimsup+i],
				      F->p[i],F->nbcolumns,
				      dimsup,permutation);
    }
    /* New saturation matrix: we can work with \verb|*p_satC| */
    if (!(*p_satC) && satF){
      *p_satC = satmat_transpose(satF,F->nbrows);
    }
    if (*p_satC){
      const satmat_t* satC = *p_satC;
      nsatC = satmat_alloc(satC->nbrows+dimsup, satC->nbcolumns);
      /* the first rows, corresponding to new lines, are already zero */
      for (i=0; i<satC->nbrows; i++){
        for (j=0; j<satC->nbcolumns; j++)
          nsatC->p[dimsup+i][j] = satC->p[i][j];
      }
    }
  }
  *p_Cres = nC;
  *p_Fres = nF;
  *p_satCres = nsatC;
}
