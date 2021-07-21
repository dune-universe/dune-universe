#include  <stdio.h>
#include "polka.h"
#include "pkint.h"
#include "poly.h"

void essai1()
{
  poly_t *PUniv, *P1, *P2, *P3;
  bool result;
  matrix_t* conP1 = matrix_alloc(2,4,false);
  /*   x >= 2  */
  pkint_set_si(conP1->p[0][0], 1);
  pkint_set_si(conP1->p[0][1], -2);
  pkint_set_si(conP1->p[0][2], 0);
  pkint_set_si(conP1->p[0][3], 1);

  /*   x > 1  */
  pkint_set_si(conP1->p[1][0], 1);
  pkint_set_si(conP1->p[1][1], -1);
  pkint_set_si(conP1->p[1][2], -1);
  pkint_set_si(conP1->p[1][3], 1);

  PUniv = poly_universe(1);
  P1 = poly_add_constraints(PUniv, conP1);
  poly_print(P1);
  poly_minimize(P1);
  poly_print(P1);

  P2 = poly_add_constraint(PUniv, conP1->p[0]);
  P3 = poly_add_constraint(PUniv, conP1->p[1]);

  result = poly_is_included_in(P2, P3);
  if(result == true)
    printf("\n\npoly[x>=2] is included in poly[x>1]\n");
  else
    printf("\n\npoly[x>=2] is not included in poly[x>1]\n");
}

void essai2()
{
  poly_t *PUniv, *PZero, *P1, *P2, *P3;
  const matrix_t* rayP1;
  matrix_t* rayP1b;
  bool result;
  int i;

  int nbdim=12;

  PUniv = poly_universe(nbdim);

  /* P1 */
  matrix_t* conP1 = matrix_alloc(nbdim*2,nbdim+3,false);
  for (i=0;i<nbdim; i++){
    pkint_set_si( conP1->p[2*i][0], 1); 
    pkint_set_si( conP1->p[2*i][polka_cst], 1000000000); 
    pkint_set_si( conP1->p[2*i][polka_dec+i], -1  );
    pkint_set_si( conP1->p[2*i+1][0], 1); 
    pkint_set_si( conP1->p[2*i+1][polka_cst], 1000000000); 
    pkint_set_si( conP1->p[2*i+1][polka_dec+i], 1    );
  }
  printf ("Here1a\n");
  P1 = poly_add_constraints(PUniv, conP1);
  poly_minimize(P1);
  matrix_free(conP1);
  printf ("Here1b\n");

  /* PZero */
  conP1 = matrix_alloc(nbdim,nbdim+3,false);
  for (i=0;i<nbdim; i++){
    pkint_set_si( conP1->p[i][polka_dec+i], 1);  
  }
  PZero = poly_add_constraints(PUniv, conP1);
  poly_minimize(PZero);
  matrix_free(conP1);
  
  poly_free(PUniv);

  /* */
  rayP1 = poly_frames(P1);
  
  for (i=0; i<5; i++){
    printf ("Here2a %d\n",i);
    rayP1b = matrix_copy(rayP1);
    matrix_qsort_rows(rayP1b);
    rayP1b->_sorted = true;
    printf ("Here2b %d\n",i);
    P2 = poly_add_frames(PZero,rayP1b);
    printf ("Here2c %d\n",i);
    matrix_free(rayP1b);
    poly_free(P2);
  }
  poly_free(P1);
  poly_free(PZero);
}



int main(int argc, char** argv)
{
  polka_initialize(true,20,10000);
 
  essai2();
           
  return 0;
}

