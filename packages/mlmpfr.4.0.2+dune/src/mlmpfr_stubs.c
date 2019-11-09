/* This file is part of mlmpfr.

  mlmpfr is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  mlmpfr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with mlmpfr. If not, see
 <http://www.gnu.org/licenses/>. */

#include "mlmpfr_stubs.h"

CAMLprim value
caml_mpfr_prec_min ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (MPFR_PREC_MIN));
}

CAMLprim value
caml_mpfr_prec_max ()
{
  CAMLparam0 ();
  if (MPFR_PREC_MAX > INT_MAX)
    CAMLreturn (Val_int (INT_MAX));
  else
    CAMLreturn (Val_int (MPFR_PREC_MAX));
}

void
precision_in_range (value prec)
{
  int p = Int_val (prec);

  if (p <= Int_val (caml_mpfr_prec_min ())
      || p >= Int_val (caml_mpfr_prec_max ()))
    caml_raise_with_arg (*caml_named_value ("precision range exception"),
                         Val_int (p));
}

void
base_in_range (value base)
{
  int b = Int_val (base);

  if ((b < 2 || b > 64) && b != 0)
    caml_raise_with_arg (*caml_named_value ("base range exception"),
                         Val_int (b));
}

/****************************/
/* Initialization Functions */
/****************************/

CAMLprim value
caml_mpfr_init2_opt (value prec)
{
  CAMLparam1 (prec);
  CAMLlocal2 (rop, p);

  p = prec == Val_none ? caml_mpfr_get_default_prec () : Some_val (prec);
  precision_in_range (p);

  rop = caml_alloc_custom (&mpfr_ops, sizeof (mpfr_t), 0, 1);
  mpfr_init2 (MPFR_val (rop), (mpfr_prec_t) Int_val (p));

  CAMLreturn (rop);
}

CAMLprim value
caml_mpfr_init2 (value prec)
{
  CAMLparam1 (prec);
  CAMLlocal1 (rop);

  precision_in_range (prec);

  rop = caml_alloc_custom (&mpfr_ops, sizeof (mpfr_t), 0, 1);
  mpfr_init2 (MPFR_val (rop), (mpfr_prec_t) Int_val (prec));

  CAMLreturn (rop);
}

CAMLprim value
caml_mpfr_inits2 (value prec, value n)
{
  CAMLparam2 (prec, n);
  CAMLlocal2 (list, tmp);

  if (Int_val (n) <= 0) // if n is zero, return empty list
    CAMLreturn (Val_int (0));

  // build a list of size n
  list = caml_alloc (2, 0);
  Store_field (list, 0, caml_mpfr_init2 (prec));
  Store_field (list, 1, Val_int (0));
  for (int i = 1; i < Int_val (n); i++)
    {
      tmp = caml_alloc (2, 0);
      Store_field (tmp, 0, caml_mpfr_init2 (prec));
      Store_field (tmp, 1, list);
      list = tmp;
    }

  CAMLreturn (list);
}

CAMLprim value
caml_mpfr_clear (value op)
{
  CAMLparam1 (op);
  mpfr_clear (MPFR_val (op));
  CAMLreturn (Val_unit);
}

void
custom_finalize (value op)
{
  mpfr_clear (MPFR_val (op));
}

CAMLprim value
caml_mpfr_set_default_prec (value prec)
{
  precision_in_range (prec);
  CAMLparam1 (prec);
  mpfr_set_default_prec (Int_val (prec));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_get_default_prec ()
{
  CAMLparam0 ();
  mpfr_prec_t prec;

  prec = mpfr_get_default_prec ();

  CAMLreturn (Val_int (prec));
}

CAMLprim value
caml_mpfr_set_prec (value op, value prec)
{
  CAMLparam2 (op, prec);
  mpfr_set_prec (MPFR_val (op), Int_val (prec));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_get_prec (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_int (mpfr_get_prec (MPFR_val2 (op))));
}

/************************/
/* Assignment Functions */
/************************/

CAMLprim value
caml_mpfr_set (value rop, value op, value rnd)
{
  CAMLparam3 (rop, op, rnd);
  CAMLreturn (
    val_ter (mpfr_set (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd))));
}

CAMLprim value
caml_mpfr_set_si (value rop, value op, value rnd)
{
  CAMLparam3 (rop, op, rnd);
  CAMLreturn (
    val_ter (mpfr_set_si (MPFR_val (rop), SI_val (op), rnd_val_opt (rnd))));
}

CAMLprim value
caml_mpfr_set_d (value rop, value op, value rnd)
{
  CAMLparam3 (rop, op, rnd);
  CAMLreturn (
    val_ter (mpfr_set_d (MPFR_val (rop), DBL_val (op), rnd_val_opt (rnd))));
}

CAMLprim value
caml_mpfr_strtofr (value rop, value op, value base, value rnd)
{
  CAMLparam4 (rop, op, base, rnd);
  int b;

  b = base == Val_none ? 0 : Some_val (base);
  base_in_range (Val_int (b));

  CAMLreturn (val_ter (mpfr_strtofr (MPFR_val (rop), String_val (op), NULL,
                                     UI_val (b), rnd_val_opt (rnd))));
}

CAMLprim value
caml_mpfr_set_nan (value rop)
{
  CAMLparam1 (rop);
  mpfr_set_nan (MPFR_val (rop));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_inf (value rop, value sign)
{
  CAMLparam2 (rop, sign);
  mpfr_set_inf (MPFR_val (rop), Int_val (sign));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_zero (value rop, value sign)
{
  CAMLparam2 (rop, sign);
  mpfr_set_zero (MPFR_val (rop), Int_val (sign));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_swap (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  mpfr_swap (MPFR_val (op1), MPFR_val (op2));
  CAMLreturn (Val_unit);
}

/***********************************************************************/
/* Combined Initialization and Assignment Functions (a functional way) */
/***********************************************************************/

CAMLprim value
caml_mpfr_init_set_mpfr (value prec, value rnd, value op)
{
  CAMLparam0 ();
  CAMLlocal2 (rop, ter);

  rop = caml_mpfr_init2_opt (prec);
  ter = caml_mpfr_set (rop, op, rnd);

  CAMLreturn (mpfr_float (rop, val_some (ter)));
}

CAMLprim value
caml_mpfr_init_set_si (value prec, value rnd, value op)
{
  CAMLparam0 ();
  CAMLlocal2 (rop, ter);

  rop = caml_mpfr_init2_opt (prec);
  ter = caml_mpfr_set_si (rop, op, rnd);

  CAMLreturn (mpfr_float (rop, val_some (ter)));
}

CAMLprim value
caml_mpfr_init_set_d (value prec, value rnd, value op)
{
  CAMLparam0 ();
  CAMLlocal2 (rop, ter);

  rop = caml_mpfr_init2_opt (prec);
  ter = caml_mpfr_set_d (rop, op, rnd);

  CAMLreturn (mpfr_float (rop, val_some (ter)));
}

CAMLprim value
caml_mpfr_init_set_str (value prec, value rnd, value base, value str)
{
  CAMLparam0 ();
  CAMLlocal2 (rop, ter);

  rop = caml_mpfr_init2_opt (prec);
  ter = caml_mpfr_strtofr (rop, str, base, rnd);

  CAMLreturn (mpfr_float (rop, val_some (ter)));
}

CAMLprim value
caml_mpfr_init_set_nan (value prec)
{
  CAMLparam0 ();
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2_opt (prec);
  caml_mpfr_set_nan (rop);

  CAMLreturn (mpfr_float (rop, Val_none));
}

CAMLprim value
caml_mpfr_init_set_inf (value prec, value sign)
{
  CAMLparam0 ();
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2_opt (prec);
  caml_mpfr_set_inf (rop, sign_val (sign));

  CAMLreturn (mpfr_float (rop, Val_none));
}

CAMLprim value
caml_mpfr_init_set_zero (value prec, value sign)
{
  CAMLparam0 ();
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2_opt (prec);
  caml_mpfr_set_zero (rop, sign_val (sign));

  CAMLreturn (mpfr_float (rop, Val_none));
}

/************************/
/* Conversion Functions */
/************************/

CAMLprim value
caml_mpfr_get_d (value rnd, value op)
{
  CAMLparam2 (op, rnd);
  CAMLreturn (
    caml_copy_double (mpfr_get_d (MPFR_val2 (op), rnd_val_opt (rnd))));
}

CAMLprim value
caml_mpfr_get_si (value rnd, value op)
{
  CAMLparam2 (op, rnd);
  CAMLreturn (Val_int (mpfr_get_si (MPFR_val2 (op), rnd_val_opt (rnd))));
}

CAMLprim value
caml_mpfr_get_d_2exp (value rnd, value op)
{
  CAMLparam2 (op, rnd);
  mpfr_exp_t exp;
  double dv;

  dv = mpfr_get_d_2exp (&exp, MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (caml_tuple2 (caml_copy_double (dv), Val_int (exp)));
}

CAMLprim value
caml_mpfr_frexp (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  mpfr_exp_t exp;
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_frexp (&exp, MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (caml_tuple2 (caml_tuple2 (rop, Val_int (ter)), Val_int (exp)));
}

CAMLprim value
caml_mpfr_get_str (value rnd, value base, value n, value op)
{
  CAMLparam4 (base, n, op, rnd);
  CAMLlocal2 (mantissa, exponent);
  char *ret, exp[21];
  mpfr_exp_t expptr;
  int b, size;

  b = base == Val_none ? 10 : UI_val (Some_val (base));
  size = n == Val_none ? 0 : UI_val (Some_val (n));
  base_in_range (Val_int (b));

  ret
    = mpfr_get_str (NULL, &expptr, b, size, MPFR_val2 (op), rnd_val_opt (rnd));
  sprintf (exp, "%ld", expptr);

  mantissa = caml_copy_string (ret);
  exponent = caml_copy_string (exp);

  mpfr_free_str (ret);

  CAMLreturn (caml_tuple2 (mantissa, exponent));
}

CAMLprim value
caml_mpfr_fits_sint_p (value rnd, value op)
{
  CAMLparam2 (op, rnd);
  CAMLreturn (Val_bool (mpfr_fits_sint_p (MPFR_val2 (op), rnd_val_opt (rnd))));
}

/******************************/
/* Basic Arithmetic Functions */
/******************************/

CAMLprim value
caml_mpfr_add (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_add);

CAMLprim value
caml_mpfr_add_si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_add_si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_add_d (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_add_d (MPFR_val (rop), MPFR_val2 (op1), DBL_val (op2),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sub (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_sub);

CAMLprim value
caml_mpfr_si_sub (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_si_sub (MPFR_val (rop), SI_val (op1), MPFR_val2 (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sub_si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_sub_si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_d_sub (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_d_sub (MPFR_val (rop), DBL_val (op1), MPFR_val2 (op2),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sub_d (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_sub_d (MPFR_val (rop), MPFR_val2 (op1), DBL_val (op2),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_mul (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_mul);

CAMLprim value
caml_mpfr_mul_si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_mul_si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_mul_d (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_mul_d (MPFR_val (rop), MPFR_val2 (op1), DBL_val (op2),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sqr (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_sqr (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_div (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_div);

CAMLprim value
caml_mpfr_si_div (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_si_div (MPFR_val (rop), SI_val (op1), MPFR_val2 (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_div_si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_div_si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_d_div (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_d_div (MPFR_val (rop), DBL_val (op1), MPFR_val2 (op2),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_div_d (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_div_d (MPFR_val (rop), MPFR_val2 (op1), DBL_val (op2),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sqrt (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_sqrt (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sqrt_ui (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (op) < 0)
    CAMLreturn (caml_mpfr_init_set_nan (prec));

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_sqrt_ui (MPFR_val (rop), UI_val (op), rnd_val_opt (rnd));
  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_rec_sqrt (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_rec_sqrt (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_cbrt (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_cbrt (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_rootn_ui (value rnd, value prec, value op, value k)
{
  CAMLparam3 (op, k, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (k) < 0)
    CAMLreturn (caml_mpfr_init_set_nan (prec));

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_rootn_ui (MPFR_val (rop), MPFR_val2 (op), UI_val (k),
                       rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_pow (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_pow);

CAMLprim value
caml_mpfr_pow_si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_pow_si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                     rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_neg (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_neg (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_abs (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_abs (MPFR_val (rop), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_dim (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_dim);

CAMLprim value
caml_mpfr_mul_2si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_mul_2si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                      rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_div_2si (value rnd, value prec, value op1, value op2)
{
  CAMLparam3 (op1, op2, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_div_2si (MPFR_val (rop), MPFR_val2 (op1), SI_val (op2),
                      rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

/************************/
/* Comparison Functions */
/************************/

CAMLprim value
caml_mpfr_cmp (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_int (mpfr_cmp (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_cmp_si (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_int (mpfr_cmp_si (MPFR_val2 (op1), SI_val (op2))));
}

CAMLprim value
caml_mpfr_cmp_d (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_int (mpfr_cmp_d (MPFR_val2 (op1), Double_val (op2))));
}

CAMLprim value
caml_mpfr_cmp_si_2exp (value op1, value op2, value exp)
{
  CAMLparam3 (op1, op2, exp);
  CAMLreturn (
    Val_int (mpfr_cmp_si_2exp (MPFR_val2 (op1), SI_val (op2), EXP_val (exp))));
}

CAMLprim value
caml_mpfr_cmpabs (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_int (mpfr_cmp_abs (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_nan_p (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_bool (mpfr_nan_p (MPFR_val2 (op))));
}

CAMLprim value
caml_mpfr_inf_p (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_bool (mpfr_inf_p (MPFR_val2 (op))));
}

CAMLprim value
caml_mpfr_number_p (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_bool (mpfr_number_p (MPFR_val2 (op))));
}

CAMLprim value
caml_mpfr_zero_p (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_bool (mpfr_zero_p (MPFR_val2 (op))));
}

CAMLprim value
caml_mpfr_regular_p (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_bool (mpfr_regular_p (MPFR_val2 (op))));
}

CAMLprim value
caml_mpfr_sgn (value op)
{
  CAMLparam1 (op);
  CAMLreturn (val_sign (mpfr_sgn (MPFR_val2 (op))));
}

CAMLprim value
caml_mpfr_greater_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_bool (mpfr_greater_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_greaterequal_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (
    Val_bool (mpfr_greaterequal_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_less_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_bool (mpfr_less_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_lessequal_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_bool (mpfr_lessequal_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_equal_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_bool (mpfr_equal_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_lessgreater_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_bool (mpfr_lessgreater_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

CAMLprim value
caml_mpfr_unordered_p (value op1, value op2)
{
  CAMLparam2 (op1, op2);
  CAMLreturn (Val_bool (mpfr_unordered_p (MPFR_val2 (op1), MPFR_val2 (op2))));
}

static int
custom_compare (value v1, value v2)
{
  return caml_mpfr_cmp (v1, v2);
}

/*********************/
/* Special Functions */
/*********************/

CAMLprim value
caml_mpfr_log (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_log);

CAMLprim value
caml_mpfr_log_ui (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (op) < 0)
    CAMLreturn (caml_mpfr_init_set_nan (prec));

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_log_ui (MPFR_val (rop), SI_val (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_log2 (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_log2);

CAMLprim value
caml_mpfr_log10 (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_log10);

CAMLprim value
caml_mpfr_exp (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_exp);

CAMLprim value
caml_mpfr_exp2 (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_exp2);

CAMLprim value
caml_mpfr_exp10 (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_exp10);

CAMLprim value
caml_mpfr_sin (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_sin);

CAMLprim value
caml_mpfr_cos (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_cos);

CAMLprim value
caml_mpfr_tan (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_tan);

CAMLprim value
caml_mpfr_sin_cos (value rnd, value sprec, value cprec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal2 (sop, cop);
  int ret;

  sop = caml_mpfr_init2_opt (sprec);
  cop = caml_mpfr_init2_opt (cprec);
  ret = mpfr_sin_cos (MPFR_val (sop), MPFR_val (cop), MPFR_val2 (op),
                      rnd_val_opt (rnd));

  if (ret == 0)
    CAMLreturn (caml_tuple2 (mpfr_float (sop, val_some (val_ter (ret))),
                             mpfr_float (cop, val_some (val_ter (ret)))));
  else
    CAMLreturn (
      caml_tuple2 (mpfr_float (sop, Val_none), mpfr_float (cop, Val_none)));
}

CAMLprim value
caml_mpfr_sec (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_sec);

CAMLprim value
caml_mpfr_csc (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_csc);

CAMLprim value
caml_mpfr_cot (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_cot);

CAMLprim value
caml_mpfr_acos (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_acos);

CAMLprim value
caml_mpfr_asin (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_asin);

CAMLprim value
caml_mpfr_atan (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_atan);

CAMLprim value
caml_mpfr_atan2 (value rnd, value prec, value x, value y)
{
  CAMLparam3 (x, y, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_atan2 (MPFR_val (rop), MPFR_val2 (x), MPFR_val2 (y),
                    rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_sinh (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_sinh);

CAMLprim value
caml_mpfr_cosh (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_cosh);

CAMLprim value
caml_mpfr_tanh (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_tanh);

CAMLprim value
caml_mpfr_sinh_cosh (value rnd, value sprec, value cprec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal2 (sop, cop);
  int ret;

  sop = caml_mpfr_init2_opt (sprec);
  cop = caml_mpfr_init2_opt (cprec);
  ret = mpfr_sinh_cosh (MPFR_val (sop), MPFR_val (cop), MPFR_val2 (op),
                        rnd_val_opt (rnd));

  if (ret == 0)
    CAMLreturn (caml_tuple2 (mpfr_float (sop, val_some (val_ter (ret))),
                             mpfr_float (cop, val_some (val_ter (ret)))));
  else
    CAMLreturn (
      caml_tuple2 (mpfr_float (sop, Val_none), mpfr_float (cop, Val_none)));
}

CAMLprim value
caml_mpfr_sech (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_sech);

CAMLprim value
caml_mpfr_csch (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_csch);

CAMLprim value
caml_mpfr_coth (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_coth);

CAMLprim value
caml_mpfr_acosh (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_acosh);

CAMLprim value
caml_mpfr_asinh (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_asinh);

CAMLprim value
caml_mpfr_atanh (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_atanh);

CAMLprim value
caml_mpfr_fac_ui (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (op) < 0)
    CAMLreturn (caml_mpfr_init_set_nan (prec));

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_fac_ui (MPFR_val (rop), SI_val (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_log1p (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_log1p);

CAMLprim value
caml_mpfr_expm1 (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_expm1);

CAMLprim value
caml_mpfr_eint (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_eint);

CAMLprim value
caml_mpfr_li2 (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_li2);

CAMLprim value
caml_mpfr_gamma (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_gamma);

CAMLprim value
caml_mpfr_gamma_inc (value rnd, value prec, value op1, value op2)
{
  CAMLparam4 (op1, op2, rnd, prec);
  CAMLlocal1 (rop);
  int ter;
  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_gamma_inc (MPFR_val (rop), MPFR_val2 (op1), MPFR_val2 (op2),
                        rnd_val_opt (rnd));
  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_lngamma (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_lngamma);

CAMLprim value
caml_mpfr_lgamma (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;
  int signp;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_lgamma (MPFR_val (rop), &signp, MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (
    caml_tuple2 (mpfr_float (rop, val_some (val_ter (ter))), val_sign (signp)));
}

CAMLprim value
caml_mpfr_digamma (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_digamma);

CAMLprim value
caml_mpfr_zeta (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_zeta);

/* CAMLprim value caml_mpfr_zeta_ui (value rnd, value prec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (op) < 0)
    caml_failwith(__FUNCTION__);

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_zeta_ui (MPFR_val (rop), SI_val (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
} */

CAMLprim value
caml_mpfr_beta (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_beta);

CAMLprim value
caml_mpfr_erf (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_erf);

CAMLprim value
caml_mpfr_erfc (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_erfc);

CAMLprim value
caml_mpfr_j0 (value rnd, value prec, value op) MPFR_REGULAR_FUNCTION1 (mpfr_j0);

CAMLprim value
caml_mpfr_j1 (value rnd, value prec, value op) MPFR_REGULAR_FUNCTION1 (mpfr_j1);

CAMLprim value
caml_mpfr_jn (value rnd, value prec, value n, value op)
{
  CAMLparam3 (n, op, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (n) < 0)
    CAMLreturn (caml_mpfr_init_set_nan (prec));

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_jn (MPFR_val (rop), SI_val (n), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_y0 (value rnd, value prec, value op) MPFR_REGULAR_FUNCTION1 (mpfr_y0);

CAMLprim value
caml_mpfr_y1 (value rnd, value prec, value op) MPFR_REGULAR_FUNCTION1 (mpfr_y1);

CAMLprim value
caml_mpfr_yn (value rnd, value prec, value n, value op)
{
  CAMLparam3 (n, op, rnd);
  CAMLlocal1 (rop);
  int ter;

  if (SI_val (n) < 0)
    CAMLreturn (caml_mpfr_init_set_nan (prec));

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_yn (MPFR_val (rop), SI_val (n), MPFR_val2 (op), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_fma (value rnd, value prec, value op1, value op2, value op3)
{
  CAMLparam4 (op1, op2, op3, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_fma (MPFR_val (rop), MPFR_val2 (op1), MPFR_val2 (op2),
                  MPFR_val2 (op3), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_fms (value rnd, value prec, value op1, value op2, value op3)
{
  CAMLparam4 (op1, op2, op3, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_fms (MPFR_val (rop), MPFR_val2 (op1), MPFR_val2 (op2),
                  MPFR_val2 (op3), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_fmma_native (value rnd, value prec, value op1, value op2, value op3,
                       value op4)
{
  CAMLparam5 (op1, op2, op3, op3, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_fmma (MPFR_val (rop), MPFR_val2 (op1), MPFR_val2 (op2),
                   MPFR_val2 (op3), MPFR_val2 (op4), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}
CAMLprim value
caml_mpfr_fmma_bytecode (value *argv, int argn)
{
  return caml_mpfr_fmma_native (argv[argn - 6], argv[argn - 5], argv[argn - 4],
                                argv[argn - 3], argv[argn - 2], argv[argn - 1]);
}

CAMLprim value
caml_mpfr_fmms_native (value rnd, value prec, value op1, value op2, value op3,
                       value op4)
{
  CAMLparam5 (op1, op2, op3, op3, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_fmms (MPFR_val (rop), MPFR_val2 (op1), MPFR_val2 (op2),
                   MPFR_val2 (op3), MPFR_val2 (op4), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}
CAMLprim value
caml_mpfr_fmms_bytecode (value *argv, int argn)
{
  return caml_mpfr_fmms_native (argv[argn - 6], argv[argn - 5], argv[argn - 4],
                                argv[argn - 3], argv[argn - 2], argv[argn - 1]);
}

CAMLprim value
caml_mpfr_agm (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_agm);

CAMLprim value
caml_mpfr_hypot (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_hypot);

CAMLprim value
caml_mpfr_ai (value rnd, value prec, value op) MPFR_REGULAR_FUNCTION1 (mpfr_ai);

CAMLprim value
caml_mpfr_const_log2 (value rnd, value prec)
{
  CAMLparam1 (rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (prec);
  ter = mpfr_const_log2 (MPFR_val (rop), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_const_pi (value rnd, value prec)
{
  CAMLparam1 (rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (prec);
  ter = mpfr_const_pi (MPFR_val (rop), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_const_euler (value rnd, value prec)
{
  CAMLparam1 (rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (prec);
  ter = mpfr_const_euler (MPFR_val (rop), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_const_catalan (value rnd, value prec)
{
  CAMLparam1 (rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (prec);
  ter = mpfr_const_catalan (MPFR_val (rop), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_free_cache (value unit)
{
  CAMLparam1 (unit);
  mpfr_free_cache ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_sum (value rnd, value prec, value tab)
{
  CAMLparam2 (tab, rnd);
  CAMLlocal2 (rop, tmp);
  int i, ter, size;

  rop = caml_mpfr_init2_opt (prec);

  // compute the size of tab
  size = 0;
  tmp = tab;
  while (Is_block (tab))
    {
      tab = Field (tab, 1);
      size++;
    }
  tab = tmp;

  mpfr_ptr ptab[size];

  for (i = 0; i < size; i++)
    {
      ptab[i] = MPFR_val2 (Field (tab, 0));
      tab = Field (tab, 1);
    }

  ter = mpfr_sum (MPFR_val (rop), ptab, size, rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

/*******************************************/
/* Integer and Remainder Related Functions */
/*******************************************/

CAMLprim value
caml_mpfr_rint (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_rint);

CAMLprim value
caml_mpfr_ceil (value prec, value op) MPFR_REGULAR_FUNCTION0 (mpfr_ceil);

CAMLprim value
caml_mpfr_floor (value prec, value op) MPFR_REGULAR_FUNCTION0 (mpfr_floor);

CAMLprim value
caml_mpfr_round (value prec, value op) MPFR_REGULAR_FUNCTION0 (mpfr_round);

CAMLprim value
caml_mpfr_roundeven (value prec, value op)
  MPFR_REGULAR_FUNCTION0 (mpfr_roundeven);

CAMLprim value
caml_mpfr_trunc (value prec, value op) MPFR_REGULAR_FUNCTION0 (mpfr_trunc);

CAMLprim value
caml_mpfr_rint_ceil (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_rint_ceil);

CAMLprim value
caml_mpfr_rint_floor (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_rint_floor);

CAMLprim value
caml_mpfr_rint_round (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_rint_round);

CAMLprim value
caml_mpfr_rint_roundeven (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_rint_roundeven);

CAMLprim value
caml_mpfr_rint_trunc (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_rint_trunc);

CAMLprim value
caml_mpfr_frac (value rnd, value prec, value op)
  MPFR_REGULAR_FUNCTION1 (mpfr_frac);

CAMLprim value
caml_mpfr_modf (value rnd, value iprec, value fprec, value op)
{
  CAMLparam2 (op, rnd);
  CAMLlocal2 (iop, fop);
  int ret;

  iop = caml_mpfr_init2_opt (iprec);
  fop = caml_mpfr_init2_opt (fprec);
  ret = mpfr_modf (MPFR_val (iop), MPFR_val (fop), MPFR_val2 (op),
                   rnd_val_opt (rnd));

  if (ret == 0)
    CAMLreturn (caml_tuple2 (mpfr_float (iop, val_some (val_ter (ret))),
                             mpfr_float (fop, val_some (val_ter (ret)))));
  else
    CAMLreturn (
      caml_tuple2 (mpfr_float (iop, Val_none), mpfr_float (fop, Val_none)));
}

CAMLprim value
caml_mpfr_fmod (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_fmod);

CAMLprim value
caml_mpfr_fmodquo (value rnd, value prec, value x, value y)
{
  CAMLparam3 (x, y, rnd);
  CAMLlocal1 (rop);
  int ter;
  long q = 0;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_fmodquo (MPFR_val (rop), &q, MPFR_val2 (x), MPFR_val2 (y),
                      rnd_val_opt (rnd));

  CAMLreturn (
    caml_tuple2 (mpfr_float (rop, val_some (val_ter (ter))), Val_int (q)));
}

CAMLprim value
caml_mpfr_remainder (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_remainder);

CAMLprim value
caml_mpfr_remquo (value rnd, value prec, value x, value y)
{
  CAMLparam3 (x, y, rnd);
  CAMLlocal1 (rop);
  int ter;
  long q = 0;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_remquo (MPFR_val (rop), &q, MPFR_val2 (x), MPFR_val2 (y),
                     rnd_val_opt (rnd));

  CAMLreturn (
    caml_tuple2 (mpfr_float (rop, val_some (val_ter (ter))), Val_int (q)));
}

CAMLprim value
caml_mpfr_integer_p (value op)
{
  CAMLparam1 (op);
  CAMLreturn (Val_bool (mpfr_integer_p (MPFR_val2 (op))));
}

/******************************/
/* Rounding Related Functions */
/******************************/

CAMLprim value
caml_mpfr_set_default_rounding_mode (value rnd)
{
  CAMLparam1 (rnd);
  mpfr_set_default_rounding_mode (rnd_val (rnd));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_get_default_rounding_mode ()
{
  CAMLparam0 ();
  int rnd;

  rnd = mpfr_get_default_rounding_mode ();

  CAMLreturn (Val_int (rnd));
}

CAMLprim value
caml_mpfr_prec_round (value rnd, value x, value prec)
{
  CAMLparam3 (rnd, x, prec);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (prec);
  ter = mpfr_set (MPFR_val (rop), MPFR_val2 (x), rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_can_round (value b, value err, value rnd1, value rnd2, value prec)
{
  CAMLparam5 (b, err, rnd1, rnd2, prec);
  int ret;

  ret = mpfr_can_round (MPFR_val2 (b), EXP_val (err), rnd_val (rnd1),
                        rnd_val (rnd2), PREC_val (prec));

  CAMLreturn (Val_bool (ret));
}

CAMLprim value
caml_mpfr_min_prec (value x)
{
  CAMLparam1 (x);
  CAMLreturn (Val_int (mpfr_min_prec (MPFR_val2 (x))));
}

/***************************/
/* Miscellaneous Functions */
/***************************/

CAMLprim value
caml_mpfr_nexttoward (value x, value y)
{
  CAMLparam2 (x, y);
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2 (caml_mpfr_get_prec (x));
  if (!mpfr_set (MPFR_val (rop), MPFR_val2 (x), MPFR_RNDN))
    mpfr_nexttoward (MPFR_val (rop), MPFR_val2 (y));
  else
    caml_failwith (__FUNCTION__);

  CAMLreturn (mpfr_float (rop, Val_none));
}

CAMLprim value
caml_mpfr_nextabove (value x)
{
  CAMLparam1 (x);
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2 (caml_mpfr_get_prec (x));
  if (!mpfr_set (MPFR_val (rop), MPFR_val2 (x), MPFR_RNDN))
    mpfr_nextabove (MPFR_val (rop));
  else
    caml_failwith (__FUNCTION__);

  CAMLreturn (mpfr_float (rop, Val_none));
}

CAMLprim value
caml_mpfr_nextbelow (value x)
{
  CAMLparam1 (x);
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2 (caml_mpfr_get_prec (x));
  if (!mpfr_set (MPFR_val (rop), MPFR_val2 (x), MPFR_RNDN))
    mpfr_nextbelow (MPFR_val (rop));
  else
    caml_failwith (__FUNCTION__);

  CAMLreturn (mpfr_float (rop, Val_none));
}

CAMLprim value
caml_mpfr_min (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_min);

CAMLprim value
caml_mpfr_max (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_max);

CAMLprim value
caml_mpfr_get_exp (value x)
{
  CAMLparam1 (x);
  CAMLreturn (Val_int (mpfr_get_exp (MPFR_val2 (x))));
}

CAMLprim value
caml_mpfr_set_exp (value x, value e)
{
  CAMLparam2 (x, e);
  CAMLlocal1 (rop);

  rop = caml_mpfr_init2 (caml_mpfr_get_prec (x));
  if (!mpfr_set (MPFR_val (rop), MPFR_val2 (x), MPFR_RNDN))
    {
      if (mpfr_set_exp (MPFR_val (rop), EXP_val (e)))
        caml_failwith (__FUNCTION__);
    }
  else
    caml_failwith (__FUNCTION__);

  CAMLreturn (mpfr_float (rop, Val_none));
}

CAMLprim value
caml_mpfr_signbit (value op)
{
  CAMLparam1 (op);
  int s;

  s = mpfr_signbit (MPFR_val2 (op));
  if (s == 0)
    CAMLreturn (Val_int (0));
  else
    CAMLreturn (Val_int (1));
}

CAMLprim value
caml_mpfr_setsign (value rnd, value prec, value op, value s)
{
  CAMLparam3 (op, s, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2_opt (prec);
  ter = mpfr_setsign (MPFR_val (rop), MPFR_val2 (op), SI_val (s),
                      rnd_val_opt (rnd));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_copysign (value rnd, value prec, value op1, value op2)
  MPFR_TWO_MPFR_OPERANDS (mpfr_copysign);

CAMLprim value
caml_mpfr_get_version ()
{
  CAMLparam0 ();
  CAMLreturn (caml_copy_string (mpfr_get_version ()));
}

/*******************************/
/* Exception Related Functions */
/*******************************/

CAMLprim value
caml_mpfr_get_emin ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (mpfr_get_emin ()));
}

CAMLprim value
caml_mpfr_get_emax ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (mpfr_get_emax ()));
}

CAMLprim value
caml_mpfr_set_emin (value exp)
{
  CAMLparam1 (exp);
  mpfr_set_emin (EXP_val (exp));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_emax (value exp)
{
  CAMLparam1 (exp);
  mpfr_set_emax (EXP_val (exp));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_get_emin_min ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (mpfr_get_emin_min ()));
}

CAMLprim value
caml_mpfr_get_emin_max ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (mpfr_get_emin_max ()));
}

CAMLprim value
caml_mpfr_get_emax_min ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (mpfr_get_emin_min ()));
}

CAMLprim value
caml_mpfr_get_emax_max ()
{
  CAMLparam0 ();
  CAMLreturn (Val_int (mpfr_get_emin_max ()));
}

CAMLprim value
caml_mpfr_check_range (value rnd, value x)
{
  CAMLparam2 (x, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (caml_mpfr_get_prec (x));
  if (!mpfr_set (MPFR_val (rop), MPFR_val2 (x), MPFR_RNDN))
    ter = mpfr_check_range (MPFR_val (rop), ter_val_opt (MPFR_val22 (x)),
                            rnd_val_opt (rnd));
  else
    caml_raise_with_string (*caml_named_value ("internal copy exception"),
                            "caml_mpfr_check_range");

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_subnormalize (value rnd, value x)
{
  CAMLparam2 (x, rnd);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (caml_mpfr_get_prec (x));
  if (!mpfr_set (MPFR_val (rop), MPFR_val2 (x), MPFR_RNDN))
    ter = mpfr_subnormalize (MPFR_val (rop), ter_val_opt (MPFR_val22 (x)),
                             rnd_val_opt (rnd));
  else
    caml_raise_with_string (*caml_named_value ("internal copy exception"),
                            "caml_mpfr_subnormalize");

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}

CAMLprim value
caml_mpfr_clear_underflow ()
{
  CAMLparam0 ();
  mpfr_clear_underflow ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_clear_overflow ()
{
  CAMLparam0 ();
  mpfr_clear_overflow ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_clear_divby0 ()
{
  CAMLparam0 ();
  mpfr_clear_divby0 ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_clear_nanflag ()
{
  CAMLparam0 ();
  mpfr_clear_nanflag ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_clear_inexflag ()
{
  CAMLparam0 ();
  mpfr_clear_inexflag ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_clear_erangeflag ()
{
  CAMLparam0 ();
  mpfr_clear_erangeflag ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_clear_flags ()
{
  CAMLparam0 ();
  mpfr_clear_flags ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_underflow ()
{
  CAMLparam0 ();
  mpfr_set_underflow ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_overflow ()
{
  CAMLparam0 ();
  mpfr_set_overflow ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_divby0 ()
{
  CAMLparam0 ();
  mpfr_set_divby0 ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_nanflag ()
{
  CAMLparam0 ();
  mpfr_set_nanflag ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_inexflag ()
{
  CAMLparam0 ();
  mpfr_set_inexflag ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_set_erangeflag ()
{
  CAMLparam0 ();
  mpfr_set_erangeflag ();
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_underflow_p ()
{
  CAMLparam0 ();
  CAMLreturn (Val_bool (mpfr_underflow_p ()));
}

CAMLprim value
caml_mpfr_overflow_p ()
{
  CAMLparam0 ();
  CAMLreturn (Val_bool (mpfr_overflow_p ()));
}

CAMLprim value
caml_mpfr_divby0_p ()
{
  CAMLparam0 ();
  CAMLreturn (Val_bool (mpfr_divby0_p ()));
}

CAMLprim value
caml_mpfr_nanflag_p ()
{
  CAMLparam0 ();
  CAMLreturn (Val_bool (mpfr_nanflag_p ()));
}

CAMLprim value
caml_mpfr_inexflag_p ()
{
  CAMLparam0 ();
  CAMLreturn (Val_bool (mpfr_inexflag_p ()));
}

CAMLprim value
caml_mpfr_erangeflag_p ()
{
  CAMLparam0 ();
  CAMLreturn (Val_bool (mpfr_erangeflag_p ()));
}

CAMLprim value
caml_mpfr_flags_clear (value flags)
{
  CAMLparam1 (flags);
  mpfr_flags_clear (flags_val (flags));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_flags_set (value flags)
{
  CAMLparam1 (flags);
  mpfr_flags_set (flags_val (flags));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_flags_test (value flags)
{
  CAMLparam1 (flags);
  int res = mpfr_flags_test (flags_val (flags));
  CAMLreturn (val_flags (res));
}

CAMLprim value
caml_mpfr_flags_save ()
{
  CAMLparam0 ();
  CAMLreturn (val_flags (mpfr_flags_save ()));
}

CAMLprim value
caml_mpfr_flags_restore (value flags1, value flags2)
{
  CAMLparam2 (flags1, flags2);
  mpfr_flags_restore (flags_val (flags1), flags_val (flags2));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_fpif_export (value file_descr, value op)
{
  CAMLparam2 (file_descr, op);
  mpfr_fpif_export (file_of_file_descr (file_descr, "w"), MPFR_val2 (op));
  CAMLreturn (Val_unit);
}

CAMLprim value
caml_mpfr_fpif_import (value file_descr)
{
  CAMLparam1 (file_descr);
  CAMLlocal1 (rop);
  int ter;

  rop = caml_mpfr_init2 (caml_mpfr_get_default_prec ());
  ter = mpfr_fpif_import (MPFR_val (rop), file_of_file_descr (file_descr, "r"));

  CAMLreturn (mpfr_float (rop, val_some (val_ter (ter))));
}
