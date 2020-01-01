/* Precision independent wrappers */

#define PLAN_VAL(v) (* (FFTW(plan) *) Data_custom_val(v))

#define ALLOC_PLAN()                                            \
  alloc_custom(&FFTW_OCAML(plan_ops), sizeof(FFTW(plan)),       \
               sizeof(FFTW(plan)), 500 * sizeof(FFTW(plan)))


static void FFTW_OCAML(plan_finalize)(value v)
{
  FFTW(destroy_plan)(PLAN_VAL(v));
}

static int FFTW_OCAML(plan_compare)(value v1, value v2)
{
  CAMLparam2(v1, v2);
  /* fftw_plan is a pointer, just compare those of v1 and v2 */
  if(PLAN_VAL(v1) < PLAN_VAL(v2)) CAMLreturn(Val_int(-1));
  else if(PLAN_VAL(v1) > PLAN_VAL(v2)) CAMLreturn(Val_int(1));
  else CAMLreturn(Val_int(0));
}

/* compare v1 v2 = 0 ==> hash(v1) = hash(v2) */
static long FFTW_OCAML(plan_hash)(value plan)
{
  CAMLparam1(plan);
  /* We do not know much about the plan internals, just return the
     pointer value as hash. */
  CAMLreturn((long) PLAN_VAL(plan));
}


static struct custom_operations FFTW_OCAML(plan_ops) = {
  "fftw3_plan", /* identifier for serialization and deserialization */
  &(FFTW_OCAML(plan_finalize)),
  &(FFTW_OCAML(plan_compare)),
  &(FFTW_OCAML(plan_hash)),
  custom_serialize_default,
  custom_deserialize_default
};


/* Executing plans
 ***********************************************************************/

CAMLexport value FFTW_OCAML(execute)(value vp)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute)(p);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}

CAMLexport value FFTW_OCAML(execute_dft)(value vp, value vi, value vo)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FFTW(complex) *i = Data_bigarray_val(vi);
  FFTW(complex) *o = Data_bigarray_val(vo);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_dft)(p, i, o);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}

CAMLexport value FFTW_OCAML(execute_split_dft)(value vp,
                                               value vri, value vii,
                                               value vro, value vio)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FLOAT *ri = Data_bigarray_val(vri);
  FLOAT *ii = Data_bigarray_val(vii);
  FLOAT *ro = Data_bigarray_val(vro);
  FLOAT *io = Data_bigarray_val(vio);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_split_dft)(p, ri, ii, ro, io);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}


CAMLexport value FFTW_OCAML(execute_dft_r2c)(value vp, value vi, value vo)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FLOAT *i = Data_bigarray_val(vi);
  FFTW(complex) *o = Data_bigarray_val(vo);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_dft_r2c)(p, i, o);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}

CAMLexport value FFTW_OCAML(execute_split_dft_r2c)(value vp,
                                                   value vi,
                                                   value vro, value vio)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FLOAT *i = Data_bigarray_val(vi);
  FLOAT *ro = Data_bigarray_val(vro);
  FLOAT *io = Data_bigarray_val(vio);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_split_dft_r2c)(p, i, ro, io);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}


CAMLexport value FFTW_OCAML(execute_dft_c2r)(value vp, value vi, value vo)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FFTW(complex) *i = Data_bigarray_val(vi);
  FLOAT *o = Data_bigarray_val(vo);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_dft_c2r)(p, i, o);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}

CAMLexport value FFTW_OCAML(execute_split_dft_c2r)(value vp,
                                                   value vri, value vii,
                                                   value vo)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FLOAT *ri = Data_bigarray_val(vri);
  FLOAT *ii = Data_bigarray_val(vii);
  FLOAT *o = Data_bigarray_val(vo);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_split_dft_c2r)(p, ri, ii, o);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}


CAMLexport value FFTW_OCAML(execute_r2r)(value vp, value vi, value vo)
{
  /* noalloc */
  FFTW(plan) p = PLAN_VAL(vp);
  FLOAT *i = Data_bigarray_val(vi);
  FLOAT *o = Data_bigarray_val(vo);

  FFTW_RAISE_NO_FFTWF;
  enter_blocking_section();  /* Allow other threads */
  FFTW(execute_r2r)(p, i, o);
  leave_blocking_section();  /* Disallow other threads */
  return(Val_unit);
}


/* Creating ND plans
 ***********************************************************************/

/* Copy the Caml arrays to the structure required by the guru interface.
 * Single transforms may be specified with [howmany_rank = 0]. */
#define MAKE_DIMS()                                             \
  const int rank = Wosize_val(vn);                              \
  const int howmany_rank = Wosize_val(vhowmany);                \
  FFTW(iodim) dims[MAX_NUM_DIMS], howmany_dims[MAX_NUM_DIMS];   \
  int k;                                                        \
                                                                \
  for(k = 0; k < rank; k++) {                                   \
    dims[k].n = Int_val(Field(vn, k));                          \
    dims[k].is = Int_val(Field(vistride, k));                   \
    dims[k].os = Int_val(Field(vostride, k));                   \
  }                                                             \
  for(k = 0; k < howmany_rank; k++) {                           \
    howmany_dims[k].n = Int_val(Field(vhowmany, k));            \
    howmany_dims[k].is = Int_val(Field(vhowmanyi, k));          \
    howmany_dims[k].os = Int_val(Field(vhowmanyo, k));          \
  }

CAMLexport
value FFTW_OCAML(guru_dft)(value vi, value vo,  value vsign, value vflags,
                           value vofsi, value vofso,
                           value vn, value vistride, value vostride,
                           value vhowmany, value vhowmanyi, value vhowmanyo)
{
  CAMLparam5(vi, vo, vsign, vflags, vofsi);
  CAMLxparam5(vofso, vn, vistride, vostride, vhowmany);
  CAMLxparam2(vhowmanyi, vhowmanyo);
  CAMLlocal1(plan);
  int sign = Int_val(vsign);
  unsigned flags = Int_val(vflags);
  FFTW(plan) p;
  FFTW(complex) *i = Data_bigarray_val(vi);
  FFTW(complex) *o = Data_bigarray_val(vo);
  MAKE_DIMS();
  
  FFTW_RAISE_NO_FFTWF;
  i += Int_val(vofsi);
  o += Int_val(vofso);
  enter_blocking_section();  /* Allow other threads */
  p = FFTW(plan_guru_dft)(rank, dims, howmany_rank, howmany_dims,
                          i, o, sign, flags);
  leave_blocking_section();  /* Disallow other threads */

  if (p == NULL) caml_failwith("Fftw3." PREC ".Genarray.dft");
  plan = ALLOC_PLAN();
  PLAN_VAL(plan) = p;
  CAMLreturn(plan);
}

CAMLexport
value FFTW_OCAML(guru_dft_bc)(value * argv, int argn)
{
  return FFTW_OCAML(guru_dft)(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5], argv[6], argv[7], argv[8], argv[9],
                              argv[10], argv[11]);
}


CAMLexport
value FFTW_OCAML(guru_r2c)(value vi, value vo, value vflags,
                           value vofsi, value vofso,
                           value vn, value vistride, value vostride,
                           value vhowmany, value vhowmanyi, value vhowmanyo)
{
  CAMLparam5(vi, vo, vflags, vofsi, vofso);
  CAMLxparam5(vn, vistride, vostride, vhowmany, vhowmanyi);
  CAMLxparam1(vhowmanyo);
  CAMLlocal1(plan);
  unsigned flags = Int_val(vflags);
  FFTW(plan) p;
  FLOAT *i = Data_bigarray_val(vi);
  FFTW(complex) *o = Data_bigarray_val(vo);
  MAKE_DIMS();
  
  FFTW_RAISE_NO_FFTWF;
  i += Int_val(vofsi);
  o += Int_val(vofso);
  enter_blocking_section();  /* Allow other threads */
  p = FFTW(plan_guru_dft_r2c)(rank, dims, howmany_rank, howmany_dims,
                              i, o, flags);
  leave_blocking_section();  /* Disallow other threads */

  if (p == NULL) caml_failwith("Fftw3." PREC ".Genarray.r2c");
  plan = ALLOC_PLAN();
  PLAN_VAL(plan) = p;
  CAMLreturn(plan);
}

CAMLexport
value FFTW_OCAML(guru_r2c_bc)(value * argv, int argn)
{
  return FFTW_OCAML(guru_r2c)(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5], argv[6], argv[7], argv[8], argv[9],
                              argv[10]);
}



CAMLexport
value FFTW_OCAML(guru_c2r)(value vi, value vo, value vflags,
                           value vofsi, value vofso,
                           value vn, value vistride, value vostride,
                           value vhowmany, value vhowmanyi, value vhowmanyo)
{
  CAMLparam5(vi, vo, vflags, vofsi, vofso);
  CAMLxparam5(vn, vistride, vostride, vhowmany, vhowmanyi);
  CAMLxparam1(vhowmanyo);
  CAMLlocal1(plan);
  unsigned flags = Int_val(vflags);
  FFTW(plan) p;
  FFTW(complex) *i = Data_bigarray_val(vi);
  FLOAT *o = Data_bigarray_val(vo);
  MAKE_DIMS();
  
  FFTW_RAISE_NO_FFTWF;
  i += Int_val(vofsi);
  o += Int_val(vofso);
  enter_blocking_section();  /* Allow other threads */
  p = FFTW(plan_guru_dft_c2r)(rank, dims, howmany_rank, howmany_dims,
                              i, o, flags);
  leave_blocking_section();  /* Disallow other threads */

  if (p == NULL) caml_failwith("Fftw3." PREC ".Genarray.c2r");
  plan = ALLOC_PLAN();
  PLAN_VAL(plan) = p;
  CAMLreturn(plan);
}

CAMLexport
value FFTW_OCAML(guru_c2r_bc)(value * argv, int argn)
{
  return FFTW_OCAML(guru_c2r)(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5], argv[6], argv[7], argv[8], argv[9],
                              argv[10]);
}


CAMLexport
value FFTW_OCAML(guru_r2r)(value vi, value vo, value vkind, value vflags,
                           value vofsi, value vofso,
                           value vn, value vistride, value vostride,
                           value vhowmany, value vhowmanyi, value vhowmanyo)
{
  CAMLparam5(vi, vo, vkind, vflags, vofsi);
  CAMLxparam5(vofso, vn, vistride, vostride, vhowmany);
  CAMLxparam2(vhowmanyi, vhowmanyo);
  CAMLlocal1(plan);
  unsigned flags = Int_val(vflags);
  FFTW(plan) p;
  FLOAT *i = Data_bigarray_val(vi);
  FLOAT *o = Data_bigarray_val(vo);
  FFTW(r2r_kind) kind[MAX_NUM_DIMS];
  MAKE_DIMS();

  FFTW_RAISE_NO_FFTWF;
  for(k = 0; k < rank; k++)
    /* OK because the order of "type r2r_kind" in fftw3SD.ml is in
     * sync with fftw3.h (see configure.ac). */
    kind[k] = Int_val(Field(vkind, k));

  i += Int_val(vofsi);
  o += Int_val(vofso);
  enter_blocking_section();  /* Allow other threads */
  p = FFTW(plan_guru_r2r)(rank, dims, howmany_rank, howmany_dims,
                          i, o, kind, flags);
  leave_blocking_section();  /* Disallow other threads */

  if (p == NULL) caml_failwith("Fftw3." PREC ".Genarray.r2r");
  plan = ALLOC_PLAN();
  PLAN_VAL(plan) = p;
  CAMLreturn(plan);
}

CAMLexport
value FFTW_OCAML(guru_r2r_bc)(value * argv, int argn)
{
  return FFTW_OCAML(guru_r2r)(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5], argv[6], argv[7], argv[8], argv[9],
                              argv[10], argv[11]);
}
