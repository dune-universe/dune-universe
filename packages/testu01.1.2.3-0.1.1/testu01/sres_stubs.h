#ifndef SRES_STUBS_H
#define SRES_STUBS_H 1

#define sres_Poisson_box(poisson, bpoisson, ops) ({bpoisson = caml_alloc_custom(&ops, sizeof(sres_Poisson*), 0, 1); memcpy(Data_custom_val(bpoisson), &poisson, sizeof(sres_Poisson*));})

#define sres_Poisson_unbox(bpoisson) (* (sres_Poisson**) Data_custom_val(bpoisson))

#endif
