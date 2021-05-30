#include <string.h>
#include <limits.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "unif01.h"
#include "bbattery.h"
#include "unif01_stubs.h"

int* int_array_from_ocaml(value brep) {
  CAMLparam1(brep);
  int len = Wosize_val(brep);
  int* rep = malloc(sizeof(int) * len);
  for (int i = 0; i < len; i++) {
    long rep_i = Long_val(Field(brep, i));
    if (rep_i < 0 || rep_i > INT_MAX)
      caml_invalid_argument("wrong value in repeat array");
    rep[i] = (int) rep_i;
  }
  CAMLreturnT(int*, rep);
}

/* ***************************** [ SmallCrush ] ***************************** */

value caml_bbattery_SmallCrush(value bgen) {
  CAMLparam1(bgen);
  unif01_Gen * gen = unif01_Gen_unbox(bgen);
  bbattery_SmallCrush(gen);
  CAMLreturn(Val_unit);
}

value caml_bbattery_SmallCrushFile(value filename) {
  CAMLparam1(filename);
  bbattery_SmallCrushFile(Bytes_val(filename));
  CAMLreturn(Val_unit);
}

value caml_bbattery_RepeatSmallCrush(value bgen, value brep) {
  CAMLparam2(bgen, brep);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  int * rep = int_array_from_ocaml(brep);
  bbattery_RepeatSmallCrush(gen, rep);
  free(rep);
  CAMLreturn(Val_unit);
}

/* ******************************* [ Crush ] ******************************** */

value caml_bbattery_Crush(value bgen) {
  CAMLparam1(bgen);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_Crush(gen);
  CAMLreturn(Val_unit);
}

value caml_bbattery_RepeatCrush(value bgen, value brep) {
  CAMLparam2(bgen, brep);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  int * rep = int_array_from_ocaml(brep);
  bbattery_RepeatCrush(gen, rep);
  free(rep);
  CAMLreturn(Val_unit);
}

/* ****************************** [ BigCrush ] ****************************** */

value caml_bbattery_BigCrush(value bgen) {
  CAMLparam1(bgen);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_BigCrush(gen);
  CAMLreturn(Val_unit);
}

value caml_bbattery_RepeatBigCrush(value bgen, value brep) {
  CAMLparam2(bgen, brep);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  int * rep = int_array_from_ocaml(brep);
  bbattery_RepeatBigCrush(gen, rep);
  free(rep);
  CAMLreturn(Val_unit);
}

/* ******************************* [ Rabbit ] ******************************* */

value caml_bbattery_Rabbit(value bgen, value nb) {
  CAMLparam2(bgen, nb);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_Rabbit (gen, Double_val(nb));
  CAMLreturn(Val_unit);
}

value caml_bbattery_RabbitFile(value filename, value nb) {
  CAMLparam2(filename, nb);
  bbattery_RabbitFile (Bytes_val(filename), Double_val(nb));
  CAMLreturn(Val_unit);
}

value caml_bbattery_RepeatRabbit(value bgen, value nb, value brep) {
  CAMLparam3(bgen, nb, brep);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  int * rep = int_array_from_ocaml(brep);
  bbattery_RepeatRabbit(gen, Double_val(nb), rep);
  free(rep);
  CAMLreturn(Val_unit);
}

/* ****************************** [ Alphabit ] ****************************** */

value caml_bbattery_Alphabit(value bgen, value nb, value r, value s) {
  CAMLparam4(bgen, nb, r, s);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_Alphabit (gen, Double_val(nb), Int_val(r), Int_val(s));
  CAMLreturn(Val_unit);
}

value caml_bbattery_AlphabitFile(value filename, value nb) {
  CAMLparam2(filename, nb);
  bbattery_AlphabitFile (Bytes_val(filename), Double_val(nb));
  CAMLreturn(Val_unit);
}

value caml_bbattery_RepeatAlphabit(value bgen, value nb, value r, value s, value brep) {
  CAMLparam5(bgen, nb, r, s, brep);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  int * rep = int_array_from_ocaml(brep);
  bbattery_RepeatAlphabit(gen, Double_val(nb), Int_val(r), Int_val(s), rep);
  free(rep);
  CAMLreturn(Val_unit);
}

value caml_bbattery_BlockAlphabit(value bgen, value nb, value r, value s) {
  CAMLparam4(bgen, nb, r, s);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_BlockAlphabit (gen, Double_val(nb), Int_val(r), Int_val(s));
  CAMLreturn(Val_unit);
}

value caml_bbattery_BlockAlphabitFile(value filename, value nb) {
  CAMLparam2(filename, nb);
  bbattery_BlockAlphabitFile (Bytes_val(filename), Double_val(nb));
  CAMLreturn(Val_unit);
}

value caml_bbattery_RepeatBlockAlphabit(value bgen, value nb, value r, value s, value brep, value w) {
  CAMLparam5(bgen, nb, r, s, brep); CAMLxparam1(w);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  int * rep = int_array_from_ocaml(brep);
  bbattery_RepeatBlockAlphabit(gen, Double_val(nb), Int_val(r), Int_val(s), rep, Int_val(w));
  free(rep);
  CAMLreturn(Val_unit);
}
value camlbytecode_bbattery_RepeatBlockAlphabit(value * argv, int argn) {
  return caml_bbattery_RepeatBlockAlphabit(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

/* ****************************** [ DIEHARD ] ******************************* */

value caml_bbattery_pseudoDIEHARD(value bgen) {
  CAMLparam1(bgen);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_pseudoDIEHARD(gen);
  CAMLreturn(Val_unit);
}

/* **************************** [ FISP 140 2 ] ****************************** */

value caml_bbattery_FIPS_140_2(value bgen) {
  CAMLparam1(bgen);
  unif01_Gen *gen = unif01_Gen_unbox(bgen);
  bbattery_FIPS_140_2(gen);
  CAMLreturn(Val_unit);
}

value caml_bbattery_FIPS_140_2File(value filename) {
  CAMLparam1(filename);
  bbattery_FIPS_140_2File(Bytes_val(filename));
  CAMLreturn(Val_unit);
}
