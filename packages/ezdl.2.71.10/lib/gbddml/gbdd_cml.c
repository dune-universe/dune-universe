
extern "C" {
#include <stdio.h>
#include <assert.h>

#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/camlidlruntime.h"
#include "caml/mlvalues.h"
}

#include "gbdd.h"
//#define GBDD_CML_DBG
#define GBDD_CML_PARANO

#ifdef GBDD_CML_PARANO
#define CHECK_NOT_NULL(v) assert(GBdd_value(v) && !GBdd_value(v)->is_null())
#define CHECK_NOT_LEAF(v) assert(!GBdd_value(v)->is_leaf())
#else
#define CHECK_NOT_NULL(v)
#define CHECK_NOT_LEAF(v)
#endif

#define _30_BITS_CODE(x) (x & 0x1FFFFFFF)

typedef GBdd* PGBdd;

extern "C" {

/*------------------------------------------------------
Les références "GBdd*" sont stockées dans des
custom blocks caml
------------------------------------------------------*/



inline GBdd** GBdd_value_adr(value v){
	return ((GBdd**)(Data_custom_val(v)));
}
#ifdef GBDD_CML_PARANO
GBdd* GBdd_value(value v){
	PGBdd* p = (PGBdd*)Data_custom_val(v);
	GBdd* res = *p;
	return res;
}
#else
inline GBdd* GBdd_value(value v){
	return *((GBdd**)(Data_custom_val(v)));
}
#endif

//------------------------------
//table des opérations
//------------------------------
//finalization:
void finalize_gbdd_block(value v){
#ifdef GBDD_CML_DBG
printf("deleting %lx\n", v);
#endif
	GBdd* pb = GBdd_value(v);
	delete pb;
}
int compare_gbdd_block(value b1, value b2){
	CAMLparam2(b1,b2);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	int res = compare(*GBdd_value(b1), *GBdd_value(b2));
	//CAMLreturn(Val_int(res));
	CAMLreturn(res);
}
long hash_gbdd_block(value b){
	CAMLparam1(b);
CHECK_NOT_NULL(b);
	GBdd* pb = GBdd_value(b);
	long res = _30_BITS_CODE(pb->code());
	CAMLreturn(res);
}

static custom_operations GBDD_BLOCK_OPS = {
  "gbdd",               //char *identifier;
  finalize_gbdd_block,  //void (*finalize)(value v);
  compare_gbdd_block,   //int (*compare)(value v1, value v2);
  hash_gbdd_block,      //long (*hash)(value v);
  custom_serialize_default,
      //void (*serialize)(value v, unsigned long* wsize_32, unsigned long* wsize_64);
  custom_deserialize_default
      //unsigned long (*deserialize)(void* dst);
};

//----------------------------------
// Allocation d'un block
//----------------------------------
// on associe au bloc un pointeur
// sur un GBdd tout neuf ...
//----------------------------------

static value alloc_gdbb_block() {
	value res =  alloc_custom(
		&GBDD_BLOCK_OPS,
		sizeof(GBdd*),
		1, //used
		1000 //max
	);
	*(GBdd_value_adr(res)) = new GBdd();
	return res;
}

static void set_gbdd_value(value b, const GBdd& v){
	*(GBdd_value(b)) = v ;
}

/*-------------------------------------------------
Implémentation des primitives ocaml
---------------------------------------------------
N.B. on descend "un cran plus bas" que
l'interface C++ de GBdd, en s'autorisant la
manipulation des GBddValue 
-------------------------------------------------*/

value gbdd_cml_compare(value b1, value b2){
	CAMLparam2(b1,b2);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	int res = compare(*GBdd_value(b1), *GBdd_value(b2));
	CAMLreturn(Val_int(res));
}

value gbdd_cml_hash(value b){
	CAMLparam1(b);
CHECK_NOT_NULL(b);
	GBdd* pb = GBdd_value(b);
	long res = _30_BITS_CODE(pb->code());
printf("gbdd_cml_hash->%d\n", res);
	CAMLreturn(Val_long(res));
}

// INIT
//value gbdd_cml_init (value /*unit*/) {
//	CAMLparam0();
//	GBdd::init_module();
	//	fprintf(stderr, "gbdd_cml_init \n");
	//	fflush(stderr);
//	GBdd::set_verbose_mode();
//	CAMLreturn(Val_unit);
//}

// INIT
value gbdd_cml_init_with_psz_verb (
	value psz,
	value verb
) {
	CAMLparam2(psz,verb);
	GBdd::init_module(Int_val(psz));
	//	fprintf(stderr, "gbdd_cml_init \n");
	//	fflush(stderr);
	if (Bool_val(verb)) GBdd::set_verbose_mode();
	CAMLreturn(Val_unit);
}

// CONSTANTES
value gbdd_cml_true (value /*unit*/) {
	CAMLparam0();
	CAMLlocal1(res);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = true\n", res);
#endif
	set_gbdd_value(res, true_bdd());
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_false (value /*unit*/) {
	CAMLparam0();
	CAMLlocal1(res);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = false\n", res);
#endif
	set_gbdd_value(res, false_bdd());
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_null (value /*unit*/) {
	CAMLparam0();
	CAMLlocal1(res);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = null\n", res);
#endif
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

// ACCES AUX NOEUDS
value gbdd_cml_root_var(value b){
	CAMLparam1(b);
CHECK_NOT_NULL(b);
CHECK_NOT_LEAF(b);
	CAMLreturn(Val_int(GBdd_value(b)->root_var()));
}

value gbdd_cml_high_part(value b){
	CAMLparam1(b);
	CAMLlocal1(res);
CHECK_NOT_NULL(b);
CHECK_NOT_LEAF(b);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = hpart %lx [", res, b);
printf("  "); GBdd_value(b)->print_mons();
printf(" => "); GBdd_value(b)->high_part().print_mons();
printf("]\n");
#endif
	set_gbdd_value(res, GBdd_value(b)->high_part());
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_low_part(value b){
	CAMLparam1(b);
	CAMLlocal1(res);
CHECK_NOT_NULL(b);
CHECK_NOT_LEAF(b);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = low_part %lx\n", res, b);
#endif
	set_gbdd_value(res, GBdd_value(b)->low_part());
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

// TESTS
value gbdd_cml_is_leaf(value b){
	CAMLparam1(b);
CHECK_NOT_NULL(b);
#ifdef GBDD_CML_DBG
GBdd* p = GBdd_value(b);
int r = p->is_leaf();
printf("calling is_leaf %lx [",  b);
printf("  "); GBdd_value(b)->print_mons();
printf(" => %d ", r);
printf("]\n");
#endif
	CAMLreturn(Val_int(GBdd_value(b)->is_leaf()));
}

value gbdd_cml_is_true(value b){
	CAMLparam1(b);
CHECK_NOT_NULL(b);
	CAMLreturn(Val_int(GBdd_value(b)->is_true()));
}

value gbdd_cml_is_false(value b){
	CAMLparam1(b);
CHECK_NOT_NULL(b);
	CAMLreturn(Val_int(GBdd_value(b)->is_false()));
}

// IDENTITE/INVERSE
value gbdd_cml_idy (value index) {
	CAMLparam1(index);
	CAMLlocal1(res);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = idy %d\n", res, Int_val(index));
#endif
	set_gbdd_value(res, idy(Int_val(index)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_nidy  (value index) {
	CAMLparam1(index);
	CAMLlocal1(res);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = nidy %d\n", res, Int_val(index));
#endif
	set_gbdd_value(res, nidy(Int_val(index)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

// OPERATIONS BOOLEENNE CLASSIQUES
value gbdd_cml_not (value b) {
	CAMLparam1(b);
	CAMLlocal1(res);
CHECK_NOT_NULL(b);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = not %lx\n", res, b);
#endif
	set_gbdd_value(res, not(*GBdd_value(b)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_or (value b1, value b2){
	CAMLparam2(b1,b2);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = or %lx %lx\n", res, b1, b2);
#endif
	set_gbdd_value(res, or(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_and (value b1, value b2){
	CAMLparam2(b1,b2);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = and %lx %lx\n", res, b1, b2);
#endif
	set_gbdd_value(res, and(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_constrain (value b1, value b2){
   CAMLparam2(b1,b2);
   CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
   res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = constrain %lx %lx\n", res, b1, b2);
#endif
   set_gbdd_value(res, constrain(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
   CAMLreturn(res);
}

value gbdd_cml_restrict (value b1, value b2){
   CAMLparam2(b1,b2);
   CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
   res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = restrict %lx %lx\n", res, b1, b2);
#endif
   set_gbdd_value(res, restrict(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
   CAMLreturn(res);
}


value gbdd_cml_xor (value b1, value b2){
	CAMLparam2(b1,b2);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = xor %lx %lx\n", res, b1, b2);
#endif
	set_gbdd_value(res, xor(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_eq (value b1, value b2){
	CAMLparam2(b1,b2);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = eq %lx %lx\n", res, b1, b2);
#endif
	set_gbdd_value(res, eq(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}


value gbdd_cml_ite (value b1, value b2, value b3){
	CAMLparam3(b1,b2,b3);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
CHECK_NOT_NULL(b3);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = ite %lx %lx $lx\n", res, b1, b2, b3);
#endif
	set_gbdd_value(res,
		ite(*GBdd_value(b1), *GBdd_value(b2), *GBdd_value(b3))
	);
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

// SUPPORT / SIZE / CUBE
value gbdd_cml_cube (value b) {
	CAMLparam1(b);
	CAMLlocal1(res);
CHECK_NOT_NULL(b);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = cube %lx [", res, b);
printf("  "); GBdd_value(b)->print_mons();
printf(" => "); GBdd_value(b)->cube().print_mons();
printf("]\n");
#endif
	set_gbdd_value(res, GBdd_value(b)->cube());
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

value gbdd_cml_size (value b) {
	CAMLparam1(b);
CHECK_NOT_NULL(b);
#ifdef GBDD_CML_PARANO
	GBdd* p = GBdd_value(b);
	int sz = p->size();
	CAMLreturn(Val_int(sz));
#else
	CAMLreturn(Val_int(GBdd_value(b)->size()));
#endif
}

value gbdd_cml_supportsize (value b) {
	CAMLparam1(b);
CHECK_NOT_NULL(b);
	CAMLreturn(Val_int(GBdd_value(b)->nb_vars()));
}

// EXTRA
value gbdd_cml_print_mons (value b) {
	CAMLparam1(b);
CHECK_NOT_NULL(b);
	GBdd_value(b)->print_mons();
	CAMLreturn(Val_unit);
}

value gbdd_cml_exist (value b1, value b2){
	CAMLparam2(b1,b2);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = exist %lx %lx\n", res, b1, b2);
#endif
	set_gbdd_value(res, exist(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}
value gbdd_cml_forall (value b1, value b2){
	CAMLparam2(b1,b2);
	CAMLlocal1(res);
CHECK_NOT_NULL(b1);
CHECK_NOT_NULL(b2);
	res = alloc_gdbb_block();
#ifdef GBDD_CML_DBG
printf("creating %lx = forall %lx %lx\n", res, b1, b2);
#endif
	set_gbdd_value(res, forall(*GBdd_value(b1), *GBdd_value(b2)));
CHECK_NOT_NULL(res);
	CAMLreturn(res);
}

}
