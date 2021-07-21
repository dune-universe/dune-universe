#include <stdlib.h>
#include <stdio.h>
//#include "tutu.h"
#include <stdarg.h>
//#include "toto.h"
#include <string.h>
#include "gbdd.h"

//extern "C" void printf(...);
//extern "C" void fprintf(...);
//extern "C" void vfprintf(...);

#ifdef DEBUG_GBDD
void CHECK_GBDD(int verb){
	int vm =  0;
	if(verb) vm = GBdd::set_verbose_mode();
	GBdd::manager.check_alive_list();
	if(verb) GBdd::set_verbose_mode(vm);
}
void DUMP_BDD(GBdd b){
	if(b == true_bdd()){
		printf("true_bdd\n");
	} else if(b == false_bdd()){
		printf("false_bdd\n");
	} else if(b.is_leaf()){
		printf("leaf{val=0x%X}\n", (unsigned int)b.leaf_info());
	} else if(b.is_bool()){
		GBddValue hv = b.high_part().value();
		GBddValue lv = b.low_part().value();
		printf("node{(%d)?", b.root_var());
		if(hv == GBdd::true_value()){
			printf(" 1 : ");
		} else if(hv == GBdd::false_value()){
			printf(" 0 : ");
		} else {
			printf(" %c 0x%x : ",
				GBdd::sign(hv)? '-' : '+',
				(unsigned int)GBdd::node(hv)
			);
		}
		if(lv == GBdd::true_value()){
			printf(" 1 }\n");
		} else if(lv == GBdd::false_value()){
			printf(" 0 }\n");
		} else {
			printf(" %c 0x%x }\n",
				GBdd::sign(lv)? '-' : '+',
				(unsigned int)GBdd::node(lv)
			);
			
		}
	} else {
		printf("generalized bdd node\n");
	}
}
void DUMP_LIST(GBddList* b){
	GBddList* l;
	for(l = b; l; l = l->tail()){
		DUMP_BDD(l->head());
	}
}
#endif

static void default_exit(){
	exit(1);
}

void (*GBdd::Exit)() = default_exit;

void GBdd::set_exit_proc(void (*e)()){
	GBdd::Exit = e;
}

void GBdd::IError(const char* fmt ...){
	va_list args;
	va_start(args,fmt);
	fprintf(stderr,"Internal Error :\n");
	vfprintf(stderr,fmt,args);
	fprintf(stderr,"\n");
	Exit();
}

int GBdd::VERBOSE = 0;

int GBdd::set_verbose_mode(int verb){
	int ovm = VERBOSE;
	VERBOSE = verb;
	return ovm;
}

int GBdd::Verbose(const char* fmt ...){
	va_list args;
	va_start(args,fmt);
	if(GBdd::VERBOSE){
		vfprintf(stderr,fmt,args);
		fflush(stderr);
	}
	return VERBOSE;
}

void GBdd::Warning(const char* fmt ...){
	va_list args;
	va_start(args,fmt);
	fprintf(stderr,"Gbdd Warning :\n");
	vfprintf(stderr,fmt,args);
	fprintf(stderr,"\n");
}

void GBdd::Error(const char* fmt ...){
	va_list args;
	va_start(args,fmt);
	fprintf(stderr,"Internal Error :\n");
	vfprintf(stderr,fmt,args);
	fprintf(stderr,"\n");
	Exit();
}

/*---------------------------------------------------------------
	Implementation de la classe GBdd (inlines)
---------------------------------------------------------------*/

/* acces aux informations de la GBddValue */
inline int GBdd::sign(GBddValue v){
	return (((GBddValue2BitString)v) & ((GBddValue2BitString)1))? 1 : 0;
}
inline int GBdd::is_bool(GBddValue v){
	return (((GBddValue2BitString)v) & ((GBddValue2BitString)2))? 1 : 0;
}
inline GBddNode* GBdd::node(GBddValue v){
	return (GBddNode*)((GBddValue2BitString)v & ~((GBddValue2BitString)3));
}
inline void GBdd::set_bool(GBddValue& v){
	v = (GBddValue)((GBddValue2BitString)v | (GBddValue2BitString)2);
}
inline void GBdd::chsign(GBddValue& v){
#ifdef DEBUG_GBDD
	if(is_bool(v)){
#endif
		v = (GBddValue)(
			((GBddValue2BitString)v | (GBddValue2BitString)1) & ~sign(v)
		);
#ifdef DEBUG_GBDD
	} else {
		Error("GBddValue::chsign called on non-boolean GBdd");
	}
#endif
}


/*---------------------------------------------------------------
	Implementation de la classe GBddNode (inlines)
---------------------------------------------------------------*/
inline int GBddNode::leaf_index(){
	return MAXINT;
}

inline int GBddNode::is_leaf(){
	return (index == leaf_index());
}

inline void GBddNode::clear_results(){
	direct_bin_op_code = GBDD_NO_OP;
	indirect_bin_op_code = GBDD_NO_OP;
	direct_op = indirect_op = (GBddValue)NULL;
	direct_res = indirect_res = (GBddValue)NULL;
	unary_res[0].reset();
	//ON N'EFFACE PAS LES INFOS D'ORDRE 1
	//unary_res[1].reset();
	size_mark = -1;
}


/*---------------------------------------------------------------
	Implementation de la classe GBddManager (inlines)
---------------------------------------------------------------*/
inline GBddValue GBddManager::app_not(GBddValue fv){
	GBddValue res = fv;
	GBdd::chsign(res);
	return res;
}


/*---------------------------------------------------------------
	Implementation de la classe GBdd
---------------------------------------------------------------*/

// Objets statiques de la classe GBdd :
GBdd GBdd::alives;
GBddManager GBdd::manager;
int GBdd::nb_op_idents;
int GBdd::nb_keys;
int GBdd::support_op_ident;
int GBdd::cube_op_ident;
int GBdd::shift_op_ident;
int GBdd::shift_op_index;
int GBdd::rename_op_ident;
int (*GBdd::rename_op_func)(int);
int GBdd::smooth_op_ident;
int GBdd::smooth_op_var;

//POUR LES STATS :
//le nombre total de noeuds alloues depuis
//l'initialisation (ne tiens pas compte du garbage collec)
int GBdd::total_nb_nodes(){
	return GBdd::manager.nb_allocated;
}

int GBdd::get_op_ident(){
	return nb_op_idents++;
}
int GBdd::get_mark_key(){
	return nb_keys++;
}

/* initialisation de la memoire */
void GBdd::init_module(int psz, void* t_info, void* f_info){
	if(! manager.init(psz,t_info,f_info)){
		Error("GBdd::init_module : can't initialize, memory not empty");
	};
}

void GBdd::init_module(int psz){
	if(! manager.init(psz)){
		Error("GBdd::init_module : can't initialize, memory not empty");
	};
}

void GBdd::init_module(void* t_info, void* f_info){
	if(! manager.init(t_info,f_info)){
		Error("GBdd::init_module : can't initialize, memory not empty");
	};
}

void GBdd::init_module(){
	if(! manager.init()){
		Error("GBdd::init_module : can't initialize, memory not empty");
	};
}

void GBdd::remove_alive(){
#ifdef DEBUG_GBDD
	if((next_alive->pred_alive != this)
			||
		(pred_alive->next_alive != this)
	)
	{
		IError("GBdd::remove_alive : bad alive list");
	}
#endif
	next_alive->pred_alive = pred_alive;
	pred_alive->next_alive = next_alive;

}
void GBdd::insert_alive(){
#ifdef DEBUG_GBDD
	//if(this != &alives)
		//manager.check_alive_list(this);
#endif

	next_alive = alives.next_alive;
	pred_alive = &alives;
	alives.next_alive = this;
	next_alive->pred_alive = this;	

}

GBdd& GBdd::operator=(const GBdd& f){
	it = f.value();
	return *this;
}

GBdd::GBdd(const GBdd& t){ 
	it = t.value(); 
	insert_alive(); 
}
GBdd::GBdd(){ 
	it = NULL;	
	insert_alive(); 
}

GBdd::~GBdd(){ 
	remove_alive(); 
}

GBdd::GBdd(GBddValue v){ 
	it = v;	
	insert_alive(); 
}

int GBddList::size() const {
	if(! this) return 0;
	GBddList* p;
	int res = 0;
	int mark = GBdd::get_op_ident();
	for(p = (GBddList*)this; p; p = p->tail()){
		GBdd s = p->head(); 
		if(! s.is_leaf()) {
			res += GBdd::node(s.value())->size(mark);
		}
	}
	return res;
}

int GBdd::size() const {
	if(is_leaf()) return 0;
	int mark = GBdd::get_op_ident();
	return GBdd::node(value())->size(mark);
}

int GBdd::unsigned_size() const {
	int mark = get_mark_key();
	return rec_unsigned_size(mark);
}
int GBdd::rec_unsigned_size(int mark) const {
	if(is_leaf()) return 0;
	if(get_mark(mark)) return 0; //déjà compté
	put_mark(mark, (void*)1);
	return (
		1 +
		low_part().rec_unsigned_size(mark) +
		high_part().rec_unsigned_size(mark)
	);
}


int GBdd::tree_size() const {
	if(is_leaf()) return 0;
	return GBdd::node(value())->tree_size();
}

GBdd GBdd::shift(int n) const {
	if((shift_op_ident < 0) || (shift_op_index != n)){
		shift_op_ident = GBdd::get_op_ident();
		shift_op_index = n;
	}
	return rec_shift(n);
}

GBdd GBdd::rec_shift(int n) const {
	if(is_leaf()) return *(this);
	GBdd res = get_res(0, shift_op_ident);
	if(res != null_bdd()){
		return res;
	} else {
		int new_arg = root_var() + n ;
		if(new_arg < 0){
			Error("GBdd::rec_shift : the shift produces negative index");
		}
		res = ite(new_arg,
			high_part().rec_shift(n),
			low_part().rec_shift(n)
		);	
		put_res(0, shift_op_ident, res);
		GBdd not_this = not(*this);
		not_this.put_res(0, shift_op_ident, not(res));
		return res;
	}
}

GBdd GBdd::rename(int (*f)(int)) const {
	if((rename_op_ident < 0) || (rename_op_func != f)){
		rename_op_ident = GBdd::get_op_ident();
		rename_op_func = f;
	}
	return rec_rename();
}

GBdd GBdd::rec_rename() const {
	if(is_leaf()) return *(this);
	GBdd res = get_res(0, rename_op_ident);
	if(res != null_bdd()){
		return res;
	} else {
		int new_arg = (*rename_op_func)(root_var());
		if(new_arg < 0){
			Error("GBdd::rec_rename : the rename produces negative index");
		}
		res = ite(new_arg,
			high_part().rec_rename(),
			low_part().rec_rename()
		);	
		put_res(0, rename_op_ident, res);
		GBdd not_this = not(*this);
		not_this.put_res(0, rename_op_ident, not(res));
		return res;
	}
}

void GBdd::dbg_print() const {
	int mark = GBdd::get_op_ident();
	printf("\n");
	GBdd::node(value())->dbg_print(mark);
}

void GBddNode::dbg_print(int mark){
	if(size_mark == mark) return;
	else {
		size_mark = mark;
		printf("%3d = ", dbg_index);
		if(index == GBddNode::leaf_index()){
        printf("[0x%x];", (long)info); 
			printf("\n");
		} else {
			printf("(%3d)?", index);
			if(GBdd::is_bool(high)){
				if(GBdd::sign(high)){
					printf(" +%3d :", GBdd::node(high)->dbg_index);
				} else {
					printf(" -%3d :", GBdd::node(high)->dbg_index);
				}
				if(GBdd::sign(low)){
					printf(" +%3d ;", GBdd::node(low)->dbg_index);
				} else {
					printf(" -%3d ;", GBdd::node(low)->dbg_index);
				}
			} else {
					printf(" %4d :", GBdd::node(high)->dbg_index);
					printf(" %4d ;", GBdd::node(low)->dbg_index);
			}
			printf("\n");
			GBdd::node(high)->dbg_print(mark);
			GBdd::node(low)->dbg_print(mark);
		}
	}
}


int GBddNode::size(int mark){
	if(size_mark == mark) return 0;
	else {
		size_mark = mark;
		return (is_leaf())? 0 : (
			((low)? GBdd::node(low)->size(mark) : 0)
			+
			((high)? GBdd::node(high)->size(mark) : 0)
			+ 1
		);
	}
}

int GBddNode::tree_size(){
	return (is_leaf())? (
		0
	):(
		1 +
		GBdd::node(low)->tree_size() +
		GBdd::node(high)->tree_size()
	);
}

void GBdd::put_mark(int mark_key, void* mark) const {
	if(is_leaf()) {
		Error("GBdd put_mark:: cannot be called on leafs");	
	}
	GBddNode* n = GBdd::node(value());
	if(n){
		if(sign(value())){
			n->direct_mark_key = mark_key;
			n->direct_mark = mark;
		} else {
			n->indirect_mark_key = mark_key;
			n->indirect_mark = mark;
		}
	}
}
void* GBdd::get_mark(int mark_key) const {
	if(is_leaf()) {
		Error("GBdd get_mark:: cannot be called on leafs");	
	}
	GBddNode* n = GBdd::node(value());
	if(n){
		if(sign(value())){
			return (n->direct_mark_key == mark_key)?
				n->direct_mark : NULL;
		} else {
			return (n->indirect_mark_key == mark_key)?
				n->indirect_mark : NULL;
		}
	}
	return NULL;
}

void GBdd::put_res(int index, int op_ident, const GBdd& res) const {
	if(is_leaf()) return; 
	GBddNode* n = GBdd::node(value());
	if(n){
		if(sign(value())){
			n->unary_res[index].set_direct(op_ident, res.value());
		} else {
			n->unary_res[index].set_indirect(op_ident, res.value());
		}
	}
}

GBdd GBdd::get_res(int index, int op_ident) const {
	if(is_leaf()) return null_bdd(); 
	GBddNode* n = GBdd::node(value());
	if(n){
		if(sign(value())){
			return GBdd(
				n->unary_res[index].get_direct(op_ident)
			);
		} else {
			return GBdd(
				n->unary_res[index].get_indirect(op_ident)
			);
		}
	} else {
		return null_bdd();
	}
}

// Decomposition de Shannon :
int GBdd::root_var() const {
	return GBdd::node(value())->index;
}

int GBdd::is_leaf() const {
	return GBdd::node(value())->is_leaf();
}

int GBdd::hash(int max) const {
	GBddValue2BitString i = (GBddValue2BitString)value();
	return (int)(
		((i & (GBddValue2BitString)0x3)? i : i >> 2)
		% 
		(GBddValue2BitString)max
	);
}

int GBdd::code() const {
  int i = (long)value();
	return (i >= 0)? i : -i;
}

int GBdd::is_bool() const {
	return is_bool(value());
}

int GBdd::is_true() const {
	return (value() == manager.internal_true);
}

int GBdd::is_false() const {
	return (value() == manager.internal_false);
}

int GBdd::is_null() const {
	return (value() == NULL);
}

GBdd GBdd::high_part() const {
	return GBdd(GBdd::manager.correct_high(value()));
}

GBdd GBdd::low_part() const {
	return GBdd(GBdd::manager.correct_low(value()));
}

GBdd GBdd::high_part(int x) const {
	return GBdd(GBdd::manager.set_true(value(), x));
}

GBdd GBdd::low_part(int x) const {
	return GBdd(GBdd::manager.set_false(value(), x));
}

int GBdd::depends(int x) const {
	return GBdd::manager.depends(value(), x);
}

GBdd GBdd::support() const {
	if(is_leaf()){
		return false_bdd();
	}
	GBdd res = get_res(0, support_op_ident);
	if(res != null_bdd()){
		return res;
	} else {
		res = idy(root_var());
		res = or(res, high_part().support());
		res = or(res, low_part().support());
		put_res(0, support_op_ident, res);
		if(GBdd::is_bool(value())){
			GBdd not_this = not(*this);
			not_this.put_res(0, support_op_ident, res);
		}
		return res;
	}
}


GBdd GBdd::cube() const {
	if(is_leaf()){
		return true_bdd();
	}
	GBdd res = get_res(0, cube_op_ident);
	if(res != null_bdd()){
		return res;
	} else {
		res = idy(root_var());
		res = and(res, high_part().cube());
		res = and(res, low_part().cube());
		put_res(0, cube_op_ident, res);
		if(GBdd::is_bool(value())){
			GBdd not_this = not(*this);
			not_this.put_res(0, cube_op_ident, res);
		}
		return res;
	}
}

int GBdd::max_var() const {
	GBdd sup = support();
	if(sup.is_leaf()){
		return -1;
	}
	while(! sup.low_part().is_leaf()){
		sup = sup.low_part();
	}
	return sup.root_var();
}

int GBdd::nb_vars() const {
	GBdd sup = support();
	int res = 0;
	while(! sup.is_leaf()){
		sup = sup.low_part();
		res++;
	}
	return res;
}

void* GBdd::eval(int (*f)(int)) const {
	if(is_leaf()){
		return leaf_info();
	} else {
		if( f(root_var())){
			return high_part().eval(f);
		} else {
			return low_part().eval(f);
		}
	}
}

int GBdd::eval_bool(int (*f)(int)) const {
	if(GBdd::is_bool(value())){
		return (eval(f) == GBdd::manager.true_info);
	} else {
		Error("GBdd eval_bool:: operand must be purely boolean gbdd");	
		return 0; //inutile
	}
}

void* GBdd::leaf_info() const {
	if(is_leaf()){
		return GBdd::manager.leaf_info(value());
	} else {
		return (void*)NULL;
	}
}

int operator==(const GBdd& t1, const GBdd& t2){
	return (t1.value() == t2.value());
}

int operator!=(const GBdd& t1, const GBdd& t2){
	return (t1.value() != t2.value());
}

int test_impl(const GBdd& t1, const GBdd& t2){
	if(GBdd::is_bool(t1.value()) && GBdd::is_bool(t2.value())){
		return GBdd::manager.test_impl(t1.value(), t2.value());
	} else {
		GBdd::Error("GBdd test_impl:: operands must be purely boolean gbdds");	
		return 0; //inutile
	}
}

int test_and(const GBdd& t1, const GBdd& t2){
	if(GBdd::is_bool(t1.value()) && GBdd::is_bool(t2.value())){
		return GBdd::manager.test_and(t1.value(), t2.value());
	} else {
		GBdd::Error("GBdd test_and:: operands must be purely boolean gbdds");	
		return 0; //inutile
	}
}

int compare(const GBdd& t1, const GBdd& t2){
	long res = ((long)t1.value()) - (long)(t2.value());
	return (res == 0)? 0 : ((res < 0)? -1 : 1);
}

GBdd leaf(void* i){
	return GBdd(GBdd::manager.get_leaf(i)); 
}

const GBdd& true_bdd(){
//GBdd true_bdd(){
	return *(GBdd::manager.true_pt);
}

const GBdd& false_bdd(){
//GBdd false_bdd(){
	return *(GBdd::manager.false_pt);
}

GBdd null_bdd(){
	return *(GBdd::manager.null_pt);
}

GBdd idy(int i){
	return GBdd(
		GBdd::manager.get_node(i, 
			*(GBdd::manager.true_pt), 
			*(GBdd::manager.false_pt)
		)
	);
}

GBdd nidy(int i){
	return GBdd(
		GBdd::manager.get_node(i, 
			*(GBdd::manager.false_pt),
			*(GBdd::manager.true_pt)
		)
	);
}

GBdd ite(const GBdd& c, const GBdd& t1, const GBdd& t2){
	if(t1 == t2) return t1;

	return GBdd(GBdd::manager.app_ite(c.value(), t1.value(), t2.value()));
}

GBdd ite(int v, const GBdd& t1, const GBdd& t2){
	return ite(idy(v), t1, t2);
}

/* Operateur negation : le + simple sur les GBdd */
GBdd not(const GBdd& t){
	if(GBdd::is_bool(t.value())){
		return GBdd(GBdd::manager.app_not(t.value()));
	} else {
		GBdd::Error("GBdd operator!:: operand must be purely boolean gbdd");	
		return null_bdd(); //inutile
	}
}

/* 
	Operateur et : 
*/
GBdd and(const GBdd& t1, const GBdd& t2){
	if(GBdd::is_bool(t1.value()) && GBdd::is_bool(t2.value())){
		return GBdd(GBdd::manager.app_and(t1.value(), t2.value()));
	} else {
		GBdd::Error("GBdd and:: operands must be purely boolean gbdds");	
		return null_bdd(); //inutile
	}
}

/* 
	Operateur ou : Pour l'instant, pas implemente de maniere efficace
		au niveau du manager, on se sert donc du et et de la negation
		(~gratuite). Ca evite les pb. de maintenance.
*/
GBdd or(const GBdd& t1, const GBdd& t2){
	GBdd result;
	if(GBdd::is_bool(t1.value()) && GBdd::is_bool(t2.value())){
		/*
		result = GBdd(
			GBdd::manager.app_not(
				GBdd::manager.app_and(
					GBdd::manager.app_not(t1.value()), 
					GBdd::manager.app_not(t2.value())
				)
			)
		);
		*/
		result = GBdd(
				GBdd::manager.app_and(
					GBdd::manager.app_not(t1.value()), 
					GBdd::manager.app_not(t2.value())
				)
		);
		result = GBdd(
			GBdd::manager.app_not(result.value())
		);
	} else {
		GBdd::Error("GBdd or:: operands must be purely boolean gbdds");	
	}
	return result;
}

/* 
	Operateur implique : Pour l'instant, pas implemente de maniere efficace
		au niveau du manager, on se sert donc du et et de la negation
		(~gratuite). Ca evite les pb. de maintenance.
		t1 => t2 eq. !(t1 && !t2)
*/
GBdd impl(const GBdd& t1, const GBdd& t2){
	GBdd result;
	if(GBdd::is_bool(t1.value()) && GBdd::is_bool(t2.value())){
		result = GBdd(
			GBdd::manager.app_not(
				GBdd::manager.app_and(
					t1.value(), 
					GBdd::manager.app_not(t2.value())
				)
			)
		);
	} else {
		GBdd::Error("GBdd impl:: operands must be purely boolean gbdds");	
	}
	return result;
}

/* 
	Operateur ou exclusif : 
*/
GBdd xor(const GBdd& t1, const GBdd& t2){
	GBdd result;
	if(GBdd::is_bool(t1.value()) && GBdd::is_bool(t2.value())){
		result = GBdd(GBdd::manager.app_xor(t1.value(), t2.value()));
	} else {
		GBdd::Error("GBdd impl:: operands must be purely boolean gbdds");	
	}
	return result;
}

/*
   Operateurs constrain et restrict :
*/
GBdd constrain(const GBdd& f, const GBdd& c){
	GBdd result;
	if(GBdd::is_bool(c.value())){
		GBddValue res = GBdd::manager.app_constrain(f.value(), c.value());
		if(res == NULL)
			GBdd::Error("GBdd constrain:: can't constrain by false gbdd");	
		result = GBdd(res);
	} else {
		GBdd::Error("GBdd constrain:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

GBdd constrain_bu(const GBdd& f, const GBdd& c){
	return constrain(f, c);
}

static GBdd simplify_td(const GBdd& lower, const GBdd& upper){
	GBdd res;
	if(lower == false_bdd()){
		res = lower;
	} else if(upper == true_bdd()){
		res = upper;
	} else if(lower.root_var() > upper.root_var()){
		res = simplify_td(
			lower,
			and(upper.high_part(), upper.low_part())
		);
	} else if(lower.root_var() < upper.root_var()){
		res = simplify_td(
			or(lower.high_part(), lower.low_part()),
			upper
		);
	} else {
		GBdd nlower = or(lower.high_part(), lower.low_part());
		GBdd nupper = and(upper.high_part(), upper.low_part());
		if(test_impl(nlower, nupper)){
			res = simplify_td(nlower, nupper);
		} else {
			res = ite(lower.root_var(),
				simplify_td(lower.high_part(), upper.high_part()),
				simplify_td(lower.low_part(), upper.low_part())
			);
		}
	}
	return res;
}

GBdd constrain_td(const GBdd& f, const GBdd& c){
	GBdd result;
	if(
		GBdd::is_bool(f.value())
		&&
		GBdd::is_bool(c.value())
	){
		if(c == false_bdd()){
			result = null_bdd();
		} else {
			GBdd lower = and(f,c);
			GBdd upper = or(f,not(c));
			result = simplify_td(lower, upper);
		}
	} else {
		GBdd::Error("GBdd constrain_td:: operands must be purely boolean gbdd");	
	}
	return result;
}

GBdd restrict(const GBdd& f, const GBdd& c){
	GBdd result;
	if(GBdd::is_bool(c.value())){
		GBddValue res = GBdd::manager.app_restrict(f.value(), c.value());
		if(res == NULL)
			GBdd::Error("GBdd restrict:: can't restrict by false gbdd");	
		result =  GBdd(res);
	} else {
		GBdd::Error("GBdd restrict:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

/*
	Operateur forall :
*/
GBdd forall(int x, const GBdd& t){
	GBdd result;
	if(GBdd::is_bool(t.value())){
		result = GBdd(
			GBdd::manager.app_forall(x, t.value())
		);
	} else {
		GBdd::Error("GBdd forall:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

GBdd GBdd::smooth(int x) const {
	if(! GBdd::is_bool(value()))
		GBdd::Error("GBdd smooth:: called on not boolean gbdd");	
		
	if((smooth_op_ident < 0) || (smooth_op_var != x)){
		smooth_op_ident = GBdd::get_op_ident();
		smooth_op_var = x;
	}
	return rec_smooth(x);
}

GBdd GBdd::rec_smooth(int x) const {
	if(is_leaf()) return *(this);
	GBdd res = get_res(0, smooth_op_ident);
	if(res != null_bdd()){
		return res;
	} else {
		if(x < root_var()){
			// x NE PEUT PLUS APPARAITRE :
			res = *(this);
		} else if(x == root_var()){
			// ELIMINATION DU x :
			res = or(
					high_part(), 
					low_part()
			);
		} else {
			// x PEUT ENCORE APPARAITRE DANS LES BRANCHES DE fv :
			GBdd b1 = high_part().rec_smooth(x);
			GBdd b0 = low_part().rec_smooth(x);
			res = ite(root_var(), b1, b0);
		}
		put_res(0, smooth_op_ident, res);
		return res;
	}
}

/*
	Operateur exist : deduit du forall
*/

GBdd exist(int x, const GBdd& t){
	GBdd result;
	if(GBdd::is_bool(t.value())){
		result = GBdd(
			GBdd::manager.app_not(
				GBdd::manager.app_forall(
					x, 
					GBdd::manager.app_not(t.value())
				)
			)
		);
	} else {
		GBdd::Error("GBdd exist:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

/*
	Operateur forall_x2max :
*/
GBdd forall_x2max(int x, const GBdd& t){
	GBdd result;
	if(GBdd::is_bool(t.value())){
		result =  GBdd(
			GBdd::manager.app_forall_sup(x, t.value())
		);
	} else {
		GBdd::Error("GBdd forall_x2max:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

/*
	Operateur exist_x2max : deduit du forall_sup
*/
GBdd exist_x2max(int x, const GBdd& t){
	GBdd result;
	if(GBdd::is_bool(t.value())){
		result = GBdd(
			GBdd::manager.app_not(
				GBdd::manager.app_forall_sup(
					x, 
					GBdd::manager.app_not(t.value())
				)
			)
		);
	} else {
		GBdd::Error("GBdd exist_x2max:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

/*
	Operateur forall :
*/

GBdd forall(const GBdd& vars, const GBdd& f){
	GBdd result;
	if(vars == true_bdd()) return f;
	if(GBdd::is_bool(f.value())){
		result = GBdd(
			GBdd::manager.app_forall_set(vars.value(), f.value())
		);
	} else {
		GBdd::Error("GBdd forall:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}

/*
	Operateur exist : deduit du forall
*/
GBdd exist(const GBdd& vars, const GBdd& f){
	GBdd result;
	if(GBdd::is_bool(f.value())){
		result = GBdd(
			GBdd::manager.app_not(
				GBdd::manager.app_forall_set(
					vars.value(), 
					GBdd::manager.app_not(f.value())
				)
			)
		);
	} else {
		GBdd::Error("GBdd exist:: 2d operand must be purely boolean gbdd");	
	}
	return result;
}


/*---------------------------------------------------------------
	Implementation de la classe GBddNode
---------------------------------------------------------------*/

void GBddNode::rec_gc_mark(int m){
	if(gc_mark == m) return;
	gc_mark = m;
	if(is_leaf()) return;

	//On garde les infos d'ordre 1 :

	GBddValue v = unary_res[1].direct_res;
	if(v) GBdd::node(v)->rec_gc_mark(m);
	v = unary_res[1].indirect_res;
	if(v) GBdd::node(v)->rec_gc_mark(m);

	GBdd::node(low)->rec_gc_mark(m);
	GBdd::node(high)->rec_gc_mark(m);
}

//Pour le debogage:
int GBddNode::dbg_counter = 0;

void GBddNode::init(void* i, GBddNode* n){
	info = i; low = high = NULL; index = leaf_index();
	gc_mark = 0; next = n;
	direct_bin_op_code = GBDD_NO_OP;
	indirect_bin_op_code = GBDD_NO_OP;
	direct_op = direct_res = indirect_op = indirect_res = NULL;
	direct_mark_key = indirect_mark_key = -1;
	direct_mark = indirect_mark = NULL;
	unary_res[0].reset();
	unary_res[1].reset();
	size_mark = -1;
   dbg_index = ++dbg_counter;
}

void GBddNode::init(int ix, GBddValue h, GBddValue l, GBddNode* n){
	info = NULL; low = l; high = h; index = ix;
	gc_mark = 0; next = n;
	direct_bin_op_code = GBDD_NO_OP;
	indirect_bin_op_code = GBDD_NO_OP;
	direct_op = direct_res = indirect_op = indirect_res = NULL;
	direct_mark_key = indirect_mark_key = -1;
	direct_mark = indirect_mark = NULL;
	unary_res[0].reset();
	unary_res[1].reset();
	size_mark = -1;
   dbg_index = ++dbg_counter;
}


/*---------------------------------------------------------------
	Implementation de la classe GBddMemory
---------------------------------------------------------------*/
GBddMemory::GBddMemory(){
	init();
} 

void GBddMemory::init(){
	first_page = (GBddMemoryItem*)NULL;
	nb_pages = 0;
	//POUR LES STATS:
	nb_allocated = 0;
	size = page_size_dflt;
	free_list = (GBddNode*)NULL;
}

GBddMemory::~GBddMemory(){
	delete_page(first_page);
}

int GBddMemory::alloc_page(){
	GBddMemoryItem* page = new GBddMemoryItem[size+1];

	if(page){
		//Chainage avec la liste deja existante :
		page[0].next_page = first_page;
		first_page = page;
	
		// Initialisation du chainage des cases libres :
		// A PARTIR DE LA CASE 1 :
		register GBddNode* ptr = &(page[1].node);
		register GBddNode* nxt = &(page[2].node);
		register int k = size;
		while(--k) (ptr++)->next = nxt++;
		ptr->next = (GBddNode*)NULL;
		free_list = &(page[1].node);
		nb_pages++;

		return 1;
	} else {
		return 0;
	}
}

void GBddMemory::delete_page(GBddMemoryItem* list){
	if(list) delete_page(list->next_page);
	delete[] list;
}

GBddNode* GBddMemory::alloc_node(){
	if(free_list){
		GBddNode* result = free_list;
		free_list = free_list->next;
		//POUR LES STATS :
		nb_allocated++;
		return result;
	} else {
		return (GBddNode*)NULL;
	}
}


void GBddMemory::unalloc_node(GBddNode* b){
	b->next = free_list;
	free_list = b;
}


/*---------------------------------------------------------------
	Implementation de la classe GBddManager
---------------------------------------------------------------*/


int GBddMemory::page_size_dflt = 10000;
char GBddManager::true_info_dflt[] = "true_bdd";
char GBddManager::false_info_dflt[] = "false_bdd";
int GBddManager::hash_size_dflt = 2557;
//int GBddManager::hash_size_dflt = 255257;
const char* GBddManager::errors[] = {
	"bad node index in get_node",
	"bad low operand in get_node" ,
	"bad high operand in get_node" ,
	"garbage collect has failed, memory size was ",
	"first operand of `ite' must be purely boolean"
};
char GBddManager::tabulation[] = "|   ";

int GBddManager::hash_code(int i, GBddValue l, GBddValue h){

	GBddValue2BitString bl = (GBddValue2BitString)l;
	GBddValue2BitString bh = (GBddValue2BitString)h;
	int code = (int)(
				(bl < bh) ? 
				((i + bl + bh) % hash_size)
				: 
				(hash_size - ((i + bl + bh) % hash_size) -1 )
			);
#ifdef DEBUG_GBDD
	if((code < 0) || (code >= hash_size)){
		GBdd::IError(
			"GBddManager::hash_code: hash index (%d) out of bounds (%d,%d)\n",
			code, 0, hash_size
		);
	}
#endif
	return code;
}

int GBddManager::hash_code(GBddValue ig){
	GBddValue2BitString i = (GBddValue2BitString)ig;
	return (int)(
		((i & (GBddValue2BitString)0x3)? i : (i >> 2)) % (GBddValue2BitString) hash_size
	);
}

static int GBDD_INIT_CPT = 0;

int GBddManager::init(int pagesz, void* t_info, void* f_info){

	GBddMemory::init();
	hash_size = hash_size_dflt;
	hash_tab = new GBddNode*[hash_size];
	memset((void*)hash_tab, '\0', sizeof(GBddNode*)*hash_size );


	/* Il y a deja au moins un noeud dans la memoire */
	if(GBddMemory::mem_size()){
		return 0;
	}
	GBddMemory::init(pagesz);

	true_info = t_info;
	false_info = f_info;


	/* le noeud "vrai" n'est pas dans la hash-tab */
	true_node.init(true_info, (GBddNode*)NULL);	
	internal_true = GBddValue(&true_node);
	GBdd::set_bool(internal_true);

	internal_false = internal_true;
	GBdd::chsign(internal_false);

	cur_gc_mark = 0;

	if(! GBDD_INIT_CPT ){
#ifdef DEBUG_GBDD
		//Forte presomption pour que ``GBdd::alives''
		//n'ait pas ete initialise automatiquement.
		GBdd::VERBOSE = 0;
		GBdd::alives.insert_alive();
#endif
		true_pt  = new GBdd(internal_true); 
		false_pt = new GBdd(internal_false);
		null_pt  =  new GBdd();
	}
	GBDD_INIT_CPT++;

	GBdd::nb_op_idents = 0;
	GBdd::nb_keys = 0;
	GBdd::support_op_ident = GBdd::get_op_ident();
	GBdd::cube_op_ident = GBdd::get_op_ident();
	GBdd::shift_op_ident = -1;
	GBdd::smooth_op_ident = -1;
	GBdd::rename_op_ident = -1;

	return 1;
}

int GBddManager::init(){
	return init(page_size_dflt, true_info_dflt, false_info_dflt);
}

int GBddManager::init(int pagesz){
	return init(pagesz, true_info_dflt, false_info_dflt);
}

int GBddManager::init(void* t_info, void* f_info){
	return init(page_size_dflt, t_info, f_info);
}

GBddManager::GBddManager():
GBddMemory(){

	init(page_size_dflt, true_info_dflt, false_info_dflt);
}

/*
	GBddManager::new_node() : s'il n'y a plus de place, 
		tente en premier lieu un garbage collect,
		si celui-ci echoue ( < a 10%), tente d'allouer un nouvelle page,
		si cette allocation echoue elle aussi : c'est foutu ....
*/

GBddNode* GBddManager::new_node(){

	GBddNode* result = alloc_node();

	if(! result){
		if (garbage_collect() >= 10) {
			result = alloc_node();
		} else {
			alloc_page();
			result = alloc_node();
		}
	} 
	if(!result) GBdd::Error("%s %d", errors[3], mem_size());
	return result;
}

GBddValue GBddManager::get_leaf(void* i){
	if(i == true_info) return internal_true;
	if(i == false_info) return internal_false; 
	int code = hash_code(i);
	GBddNode* node = hash_tab[code];
	while(node){
		if(node->is_leaf() && (node->info == i))
			return GBddValue(node);
		else
			node = node->next;
	}
	GBddNode* result = new_node();
	result->init(i, hash_tab[code]);
	hash_tab[code] = result;
	return GBddValue(result);
}

GBddValue GBddManager::correct_value(
	GBddNode* n, 
	unsigned is_bool, 
	unsigned sign
){
	GBddValue res = GBddValue(n);
	if(is_bool){
		GBdd::set_bool(res);
		if(sign) GBdd::chsign(res);
	}		
	return res;
}

GBddValue GBddManager::correct_low(GBddValue bv){
	GBddValue res = GBdd::node(bv)->low;
	if(GBdd::sign(bv)) GBdd::chsign(res);
	return res;
}

GBddValue GBddManager::correct_high(GBddValue bv){
	GBddValue res = GBdd::node(bv)->high;
	if(GBdd::sign(bv)) GBdd::chsign(res);
	return res;
}


GBddValue GBddManager::get_node(int ix, const GBdd& hp, const GBdd& lp){

#ifdef DEBUG_GBDD
	if(ix == GBddNode::leaf_index()) 
		fprintf(stderr,"GBdd : %s", errors[0] );
	if(GBdd::node(lp.value())->index <= ix) 
		fprintf(stderr,"GBdd : %s", errors[1] );
	if(GBdd::node(hp.value())->index <= ix) 
		fprintf(stderr,"GBdd : %s", errors[2] );
#endif

	if(lp == hp) return lp.value();

	GBddValue hp_val = hp.value();
	GBddValue lp_val = lp.value();

	//Correction booleenne :
	GBddValue2BitString result_sign = (GBddValue2BitString)0;
	GBddValue2BitString bool_result = 
		(GBdd::is_bool(hp_val) && GBdd::is_bool(lp_val));
	if(bool_result){
		if(GBdd::sign(hp_val)){
			GBdd::chsign(hp_val);	
			GBdd::chsign(lp_val);
			result_sign = (GBddValue2BitString)1; 
		}
	}

	int code = hash_code(ix, hp_val, lp_val);
	GBddNode* node = hash_tab[code];

	GBddNode* result = (GBddNode*)NULL;
	while (node) {
		if((node->index == ix)&&(node->low == lp_val)&&(node->high == hp_val)){
			result = node;
			node = (GBddNode*)NULL;
		} else
			node = node->next;
	}
	if(! result){
		result = new_node();
		result->init(ix, hp_val, lp_val, hash_tab[code]);
		hash_tab[code] = result;
	}

	//Correction booleenne :
	return correct_value(result, bool_result, result_sign);
}

void GBddManager::check_alive_list(){
	check_alive_list((GBdd*)NULL);
}

void GBddManager::check_alive_list(GBdd* ref){
	GBdd* c;
	c = & GBdd::alives;
	GBdd::Verbose("**********************\n");
	GBdd::Verbose("* Alives GBdd refs : *\n");
	GBdd::Verbose("**********************\n");
	do {
		if(ref && (ref == c)){
			GBdd::Warning("GBdd reference 0x%x already in alive list", ref);
		}
		GBdd::Verbose("ADDRESS: 0x%x ", c);
		GBdd::Verbose("(next: 0x%x pred: 0x%x)\n", c->next_alive, c->pred_alive);
		if(
			(c->pred_alive->next_alive != c)
				||
			(c->next_alive->pred_alive != c)
		){
			GBdd::Error("gbdd::bad alive list");
		}
		c = c->next_alive;
	} while(c != & GBdd::alives);
}

int GBdd::used_nodes(){
   return GBdd::manager.alive_size();
}

int GBddManager::alive_size(){
   int res = 0;
   int mark = GBdd::get_op_ident();

   GBdd* ptr = &GBdd::alives;
   //On casse le chainage pour aller + vite :
   ptr->pred_alive->next_alive = (GBdd*)NULL;

   GBddNode* cur_node;
   while((ptr = ptr->next_alive)){
      if((cur_node = GBdd::node(ptr->value())))
         res += cur_node->size(mark);
   }
   //On restaure le chainage pour aller + vite :
   GBdd::alives.pred_alive->next_alive = &GBdd::alives;

	return res;
}

int GBdd::garbage_collect(){
	return GBdd::manager.garbage_collect();
}

int GBddManager::garbage_collect(){

	if(!mem_size()) return 0;

#ifdef DEBUG_GBDD
	check_alive_list();
#endif

	GBdd::Verbose("GBdd garbage collect ... ");

	// Commence par parcourir la liste des references valides :

	cur_gc_mark++;
	GBdd* ptr = &GBdd::alives;
	//On casse le chainage pour aller + vite :
	ptr->pred_alive->next_alive = (GBdd*)NULL;

	GBddNode* cur_node;
	while((ptr = ptr->next_alive)){
		if((cur_node = GBdd::node(ptr->value())))
			cur_node->rec_gc_mark(cur_gc_mark);			
	}
	//On restaure le chainage pour aller + vite :
	GBdd::alives.pred_alive->next_alive = &GBdd::alives;

	// Puis on parcours les nodes connus (via la table de hash)
	int nb_free = 0;
	for(int k = 0; k < hash_size; k++){
		GBddNode** cur_list = &hash_tab[k];
		while(*cur_list){
			(*cur_list)->clear_results();
			if((*cur_list)->gc_mark == cur_gc_mark){
				cur_list = &((*cur_list)->next);
			} else { 
				// On le vire :
				GBddNode* to_unalloc = *cur_list;
				*cur_list = (*cur_list)->next;
				unalloc_node(to_unalloc);	
				nb_free++;
			}
		}
	}
	int per_cent = ((int)((nb_free*100)/mem_size()));
	GBdd::Verbose("done : %d%% (%d on %d)\n", 
		per_cent, 
		nb_free,
		mem_size()
	);
	return per_cent;
}

inline void GBddManager::tabule(FILE* f, int t){ 
	while(t--) fprintf(f, "%s", tabulation); 
}

void* GBddManager::leaf_info(GBddValue bv){
	if(bv == internal_true) return true_info;
	if(bv == internal_false) return false_info;
	return GBdd::node(bv)->info;
}

void GBddManager::print_ite
(FILE* f, GBddValue bv, void (*ppl)(FILE*,void*), int t){
	GBddNode* b = GBdd::node(bv); 
	if(b->is_leaf()){
		tabule(f, t);
      (*ppl)(f,leaf_info(bv));
   } else {
		tabule(f, t);
      fprintf(f,"if(%d)\n", b->index);
      print_ite(f, correct_high(bv), ppl, t+1);
		tabule(f, t);
      fprintf(f,"else\n");
      print_ite(f, correct_low(bv), ppl, t+1);
		tabule(f, t);
      fprintf(f,"fi\n");
   }
}

void GBddManager::print
(FILE* f, GBddValue bv, void (*ppl)(FILE*,void*), char* pfx, int hp, int lp){
	GBddNode* b = GBdd::node(bv); 
	if(b->is_leaf()){
		fprintf(f,"%s", pfx);
		if(hp || lp){
			fprintf(f,"|- ");	
		}
      (*ppl)(f,leaf_info(bv));
   } else {
		if(hp){
			strcat(pfx, "   ");
		} else if (lp){
			strcat(pfx, "|  ");
		}
      print(f, correct_high(bv), ppl, pfx, 1, 0);
		pfx[strlen(pfx)-3] = '\0';
		fprintf(f,"%s", pfx);
		if(hp || lp){
			fprintf(f,"|- ");
		}
      fprintf(f,"%d\n", b->index);

		if(hp){
			strcat(pfx, "|  ");
		} else if (lp){
			strcat(pfx, "   ");
		}
      print(f, correct_low(bv), ppl, pfx, 0, 1);
		pfx[strlen(pfx)-3] = '\0';
   }
}

int GBddManager::depends(GBddValue bv, int x){
	int index = GBdd::node(bv)->index;
	if(index > x){
		return 0;
	} else if(index == x) {
		return 1;
	} else {
		return  (
			depends(GBdd::node(bv)->low, x) 
				|| 
			depends(GBdd::node(bv)->high, x)
		);
	}
}

GBddValue GBddManager::set_true(GBddValue bv, int x){
	int index = GBdd::node(bv)->index;
	if(index > x){
		return bv;
	} else if(index == x) {
		return correct_high(bv);
	} else {
		GBdd hp = set_true(correct_high(bv), x);
		GBdd lp = set_true(correct_low(bv), x);
		return get_node(index, hp, lp);
	}	
}

GBddValue GBddManager::set_false(GBddValue bv, int x){
	int index = GBdd::node(bv)->index;
	if(index > x){
		return bv;
	} else if(index == x) {
		return correct_low(bv);
	} else {
		GBdd hp = set_false(correct_high(bv), x);
		GBdd lp = set_false(correct_low(bv), x);
		return get_node(index, hp, lp);
	}	
}

GBddValue GBddManager::app_forall(int x, GBddValue fv){

	int f_index = GBdd::node(fv)->index;
	GBdd b1, b2;

	if(x < f_index){
		// x NE PEUT PLUS APPARAITRE DANS fv :
		return fv;
	} else if(x == f_index){
		// ELIMINATION DU x :
		return app_and(
				correct_high(fv), 
				correct_low(fv)
		);
	} else {
		// x PEUT ENCORE APPARAITRE DANS LES BRANCHES DE fv :
		b1 = GBdd(app_forall(x, correct_high(fv)));
		b2 = GBdd(app_forall(x, correct_low(fv)));
		return get_node(f_index, b1, b2);
	}
}

GBddValue GBddManager::app_forall_sup(int x, GBddValue fv){

	int f_index = GBdd::node(fv)->index;
	GBdd b1, b2;

	if(x <= f_index){
		return (fv == internal_true)? internal_true : internal_false ;
	} else {
		// x PEUT ENCORE APPARAITRE DANS LES BRANCHES DE fv :
		GBdd b1 = GBdd(app_forall_sup(x, correct_high(fv)));
		GBdd b2 = GBdd(app_forall_sup(x, correct_low(fv)));
		return get_node(f_index, b1, b2);
	}
}

GBddValue GBddManager::app_forall_set(GBddValue set, GBddValue fv){

	int set_index = GBdd::node(set)->index;
	int fv_index = GBdd::node(fv)->index;

	while(set_index < fv_index){
		set = correct_high(set);
		set_index = GBdd::node(set)->index;
	}
	if(set == internal_true)
		return fv;
#ifdef DEBUG_GBDD
	if(GBdd::node(set)->is_leaf())
		GBdd::Error("GBddManager::app_forall_set: bad first operand");
#endif

	GBdd b1, b2;
	if(set_index > fv_index){
		b1 = GBdd(app_forall_set(set, correct_high(fv)));
		b2 = GBdd(app_forall_set(set, correct_low(fv)));
		return get_node(fv_index, b1, b2);
	} else { 
		//set_index == f_index
		b1 = GBdd(app_forall_set(correct_high(set), correct_high(fv)));
		b2 = GBdd(app_forall_set(correct_high(set), correct_low(fv)));
		return app_and(b1.value(), b2.value());
	}
}

GBddValue GBddManager::app_ite(GBddValue fv0, GBddValue fv1, GBddValue fv2){

	if(fv1 == fv2) return fv1;

	// Cas terminal :
	if(GBdd::node(fv0)->is_leaf()){
		if(fv0 == internal_true) return fv1;
		if(fv0 == internal_false) return fv2;
		GBdd::Error("%s", errors[4]);
	}

	// Premiere simplification :
	if(GBdd::node(fv0) == GBdd::node(fv1)){
		fv1 = (fv0 == fv1)? internal_true : internal_false ;
	}
	if(GBdd::node(fv0) == GBdd::node(fv2)){
		fv2 = (fv0 == fv2)? internal_false : internal_true ;
	}

	//Autre simplification :
	/*
	if(
		(fv1 == app_not(fv2))&&
		(GBdd::node(fv1)->index < GBdd::node(fv0)->index)
	){
		GBddValue tamp = fv0;
		fv0 = fv1;
		fv1 = tamp;
		fv2 = app_not(tamp);
	}
	*/

	// Cas semi-terminaux (branchement sur operations binaires) :
	if(GBdd::is_bool(fv1) && GBdd::is_bool(fv2)){
		if(fv1 == internal_true){
			return app_not(app_and(
				app_not(fv0),
				app_not(fv2)
			));
		} else if(fv1 == internal_false){
			return app_and(
				app_not(fv0),
				fv2
			);
		} else if(fv2 == internal_true){
			return app_not(app_and(
				fv0,
				app_not(fv1)
			));
		} else if(fv2 == internal_false){
			return app_and(
				fv0,
				fv1
			);
		} else if(GBdd::node(fv1) == GBdd::node(fv2)){
		/* i.e. fv0 ? not fv2 : fv2 */
			return app_xor(fv0, fv2);
		}
	}

	//Cas general ....
	int f0_index = GBdd::node(fv0)->index;
	int f1_index = GBdd::node(fv1)->index;
	int f2_index = GBdd::node(fv2)->index;

	int pivot = (f0_index < f1_index)? f0_index : f1_index;
	pivot = (pivot < f2_index)? pivot : f2_index;

	GBddValue f0_l, f0_h, f1_l, f1_h, f2_l, f2_h;
	if(f0_index == pivot){
		f0_l = correct_low(fv0);
		f0_h = correct_high(fv0);
	} else {
		f0_l = f0_h = fv0;
	}
	if(f1_index == pivot){
		f1_l = correct_low(fv1);
		f1_h = correct_high(fv1);
	} else {
		f1_l = f1_h = fv1;
	}
	if(f2_index == pivot){
		f2_l = correct_low(fv2);
		f2_h = correct_high(fv2);
	} else {
		f2_l = f2_h = fv2;
	}

	GBdd r_low = GBdd(app_ite(f0_l, f1_l, f2_l));
	GBdd r_high = GBdd(app_ite(f0_h, f1_h, f2_h));

	return get_node(pivot, r_high, r_low);
}


GBddValue GBddManager::app_constrain(GBddValue fv, GBddValue cv){

	if(cv == internal_true) return fv;
	if(cv == internal_false) return NULL;

	if(fv == cv) 
		return internal_true;

	if(GBdd::node(fv) == GBdd::node(cv))
		return internal_false;

	if(GBdd::node(fv)->is_leaf())
		return fv;

	GBddValue result = get_bin_op_res(GBDD_APP_CONSTRAIN,fv,cv);
	if(result)
		return result;

	int pivot;
	int f_index = GBdd::node(fv)->index;
	int c_index = GBdd::node(cv)->index;

	GBddValue f_l, f_h, c_l, c_h;
	if(f_index < c_index){
		pivot = f_index;
		f_h = correct_high(fv);
		f_l = correct_low(fv);
		c_h = c_l = cv;
	} else if(c_index < f_index){
		pivot = c_index;
		f_h = f_l = fv;
		c_h = correct_high(cv);
		c_l = correct_low(cv);
	} else {
		f_h = correct_high(fv);
		pivot = f_index;
		f_l = correct_low(fv);
		c_h = correct_high(cv);
		c_l = correct_low(cv);
	}

	GBdd r_low = GBdd(app_constrain(f_l, c_l));
	GBdd r_high = GBdd(app_constrain(f_h, c_h));

	if(r_low.value() == NULL){
		result = r_high.value();
	} else if(r_high.value() == NULL){
		result = r_low.value();
	} else {
		result = get_node(pivot, r_high, r_low);
	}

	put_bin_op_res(GBDD_APP_CONSTRAIN,fv,cv, result);

	return result;
}

GBddValue GBddManager::app_restrict(GBddValue fv, GBddValue cv){

	if(cv == internal_true) return fv;
	if(cv == internal_false) return NULL;

	if(fv == cv) 
		return internal_true;

	if(GBdd::node(fv) == GBdd::node(cv))
		return internal_false;

	if(GBdd::node(fv)->is_leaf())
		return fv;

	GBddValue result = get_bin_op_res(GBDD_APP_RESTRICT, fv, cv);
	if(result)
		return result;

	int pivot = -1; //bidon
	int f_index = GBdd::node(fv)->index;
	int c_index = GBdd::node(cv)->index;
	GBddValue f_l, f_h, c_l, c_h;
	f_l = f_h =  c_l =  c_h = (GBddValue)NULL;

	if(f_index < c_index){
		pivot = f_index;
		f_h = correct_high(fv);
		f_l = correct_low(fv);
		c_h = c_l = cv;
	} else if(c_index < f_index){
		GBdd new_cond = GBdd(
			app_not(
				app_and(
					app_not(correct_high(cv)),
					app_not(correct_low(cv))
				)
			)
		);
		result = app_restrict( fv, new_cond.value() );
	} else {
		f_h = correct_high(fv);
		pivot = f_index;
		f_l = correct_low(fv);
		c_h = correct_high(cv);
		c_l = correct_low(cv);
	}

	if(result == NULL){
		GBdd r_low = GBdd(app_restrict(f_l, c_l));
		GBdd r_high = GBdd(app_restrict(f_h, c_h));
	
		if(r_low.value() == NULL){
			result = r_high.value();
		} else if(r_high.value() == NULL){
			result = r_low.value();
		} else {
			result = get_node(pivot, r_high, r_low);
		}
	}

	put_bin_op_res(GBDD_APP_RESTRICT, fv, cv, result);
	return result;
}

/*
	Teste l'implication :
*/
int GBddManager::test_impl(GBddValue fv1, GBddValue fv2){
	if((fv1 == internal_false) || (fv2 == internal_true)) return 1;
	if((fv1 == internal_true) || (fv2 == internal_false)) return 0;
	if(fv1 == fv2) return 1;

	int f1_index = GBdd::node(fv1)->index;
	int f2_index = GBdd::node(fv2)->index;

	int pivot = (f1_index < f2_index)? f1_index : f2_index;

	GBddValue f1_l, f1_h, f2_l, f2_h;

	if(f1_index == pivot){
		f1_l = correct_low(fv1);
		f1_h = correct_high(fv1);
	} else {
		f1_l = f1_h = fv1;
	}
	if(f2_index == pivot){
		f2_l = correct_low(fv2);
		f2_h = correct_high(fv2);
	} else {
		f2_l = f2_h = fv2;
	}

	if(test_impl(f1_l, f2_l)){
		return test_impl(f1_h, f2_h);
	} else {
		return 0;
	}
}

int GBddManager::test_and(GBddValue fv1, GBddValue fv2){
	if((fv1 == internal_false) || (fv2 == internal_false)) return 0;
	if((fv1 == internal_true) || (fv2 == internal_true)) return 1;
	if(fv1 == fv2) return 1;
	if(GBdd::node(fv1) == GBdd::node(fv2))
		return 0;

	int f1_index = GBdd::node(fv1)->index;
	int f2_index = GBdd::node(fv2)->index;

	int pivot = (f1_index < f2_index)? f1_index : f2_index;

	GBddValue f1_l, f1_h, f2_l, f2_h;

	if(f1_index == pivot){
		f1_l = correct_low(fv1);
		f1_h = correct_high(fv1);
	} else {
		f1_l = f1_h = fv1;
	}
	if(f2_index == pivot){
		f2_l = correct_low(fv2);
		f2_h = correct_high(fv2);
	} else {
		f2_l = f2_h = fv2;
	}

	if(test_and(f1_l, f2_l)){
		return 1;
	} else {
		return test_and(f1_h, f2_h);
	}
}

GBddValue GBddManager::get_bin_op_res(
	GBddOpCode code,
	GBddValue fv1,
	GBddValue fv2
){

	if(GBdd::sign(fv1)){
		if(
			(GBdd::node(fv1)->indirect_bin_op_code == code)
		 	&&
			(GBdd::node(fv1)->indirect_op == fv2)
		){
			return GBdd::node(fv1)->indirect_res;
		}
	} else {
		if(
			(GBdd::node(fv1)->direct_bin_op_code == code)
		 	&&
			(GBdd::node(fv1)->direct_op == fv2)
		){
			return GBdd::node(fv1)->direct_res;
		}
	}
	return NULL;
}

void GBddManager::put_bin_op_res(
	GBddOpCode code,
	GBddValue fv1,
	GBddValue fv2,
	GBddValue res
){
	if(GBdd::sign(fv1)){
		GBdd::node(fv1)->indirect_bin_op_code = code;
		GBdd::node(fv1)->indirect_op = fv2;
		GBdd::node(fv1)->indirect_res = res;
	} else {
		GBdd::node(fv1)->direct_bin_op_code = code;
		GBdd::node(fv1)->direct_op = fv2;
		GBdd::node(fv1)->direct_res = res;
	}
}

GBddValue GBddManager::app_and(GBddValue fv1, GBddValue fv2){

	if((fv1 == internal_true) || (fv2 == internal_false)) return fv2;
	if((fv2 == internal_true) || (fv1 == internal_false)) return fv1;

	if(fv1 == fv2) 
		return fv1;
	if(GBdd::node(fv1) == GBdd::node(fv2))
		return internal_false;
 
	/* optimisation : recherche du resultat */
	GBddValue result;
	result = get_bin_op_res(GBDD_APP_AND, fv1, fv2);
	if(result)
		return result;
	result = get_bin_op_res(GBDD_APP_AND, fv2, fv1);
	if(result)
		return result;


	int f1_index = GBdd::node(fv1)->index;
	int f2_index = GBdd::node(fv2)->index;

	int pivot = (f1_index < f2_index)? f1_index : f2_index;

	GBddValue f1_l, f1_h, f2_l, f2_h;

	if(f1_index == pivot){
		f1_l = correct_low(fv1);
		f1_h = correct_high(fv1);
	} else {
		f1_l = f1_h = fv1;
	}
	if(f2_index == pivot){
		f2_l = correct_low(fv2);
		f2_h = correct_high(fv2);
	} else {
		f2_l = f2_h = fv2;
	}

	GBdd r_low = GBdd(app_and(f1_l, f2_l));
	GBdd r_high = GBdd(app_and(f1_h, f2_h));

	result = get_node(pivot, r_high, r_low);

	/* optimisation : stockage du resultat */
	put_bin_op_res(GBDD_APP_AND, fv1, fv2, result);
	put_bin_op_res(GBDD_APP_AND, fv2, fv1, result);

	return result;

}

GBddValue GBddManager::app_xor(GBddValue fv1, GBddValue fv2){

	if(fv1 == internal_true) return app_not(fv2);
	if(fv1 == internal_false) return fv2;
	if(fv2 == internal_true) return app_not(fv1);
	if(fv2 == internal_false) return fv1;

	if(fv1 == fv2) 
		return internal_false;
	if(GBdd::node(fv1) == GBdd::node(fv2))
		return internal_true;

	/* optimisation : recherche du resultat */
	GBddValue result;
	result = get_bin_op_res(GBDD_APP_XOR, fv1, fv2);
	if(result)
		return result;
	result = get_bin_op_res(GBDD_APP_XOR, fv2, fv1);
	if(result)
		return result;


	int f1_index = GBdd::node(fv1)->index;
	int f2_index = GBdd::node(fv2)->index;

	int pivot = (f1_index < f2_index)? f1_index : f2_index;

	GBddValue f1_l, f1_h, f2_l, f2_h;

	if(f1_index == pivot){
		f1_l = correct_low(fv1);
		f1_h = correct_high(fv1);
	} else {
		f1_l = f1_h = fv1;
	}
	if(f2_index == pivot){
		f2_l = correct_low(fv2);
		f2_h = correct_high(fv2);
	} else {
		f2_l = f2_h = fv2;
	}

	GBdd r_low = GBdd(app_xor(f1_l, f2_l));
	GBdd r_high = GBdd(app_xor(f1_h, f2_h));

	result = get_node(pivot, r_high, r_low);

	/* optimisation : stockage du resultat */
	put_bin_op_res(GBDD_APP_XOR, fv1, fv2, result);
	put_bin_op_res(GBDD_APP_XOR, fv2, fv1, result);

	return result;
}


/*
AFFICHAGE DES BDDS
*/


void GBdd::print_ite(FILE* f, void (*ppl)(FILE*,void*)) const {
	if(GBdd::node(value()))	
		manager.print_ite(f, value(), ppl, 0);
	else 
		fprintf(f,"NULL\n");
}

void GBdd::print(FILE* f, void (*ppl)(FILE*,void*)) const {
	char buff[1024];
	buff[0] = '\0';
	if(GBdd::node(value()))	
		manager.print(f, value(), ppl, buff, 0, 0);
	else 
		fprintf(f,"NULL\n");
}

void GBddPrintLeafDflt(FILE* f, void* v){
	if(v == true_bdd().leaf_info())
		fprintf(f,"true\n");	
	else if(v == false_bdd().leaf_info())
		fprintf(f,"false\n");	
	else fprintf(f,"0x%x\n", (int)(GBddValue2BitString)v);	
}

void GBdd::print_ite() const {
	print_ite(stdout, GBddPrintLeafDflt);
}

void GBdd::print() const {
	print(stdout, GBddPrintLeafDflt);
}

static char GbddArgBuff[32];

static const char* GBddGetArgDflt(int i){
	sprintf(GbddArgBuff,"X%d", i);
	return &GbddArgBuff[0];
}

int GBdd::first_mon;
const char* GBdd::trueop = "true";
const char* GBdd::falseop = "false";
const char* GBdd::notop = "!";
const char* GBdd::andop = ".";
const char* GBdd::orop = " +\n";
const char* GBdd::idyop = "";
const char* GBdd::openpar = "";
const char* GBdd::closepar = "";

void GBdd::set_mons_strings(
	const char* ts,
	const char* fs,
	const char* ns,
	const char* as,
	const char* os,
	const char* iop,
	const char* opar,
	const char* cpar
){
	trueop = ts;
	falseop = fs;
	notop = ns;
	andop = as;
	orop = os;
	idyop = iop;
	openpar = opar;
	closepar = cpar;
}


void GBdd::print_mons(
	FILE* f, 
	const char* (*get_arg)(int)
) const {
	if(! is_bool()){
	}
	if(*this == null_bdd()){
		fprintf(f,"null");
	} else if(*this == true_bdd()){
		fprintf(f,"%s", trueop);
	} else if(*this == false_bdd()){
		fprintf(f,"%s", falseop);
	} else {
		char buffer[1024];
		buffer[0] = '\0';
		first_mon = 1;
		rec_print_mons( f, get_arg, &buffer[0] );
	}
	fflush(stdout);
	//fprintf(f,"\n");
}

void GBdd::rec_print_mons(
	FILE* f, 
	const char* (*get_arg)(int), 
	char* prefix
) const {

	if(*this == true_bdd()){
		if(! first_mon){
			fprintf(f, "%s", orop);
		}
		first_mon = 0;
		fprintf(f, "%s%s%s", openpar, prefix, closepar);
			
	} else if (*this == false_bdd()){
	} else {
		int k = root_var();
		int ln = strlen(prefix);
		GBdd cons = and(high_part(), low_part());
		GBdd nh = restrict( high_part(), not(cons));
		GBdd nl = restrict( low_part(), not(cons));

		if ((nh == high_part()) && (nl == low_part())){
			//Inutile de traiter le consensus => ce ne fera
			//que compliquer l'expression !
			nh = high_part();
			nl = low_part();
			cons = false_bdd();
		}

		if(nh.is_leaf()){
			sprintf(&prefix[ln], "%s%s", idyop, get_arg(k));
		} else {
			sprintf(&prefix[ln], "%s%s%s", idyop, get_arg(k), andop);
		}
		nh.rec_print_mons(f, get_arg, prefix);

		prefix[ln] = '\0';
		if(nl.is_leaf()){
			sprintf(&prefix[ln], "%s%s", notop, get_arg(k));
		} else {
			sprintf(&prefix[ln], "%s%s%s", notop, get_arg(k), andop);
		}
		nl.rec_print_mons(f, get_arg, prefix);
		
		prefix[ln] = '\0';
		cons.rec_print_mons(f, get_arg, prefix);

	}
}

static const char** GbddArgTab;

static const char* GBddGetArgTab(int i){
	return GbddArgTab[i];
}

void GBdd::print_mons(FILE* f, const char** args) const {
	GbddArgTab = args;
	print_mons(f, GBddGetArgTab);
}

void GBdd::print_mons() const {
	print_mons(stdout, GBddGetArgDflt);
}
void GBdd::print_mons_ln() const {
	print_mons(stdout, GBddGetArgDflt);
	printf("\n");
	fflush(stdout);
}
void GBdd::print_mons_ln(FILE* f, const char** args) const {
	GbddArgTab = args;
	print_mons(f, GBddGetArgTab);
	printf("\n");
	fflush(stdout);
}
void GBdd::print_mons_ln( FILE* f, const char* (*get_arg)(int)) const {
	print_mons(f, get_arg);
	printf("\n");
	fflush(stdout);
}
