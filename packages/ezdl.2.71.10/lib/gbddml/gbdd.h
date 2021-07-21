#include <stdlib.h>
#include <stdio.h>
//#include <values.h>
#include <memory.h>
#define MAXINT ((int)((unsigned)(~(unsigned)0) >> 1))

/****************************************************************
	Classe : GBdd, GBddList
*****************************************************************

	Nature : Permet de definir des variables contenant des valeurs
de type "bdd general". Ce type doit etre utilise comme un type
simple !

*****************************************************************
							Utilisation :
*****************************************************************

0) Versions :
-------------

	const char* GBdd::version() (méthode statique)

	1.1 
	Compatibilité avec g++ > 4

	1.0 (novembre 2001) : 
	- gestion des versions,
	- class GBddList en standard (voir 10)

	< 1.0 : 1997 -> 2001

1) Declaration d'un GBdd :
_________________________

	GBdd toto;

2) Affectation :
________________

	GBdd toto;
	toto = titi(....); *

3) Declaration + initialisation :
________________________________

	GBdd toto = titi(....); * 

* ou titi(...) est une fonction qq dont le resultat est du type GBdd

4) Comparaison entre GBdd : 
___________________________
- les opérateurs == et != sont definis et rendent un int (0 ou 1).
(N.B. ne pas confondre avec eq et neq !)

- la fonction test_impl(t1,t2) teste si t1 => t2 (resultat 0 ou 1)
(N.B. ne pas confondre avec impl(t1,t2) qui construit "not t1 or t2")

5) Fonctions renvoyant des GBdd :
________________________________

	5-1) Constantes pre-definies :
   ++++++++++++++++++++++++++++++

GBdd true_bdd();
GBdd false_bdd();

	5-1-bis) Permet de retourner une valeur "nulle" :
   ++++++++++++++++++++++++++++++

GBdd null_bdd();

	5-2) Creation d'une nouvelle constante :
   ++++++++++++++++++++++++++++++

GBdd leaf(void* i); // Feuille decoree avec "i"

	5-3) Creation de noeuds binaires :
   ++++++++++++++++++++++++++++++

GBdd idy(int i); // Fonction identite' sur la ieme var
GBdd neg(int i); // Fonction complement sur la ieme var

	5-4) Operateur if then else :
   ++++++++++++++++++++++++++++++
le premier operateur est soit un GBdd
strictement booleen, soit un indice de variable :

GBdd ite(GBdd cond, GBdd then_value, GBdd else_value);
GBdd ite(int var, GBdd then_value, GBdd else_value); *

* on a : ite(var,then_value,else_value) == ite(idy(var),then_value,else_value)

	5-5) Batterie d'operateurs booleens :
   ++++++++++++++++++++++++++++++

GBdd not(GBdd t);
GBdd and(GBdd t1, GBdd t2);
GBdd or(GBdd t1, GBdd t2);
GBdd impl(GBdd t1, GBdd t2);
GBdd xor(GBdd t1, GBdd t2);
GBdd eq(GBdd t1, GBdd t2)
GBdd neq(GBdd t1, GBdd t2)

	5-5bis) Operateurs booleens "non standard" :
   ++++++++++++++++++++++++++++++

GBdd constrain(GBdd f, GBdd c)
GBdd restrict(GBdd f, GBdd c)

GBdd constrain_bu(GBdd f, GBdd c)
GBdd constrain_td(GBdd f, GBdd c)

	5-6) Quantificateurs :
   ++++++++++++++++++++++++++++++

a) simples : l'argument est un indice de variable
GBdd forall(int x, GBdd t);
GBdd exist(int x, GBdd t);

abis) GBdd GBdd::smooth(int x)
Une implementation alternative du exist (attention : methode !)

b) multiples "x2max" : l'ensemble de variables a quantifier est x,x+1,..max
//Correspond a : "quel que soit x,x+1,..,max ..."
GBdd exist_x2max(int x, GBdd); 
//Correspond a : "il existe x,x+1,..,max tel que ..."
GBdd forall_x2max(int x, GBdd); 

c) multiples quelconques : l'ensemble de variables a quantifier est quelconque
(X = {x1, x2, ... , xn}) represente par : 
	*le bdd "x1 and x2 and ... and xn" 
GBdd exist(GBdd vars, GBdd f); 
GBdd forall(GBdd vars, GBdd f); 

	5-7) Impression des bdds generaux (methodes)
   ++++++++++++++++++++++++++++++

void print(FILE* f, void (*ppl)(FILE*,void*)) const;
	Impression dans f, la fonction ppl etant destine' a l'impression
	des feuilles.
void print() const;
	Version simplifiee : impression sur stdout, les feuilles sont imprimee
	par une fonction par defaut : "true", "false" ou alors un entier.

	5-8) Impression des bdds booleens (somme de monomes)
	Valable UNIQUEMENT pour des GBdd booleens :
   ++++++++++++++++++++++++++++++

	deux possibilite' pour l'impression des arguments :
	. soit les identificateurs associes aux arguments sont passes
	en parametre dans un tableau "args", 
	. soit l pa identificateurs associes aux arguments sont fournis
	par une fonction "get_arg(int)".
	Les representations des fonctions identiquement vrai et identiquement
	fausse sont donne'es respectivement par "true_str" et "false_str" 
	Par de'faut, le fichier de sortie est stdout, est les arguments
	sont "x0, x1, ..., xn ....", et les fonctions de base sont "true"
	et "false". 
	Toutes les operateurs peuvent etre redefinis :
static void GBdd::set_mons_strings (
   char* ts, char* fs,                //true et false
   char* ns, char* as, char* os,      //not, and, or
   char* iop = "",                    //identite
   char* opar = "", char* cpar = ""   //parentheses
)
void print_mons(FILE* f, char* (*get_arg)(int)) const;
void print_mons(FILE* f, char** args) const;
void print_mons() const;
void print_mons_ln(FILE* f, char* (*get_arg)(int)) const;
void print_mons_ln(FILE* f, char** args) const;
void print_mons_ln() const;


6) Decomposition de Shannon : (METHODES)
------------------------------------------

	6-1) Variable d'indice minimal apparraissant dans la fonction : 
   ++++++++++++++++++++++++++++++

int root_var(); (MAXINT si constante)

	6-1-bis) Teste si constante (equiv a "root_var() == MAXINT) :
   ++++++++++++++++++++++++++++++

int is_leaf();

	6-2) Les fonctions fh et fl telles que : 
   ++++++++++++++++++++++++++++++

"f = if root_var(f) then fh else fl" (IDENTITE POUR LES CONSTANTES !) :

GBdd high_part();
GBdd low_part();

	6-3) Decomposition selon une variable quelconque :
   ++++++++++++++++++++++++++++++

	Les fonctions x_fh et x_fl telles que : 
"f = if x then x_fh else x_fl" (IDENTITE POUR LES CONSTANTES !) :

GBdd high_part(int x);
GBdd low_part(int x);

7) Acces a l'identificateur d'une feuille : (METHODE)
-----------------------------------------------------
	Si une feuille a e'te' cre'e avec l'info void* x, cette valeur x
est accessible via la methode :

void* leaf_info(); NULL si pas feuille !
	
8) Dependance selon les variable (METHODE) : 
--------------------------------------------

int depends(int i); equivalent a "forall(i,*this) == *this "


9) Evaluation (METHODE) :
------------------------
void* eval(int (*f)(int));

Le parametre "f" est une fonction a argument entier et a resultat
booleen qui donne la valeur des arguments du bdd. Le resultat
est l'info qui decore la feuille correspondante.

int eval_bool(int (*f)(int));
Meme chose mais applicable sur les bdd purement booleens : le resultat
est 0 pour false_bdd(), 1 pour true_bdd().

10) Liste Chaînée de bdds :
---------------------------
	Attention très sommaire : la liste vide est codée par NULL,
	il n'y a aucun partage ni gestion mémoire =>
	- ne manipuler QUE les têtes de liste,
	- pensez à détruire les listes inutiles (N.B. la destruction est 
	récursive)

Construction :
	GBddList(GBdd s, GBddList* nxt);

Destruction :
	~GBddList()

Tête et queue :
	GBdd head()
	GBddList* tail()

Taille en noeuds (signed-bdd)
	int size();

Taille en sous-fonctions (partage, mais pas signe).
	int unsigned_size();

Modif en place (pas beau) :
	 void set_head(const GBdd& v)
----------------------------------------------------------------------
ANNEXE :
----------------------------------------------------------------------
	Pour des traitements compliques : 

A0) Général :
++++++++++++++
void GBdd::set_verbose_mode()
int GBdd::garbage_collect() -> renvoie le %age de noeuds libérés

A1) hashage :
++++++++++++++
int b.hash(int max) renvoie un code compris entre 0 et max
qui peut etre utilise pour hashcoder des GBdd

A2) optimiser une fonction GBdd -> GBdd :
+++++++++++++++++++++++++++++++++++++++++

	int get_op_ident(); Renvoie un entier unique qui pourra e^tre
utilise comme cle dans "put_res" et "get_res".

	void put_res(int index, int op_ident, GBdd res) Cette methode associe
au bdd le resultat "res" de l'operation identifiee de maniere
unique par "op_ident". ATTENTION ne garantit pas que cette
information sera gardee ! 

	GBdd get_res(int index, int op_ident) Renvoie le dernier resultat 
associe a this par put_res(op_ident), sinon "null_bdd()",

ATTENTION : pour index=0 le dernier resultat associe est peut-etre perdu !
pour index=1, le dernier resultat est sauvegarde. meme en cas de
garbage collect

A2) associer une ``marque'' a un GBdd :
+++++++++++++++++++++++++++++++++++++++
N.B. : CES METHODES UTILISENT UN CHAMP ``LIBRE'' 
UNIQUEMENT DANS LES BDDS BINAIRES => ELLES NE PEUVENT PAS ETRE 
UTILISEES SUR LES FEUILLES.
	EN CAS DE GARBAGE COLLECT, des noeuds auxquels sont associes
des infos peuvent etre perdus : pensez a detruire les infos des noeuds
qui ne sont plus references pour eviter les ``fuites'' de memoire ! 

	int get_mark_key(); Renvoie un entier unique qui pourra e^tre
utilise comme cle dans "put_mark" et "get_mark".
INDEPENDANT DE ``get_op_ident''.

	void put_mark(int mark_key, void* mark);
	void* get_mark(int mark_key);

A5) support d'un bdd :
++++++++++++++++++++++

* GBdd support() renvoie le "ou" de tous les arguments 
qui influence la valeur de this. (i.e. tous les arguments qui
apparaissent dans le bdd), false si c'est une constante 
(i.e. une feuille).

* GBdd cube() renvoie le "et" de tous les arguments,
true si c'est une constante.

* int nb_vars() renvoie le nombre de variables du support

* int max_var() renvoie le plus grand index de var
apparaissant dans le bdd (-1 si feuille)

A6) ``taille'' d'un bdd:
++++++++++++++++++++++++

int GBdd::size(); renvoie la ``taille'' de la representation interne
du bdd. Elle est exprimee en nombre de noeud et tiend compte du partage.

int GBdd::unsigned_size(); renvoie la ``taille'' du graphe de Shannon
en nombre de noeuds binaires : i.e. ne tiens pas compte du
signe. 

int GBdd::tree_size(); renvoie la ``taille'' de l'arbre de Shannon
equivalent au bdd NE TIEND PAS COMPTE DU PARTAGE !!!!

A7) Decalage des arguments
++++++++++++++++++++++++++

GBdd GBdd::shift(int n) Renvoie un bdd de me^me structure, ou` les
	arguments  ont e'te' de'cale's de ``n'' (l'argument i devient
	l'argument i+n). 
	ATTENTION ! Erreur si l'argument final (i+n) est ne'gatif.

A8) Renommage des arguments
++++++++++++++++++++++++++++

GBdd GBdd::rename(int (*f)(int)) Renvoie un bdd ou les arguments
ont été renommés par f.
	(l'argument i devient l'argument f(i)). 
	ATTENTION ! Erreur si un argument f(i) est ne'gatif!

---------------------------------------------------------------*/

class GBddManager;
class GBddNode;
class GBdd;

typedef void* GBddValue;
typedef void (*void2void)() ;
/* Pour faire des operations bit a bit sur les GBddValue */
typedef unsigned long int GBddValue2BitString;

class GBddList;

class GBdd {
friend class GBddList;
friend class GBddNode;
friend class GBddManager;
friend void GBddPrintLeafDflt(FILE* f, void* v);
friend void INIT_GBDD_GLOBALS();

//#ifdef DEBUG_GBDD
//friend void CHECK_GBDD(int verb = 0);
//friend void DUMP_BDD(GBdd* b);
//friend void DUMP_LIST(GBddList* b);
//#endif
	

	GBddValue it;	
	GBdd* next_alive;
	GBdd* pred_alive;

	void insert_alive();
	void remove_alive();

protected :
#ifdef DEBUG_GBDD
friend void CHECK_GBDD(int verb);
friend void DUMP_BDD(GBdd b);
friend void DUMP_LIST(GBddList* b);
#endif

	static void IError(const char* fmt, ...);
	static void Error(const char* fmt, ...);
	static int Verbose(const char* fmt, ...);
	static void Warning(const char* fmt, ...);
	static void (*Exit)();
	static int VERBOSE;
	static int Verbose(){ return VERBOSE; }

	/* accessibles pour le manager de la memoire */
	static GBdd alives;
	GBdd(GBddValue v);
	inline GBddValue value() const {
		return it;
	};

	/* acces aux informations de la GBddValue */
	static int sign(GBddValue v);
	static int is_bool(GBddValue v);
	static GBddNode* node(GBddValue v);
	static void set_bool(GBddValue& v);
	static void chsign(GBddValue& v);
	static int nb_op_idents;
	static int nb_keys;
	static int support_op_ident;
	static int cube_op_ident;

	static int first_mon;
	static const char* trueop;
	static const char* falseop;
	static const char* notop;
	static const char* andop;
	static const char* orop;
	static const char* idyop;
	static const char* openpar;
	static const char* closepar;
	void rec_print_mons
		(FILE* f, const char* (*get_arg)(int), char* prefix) const;

	/* Implementation du shift */
	static int shift_op_ident;
	static int shift_op_index;
	GBdd rec_shift(int n) const;

	/* Implementation du rename */
	static int rename_op_ident;
	static int (*rename_op_func)(int);
	GBdd rec_rename() const;


	/* Implementation du smooth */
	static int smooth_op_ident;
	static int smooth_op_var;
	GBdd rec_smooth(int x) const;

	static GBddManager manager;
	static GBddValue true_value();
	static GBddValue false_value();

	/* Implementation de unsigned_size */
	int rec_unsigned_size(int mark) const;

public :

	//POUR LES STATS :
	static int total_nb_nodes();

	static const char* version(){ return "1.1"; }

	/* Initialisation du module  :
		L'initialisation n'est pas necessaire : il y une valeur
	par defaut pour tout. On peut modifier :
		- la taille d'une page d'allocation memoire
		- la valeur associees aux constantes true
	et false (true_info, false_info). Par defaut les info de ces
	deux constantes sont des pointeurs sur deux chaines de caracteres
	statique "true_gdbb" et "false_bdd".
	*/
	static void init_module(void* t_info, void* f_info);
	static void init_module(int pagesz);
	static void init_module(int pagesz, void* t_info, void* f_info);
	static void init_module();
	static int get_op_ident();
	static int get_mark_key();

	/* exit propre
	*/
	static void set_exit_proc(void (*e)());
	static int set_verbose_mode(int verb = 1);

	/* Appel du gc
 	Renvoie le % de noeuds recuperes
	*/
	static int garbage_collect();

	/*
	Le nombre de noeuds réellement utilisés
	*/
	static int used_nodes();

	/* Les operateurs suivants sont redefinis pour
		prendre en compte le mecanisme de chainage
	*/
	GBdd& operator=(const GBdd& f);
	GBdd(const GBdd& t);
	GBdd();
	~GBdd();

	/*--------------------------------
	Gestion de l'ordre des variables :
	(non implemente)
	--------------------------------*/
	static void sift(){
		Warning("GBdd::sift() not implemented\n");
	};
	static int auto_sift(){
		Warning("GBdd::auto_sift() not implemented\n");
		return 0;
	};
	static void auto_sift_on(){
		Warning("GBdd::auto_sift_on() not implemented\n");
	};
	static void auto_sift_off(){
		Warning("GBdd::auto_sift_off() not implemented\n");
	};

	/* Un codage "disperseur" GBdd -> int */
	int hash(int max) const;
	int code() const;

	/* associe une info */
	void put_res(int index, int op_ident, const GBdd& res) const;
	GBdd get_res(int index, int op_ident) const;

	void put_mark(int mark_key, void* mark) const;
	void* get_mark(int mark_key) const;

	/* Autre operations : */
	//Constante pre-definie "true"
	friend const GBdd& true_bdd();	
	//friend GBdd true_bdd();	

	//Constante pre-definie "false"
	friend const GBdd& false_bdd();
	//friend GBdd false_bdd();

	friend GBdd null_bdd();	//Un GBdd non initialise

	friend GBdd leaf(void* i); // Feuille decoree avec "i"

	friend GBdd idy(int i); // Fonction identite sur la ieme var
	friend GBdd nidy(int i); // Fonction complement sur la ieme var

	/* Decomposition de shannon : */

	// La variable min qui apparrait dans le GBdd (MAXINT si constante)
	int root_var() const;

	int is_leaf() const;
	int is_bool() const;
	int is_true() const;
	int is_false() const;
	int is_null() const;

	// Les fonctions fh et fl telles que :
	// "f = if head_var(f) then fh else fl"
	// IDENTITE POUR LES CONSTANTES !
	GBdd high_part() const;
	GBdd low_part() const;

	// Decomposition selon une variable qq :
	// IDENTITE POUR LES CONSTANTES !
	GBdd high_part(int x) const;
	GBdd low_part(int x) const;

	//Info d'une feuille :
	void* leaf_info() const;

	int depends(int x) const; // 1 ssi depend de la variable x

	GBdd support() const; //Le "ou" des var apparraissant dans this
	GBdd cube() const; //Le "et" des var apparraissant dans this
	int max_var() const;  //La plus grande var de this
	int nb_vars() const;  //Le nombre de vars du support

	//Le nombre de noeuds dans this 
	int size() const;
	//Le nombre de noeuds dans le bdd non-signe equiv a this
	int unsigned_size() const;
	//Le nombre de noeuds dans l'arbre equiv a this 
	int tree_size() const;

	GBdd shift(int n) const; // decale les arguments de +n
	GBdd rename(int (*f)(int)) const; // renomme les arguments

	GBdd smooth(int n) const; // quantificateur existentiel

	//Pour le debogage :
	//Imprime des triplets de la forme :
	//(var) (high) (low)
	void dbg_print() const; 

	//Evaluation :
	void* eval(int (*f)(int)) const;
	int eval_bool(int (*f)(int)) const;

	// retourne 0 SSI égalité
	friend int compare(const GBdd& t1, const GBdd& t2);

	//Comparaisons
	friend int operator==(const GBdd& t1, const GBdd& t2);
	friend int operator!=(const GBdd& t1, const GBdd& t2);
	friend int test_impl(const GBdd& t1, const GBdd& t2);
	friend int test_and(const GBdd& t1, const GBdd& t2);

	/* if .. then .. else : le premier argument doit etre pur booleen */
	friend GBdd ite(const GBdd& c, const GBdd& t1, const GBdd& t2);
	/* .... ou un entier : */
	friend GBdd ite(int v, const GBdd& t1, const GBdd& t2);

	friend GBdd not(const GBdd& t);
	friend GBdd and(const GBdd& t1, const GBdd& t2);
	friend GBdd or(const GBdd& t1, const GBdd& t2);
	friend GBdd impl(const GBdd& t1, const GBdd& t2);
	friend GBdd xor(const GBdd& t1, const GBdd& t2);
	//inline
	friend GBdd eq(const GBdd& t1, const GBdd& t2)
		//{ return not(xor(t1, t2)); }
	;
	//inline
	friend GBdd neq(const GBdd& t1, const GBdd& t2)
		//{ return xor(t1, t2); }
	;

	friend GBdd constrain(const GBdd& f, const GBdd& c);
	friend GBdd restrict(const GBdd& f, const GBdd& c);
	friend GBdd constrain_bu(const GBdd& f, const GBdd& c);
	friend GBdd constrain_td(const GBdd& f, const GBdd& c);

	friend GBdd forall(int x, const GBdd& t);
	friend GBdd exist(int x, const GBdd& t);
	friend GBdd exist_x2max(int x, const GBdd& t);
	friend GBdd forall_x2max(int x, const GBdd& t);
	friend GBdd exist(const GBdd& vars, const GBdd& f); 
	friend GBdd forall(const GBdd& vars, const GBdd& f); 

#ifdef GBDD_OVERLOAD
	inline friend GBdd operator!(const GBdd& t)
		{ return not(t); };
	inline friend GBdd operator&&(const GBdd& t1, const GBdd& t2)
		{ return and(t1, t2); };
	inline friend GBdd operator||(const GBdd& t1, const GBdd& t2)
		{ return or(t1, t2); };
#endif

	void print_ite(FILE* f, void (*ppl)(FILE*,void*)) const;
	void print_ite() const;

	void print(FILE* f, void (*ppl)(FILE*,void*)) const;
	void print() const;

	//IMPRESION SOUS FORME DE POLYNOMES :
	// Les chaines de caracteres correspondant aux constantes
	// et aux operateurs "not", "and", "or" peuvent etre 
	// redefinies :
	//On peut redefinir la forme des parentheses ouvrantes
	//et fermantes (n.b. par defaut, il n'y en a pas)
	//ainsi que l'operateur ``identite'' (par defaut, vide)
	static void set_mons_strings (
		const char* ts, const char* fs,             //true et false
		const char* ns, const char* as, const char* os,      //not, and, or
		const char* iop = "",                    //identite
		const char* opar = "", const char* cpar = ""   //parentheses
	);
	// L'impression des variables peut etre parametree par :
	// - une fonction qui associe a chaque index un chaine de cars.
	void print_mons(FILE* f, const char* (*get_arg)(int)) const;
	void print_mons_ln(FILE* f, const char* (*get_arg)(int)) const;
	// - un tableau de chaines indexe par les vars :
	void print_mons(FILE* f, const char** args) const;
	void print_mons_ln(FILE* f, const char** args) const;
	// - par defaut, imprime X0, X1 ... Xn :
	void print_mons() const;
	void print_mons_ln() const;
};
/*---------------------------------------------------------------
   Version 1.1 => à partir de g++ > 4
   les fonctions friend doivent être re-déclarées en dehors
   de la classe pour être référencées comme exporté dans le .o
----------------------------------------------------------------*/

const GBdd& true_bdd();	
const GBdd& false_bdd();
GBdd null_bdd();	//Un GBdd non initialise
GBdd leaf(void* i); // Feuille decoree avec "i"
GBdd idy(int i); // Fonction identite sur la ieme var
GBdd nidy(int i); // Fonction complement sur la ieme var
int compare(const GBdd& t1, const GBdd& t2); // retourne 0 SSI égalité
//Comparaisons
int operator==(const GBdd& t1, const GBdd& t2);
int operator!=(const GBdd& t1, const GBdd& t2);
int test_impl(const GBdd& t1, const GBdd& t2);
int test_and(const GBdd& t1, const GBdd& t2);
/* if .. then .. else : le premier argument doit etre pur booleen */
GBdd ite(const GBdd& c, const GBdd& t1, const GBdd& t2);
/* .... ou un entier : */
GBdd ite(int v, const GBdd& t1, const GBdd& t2);
GBdd not(const GBdd& t);
GBdd and(const GBdd& t1, const GBdd& t2);
GBdd or(const GBdd& t1, const GBdd& t2);
GBdd impl(const GBdd& t1, const GBdd& t2);
GBdd xor(const GBdd& t1, const GBdd& t2);
inline GBdd eq(const GBdd& t1, const GBdd& t2) { return not(xor(t1, t2)); };
inline GBdd neq(const GBdd& t1, const GBdd& t2) { return xor(t1, t2); };
GBdd constrain(const GBdd& f, const GBdd& c);
GBdd restrict(const GBdd& f, const GBdd& c);
GBdd constrain_bu(const GBdd& f, const GBdd& c);
GBdd constrain_td(const GBdd& f, const GBdd& c);
GBdd forall(int x, const GBdd& t);
GBdd exist(int x, const GBdd& t);
GBdd exist_x2max(int x, const GBdd& t);
GBdd forall_x2max(int x, const GBdd& t);
GBdd exist(const GBdd& vars, const GBdd& f); 
GBdd forall(const GBdd& vars, const GBdd& f); 


/*---------------------------------------------------------------
	Classe GBddList 
-----------------------------------------------------------------
	Liste chaînée sommaire
---------------------------------------------------------------*/
class GBddList {
	int len;
   GBdd son;
   GBddList* nxt;
   GBddList* lst;
   void uplast(){
      if(! this) return;
      if(lst->nxt){
         lst->uplast();
         len = len + lst->len -1;
         lst = lst->lst;
      }
   }
public:
   GBdd head(){ return son; }
   GBddList* tail(){ return nxt; }
	//Renvoie le suivant et détruit la tête
	GBddList* tail_then_del(){
		GBddList* r = nxt;
		nxt = NULL;
		delete (this);
		return r;
	}
	int length(){ if(! this) return 0; uplast(); return len; }
	GBddList(GBdd s){ len = 1; son = s; nxt = NULL; lst = this; }
   GBddList(GBdd s, GBddList* t){
		son = s; nxt = t; 
		if(t){ len = t->len+1; lst = t->lst; }
		else { len = 1; lst = this; }
	}
	//Destruction recursive
   ~GBddList(){ if(nxt) delete nxt; }
	void set_head(const GBdd& v){ son = v; }
   GBddList* append(GBddList* l){
      if(! l) return this;
      if(this){
         uplast();
         l->uplast();
         lst->len += l->len;
         lst->nxt = l;
         lst->lst = l->lst;
         if(lst != l->lst){
            len += l->len;
            lst = l->lst;
         }
         return this;
      } else {
         return l;
      }
   }
   GBddList* append(GBdd s){
      GBddList* l = new GBddList(s);
      return append(l);
   }
	int size() const;
};

/*---------------------------------------------------------------
	Classe : GBddNode

	Nature : Structure de stockage d'un noeud de bdd generalise.

	Les valeur du type bdd sont definis recursivement comme :
- des feuilles, caracterisees par une valeur de type "void *",
pour lesquelles l'equivalence est l'equalite de cette valeur.
Cette methode n'est pas tres propre du point de vue type, mais
elle permet de ranger n'importe qu'elle information dnas une
feuille.
- des noeuds binaires, en fait des triplets composes d'un
entier (index),  et de deux pointeurs sur des GBddNode
(low et high)
---------------------------------------------------------------*/
class GBddUnaryRes {
friend class GBdd;
friend class GBddNode;
protected :
	int direct_key;
	GBddValue direct_res;
	int indirect_key;
	GBddValue indirect_res;
	void set_direct(int k, GBddValue v){
		direct_key = k; direct_res = v;
	}	
	GBddValue get_direct(int k){
		if(direct_key == k) return direct_res;
		else return NULL;
	}
	void set_indirect(int k, GBddValue v){
		indirect_key = k; indirect_res = v;
	}
	GBddValue get_indirect(int k){
		if(indirect_key == k) return indirect_res;
		else return NULL;
	}
	void reset(){
		direct_key = indirect_key = -1;
		direct_res = indirect_res = NULL;
	}
};

enum GBddOpCode {
	GBDD_NO_OP, 
	GBDD_APP_AND,
	GBDD_APP_XOR,
	GBDD_APP_RESTRICT,
	GBDD_APP_CONSTRAIN
};

class GBddNode {
friend class GBddMemoryItem;
friend class GBddMemory;
friend class GBddManager;
friend class GBdd;
friend class GBddList;

	// Informations Propres : 
	int index;
	void* info;
	GBddValue low; 
	GBddValue high; 

	// Informations liees a la gestion memoire : 
	GBddNode* next;
	int gc_mark;
	void rec_gc_mark(int m);

	//Pour le debogage :
	static int dbg_counter;
	int dbg_index;
	void dbg_print(int mark);

	//Creation et initialisations :
	//GBddNode(){}; 
	void init(void* i, GBddNode* n);
	void init(int ix, GBddValue h, GBddValue l, GBddNode* n); 

	//gestion des feuilles :
	static int leaf_index();
	int is_leaf();

	//optimisation des calculs :
	void clear_results();
	GBddOpCode direct_bin_op_code;
	GBddOpCode indirect_bin_op_code;
	GBddValue direct_op;	
	GBddValue direct_res;	
	GBddValue indirect_op;	
	GBddValue indirect_res;	

	int size_mark;
	int size(int rec_mark);
	int tree_size();

	GBddUnaryRes unary_res[2];

	int direct_mark_key;
	void* direct_mark;
	int indirect_mark_key;
	void* indirect_mark;
};

/*---------------------------------------------------------------
	Classe : GBddMemory

	Nature : Gere un espace memoire dedie a l'allocation 
	la memoire est allouee par pages, ces pages sont chainees
	pour permettre une destruction propre.
---------------------------------------------------------------*/

class GBddMemoryItem {
friend class GBddMemory;
protected :
	union {
		GBddNode node;
		GBddMemoryItem* next_page;
	};
};

class GBddMemory {
friend void INIT_GBDD_GLOBALS();
private :

	GBddMemoryItem* first_page;	//Liste des pages allouees
	int nb_pages;						//nbre de     "       "
	void delete_page		// Desalloue recursivement la liste de page 
		(GBddMemoryItem* list);	
	int size;				// La taille (en GBddNode) d'une page
	GBddNode* free_list; // La premiere case libre

protected :
	static int page_size_dflt;
	inline void init(int psz){ size = psz; };

	//POUR LES STATS :
	int nb_allocated;

	GBddMemory();		// Creation 
	void init();
	~GBddMemory();		// Destruction propre : desalloue les pages

	inline int mem_size(){ return (size*nb_pages); };

	void unalloc_node(GBddNode* b);	// Desaloue le noeud b
	GBddNode* alloc_node();				// Allocation d'une case libre
	int alloc_page();						// Alloue une page : renvoie 0 si echoue
};

/*---------------------------------------------------------------
	Classe : GBddManager derivee de GBddMemory

	Nature : Gere la canonicite des valeurs "GBdd" via une
table de hash. C'est cette classe qui a la matrise du garbage
collect puisqu'elle doit mettre a jour sa table de hash a 
chaque appel de cette procedure.
---------------------------------------------------------------*/

class GBddManager : public GBddMemory {
friend void INIT_GBDD_GLOBALS();
friend class GBdd;

	/* table de hash : */
	GBddNode** hash_tab;
	int hash_size;

	int hash_code(int i, GBddValue h, GBddValue l);
	int hash_code(void* i);

	/* valeurs par defaut des infos de true et false : */
	static char true_info_dflt[];
	static char false_info_dflt[];
	static int hash_size_dflt;

	/* quelques messages d'erreurs */
	static const char* errors[];	

	/* gestion de la memoire : */
	GBddNode* new_node();
	int cur_gc_mark;
	int garbage_collect();
	int alive_size();

	/* Customisation de la memoire :
		Renvoie 0 si deja initialise
	*/
	int init(int pagesz, void* t_info, void* f_info);
	int init(int pagesz );
	int init(void* t_info, void* f_info);
	int init();

	/* print des bdds : */
	static char tabulation[];
	void tabule(FILE* f, int k);

	/*--------------------------------------------------------------
		Constantes internes :
	----------------------------------------------------------------
		Un seul noeud pour true et false (true_node) :
	c'est le signe de la GBddValue qui les differencie
	(internal_true et internal_false).
		Ce noeud n'est pas dans la memoire ni dans la table de hash,
	mais il est quand meme accessible via get_leaf qui prevoie ce cas
	particulier en interceptant true_info et false_info.
	--------------------------------------------------------------*/
	GBddNode true_node;
	void* true_info;
	void* false_info;
	GBddValue internal_true;
	GBddValue internal_false;

	/* correction du signe des gbdd booleens */
	GBddValue correct_value(GBddNode* n, unsigned bool_mask, unsigned sign_mask);

	GBddValue correct_high(GBddValue bv);
	GBddValue correct_low(GBddValue bv);

	int depends(GBddValue bv, int x);
	GBddValue set_true(GBddValue bv, int x);
	GBddValue set_false(GBddValue bv, int x);

public :
	void check_alive_list();
	void check_alive_list(GBdd* ref);

	/* Construction : 
		Aucun parametre : il existe des valeurs par defaut pour
	tout. Pour customiser, il faut appeler la fonction
	"init..." AVANT D'UTILISER LES GBDD
		On peut fixer : la taille de la table de hash (hashsz)
	donner des valeurs aux infos associees aux constantes true
	et false (true_info, false_info). Par defaut les info de ces
	deux constantes sont des pointeurs sur deux chaines de caracteres
	statique "true_gdbb" et "false_bdd".
	*/
	GBddManager();

	/* Acces aux GBddNode (reserve a la classe GBdd ! ) */
	GBdd* true_pt;
	GBdd* false_pt;
	GBdd* null_pt;
	GBddValue get_leaf(void* i);
	GBddValue get_node(int i, const GBdd& hp, const GBdd& lp);

	/* Operations optimisee sur les GBddNode : 
		ATTENTION : au niveau le + haut, les arguments doivent etre
		"stockes" dans des GBdd !
	*/
	int test_impl(GBddValue f1, GBddValue f2);
	int test_and(GBddValue f1, GBddValue f2);
	GBddValue app_ite(GBddValue f0, GBddValue f1, GBddValue f2);
	GBddValue app_and(GBddValue f1, GBddValue f2);
	GBddValue app_xor(GBddValue f1, GBddValue f2);
	GBddValue app_not(GBddValue f);
	GBddValue app_restrict(GBddValue fv, GBddValue cv);
	GBddValue app_constrain(GBddValue fv, GBddValue cv);
	GBddValue app_forall(int x, GBddValue f);
	GBddValue app_forall_sup(int x, GBddValue f);
	GBddValue app_forall_set(GBddValue set, GBddValue fv);
	GBddValue get_bin_op_res(GBddOpCode code, GBddValue f1, GBddValue f2);
	void put_bin_op_res
		(GBddOpCode code, GBddValue f1, GBddValue f2, GBddValue res);

	/* correction des infos associees aux feuilles : (cause tdg) */
	void* leaf_info(GBddValue bv);
	
	/* pretty print des bdd's */
	void print_ite(FILE* f, GBddValue b, void (*ppl)(FILE*,void*), int t);
	void print(FILE* , GBddValue , void (*ppl)(FILE*,void*), char*, int, int);

};

inline GBddValue GBdd::true_value(){
	return manager.internal_true;
}
inline GBddValue GBdd::false_value(){
	return manager.internal_false;
}

