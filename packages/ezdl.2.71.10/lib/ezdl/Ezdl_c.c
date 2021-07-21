/*
  
*/
#include <stdio.h>
#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"

// unix or win32 ?
#ifdef WIN32
#include <windows.h>
#define HANDLE HANDLE
#ifndef EZDL_ERROR_BUFF_ALOC
#define EZDL_ERROR_BUFF_ALOC
char EZDL_ERROR_BUFF[1024];
#endif
#define RTLD_LAZY 0
#define ezdlopen(n) ((void*)LoadLibrary(n))
#define ezdlsym(l,n) ((void*)GetProcAddress(l,n))
#define ezdlerror() (sprintf(EZDL_ERROR_BUFF,"win32 dll error %d", GetLastError()),&EZDL_ERROR_BUFF[0])
#define ezdlclose(l) (FreeLibrary(l))
#else
#include <dlfcn.h>
#define ezdlopen(n) (dlopen(n,RTLD_LAZY))
#define ezdlsym(l,n) (dlsym(l,n))
#define ezdlerror() (dlerror())
#define ezdlclose(l) (dlclose(l))
#endif

typedef unsigned long word;

char* EZDL_ACCESS;
char EZDL_ERROR[1024];

CAMLprim value c_dlopen (value filename) {
	CAMLparam1(filename);
	void* res;
//printf("sizeof(value) = %d\n", sizeof(value));
	res = ezdlopen(String_val(filename));
	if(res == NULL) {
		sprintf(&EZDL_ERROR[0],"Ezdl.dlopen: %s", ezdlerror());
		failwith(&EZDL_ERROR[0]); 
	}
	CAMLreturn((value)res);
}

CAMLprim value c_dlsym (value handle, value symbol) {
	CAMLparam2(handle,symbol);
	void* res;
	res = ezdlsym((void*)handle, String_val(symbol));
	if(res == NULL) {
		sprintf(&EZDL_ERROR[0],"Ezdl.dlsym: %s", ezdlerror());
		failwith(&EZDL_ERROR[0]); 
	}
	CAMLreturn((value)res);
}

CAMLprim value c_dlclose (value handle) {
	CAMLparam1(handle);
	if(! ezdlclose((void*)handle)) {
		sprintf(&EZDL_ERROR[0],"Ezdl.dlclose: %s", ezdlerror());
		failwith(&EZDL_ERROR[0]); 
	}
	CAMLreturn(Val_unit);
}

CAMLprim value c_f2f(value handler, value x)
{        
   CAMLparam2(handler, x);
   CAMLlocal1(cml_res);
         
   double (*pf)(double);
   double res;

   pf = (double(*)(double))handler;
   res = (*pf)(Double_val(x));

   cml_res = copy_double(res);
   
   CAMLreturn(cml_res);
}

CAMLprim value c_ff2f(value handler, value x, value y)
{        
   CAMLparam3(handler, x, y);
   CAMLlocal1(cml_res);
         
   double (*pf)(double,double);
   double res;

   pf = (double(*)(double,double))handler;
   res = (*pf)(Double_val(x), Double_val(y));

   cml_res = copy_double(res);
   
   CAMLreturn(cml_res);
}

CAMLprim value c_i2i(value handle, value x)
{
	CAMLparam2(handle, x);
	CAMLlocal1(cml_res);

	int (*pf)(int);
	int res;

	pf = (int(*)(int))handle;
	res = (*pf)(Int_val(x));
	cml_res = Val_int(res);
	
	CAMLreturn(cml_res);
}

CAMLprim value c_ii2i(value handle, value x, value y)
{
	CAMLparam3(handle, x, y);
	CAMLlocal1(cml_res);

	int (*pf)(int,int);
	int res;

	pf = (int(*)(int,int))handle;
	res = (*pf)(Int_val(x),Int_val(y));
	cml_res = Val_int(res);
	
	CAMLreturn(cml_res);
}

CAMLprim value c_iii2i(value handle, value x, value y, value z)
{
	CAMLparam4(handle, x, y, z);
	CAMLlocal1(cml_res);

	int (*pf)(int,int,int);
	int res;

	pf = (int(*)(int,int,int))handle;
	res = (*pf)(Int_val(x),Int_val(y),Int_val(z));
	cml_res = Val_int(res);
	
	CAMLreturn(cml_res);
}

//cast value -> cptr
// => se contente de vérifier que la value est bien "castable"
// sinon => erreur

CAMLprim value c_cptr_of(value v){
	CAMLparam1(v);
	CAMLreturn(v);
}

// Constructiom/déconstruction d'un type liste

CAMLprim value make_list(value h, value t) {
   CAMLparam2(h,t);
   CAMLlocal1(res);

//printf("calling alloc\n");
   res = alloc(2,0);
//printf("calling Store_field 0\n");
   Store_field(res, 0, h);
//printf("calling Store_field 1\n");
   Store_field(res, 1, t);

   CAMLreturn(res);
}

inline value head_list(value l) { return Field(l, 0); }
inline value tail_list(value l) { return Field(l, 1); }

/*

TAGS DU TYPE UNION :
Attention il faut que ce soit coherant avec le mli !

N.B. pour simplifier, on par du principe qu'il n'y a que
2 "sortes" d'arguments C :
- les doubles, dont le passage est en général particulier
- les types "word", c'est-à-dire tous les autres, qui
  sont passés de la manière

Avec de 1 à 4 arguments ca fait quand même 2^4+2^3+2^2+2 = 30 
possibilités !


*/

#define Int_carg     0
#define Double_carg  1
#define String_carg  2
#define Ptr_carg     3

//Le "décodage" des arguments est commun
//extern void inspect_block(value);

word decode_args(
	value args, long* largs, double* dargs	
) {
/*
Le "profil C" est codé sur 4*4 bits :
(4 bits par arguments car c'est plus facile à lire en hexadécimal !)
0x0 -> pas d'args
0x1 -> arg long
0x2 -> arg double
-> ca laisse plein de place pour la suite !
*/
	CAMLparam1(args);
	CAMLlocal3(l,h,cstr);

	word prof=0;

	int cpt = 0;
	for(l = args; l != Val_int(0); l = tail_list(l)){
		h = head_list(l);
		if(cpt > 3){
			sprintf(&EZDL_ERROR[0],
				"Ezdl.%s: too many args %d (limit is 4)", EZDL_ACCESS, cpt);
			failwith(&EZDL_ERROR[0]); 
		}
//ispect_block(h);
  		switch (Tag_val(h)) {
			case Int_carg :
//printf("Int_carg(%ld)\n", Long_val(Field(h,0)));
				largs[cpt] = Long_val(Field(h,0));
				prof = (prof << 4) | (0x1);
			break;
			case Double_carg :
//printf("Double_carg(%lf)\n", Double_val(Field(h,0)));
				dargs[cpt] = Double_val(Field(h,0));
				prof = (prof << 4) | (0x2);
			break;
			case String_carg :
//printf("String_carg(%s)\n", String_val(Field(h,0)));
				largs[cpt] = (word)String_val(Field(h,0));
				prof |= (0x1 << 4*cpt);				
			break;
			case Ptr_carg :
//printf("Ptr_carg(0x%lX)\n", Field(h,0));
				largs[cpt] = Field(h,0);
				prof |= (0x1 << 4*cpt);				
			break;
		}
		cpt++;
	}
	return prof;

}

long call_long(long handle, word prof, long* L, double* D)
{
	//On peut caster "surement" handle sur une fonction
	//qui rend un long et qui prend en argument
	//une combinaison de long/double selon le code de prof
	long res;
	long (*f)() = (long(*)()) handle;
	switch (prof){
	//0 arg
	case 0x0 :      res = (*f)(); break;
	//1 arg
	case 0x1 :      res = (*f)(L[0]); break;
	case 0x2 :      res = (*f)(D[0]); break;
	//2 args
	case 0x11 :     res = (*f)(L[0],L[1]); break;
	case 0x12 :     res = (*f)(L[0],D[1]); break;
	case 0x21 :     res = (*f)(D[0],L[1]); break;
	case 0x22 :     res = (*f)(D[0],D[1]); break;
	//3 args
	case 0x111 :    res = (*f)(L[0],L[1],L[2]); break;
	case 0x112 :    res = (*f)(L[0],L[1],D[2]); break;
	case 0x121 :    res = (*f)(L[0],D[1],L[2]); break;
	case 0x122 :    res = (*f)(L[0],D[1],D[2]); break;
	case 0x211 :    res = (*f)(D[0],L[1],L[2]); break;
	case 0x212 :    res = (*f)(D[0],L[1],D[2]); break;
	case 0x221 :    res = (*f)(D[0],D[1],L[2]); break;
	case 0x222 :    res = (*f)(D[0],D[1],D[2]); break;
	//4 args
	case 0x1111 :    res = (*f)(L[0],L[1],L[2],L[3]); break;
	case 0x1112 :    res = (*f)(L[0],L[1],L[2],D[3]); break;
	case 0x1121 :    res = (*f)(L[0],L[1],D[2],L[3]); break;
	case 0x1122 :    res = (*f)(L[0],L[1],D[2],D[3]); break;
	case 0x1211 :    res = (*f)(L[0],D[1],L[2],L[3]); break;
	case 0x1212 :    res = (*f)(L[0],D[1],L[2],D[3]); break;
	case 0x1221 :    res = (*f)(L[0],D[1],D[2],L[3]); break;
	case 0x1222 :    res = (*f)(L[0],D[1],D[2],D[3]); break;
	case 0x2111 :    res = (*f)(D[0],L[1],L[2],L[3]); break;
	case 0x2112 :    res = (*f)(D[0],L[1],L[2],D[3]); break;
	case 0x2121 :    res = (*f)(D[0],L[1],D[2],L[3]); break;
	case 0x2122 :    res = (*f)(D[0],L[1],D[2],D[3]); break;
	case 0x2211 :    res = (*f)(D[0],D[1],L[2],L[3]); break;
	case 0x2212 :    res = (*f)(D[0],D[1],L[2],D[3]); break;
	case 0x2221 :    res = (*f)(D[0],D[1],D[2],L[3]); break;
	case 0x2222 :    res = (*f)(D[0],D[1],D[2],D[3]); break;

	default:
		sprintf(&EZDL_ERROR[0],
			"Ezdl.%s: unsupported profile 0x%lX", EZDL_ACCESS, prof);
		failwith(&EZDL_ERROR[0]); 
	break;
	}

	return res;
}

CAMLprim value c_cargs2i(value handle, value args) {
	CAMLparam2(handle, args);

	long ltab[4];
	double dtab[4];
	word prof_code;
	long res;	
	
	EZDL_ACCESS = "cargs2i";
	prof_code = decode_args(args, &ltab[0], &dtab[0]);


	res = call_long(handle, prof_code, ltab, dtab);

	CAMLreturn(Val_long(res));
}

CAMLprim value c_cargs2s(value handle, value args) {
	CAMLparam2(handle, args);

	long ltab[4];
	double dtab[4];
	word prof_code;
	long res ;
	
	EZDL_ACCESS = "cargs2s";
	prof_code = decode_args(args, &ltab[0], &dtab[0]);

	res = call_long(handle, prof_code, ltab, dtab);

	CAMLreturn(copy_string((char*)res));
}

double call_double( long handle, word prof, long* L, double* D)
{
	//On peut caster "surement" handle sur une fonction
	//qui rend un DOUBLE et qui prend en argument
	//une combinaison de long/double selon le code de prof
	double res ;
	double (*f)() = (double(*)()) handle;
//printf("call_double, prof=0x%X\n", prof);
	switch (prof){
	//0 arg
	case 0x0 :      res = (*f)(); break;
	//1 arg
	case 0x1 :      res = (*f)(L[0]); break;
	case 0x2 :      res = (*f)(D[0]); break;
	//2 args
	case 0x11 :     res = (*f)(L[0],L[1]); break;
	case 0x12 :     res = (*f)(L[0],D[1]); break;
	case 0x21 :     res = (*f)(D[0],L[1]); break;
	case 0x22 :     res = (*f)(D[0],D[1]); break;
	//3 args
	case 0x111 :    res = (*f)(L[0],L[1],L[2]); break;
	case 0x112 :    res = (*f)(L[0],L[1],D[2]); break;
	case 0x121 :    res = (*f)(L[0],D[1],L[2]); break;
	case 0x122 :    res = (*f)(L[0],D[1],D[2]); break;
	case 0x211 :    res = (*f)(D[0],L[1],L[2]); break;
	case 0x212 :    res = (*f)(D[0],L[1],D[2]); break;
	case 0x221 :    res = (*f)(D[0],D[1],L[2]); break;
	case 0x222 :    res = (*f)(D[0],D[1],D[2]); break;
	//4 args
	case 0x1111 :    res = (*f)(L[0],L[1],L[2],L[3]); break;
	case 0x1112 :    res = (*f)(L[0],L[1],L[2],D[3]); break;
	case 0x1121 :    res = (*f)(L[0],L[1],D[2],L[3]); break;
	case 0x1122 :    res = (*f)(L[0],L[1],D[2],D[3]); break;
	case 0x1211 :    res = (*f)(L[0],D[1],L[2],L[3]); break;
	case 0x1212 :    res = (*f)(L[0],D[1],L[2],D[3]); break;
	case 0x1221 :    res = (*f)(L[0],D[1],D[2],L[3]); break;
	case 0x1222 :    res = (*f)(L[0],D[1],D[2],D[3]); break;
	case 0x2111 :    res = (*f)(D[0],L[1],L[2],L[3]); break;
	case 0x2112 :    res = (*f)(D[0],L[1],L[2],D[3]); break;
	case 0x2121 :    res = (*f)(D[0],L[1],D[2],L[3]); break;
	case 0x2122 :    res = (*f)(D[0],L[1],D[2],D[3]); break;
	case 0x2211 :    res = (*f)(D[0],D[1],L[2],L[3]); break;
	case 0x2212 :    res = (*f)(D[0],D[1],L[2],D[3]); break;
	case 0x2221 :    res = (*f)(D[0],D[1],D[2],L[3]); break;
	case 0x2222 :    res = (*f)(D[0],D[1],D[2],D[3]); break;

	default:
		sprintf(&EZDL_ERROR[0],
			"Ezdl.%s: unsupported profile 0x%lX", EZDL_ACCESS, prof);
		failwith(&EZDL_ERROR[0]); 
	break;
	}
	return res;
}

CAMLprim value c_cargs2f(value handle, value args) {
	CAMLparam2(handle, args);

	long ltab[4];
	double dtab[4];
	word prof_code;
	double res;

	EZDL_ACCESS = "cargs2f";
	prof_code = decode_args(args, &ltab[0], &dtab[0]);

	res = call_double( handle, prof_code, ltab, dtab);

	CAMLreturn(copy_double(res));
}

