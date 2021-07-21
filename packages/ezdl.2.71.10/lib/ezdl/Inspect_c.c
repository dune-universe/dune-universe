#include<stdlib.h>
#include<stdio.h>
#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"


void margin (int n)
  { while (n-- > 0) printf(".");  return; }

void print_block (value v,int m) 
{
  int size, i;
  margin(m);
  if (Is_long(v)) 
    { printf("immediate value (%ld)\n", Long_val(v));  return; };
  printf ("memory block: size=%d  -  ", size=Wosize_val(v));
  switch (Tag_val(v))
   {
    case Closure_tag : 
        printf("closure with %d free variables\n", size-1);
        margin(m+4); printf("code pointer: %p\n",Code_val(v)) ;
        for (i=1;i<size;i++)  print_block(Field(v,i), m+4);
        break;
    case String_tag :
        printf("string: %s (%s)\n", String_val(v),(char *) v);  
        break;
    case Double_tag:  
        printf("float: %g\n", Double_val(v));
        break;
    case Double_array_tag : 
        printf ("float array: "); 
        for (i=0;i<size/Double_wosize;i++)  printf("  %g", Double_field(v,i));
        printf("\n");
        break;
    case Abstract_tag : printf("abstract type\n"); break;
    default:  
        if (Tag_val(v)>=No_scan_tag) { printf("unknown tag"); break; }; 
        printf("structured block (tag=%d):\n",Tag_val(v));
        for (i=0;i<size;i++)  print_block(Field(v,i),m+4);
   }
  return ;
}

value inspect_block (value v)  
  { print_block(v,4); fflush(stdout); return v; }

