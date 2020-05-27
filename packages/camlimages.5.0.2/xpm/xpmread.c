/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            François Pessaux, projet Cristal, INRIA Rocquencourt     */
/*            Pierre Weis, projet Cristal, INRIA Rocquencourt          */
/*            Jun Furuse, projet Cristal, INRIA Rocquencourt           */
/*                                                                     */
/*  Copyright 1999,2000                                                */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

#include "../config/config.h"

#ifdef HAS_XPM

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <X11/xpm.h>

#include <stdio.h>

value read_xpm_file( name )
     value name;
{
  CAMLparam1(name);
  CAMLlocal3(cmap,imap,result);

  char *filename;
  XpmImage image;
  int i;

  filename = String_val( name );

  if( !XpmReadFileToXpmImage( filename, &image, NULL ) ){

    /*
    fprintf(stderr, "width = %d\n", image.width);
    fprintf(stderr, "height = %d\n", image.height);
    fprintf(stderr, "colors = %d\n", image.ncolors);
    */

    cmap = alloc_tuple( image.ncolors );
    for(i=0; i<image.ncolors; i++){
      XpmColor *color = image.colorTable + i;
      /*
      fprintf(stderr, "  string= %s", color->string);
      fprintf(stderr, "  symbolic= %s", color->symbolic);
      fprintf(stderr, "  m_color= %s", color->m_color);
      fprintf(stderr, "  g4_color= %s", color->g4_color);
      fprintf(stderr, "  g_color= %s", color->g_color);
      */
      /*
	fprintf(stderr, "%d  c_color= %s\n", i, color->c_color);
      */

      if( color->c_color == NULL ) {
          fprintf(stderr, "color id %d has no c_color\n", i);
          Store_field(cmap, i, copy_string("null"));
      } else {
          Store_field(cmap, i, copy_string(color->c_color));
      }
    }

    { /* image map alloc */
      int size = image.width * image.height;

      imap = alloc_tuple( size );
      for(i=0; i<size; i++){
          Store_field(imap,i,Val_int(image.data[i]));
      }
    }

    
    /* connect the result */
    result = alloc_small(4,0);
    Field(result,0) = Val_int(image.width);
    Field(result,1) = Val_int(image.height);
    Field(result,2) = cmap;
    Field(result,3) = imap;

    XpmFreeXpmImage( &image );

    CAMLreturn(result);
  } else {
    failwith("failed to open xpm file");
  }
}

#else // HAS_XPM

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value read_xpm_file(){ failwith("unsupported"); }

#endif
