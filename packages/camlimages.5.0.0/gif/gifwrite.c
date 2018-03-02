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

#ifdef HAS_GIF

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdio.h>
#include <string.h>

#include <gif_lib.h>

// GIFLIB_MAJOR is only defined in libgif >= 4.2.0
#if !defined(GIFLIB_MAJOR)
#  define GIFLIB_MAJOR 4
#endif

int list_length( value list )
{
  CAMLparam1(list);
  CAMLlocal1(l);
  register int length = 0;
  for(l = list; l != Val_int(0); l = Field(l,1)){
    length ++;
  }
  CAMLreturn(length);
}

ColorMapObject *ColorMapObject_val( value cmap )
{
  // no caml allocation inside.
  int len;
  int i;
  ColorMapObject *cmapobj;

  if( cmap == Atom(0) ){ return NULL; } 

  len = Wosize_val(cmap);

/*
fprintf(stderr, "Creating map with length = %d ...\n", len);
fflush(stderr);
*/
#if (GIFLIB_MAJOR == 4)
    cmapobj = MakeMapObject( len, NULL );
#else
    cmapobj = GifMakeMapObject( len, NULL );
#endif

  for(i=0; i< len; i++){
    cmapobj->Colors[i].Red   = Int_val(Field(Field(cmap,i),0));
    cmapobj->Colors[i].Green = Int_val(Field(Field(cmap,i),1));
    cmapobj->Colors[i].Blue  = Int_val(Field(Field(cmap,i),2));
  }
  return cmapobj; 
}

value eGifOpenFileName( name )
     value name;
{
  CAMLparam1(name);

  GifFileType *GifFileOut;

#if (GIFLIB_MAJOR <= 4)
    GifFileOut = EGifOpenFileName( String_val( name ), 0 );
#else
    GifFileOut = EGifOpenFileName( String_val( name ), 0, NULL );
#endif

  if (GifFileOut == NULL) {
    failwith("EGifOpenFileName");
  }
  /* gcc -fwritable-strings is required to compile libungif */
#if GIFLIB_BUG_FIXED
  EGifSetGifVersion("89a");
#endif

  CAMLreturn((value)GifFileOut);
}

void eGifCloseFile( value hdl )
{
  CAMLparam1(hdl);

  /* For the bug libungif/giflib 4.1.0 */
  /* This may add a new memory leak, but it is better than having 
     segmentation faults */
  ((GifFileType *)hdl)->Image.ColorMap = NULL; 

#if (GIFLIB_MAJOR <= 4)
  EGifCloseFile( (GifFileType *) hdl );
#else
  EGifCloseFile( (GifFileType *) hdl, NULL );
#endif
  CAMLreturn0;
}

void eGifPutScreenDesc( value oc, value sdesc )
{
  CAMLparam2(oc,sdesc);

  GifFileType *GifFileOut = (GifFileType*) oc;
  if ( EGifPutScreenDesc(GifFileOut, 
			 Int_val(Field(sdesc, 0)),
			 Int_val(Field(sdesc, 1)),
			 Int_val(Field(sdesc, 2)),
			 Int_val(Field(sdesc, 3)),
			 ColorMapObject_val( Field(sdesc, 4) )) == GIF_ERROR){
    failwith("EGifPutScreenDesc");
  }
  CAMLreturn0;
}

void eGifPutImageDesc( value oc, value idesc )
{
  CAMLparam2(oc,idesc);

  GifFileType *GifFileOut = (GifFileType*) oc;
  if ( EGifPutImageDesc(GifFileOut,
			Int_val(Field(idesc, 0)),
			Int_val(Field(idesc, 1)),
			Int_val(Field(idesc, 2)),
			Int_val(Field(idesc, 3)),
			Int_val(Field(idesc, 4)),
			ColorMapObject_val( Field(idesc, 5) )) == GIF_ERROR){
    failwith("EGifPutImageDesc");
  }
  CAMLreturn0;
}

void eGifPutLine( value oc, value buf )
{
  CAMLparam2(oc,buf);

  GifFileType *GifFileOut = (GifFileType*) oc;

  if ( EGifPutLine(GifFileOut, (unsigned char*)String_val(buf), GifFileOut->Image.Width) 
       == GIF_ERROR ){
    // PrintGifError ();
    failwith("EGifPutLine");
  }
  CAMLreturn0;
}

void eGifPutExtension( value oc, value ext )
{
  CAMLparam2(oc,ext);
  CAMLlocal1(l);

  GifFileType *GifFileOut = (GifFileType*) oc;
  int extCode;
  int extLen;
  char **extension;
  int i;

  extCode = Int_val(Field(ext,0));
  extLen = list_length( Field(ext,1) );
  if( (extension = malloc(sizeof(char*) * extLen)) == NULL ){
    failwith("EGifPutExtension");
  }
  for( i=0, l= Field(ext,1); i<extLen; i++, l= Field(l,1)){
    int len;
    char *str;
    len = string_length( Field(l,0) );
    if( len > 255 ){
      failwith("EGifPutExtension: strlen > 255");
    }
    if( (str = malloc(len + 1)) == NULL ){
      failwith("EGifPutExtension");
    }
    str[0] = len;
    memcpy(str+1, String_val(Field(l,0)), len);
    extension[i] = str;
  }
  if( EGifPutExtension(GifFileOut, extCode, extLen, extension) == GIF_ERROR ){
    for(i=0; i<extLen; i++){
      free(extension[i]);
    }
    free(extension);
    failwith("EGifPutExtension");
  }
  CAMLreturn0;
}

#endif // HAS_GIF
