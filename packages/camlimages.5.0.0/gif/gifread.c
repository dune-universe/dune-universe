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

#include "../include/oversized.h"

#include <stdio.h>
#include <string.h>

#include <gif_lib.h>

// GIFLIB_MAJOR is only defined in libgif >= 4.2.0
#if !defined(GIFLIB_MAJOR)
#  define GIFLIB_MAJOR 4
#endif

value Val_GifColorType( GifColorType *color )
{
  CAMLparam0();
  CAMLlocal1(res);
  CAMLlocalN(r,3);
  int i;

  r[0] = Val_int( color->Red );
  r[1] = Val_int( color->Green );
  r[2] = Val_int( color->Blue );
  res = alloc_small(3,0);
  for(i=0; i<3; i++) Field(res, i) = r[i];
#ifdef DEBUG_GIF
fprintf(stderr, "Color(%d,%d,%d)\n", color->Red, color->Green, color->Blue);
fflush(stderr);
#endif
  CAMLreturn(res);
}

value Val_ColorMapObject( ColorMapObject *colorMap )
{
  CAMLparam0();
  CAMLlocal1(cmap);
  int i;

  if ( colorMap != NULL ){
#ifdef DEBUG_GIF
    fprintf(stderr, "Colormap(%d)...\n", colorMap->ColorCount);
    fflush(stderr);
#endif 

    cmap = alloc_tuple(colorMap->ColorCount);
    for(i= 0; i< colorMap->ColorCount; i++){
        Store_field(cmap,i, Val_GifColorType( &colorMap->Colors[i] ));
    }
  } else {
#ifdef DEBUG_GIF
    fprintf(stderr, "Colormap(0)...\n");
    fflush(stderr);
#endif 
    cmap = Atom(0); /* cmap = alloc_tuple(0); is WRONG!! */
  }
  CAMLreturn(cmap);
}

value Val_GifImageDesc( GifImageDesc *imageDesc )
{
  CAMLparam0();
  CAMLlocal1(res);
  CAMLlocalN(r,6);
  int i;

#ifdef DEBUG_GIF
fprintf(stderr, "imagedesc...\n");
fflush(stderr);
#endif

/*
    {
      int Len,i,j;
      Len = 1 << imageDesc->ColorMap->BitsPerPixel;
      for (i = 0; i < Len; i+=4) {
	for (j = 0; j < 4 && j < Len; j++) {
	  printf("%3d: %02xh %02xh %02xh   ", i + j,
		 imageDesc->ColorMap->Colors[i + j].Red,
		 imageDesc->ColorMap->Colors[i + j].Green,
		 imageDesc->ColorMap->Colors[i + j].Blue);
	}
	printf("\n");
      }
    }
*/


  r[0] = Val_int( imageDesc->Left );
  r[1] = Val_int( imageDesc->Top );
  r[2] = Val_int( imageDesc->Width );
  r[3] = Val_int( imageDesc->Height );
  r[4] = Val_int( imageDesc->Interlace );
  r[5] = Val_ColorMapObject( imageDesc->ColorMap );
  res = alloc_small(6,0);
  for(i=0; i<6; i++) Field(res, i) = r[i];
  CAMLreturn(res);
}

value Val_ScreenInfo( GifFileType *GifFile )
{
  CAMLparam0();
  CAMLlocal1(res);
  CAMLlocalN(r,5);

  int i;

  r[0] = Val_int(GifFile->SWidth);
  r[1] = Val_int(GifFile->SHeight);
  r[2] = Val_int(GifFile->SColorResolution);
  r[3] = Val_int(GifFile->SBackGroundColor);
  r[4] = Val_ColorMapObject(GifFile->SColorMap);
  res = alloc_small(5,0);
  for(i=0; i<5; i++) Field(res, i) = r[i];

  CAMLreturn(res);
}

value dGifOpenFileName( value name )
{
  CAMLparam1(name);
  CAMLlocal1(res);
  CAMLlocalN(r,2);

  GifFileType *GifFile;
  int i;

#if (GIFLIB_MAJOR <= 4)
    GifFile = DGifOpenFileName( String_val(name) );
#else
    GifFile = DGifOpenFileName( String_val(name), NULL);
#endif

  if(GifFile == NULL){
    failwith("DGifOpenFileName");
  }

  r[0] = Val_ScreenInfo( GifFile );
  r[1] = (value) GifFile;
  res = alloc_small(2,0);
  for(i=0; i<2; i++) Field(res, i) = r[i];

  CAMLreturn(res);
} 

void dGifCloseFile( value hdl )
{
  CAMLparam1(hdl);

  /* For the bug libungif/giflib 4.1.0 */
  /* This may add a new memory leak, but it is better than having 
     segmentation faults */
  ((GifFileType *)hdl)->Image.ColorMap = NULL; 

#if (GIFLIB_MAJOR <= 4)
  DGifCloseFile( (GifFileType *) hdl);
#else
  DGifCloseFile( (GifFileType *) hdl, NULL );
#endif
  CAMLreturn0;
}

value dGifGetRecordType( value hdl )
{
  CAMLparam1(hdl);

  GifRecordType RecordType;
  if (DGifGetRecordType((GifFileType*) hdl, &RecordType) == GIF_ERROR) {
    failwith("DGifGetRecordType");
  }
  CAMLreturn(Val_int(RecordType));
}

value dGifGetImageDesc( value hdl )
{
  CAMLparam1(hdl);

  if (DGifGetImageDesc( (GifFileType*) hdl )  == GIF_ERROR){
    failwith("DGIFGetImageDesc");
  }
  CAMLreturn(Val_GifImageDesc( &((GifFileType*) hdl)-> Image ));
}

value dGifGetLine( value hdl )
{
  CAMLparam1(hdl);
  CAMLlocal1(buf);

  GifFileType *GifFile = (GifFileType*) hdl;

  if( oversized( GifFile->Image.Width, sizeof(GifPixelType) ) ){
    failwith_oversized("gif");
  }
  buf = alloc_string( GifFile->Image.Width * sizeof(GifPixelType) ); 

  if( DGifGetLine(GifFile, (unsigned char*)String_val(buf), GifFile->Image.Width ) 
      == GIF_ERROR ){
    // PrintGifError ();
    failwith("DGifGetLine");
  }
  CAMLreturn(buf);
}

value dGifGetExtension( value hdl )
{
  CAMLparam1(hdl);
  CAMLlocal3(ext,exts,res);
  CAMLlocal1(newres);

  GifFileType *GifFile = (GifFileType*) hdl;
  int func;
  GifByteType *extData;

  exts = Val_int(0);

  if (DGifGetExtension(GifFile,&func, &extData) == GIF_ERROR){
    failwith("DGifGetExtension");
  }

  while( extData != NULL ){
    ext= alloc_string(extData[0]);
    memcpy(String_val(ext), &extData[1], extData[0]);
    newres = alloc_small(2,0);
    Field(newres, 0)= ext;
    Field(newres, 1)= exts;
    exts= newres;
    DGifGetExtensionNext(GifFile, &extData);
  }
  res = alloc_small(2,0);
  Field(res,0) = Val_int(func);
  Field(res,1) = exts;

  CAMLreturn(res);
}

#else // HAS_GIF

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value Val_GifColorType(){ failwith("unsupported"); }
value Val_ColorMapObject(){ failwith("unsupported"); }
value Val_GifImageDesc(){ failwith("unsupported"); }
value Val_ScreenInfo(){ failwith("unsupported"); }
value dGifOpenFileName(){ failwith("unsupported"); }
value dGifCloseFile(){ failwith("unsupported"); }
value dGifGetRecordType(){ failwith("unsupported"); }
value dGifGetImageDesc(){ failwith("unsupported"); }
value dGifGetLine(){ failwith("unsupported"); }
value dGifGetExtension(){ failwith("unsupported"); }
value eGifOpenFileName(){ failwith("unsupported"); }
value eGifCloseFile(){ failwith("unsupported"); }
value eGifPutScreenDesc(){ failwith("unsupported"); }
value eGifPutImageDesc(){ failwith("unsupported"); }
value eGifPutLine(){ failwith("unsupported"); }
value eGifPutExtension(){ failwith("unsupported"); }

#endif // HAS_GIF
