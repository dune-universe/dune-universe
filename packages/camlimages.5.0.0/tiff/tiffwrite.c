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

#ifdef HAS_TIFF

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

// This is to resolve the conflict of these int types in caml/config.h and tiff.h
// Replace all the occurrences of (u?int[0-9]+) by $1_tiff, tiff.h's int types have
// now different names from caml's.
#undef int16
#undef uint16
#undef int32
#undef uint32
#undef int64
#undef uint64
#define int16 int16_tiff
#define uint16 uint16_tiff
#define int32 int32_tiff
#define uint32 uint32_tiff
#define int64 int64_tiff
#define uint64 uint64_tiff

#include <tiffio.h>

extern value *imglib_error;

value open_tiff_file_for_write( value file,
			        value width,
			        value height,
			        value resolution )
{
  CAMLparam4(file,width,height,resolution);
  int image_width;
  int image_height;
  double res;
  TIFF* tif;

  image_width = Int_val( width );
  image_height = Int_val( height );
  res = Double_val( resolution );

  tif = TIFFOpen(String_val( file ), "w");
  if( tif ){
    /* needs */
    /* Resolution */
    /* FillOrder */
    
    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, (uint32) image_width);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, (uint32) image_height);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 3);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_LZW); /* LZW */
    TIFFSetField(tif, TIFFTAG_PREDICTOR, 2); /* ??? */
    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, image_height); /* ??? */

    TIFFSetField(tif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
    TIFFSetField(tif, TIFFTAG_XRESOLUTION, res);
    TIFFSetField(tif, TIFFTAG_YRESOLUTION, res);

    CAMLreturn( (value) tif);
  } else {
    failwith("failed to open tiff file to write");
  }
}

value write_tiff_scanline( tiffh, buf, row )
     value tiffh;
     value buf;
     value row;
{
  CAMLparam3(tiffh,buf,row);
  TIFFWriteScanline((TIFF*)tiffh, String_val(buf), Int_val(row), 0);
  CAMLreturn(Val_unit);
}

#endif // HAS_TIFF
