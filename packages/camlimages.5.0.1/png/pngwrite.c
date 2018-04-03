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

/* $Id: Exp */

#include "../config/config.h"

#ifdef HAS_PNG

#include <png.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value write_png_file_rgb( name, buffer, width, height, with_alpha )
     value name;
     value buffer;
     value width;
     value height;
     value with_alpha;
{
  CAMLparam5 ( name, buffer, width, height, with_alpha );

  FILE *fp;
  png_structp png_ptr;
  png_infop info_ptr;

  int w, h;
  int a;

  w = Int_val(width);
  h = Int_val(height);
  a = Bool_val(with_alpha);

  if (( fp = fopen(String_val(name), "wb")) == NULL ){
    failwith("png file open failed");
  }

  if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                        NULL, NULL, NULL)) == NULL ){
    fclose(fp);
    failwith("png_create_write_struct");
  }

  if( (info_ptr = png_create_info_struct(png_ptr)) == NULL ){
    fclose(fp);
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    failwith("png_create_info_struct");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fclose(fp);
    /* If we get here, we had a problem writing the file */
    failwith("png write error");
  }

  /* use standard C stream */
  png_init_io(png_ptr, fp);

  /* we use system default compression */
  /* png_set_filter( png_ptr, 0, PNG_FILTER_NONE |
     PNG_FILTER_SUB | PNG_FILTER_PAETH ); */
  /* png_set_compression...() */

  png_set_IHDR( png_ptr, info_ptr, w, h,
                8 /* fixed */,
                a ? PNG_COLOR_TYPE_RGB_ALPHA : PNG_COLOR_TYPE_RGB, /* fixed */
                PNG_INTERLACE_ADAM7,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT );

  /* infos... */

  png_write_info(png_ptr, info_ptr);

  {
    int rowbytes, i;
    png_bytep *row_pointers;
    char *buf = String_val(buffer);

    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * h);

    rowbytes= png_get_rowbytes(png_ptr, info_ptr);
#if 0
    printf("rowbytes= %d width=%d\n", rowbytes, w);
#endif
    for(i=0; i< h; i++){
      row_pointers[i] = (png_bytep)(buf + rowbytes * i);
    }

    png_write_image(png_ptr, row_pointers);
    stat_free((void*)row_pointers);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);

  CAMLreturn(Val_unit);
}

void PngPalette_val( value cmap, png_colorp *pltep, int *lenp )
{
  int i;

  if( cmap == Atom(0) ){
    *pltep = NULL;
    *lenp = 0;
    return;
  }
  *lenp = Wosize_val( cmap );
  *pltep = malloc( sizeof( png_color ) * *lenp );

  for(i=0; i< *lenp; i++){
    (*pltep)[i].red = Int_val(Field(Field(cmap,i),0));
    (*pltep)[i].green = Int_val(Field(Field(cmap,i),1));
    (*pltep)[i].blue = Int_val(Field(Field(cmap,i),2));
  }
  return;
}

value write_png_file_index( name, buffer, cmap, width, height )
     value name;
     value buffer;
     value cmap;
     value width;
     value height;
{
  CAMLparam5 ( name, buffer, cmap, width, height );

  FILE *fp;
  png_structp png_ptr;
  png_infop info_ptr;

  int w, h;

  w = Int_val(width);
  h = Int_val(height);

  if (( fp = fopen(String_val(name), "wb")) == NULL ){
    failwith("png file open failed");
  }

  if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                        NULL, NULL, NULL)) == NULL ){
    fclose(fp);
    failwith("png_create_write_struct");
  }

  if( (info_ptr = png_create_info_struct(png_ptr)) == NULL ){
    fclose(fp);
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    failwith("png_create_info_struct");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fclose(fp);
    /* If we get here, we had a problem writing the file */
    failwith("png write error");
  }

  /* use standard C stream */
  png_init_io(png_ptr, fp);

  /* we use system default compression */
  /* png_set_filter( png_ptr, 0, PNG_FILTER_NONE |
     PNG_FILTER_SUB | PNG_FILTER_PAETH ); */
  /* png_set_compression...() */

  png_set_IHDR( png_ptr, info_ptr, w, h,
                8 /* fixed */,
                PNG_COLOR_TYPE_PALETTE, /* fixed */
                PNG_INTERLACE_ADAM7,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT );

  {
    png_colorp palette;
    int num_palette;

    PngPalette_val( cmap, &palette, &num_palette );

    if( num_palette <= 0 ){
      png_destroy_write_struct(&png_ptr, &info_ptr);
      fclose(fp);
      /* If we get here, we had a problem writing the file */
      failwith("png write error (null colormap)");
    }
    png_set_PLTE( png_ptr, info_ptr, palette, num_palette );
  }

  /* infos... */

  png_write_info(png_ptr, info_ptr);

  {
    int rowbytes, i;
    png_bytep *row_pointers;
    char *buf = String_val(buffer);

    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * h);

    rowbytes= png_get_rowbytes(png_ptr, info_ptr);
#if 0
    printf("rowbytes= %d width=%d\n", rowbytes, w);
#endif

    if( rowbytes != w && rowbytes != w * 2 ){
      png_destroy_write_struct(&png_ptr, &info_ptr);
      fclose(fp);
      /* If we get here, we had a problem writing the file */
      failwith("png write error (illegal byte/pixel)");
    }
    for(i=0; i< h; i++){
      row_pointers[i] = (png_bytep)(buf + rowbytes * i);
    }

    png_write_image(png_ptr, row_pointers);
    stat_free((void*)row_pointers);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);

  CAMLreturn(Val_unit);
}

#endif // HAS_PNG
