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

/* $Id: pngread.c,v 1.5 2009/07/04 04:02:42 furuse Exp $ */

#include "../config/config.h"

#ifdef HAS_PNG

#include "string.h"
#include "../include/oversized.h"
#include <png.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define PNG_TAG_RGB24 0
#define PNG_TAG_RGBA32 1
#define PNG_TAG_INDEX8 2
#define PNG_TAG_INDEX16 3
#define PNG_TAG_INDEX4 4

value read_png_file_as_rgb24( name )
     value name;
{
  CAMLparam1 ( name );
  CAMLlocal3 ( res,r,tmp );

  char *filename;
  png_structp png_ptr;
  png_infop info_ptr;
  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  FILE *fp;
  size_t rowbytes;

  filename = String_val( name );

  if (( fp = fopen(filename, "rb")) == NULL ){
    failwith("png file open failed");
  }

   png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
                                    NULL, NULL, NULL);
  /* (void *)user_error_ptr, user_error_fn, user_warning_fn); */

  if( png_ptr == NULL ){
    fclose(fp);
    failwith("it is not a png file.");
  }

  info_ptr = png_create_info_struct(png_ptr);
  if(info_ptr == NULL ){
    fclose(fp);
    png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
    failwith("not enough memory");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    /* If we get here, we had a problem reading the file */
    failwith("png read error");
  }

  /* use standard C stream */
  png_init_io(png_ptr, fp);

  /* png_set_sig_bytes(png_ptr, sig_read (= 0) ); */

  png_read_info(png_ptr, info_ptr);

  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, NULL, NULL);

  if (oversized(width, height)){
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    failwith_oversized("png");
  }

  if ( color_type == PNG_COLOR_TYPE_GRAY ||
       color_type == PNG_COLOR_TYPE_GRAY_ALPHA ) {
    png_set_gray_to_rgb(png_ptr);
  }
  if ( color_type & PNG_COLOR_TYPE_PALETTE ) png_set_expand(png_ptr);
  if ( bit_depth == 16 ) png_set_strip_16(png_ptr);
  if ( color_type & PNG_COLOR_MASK_ALPHA ) png_set_strip_alpha(png_ptr);

  png_read_update_info(png_ptr, info_ptr);

  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, NULL, NULL);

  if ( color_type != PNG_COLOR_TYPE_RGB || bit_depth != 8 ) {
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    failwith("unsupported color type");
  }

  rowbytes = png_get_rowbytes(png_ptr, info_ptr);

  // rowbytes * height should be the maximum malloc size in this function
  if (oversized(rowbytes, height) || oversized(sizeof(png_bytep), height)){
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    failwith("png error: image contains oversized or bogus width and height");
  }
  
  {
    int i;
    png_bytep *row_pointers;
    void * buf;

    row_pointers = (png_bytep*) stat_alloc(sizeof(png_bytep) * height);
    buf = stat_alloc( rowbytes * height );
    for( i = 0; i < height; i ++ ){
      row_pointers[i] = buf + rowbytes * i;
    }
    png_set_rows(png_ptr, info_ptr, row_pointers);

    /* Later, we can return something */
    if (setjmp(png_jmpbuf(png_ptr))) {
      /* Free all of the memory associated with the png_ptr and info_ptr */
      png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose(fp);
      /* If we get here, we had a problem reading the file */
      fprintf(stderr, "png short file\n");
      stat_free(row_pointers);
      stat_free(buf);
      CAMLreturn(res);
    }

    png_read_image(png_ptr, row_pointers);
    png_read_end(png_ptr, info_ptr);
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);

    r = alloc_tuple(height);
    for( i = 0; i < height; i ++ ){
      tmp = caml_alloc_string(rowbytes);
      memcpy(String_val(tmp), buf+rowbytes*i, rowbytes);
      Store_field( r, i, tmp );
    }
    res = alloc_small(3,0);
    Field( res, 0 ) =  Val_int(width);
    Field( res, 1 ) =  Val_int(height);
    Field( res, 2 ) =  r;

    /* close the file */
    fclose(fp);

    stat_free((void*)row_pointers);
    stat_free(buf);
    CAMLreturn(res);
  }
}

value Val_PngColor( png_color *col )
{
  CAMLparam0();
  CAMLlocal1(res);
  CAMLlocalN(r,3);

  int i;

  res = Val_int(0);

  r[0] = Val_int( col->red );
  r[1] = Val_int( col->green );
  r[2] = Val_int( col->blue );
  res = alloc_small(3,0);
  for(i=0; i<3; i++) Field(res, i) = r[i];

  CAMLreturn(res);
}

value Val_PngPalette( png_colorp plte, int len )
{
  CAMLparam0();
  CAMLlocal1(cmap);
  int i;

  if ( len != 0 ) {
    cmap = alloc_tuple( len );
    for(i= 0; i< len; i++){
        Store_field(cmap, i, Val_PngColor( &plte[i] ));
    }
  } else {
    cmap = Atom(0);
  }

  CAMLreturn(cmap);
}

value read_png_file( name )
     value name;
{
  CAMLparam1 ( name );
  CAMLlocal4 ( res,r1,r2,tmp );

  char *filename;
  png_structp png_ptr;
  png_infop info_ptr;
  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  FILE *fp;
  size_t rowbytes;

  filename = String_val( name );

  if (( fp = fopen(filename, "rb")) == NULL ){
    failwith("png file open failed");
  }

   png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
                                    NULL, NULL, NULL);
  /* (void *)user_error_ptr, user_error_fn, user_warning_fn); */

  if( png_ptr == NULL ){
    fclose(fp);
    failwith("it is not a png file.");
  }

  info_ptr = png_create_info_struct(png_ptr);
  if(info_ptr == NULL ){
    fclose(fp);
    png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
    failwith("not enough memory");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    /* If we get here, we had a problem reading the file */
    failwith("png read error");
  }

  /* use standard C stream */
  png_init_io(png_ptr, fp);

  /* png_set_sig_bytes(png_ptr, sig_read (= 0) ); */

  png_read_info(png_ptr, info_ptr);

  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, NULL, NULL);

  if (oversized(width, height)){
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    failwith_oversized("png");
  }

  if ( color_type == PNG_COLOR_TYPE_GRAY ||
       color_type == PNG_COLOR_TYPE_GRAY_ALPHA ) {
    png_set_gray_to_rgb(png_ptr);
  }
  /* We have no support for 48bit depth colors yet */
  if ( bit_depth == 16 ) png_set_strip_16(png_ptr);

  png_read_update_info(png_ptr, info_ptr);

  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, NULL, NULL);

  rowbytes = png_get_rowbytes(png_ptr, info_ptr);

  // rowbytes * height should be the maximum malloc size in this function
  if (oversized(rowbytes, height) || oversized(sizeof(png_bytep), height)){
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose(fp);
    failwith_oversized("png");
  }
  
  {
    int i;
    png_bytep *row_pointers;
    void *buf;
    char mesg[256];

    buf = stat_alloc(rowbytes * height);
    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * height);
    for(i=0; i<height; i++){
      row_pointers[i] = buf + rowbytes * i; 
    }
    png_set_rows(png_ptr, info_ptr, row_pointers);

    /* Later, we can return something */
    if (setjmp(png_jmpbuf(png_ptr))) {
      /* Free all of the memory associated with the png_ptr and info_ptr */
      png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose(fp);
      /* If we get here, we had a problem reading the file */
      fprintf(stderr, "png short file\n");
      stat_free((void*)row_pointers);
      stat_free(buf);
      CAMLreturn(res);
    }
    png_read_image(png_ptr, row_pointers);

    res = alloc_tuple(3);

    switch(color_type){
    case PNG_COLOR_TYPE_PALETTE:

      {
        png_colorp palette;
        int num_palette;
        int tag;

/*
fprintf(stderr, "pngread.c: indexed image\n"); fflush(stderr);
*/
        png_get_PLTE( png_ptr, info_ptr, &palette, &num_palette );

/*
fprintf(stderr, "pngread.c: byte/pix= %d/%d\n", (int)(rowbytes), (int)width); fflush(stderr);
*/

        if ( rowbytes == width ){
          tag = PNG_TAG_INDEX8;
        } else if ( rowbytes == width * 2 ){
          tag = PNG_TAG_INDEX8;
        } else if ( rowbytes * 2 == width || rowbytes * 2 == width + 1 ) {
          tag = PNG_TAG_INDEX4;
        } else {
          png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
          fclose(fp);
          sprintf(mesg, "png error (unsupported bytes/pixel=%d/%d)",
                  (int)rowbytes, (int)width);
	  stat_free(buf);
          stat_free((void*)row_pointers);
          failwith(mesg);
        }

        r1 = alloc( 2, tag );
        r2 = alloc_tuple(height);
        for( i = 0; i < height; i ++ ){
            tmp = caml_alloc_string(rowbytes);
            memcpy(String_val(tmp), buf+rowbytes*i, rowbytes);
            Store_field( r2, i, tmp );
        }
        Store_field( r1, 0, r2 );
        Store_field( r1, 1, Val_PngPalette( palette, num_palette ) );

        Store_field( res, 0, Val_int(width) );
        Store_field( res, 1, Val_int(height) );
        Store_field( res, 2, r1 );

      }
      break;

    case PNG_COLOR_TYPE_RGB:
    case PNG_COLOR_TYPE_RGB_ALPHA:
      /*
        fprintf(stderr, "pngread.c: rgb image\n"); fflush(stderr);
sc      fprintf(stderr, "width rowbytes: %d %d\n", width, rowbytes); fflush(stderr);
      */
      r1 = alloc( 1,
                  color_type == PNG_COLOR_TYPE_RGB ?
                                PNG_TAG_RGB24 : PNG_TAG_RGBA32 );
      r2 = alloc_tuple( height );
      for( i = 0; i < height; i ++ ){
          tmp = caml_alloc_string(rowbytes);
          memcpy(String_val(tmp), buf+rowbytes*i, rowbytes);
          Store_field( r2, i, tmp );
      }
      Store_field( r1, 0, r2 );
      Store_field( res, 0, Val_int(width) );
      Store_field( res, 1, Val_int(height) );
      Store_field( res, 2, r1 );
      break;

    default:
      sprintf(mesg, "png error (unsupported color_type=%d)",
              (int)color_type);
      stat_free(buf);
      stat_free((void*)row_pointers);
      failwith(mesg);
    }

    png_read_end(png_ptr, info_ptr);
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);

    /* close the file */
    fclose(fp);
    stat_free(buf);
    stat_free((void*)row_pointers);

    CAMLreturn(res);
  }
}

#else // HAS_PNG

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value read_png_file_as_rgb24(){ failwith("unsupported"); }
value Val_PngColor(){ failwith("unsupported"); }
value Val_PngPalette(){ failwith("unsupported"); }
value read_png_file(){ failwith("unsupported"); }
value write_png_file_rgb(){ failwith("unsupported"); }
value write_png_file_index(){ failwith("unsupported"); }

#endif // HAS_PNG
