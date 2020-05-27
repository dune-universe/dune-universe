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

#ifdef HAS_JPEG

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdio.h>
#include <setjmp.h>

/*
 * Include file for users of JPEG library.
 * You will need to have included system headers that define at least
 * the typedefs FILE and size_t before you can include jpeglib.h.
 * (stdio.h is sufficient on ANSI-conforming systems.)
 * You may also wish to include "jerror.h".
 */

#include "jpeglib.h"

/*
 * Sample routine for JPEG compression.  We assume that the target file name
 * and a compression quality factor are passed in.
 */

#ifdef DEBUG_JPEG
#  define DEBUGF(...) do{fprintf(stderr, __VA_ARGS__); fflush(stderr); }while(0)
#else
#  define DEBUGF(...) do{/*do nothing*/}while(0)
#endif

extern char jpg_error_message[JMSG_LENGTH_MAX];
struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */

  jmp_buf setjmp_buffer;	/* for return to caller */
};
typedef struct my_error_mgr * my_error_ptr;
extern void my_error_exit (j_common_ptr);

value open_jpeg_file_for_write_colorspace( name, width, height, qual, colorspace )
     value name;
     value width;
     value height;
     value qual;
     J_COLOR_SPACE colorspace;
{
  CAMLparam0();
  CAMLlocal1(res);
  char *filename;
  int image_height;
  int image_width;
  int quality;

  struct jpeg_compress_struct* cinfop;
  struct my_error_mgr *jerrp;
  /* More stuff */
  FILE * outfile;		/* source file */

  image_width= Int_val( width );
  image_height= Int_val( height );
  filename= String_val( name );
  quality= Int_val(qual);
 
  if ((outfile = fopen(filename, "wb")) == NULL) {
    failwith("failed to open jpeg file");
  }

  cinfop = malloc(sizeof (struct jpeg_compress_struct));
  jerrp = malloc(sizeof (struct my_error_mgr));
  cinfop->err = jpeg_std_error(&jerrp->pub);
  jerrp->pub.error_exit = my_error_exit;

  if (setjmp(jerrp->setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_compress(cinfop);
    free(jerrp);
    fclose(outfile);
    failwith(jpg_error_message);
  }

  jpeg_create_compress(cinfop);
  jpeg_stdio_dest(cinfop, outfile);

  cinfop->image_width= image_width; 
  cinfop->image_height= image_height; 
  cinfop->input_components = (colorspace == JCS_RGB ? 3 : 4);
  cinfop->in_color_space= colorspace /* JCS_RGB or JCS_CMYK */;
  jpeg_set_defaults(cinfop);
  jpeg_set_quality(cinfop, quality, TRUE);
  jpeg_start_compress(cinfop, TRUE);

  res = alloc_small(3,0);
  Field(res, 0) = (value)cinfop;
  Field(res, 1) = (value)outfile;
  Field(res, 2) = (value)jerrp;

  /*
  fprintf(stderr, "cinfop= %d outfile= %d %d %d \n", cinfop, infile, cinfop->output_scanline, cinfop->output_height); 
  fflush(stderr);
  */
  CAMLreturn(res);
}

value open_jpeg_file_for_write( name, width, height, qual )
     value name;
     value width;
     value height;
     value qual;
{
  return
    open_jpeg_file_for_write_colorspace( name, width, height, qual, JCS_RGB );
}

value open_jpeg_file_for_write_cmyk( name, width, height, qual )
     value name;
     value width;
     value height;
     value qual;
{
  return
    open_jpeg_file_for_write_colorspace( name, width, height, qual, JCS_CMYK );
}

void caml_jpeg_write_marker( value jpegh, value raw )
{
    struct jpeg_compress_struct *cinfop;
    cinfop = (struct jpeg_compress_struct *) Field( jpegh, 0 );

    // EXTERN(void) jpeg_write_marker
	// JPP((j_compress_ptr cinfo, int marker,
    // const JOCTET * dataptr, unsigned int datalen));
    
    int code = Int_val(Field(raw,0));
    char *data = String_val(Field(raw,1));
    unsigned int len = caml_string_length(Field(raw,1));
    jpeg_write_marker(cinfop, code, data, len); // This actually writes bytes, so data seems to be ok being GCed.
}

// You can write special markers immediately following the datastream header by
// calling jpeg_write_marker() after jpeg_start_compress() and before the first
// call to jpeg_write_scanlines().  When you do this, the markers appear after
// the SOI and the JFIF APP0 and Adobe APP14 markers (if written), but before
// all else.  Specify the marker type parameter as "JPEG_COM" for COM or
// "JPEG_APP0 + n" for APPn.  (Actually, jpeg_write_marker will let you write
// any marker type, but we don't recommend writing any other kinds of marker.)
// For example, to write a user comment string pointed to by comment_text:
// 	jpeg_write_marker(cinfo, JPEG_COM, comment_text, strlen(comment_text));

value write_jpeg_scanline( jpegh, buf )
value jpegh, buf;
{
  struct jpeg_compress_struct *cinfop;
  JSAMPROW row[1];

  cinfop = (struct jpeg_compress_struct *) Field( jpegh, 0 );

  row[0] = String_val( buf );

  jpeg_write_scanlines( cinfop, row, 1 );
  return Val_unit;
}

value close_jpeg_file_for_write( jpegh )
     value jpegh;
{
  struct jpeg_compress_struct *cinfop;
  struct my_error_mgr *jerrp;
  FILE *outfile;

  DEBUGF( "closing\n");

  cinfop = (struct jpeg_compress_struct *) Field( jpegh, 0 );
  outfile = (FILE *) Field( jpegh, 1 );
  jerrp = (struct my_error_mgr *) Field( jpegh, 2 );

  DEBUGF( "cinfop= %d outfile= %d %d %d \n", cinfop, outfile, cinfop->next_scanline, cinfop->image_height); 

   if( cinfop->next_scanline >= cinfop->image_height ){ 
     DEBUGF( "finish\n");
     jpeg_finish_compress( cinfop );
   }
  DEBUGF( "destroy\n");
  jpeg_destroy_compress( cinfop ); 
  
  free(cinfop);
  free(jerrp);
  DEBUGF( "file close\n");
  fclose(outfile);

  return Val_unit;
}

#endif // HAS_JPEG
