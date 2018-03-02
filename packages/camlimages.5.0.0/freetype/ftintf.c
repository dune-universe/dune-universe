/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Jun Furuse, projet Cristal, INRIA Rocquencourt           */
/*                                                                     */
/*  Copyright 1999,2000                                                */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

#include "../config/config.h"

#ifdef HAS_FREETYPE

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <ft2build.h>
#include FT_FREETYPE_H

value init_FreeType()
{
  CAMLparam0();
  FT_Library *library;

  if( (library = stat_alloc( sizeof(FT_Library) )) == NULL ){
    failwith( "init_FreeType: Memory over" );
  }
  if( FT_Init_FreeType( library ) ){
    stat_free(library);
    failwith( "FT_Init_FreeType" );
  }
  CAMLreturn( (value) library );
}

value done_FreeType( library )
     value library;
{
  CAMLparam1(library);
  if ( FT_Done_FreeType( *(FT_Library *)library ) ){
    failwith( "FT_Done_FreeType" );
  }
  stat_free( (void *) library );
  CAMLreturn(Val_unit);
}

#include <stdio.h>
value new_Face( library, fontpath, idx )
     value library;
     value fontpath;
     value idx;
{
  CAMLparam3(library, fontpath, idx );
  FT_Face *face;

  if( (face = stat_alloc( sizeof(FT_Face) )) == NULL ){
    failwith( "new_Face: Memory over" );
  }
  if( FT_New_Face( *(FT_Library *)library, String_val( fontpath ), Int_val( idx ), face ) ){
    stat_free(face);
    failwith( "new_Face: Could not open face" );
  }
  CAMLreturn( (value) face );
}

value face_info( facev )
     value facev;
{
  CAMLparam1(facev);
  CAMLlocal1(res);

  FT_Face face = *(FT_Face *)facev;
  res = alloc_tuple(14);
  Store_field(res, 0, Val_int( face->num_faces ));
  Store_field(res, 1, Val_int( face->num_glyphs ));
  Store_field(res, 2, copy_string( face->family_name == NULL ? "" : face->family_name ));
  Store_field(res, 3, copy_string( face->style_name == NULL ? "" : face->style_name ));
  Store_field(res, 4, Val_bool( FT_HAS_HORIZONTAL( face ) ));
  Store_field(res, 5, Val_bool( FT_HAS_VERTICAL( face ) ));
  Store_field(res, 6, Val_bool( FT_HAS_KERNING( face ) ));
  Store_field(res, 7, Val_bool( FT_IS_SCALABLE( face ) ));
  Store_field(res, 8, Val_bool( FT_IS_SFNT( face ) ));
  Store_field(res, 9, Val_bool( FT_IS_FIXED_WIDTH( face ) ));
  Store_field(res,10, Val_bool( FT_HAS_FIXED_SIZES( face ) ));
  Store_field(res,11, Val_bool( FT_HAS_FAST_GLYPHS( face ) ));
  Store_field(res,12, Val_bool( FT_HAS_GLYPH_NAMES( face ) ));
  Store_field(res,13, Val_bool( FT_HAS_MULTIPLE_MASTERS( face ) ));

  CAMLreturn(res);
}

value done_Face( face )
     value face;
{
  CAMLparam1(face);
  if ( FT_Done_Face( *(FT_Face *) face ) ){
    failwith("FT_Done_Face");
  }
  CAMLreturn( Val_unit );
}

value get_num_glyphs( face )
     value face;
{
  CAMLparam1(face);
  CAMLreturn( Val_int ((*(FT_Face *) face)->num_glyphs) );
}


value set_Char_Size( face, char_w, char_h, res_h, res_v )
     value face;
     value char_w, char_h; /* 26.6 1 = 1/64pt */
     value res_h, res_v; /* dpi */
{
  CAMLparam5( face, char_w, char_h, res_h, res_v );
  if ( FT_Set_Char_Size( *(FT_Face *) face,
		    Int_val(char_w), Int_val(char_h),
			 Int_val(res_h), Int_val(res_v) ) ){
    failwith("FT_Set_Char_Size");
  }
  CAMLreturn(Val_unit);
}

/* to be done: query at face->fixed_sizes
 */

value set_Pixel_Sizes( face, pixel_w, pixel_h )
     value face;
     value pixel_w, pixel_h; /* dot */
{
  CAMLparam3(face,pixel_w,pixel_h);
  if ( FT_Set_Pixel_Sizes( *(FT_Face *) face,
			 Int_val(pixel_w), Int_val(pixel_h) ) ){
    failwith("FT_Set_Pixel_Sizes");
  }
  CAMLreturn(Val_unit);
}

value val_CharMap( charmapp )
     FT_CharMap *charmapp;
{
  CAMLparam0();
  CAMLlocal1(res);

  res = alloc_tuple(2);
  Store_field(res,0, Val_int((*charmapp)->platform_id));
  Store_field(res,1, Val_int((*charmapp)->encoding_id));

  CAMLreturn(res);
}

value get_CharMaps( facev )
     value facev;
{
  CAMLparam1(facev);
  CAMLlocal3(list,last_cell,new_cell);
  int i = 0;
  FT_Face face;

  face = *(FT_Face *) facev;

  list = last_cell = Val_unit;

  while( i < face->num_charmaps ){
    new_cell = alloc_tuple(2);
    Store_field(new_cell,0, val_CharMap( face->charmaps + i ));
    Store_field(new_cell,1, Val_unit);
    if( i == 0 ){
      list = new_cell;
    } else {
      Store_field(last_cell,1, new_cell);
    }
    last_cell = new_cell;
    i++;
  }

  CAMLreturn(list);
}

value set_CharMap( facev, charmapv )
     value facev;
     value charmapv;
{
  CAMLparam2(facev,charmapv);
  int i = 0;
  FT_Face face;
  FT_CharMap charmap;
  int my_pid, my_eid;

  face = *(FT_Face *) facev;
  my_pid = Int_val(Field(charmapv, 0));
  my_eid = Int_val(Field(charmapv, 1));

  while( i < face->num_charmaps ){
    charmap = face->charmaps[i];
    if ( charmap->platform_id == my_pid &&
	 charmap->encoding_id == my_eid ){
      if ( FT_Set_Charmap( face, charmap ) ){
	failwith("FT_Set_Charmap");
      }
      CAMLreturn(Val_unit);
    } else {
      i++;
    }
  }
  failwith("freetype:set_charmaps: selected pid+eid do not exist");
}

value get_Char_Index( face, code )
     value face, code;
{
  CAMLparam2(face,code);
  CAMLreturn(Val_int(FT_Get_Char_Index( *(FT_Face *)face, Int_val(code))));
}

value load_Glyph( face, index, flags )
     value face, index, flags;
{
  CAMLparam3(face,index,flags);
  CAMLlocal1(res);

  if( FT_Load_Glyph( *(FT_Face *) face, Int_val(index), FT_LOAD_DEFAULT | Int_val(flags)) ){
    failwith("FT_Load_Glyph");
  }

  res = alloc_tuple(2);
  Store_field(res,0, Val_int( (*(FT_Face*)face)->glyph->advance.x ));
  Store_field(res,1, Val_int( (*(FT_Face*)face)->glyph->advance.y ));

  CAMLreturn(res);
}

value load_Char( face, code, flags )
     value face, code, flags;
{
  CAMLparam3(face,code,flags);
  CAMLlocal1(res);

  /* FT_Load_Glyph(face, FT_Get_Char_Index( face, code )) */
  if( FT_Load_Char( *(FT_Face *) face, Int_val(code), FT_LOAD_DEFAULT | Int_val(flags)) ){
    failwith("FT_Load_Char");
  }

  res = alloc_tuple(2);
  Store_field(res,0, Val_int( (*(FT_Face*)face)->glyph->advance.x ));
  Store_field(res,1, Val_int( (*(FT_Face*)face)->glyph->advance.y ));

  CAMLreturn(res);
}

value render_Glyph_of_Face( face, mode )
     value face;
     value mode;
{
  CAMLparam2(face,mode);
  if (FT_Render_Glyph( (*(FT_Face *)face)->glyph , Int_val(mode) )){
    failwith("FT_Render_Glyph");
  }
  CAMLreturn(Val_unit);
}

value render_Char( face, code, flags, mode )
     value face, code, flags, mode;
{
  CAMLparam4(face,code,flags,mode);
  CAMLlocal1(res);

  /* FT_Load_Glyph(face, FT_Get_Char_Index( face, code ), FT_LOAD_RENDER) */
  if( FT_Load_Char( *(FT_Face *) face, Int_val(code),
		    FT_LOAD_RENDER |
		    Int_val(flags) |
		    (Int_val(mode) ? FT_LOAD_MONOCHROME : 0)) ){
    failwith("FT_Load_Char");
  }

  res = alloc_tuple(2);
  Store_field(res,0, Val_int( (*(FT_Face*)face)->glyph->advance.x ));
  Store_field(res,1, Val_int( (*(FT_Face*)face)->glyph->advance.y ));

  CAMLreturn(res);
}

value set_Transform( face, vmatrix, vpen )
     value face, vmatrix, vpen;
{
  CAMLparam3(face, vmatrix, vpen);
  FT_Matrix matrix;
  FT_Vector pen;

  matrix.xx = (FT_Fixed)( Int_val(Field(vmatrix,0)) );
  matrix.xy = (FT_Fixed)( Int_val(Field(vmatrix,1)) );
  matrix.yx = (FT_Fixed)( Int_val(Field(vmatrix,2)) );
  matrix.yy = (FT_Fixed)( Int_val(Field(vmatrix,3)) );
  pen.x = (FT_Fixed)( Int_val(Field(vpen,0)) );
  pen.y = (FT_Fixed)( Int_val(Field(vpen,1)) );

  FT_Set_Transform( *(FT_Face *)face, &matrix, &pen );

  CAMLreturn(Val_unit);
}

value get_Bitmap_Info( vface )
     value vface;
{
  CAMLparam1(vface);
  CAMLlocal1(res);

  FT_GlyphSlot glyph = (*(FT_Face *)vface)->glyph;
  FT_Bitmap bitmap = glyph->bitmap;

  switch ( bitmap.pixel_mode ) {
  case ft_pixel_mode_grays:
    if ( bitmap.num_grays != 256 ){
      failwith("get_Bitmap_Info: unknown num_grays");
    }
    break;
  case ft_pixel_mode_mono:
    break;
  default:
    failwith("get_Bitmap_Info: unknown pixel mode");
  }

  res = alloc_tuple(5);
  Store_field(res,0, Val_int(glyph->bitmap_left));
  Store_field(res,1, Val_int(glyph->bitmap_top));
  Store_field(res,2, Val_int(bitmap.width));
  Store_field(res,3, Val_int(bitmap.rows));

  CAMLreturn(res);
}

value read_Bitmap( vface, vx, vy ) /* This "y" is in Y upwards convention */
     value vface, vx, vy;
{
  /* no boundary check !!! */
  int x, y;
  CAMLparam3(vface, vx, vy);

  FT_Bitmap bitmap = (*(FT_Face *)vface)->glyph->bitmap;

  unsigned char *row;

  x = Int_val(vx);
  y = Int_val(vy);

  switch ( bitmap.pixel_mode ) {
  case ft_pixel_mode_grays:
    if (bitmap.pitch > 0){
      row = bitmap.buffer + (bitmap.rows - 1 - y) * bitmap.pitch;
    } else {
      row = bitmap.buffer - y * bitmap.pitch;
    }
    CAMLreturn (Val_int(row[x]));

  case ft_pixel_mode_mono:
    if (bitmap.pitch > 0){
      row = bitmap.buffer + (bitmap.rows - 1 - y) * bitmap.pitch;
    } else {
      row = bitmap.buffer - y * bitmap.pitch;
    }
    CAMLreturn (Val_int(row[x >> 3] & (128 >> (x & 7)) ? 255 : 0));
    break;

  default:
    failwith("read_Bitmap: unknown pixel mode");
  }

}

value get_Glyph_Metrics( face )
     value face;
{
  CAMLparam1(face);
  CAMLlocal3(res1,res2,res);

  /* no soundness check ! */
  FT_Glyph_Metrics *metrics = &((*(FT_Face *)face)->glyph->metrics);

  res1 = alloc_tuple(3);
  Store_field(res1,0, Val_int(metrics->horiBearingX));
  Store_field(res1,1, Val_int(metrics->horiBearingY));
  Store_field(res1,2, Val_int(metrics->horiAdvance));

  res2 = alloc_tuple(3);
  Store_field(res2,0, Val_int(metrics->vertBearingX));
  Store_field(res2,1, Val_int(metrics->vertBearingY));
  Store_field(res2,2, Val_int(metrics->vertAdvance));

  res = alloc_tuple(4);
  Store_field(res,0, Val_int(metrics->width));
  Store_field(res,1, Val_int(metrics->height));
  Store_field(res,2, res1);
  Store_field(res,3, res2);

  CAMLreturn(res);
}

value get_Size_Metrics( face )
     value face;
{
  CAMLparam1(face);
  CAMLlocal1(res);

  FT_Size_Metrics *imetrics = &((*(FT_Face*)face)->size->metrics);

  res = alloc_tuple(4);
  Store_field(res,0, Val_int(imetrics->x_ppem));
  Store_field(res,1, Val_int(imetrics->y_ppem));
  Store_field(res,2, Val_int(imetrics->x_scale));
  Store_field(res,3, Val_int(imetrics->y_scale));

  CAMLreturn(res);
}

value get_Outline_Contents(value face) {
/* *****************************************************************

   Concrete definitions of TT_Outline might vary from version to
   version.

   This definition assumes freetype 2.0.1

     ( anyway, this function is wrong...)

 ***************************************************************** */
  CAMLparam1(face);
  CAMLlocal5(points, tags, contours, res, tmp);
  int i;

  FT_Outline* outline = &((*(FT_Face *)face)->glyph->outline);

  int n_contours = outline->n_contours;
  int n_points   = outline->n_points;

  points   = alloc_tuple(n_points);
  tags     = alloc_tuple(n_points);
  contours = alloc_tuple(n_contours);

  for( i=0; i<n_points; i++ ) {
    FT_Vector* raw_points = outline->points;
    char* raw_flags  = outline->tags;
    tmp = alloc_tuple(2);
    /* caution: 26.6 fixed into 31 bit */
    Store_field(tmp, 0, Val_int(raw_points[i].x));
    Store_field(tmp, 1, Val_int(raw_points[i].y));
    Store_field(points, i, tmp);
    if ( raw_flags[i] & FT_Curve_Tag_On ) {
      Store_field(tags, i, Val_int(0)); /* On point */
    } else if ( raw_flags[i] & FT_Curve_Tag_Cubic ) {
      Store_field(tags, i, Val_int(2)); /* Off point, cubic */
    } else {
      Store_field(tags, i, Val_int(1)); /* Off point, conic */
    }
  }

  for( i=0; i<n_contours; i++ ) {
    short* raw_contours = outline->contours;
    Store_field(contours, i, Val_int(raw_contours[i]));
  }

  res = alloc_tuple(5);
  Store_field(res, 0, Val_int(n_contours));
  Store_field(res, 1, Val_int(n_points));
  Store_field(res, 2, points);
  Store_field(res, 3, tags);
  Store_field(res, 4, contours);

  CAMLreturn(res);
}

#else // HAS_FREETYPE

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value init_FreeType(){ failwith("unsupported"); }
value done_FreeType(){ failwith("unsupported"); }
value new_Face(){ failwith("unsupported"); }
value face_info(){ failwith("unsupported"); }
value done_Face(){ failwith("unsupported"); }
value get_num_glyphs(){ failwith("unsupported"); }
value set_Char_Size(){ failwith("unsupported"); }
value set_Pixel_Sizes(){ failwith("unsupported"); }
value val_CharMap(){ failwith("unsupported"); }
value get_CharMaps(){ failwith("unsupported"); }
value set_CharMap(){ failwith("unsupported"); }
value get_Char_Index(){ failwith("unsupported"); }
value load_Glyph(){ failwith("unsupported"); }
value load_Char(){ failwith("unsupported"); }
value render_Glyph_of_Face(){ failwith("unsupported"); }
value render_Char(){ failwith("unsupported"); }
value set_Transform(){ failwith("unsupported"); }
value get_Bitmap_Info(){ failwith("unsupported"); }
value read_Bitmap(){ failwith("unsupported"); }
value get_Glyph_Metrics(){ failwith("unsupported"); }
value get_Size_Metrics(){ failwith("unsupported"); }
value get_Outline_Contents(){ failwith("unsupported"); }

#endif // HAS_FREETYPE
