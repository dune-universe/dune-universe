

#include <assert.h>
#include <string.h>

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <cairo_ocaml.h>

#include <ft2build.h>
#include FT_FREETYPE_H

CAMLprim value
ml_FT_Get_Name_Index(value font, value char_name)
{
  int index = FT_Get_Name_Index (FT_FACE_VAL (font),
                                 String_val (char_name));
  return Val_int (index);
}


CAMLprim value
ml_FT_Get_Char_Index(value font, value charcode)
{
  int index = FT_Get_Char_Index (FT_FACE_VAL (font),
                                 Long_val (charcode));
  return Val_int (index);
}

CAMLprim value
ml_FT_num_charmaps(value font)
{
    FT_Face face = FT_FACE_VAL (font);

    return Val_int (face->num_charmaps);
}

CAMLprim value
ml_FT_set_charmap(value font, value charmap_index)
{
    FT_Face face = FT_FACE_VAL (font);
    FT_CharMap charmap = (face->charmaps)[Int_val(charmap_index)];

    return Val_int (FT_Set_Charmap(face,charmap));
}
