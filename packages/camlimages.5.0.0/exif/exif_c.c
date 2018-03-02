/***********************************************************************/
/*                                                                     */
/*                           CamlImages                                */
/*                                                                     */
/*                          Jun Furuse                                 */
/*                                                                     */
/*  Copyright 1999-2013                                                */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

#include "../config/config.h"

#ifdef HAS_EXIF

#include <stdio.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <libexif/exif-byte-order.h>
#include <libexif/exif-data-type.h>
#include <libexif/exif-ifd.h>
#include <libexif/exif-log.h>
#include <libexif/exif-tag.h>
#include <libexif/exif-content.h>
#include <libexif/exif-mnote-data.h>
#include <libexif/exif-mem.h>

value Val_ExifBytes(unsigned char *p, value vsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,Val_int(p[i]));
    }
    CAMLreturn(res);
}

value Val_ExifSBytes(signed char *p, value vsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,Val_int(p[i]));
    }
    CAMLreturn(res);
}

value Val_ExifShorts(unsigned short *p, value vsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,Val_int(p[i]));
    }
    CAMLreturn(res);
}

value Val_ExifSShorts(short *p, value vsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,Val_int(p[i]));
    }
    CAMLreturn(res);
}

value Val_ExifLongs(unsigned long *p, value vsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,caml_copy_int64(p[i]) /* too big... */ );
    }
    CAMLreturn(res);
}

value Val_ExifSLongs(long *p, value vsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,caml_copy_int32(p[i]) /* too big... */ );
    }
    CAMLreturn(res);
}

value Val_ExifRationals(unsigned long *p, value vsize)
{
    CAMLparam0();
    CAMLlocal2(res,tmp);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        tmp = alloc(2,0);
        Store_field(tmp,0, caml_copy_int64(p[i*2]) /* too big... */ );
        Store_field(tmp,1, caml_copy_int64(p[i*2+1]) /* too big... */ );
        Store_field(res,i, tmp);
    }
    CAMLreturn(res);
}

value Val_ExifSRationals(long *p, value vsize)
{
    CAMLparam0();
    CAMLlocal2(res,tmp);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        tmp = alloc(2,0);
        Store_field(tmp,0,caml_copy_int32(p[i*2]) /* too big... */ );
        Store_field(tmp,1,caml_copy_int32(p[i*2]+1) /* too big... */ );
        Store_field(res,i,tmp);
    }
    CAMLreturn(res);
}

value Val_ExifFloats(float *p, value vsize)
{
    CAMLparam0();
    CAMLlocal2(res,tmp);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i,caml_copy_double(p[i]));
    }
    CAMLreturn(res);
}

value Val_ExifDoubles(double *p, value vsize)
{
    CAMLparam0();
    CAMLlocal2(res,tmp);
    int i;
    res = alloc(Int_val(vsize),0);
    for(i=0; i<Int_val(vsize); i++){
        Store_field(res,i, caml_copy_double(p[i]));
    }
    CAMLreturn(res);
}

value caml_exif_tag_get_name_in_ifd(value tag, value ifd)
{
    return caml_copy_string(exif_tag_get_name_in_ifd(Int_val(tag), Int_val(ifd)));
}

//////////


value caml_val_exif_data(value string)
{
    CAMLparam1(string);
    CAMLlocal1(res);
    ExifData *data = exif_data_new_from_data(String_val(string), 
                                             caml_string_length(string));

    if( !data ){ failwith("exif_data_new_from_data"); }

    // exif_data_free(data);

    //fprintf(stderr, "data=%x\n", data);
    res = alloc_small(1,0);
    Field(res,0) = (value)data;

    CAMLreturn(res);
}

void caml_exif_set_byte_order(value data, value order)
{
    exif_data_set_byte_order((ExifData *)Field(data, 0), Int_val(order));
}

value caml_exif_get_byte_order(value data)
{
    CAMLparam1(data);
    CAMLreturn(Val_int(exif_data_get_byte_order((ExifData *)Field(data, 0))));
}

void caml_exif_data_fix(value data)
{
    exif_data_fix((ExifData *)Field(data, 0));
}

void caml_exif_data_unref(value data)
{
    exif_data_unref((ExifData*)Field(data, 0));
}
 
void caml_exif_data_dump(value data)
{
    exif_data_dump((ExifData*)Field(data, 0));
}

value caml_exif_data_contents(value vdata)
{
    CAMLparam1(vdata);
    CAMLlocal3(res, tmp, tmp2);
    int i;
    ExifData *data = (ExifData*)Field(vdata,0);
    //fprintf(stderr, "data=%x\n", data);
    res = alloc_tuple(EXIF_IFD_COUNT);
    for(i=0; i< EXIF_IFD_COUNT; i++){
        ExifContent *p = data->ifd[i];
        if( p ){ 
            exif_content_ref(p);
            tmp = alloc_small(1,0);
            Field(tmp,0) = (value)p;
            tmp2 = alloc_small(1,0);
            //fprintf(stderr, "content=%x (count=%d)\n", p, p->count);
            Field(tmp2,0) = tmp;
            Store_field(res,i,tmp2);
        } else {
            Store_field(res,i,Val_int(0));
        }
    }
    CAMLreturn(res);
}

void caml_exif_content_unref(value vdata)
{
    ExifContent *c = (ExifContent*)Field(vdata,0);
    exif_content_unref(c);
}

value caml_exif_content_entries(value vdata)
{
    CAMLparam1(vdata);
    CAMLlocal3(res, tmp, tmp2);
    int i;
    ExifContent *c = (ExifContent *)Field(vdata,0);
    //fprintf(stderr, "content=%x (count=%d)\n", c, c->count);
    res = Val_int(0); // null
    for(i=c->count-1; i>=0; i--){
        ExifEntry *e = c->entries[i];
        if( e ){ 
            exif_entry_ref(e);
            
            // boxing
            tmp = alloc_small(1,0);
            Field(tmp,0) = (value)e;

            // cons
            tmp2 = alloc_small(2,0);
            Field(tmp2,0) = tmp;
            Field(tmp2,1) = res;

            // rewire
            res = tmp2;
        }
    }
    CAMLreturn(res);
}

void caml_exif_entry_unref(value vdata)
{
    ExifEntry *c = (ExifEntry*)Field(vdata,0);
    exif_entry_unref(c);
}

value caml_exif_decode_entry(value vdata)
{
    CAMLparam1(vdata);
    CAMLlocal1(tpl);
    ExifEntry *p = (ExifEntry *)Field(vdata,0);

    tpl = alloc_tuple(4);
    Store_field(tpl,0,Val_int(p->tag));
    Store_field(tpl,1,Val_int(p->format));
    Store_field(tpl,2,Val_int(p->components)); // hope it never overflow...
    Store_field(tpl,3,alloc_string(p->size));
    memcpy(String_val(Field(tpl,3)), p->data, p->size);
    CAMLreturn(tpl);
}

#else // HAS_EXIF

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define NA(x) value x(){ failwith("unsupported"); }

NA(Val_ExifBytes)
NA(Val_ExifSBytes)
NA(Val_ExifShorts)
NA(Val_ExifSShorts)
NA(Val_ExifLongs)
NA(Val_ExifSLongs)
NA(Val_ExifRationals)
NA(Val_ExifSRationals)
NA(Val_ExifFloats)
NA(Val_ExifDoubles)
NA(caml_exif_tag_get_name_in_ifd)
NA(caml_val_exif_data)
NA(caml_exif_set_byte_order)
NA(caml_exif_get_byte_order)
NA(caml_exif_data_fix)
NA(caml_exif_data_unref)
NA(caml_exif_data_dump)
NA(caml_exif_data_contents)
NA(caml_exif_content_unref)
NA(caml_exif_content_entries)
NA(caml_exif_entry_unref)
NA(caml_exif_decode_entry)

#endif // HAS_EXIF
