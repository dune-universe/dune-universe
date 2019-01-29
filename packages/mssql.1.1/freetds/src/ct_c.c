/*
  This file is part of ocaml-freetds - An OCaml binding to the FreeTDS library
  Copyright (C) 2004 Kenneth Knowles
  
  ocaml-freetds is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.
  
  ocaml-freetds is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public License
  along with ocaml-freetds; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* http://infocenter.sybase.com/help/index.jsp?topic=/com.sybase.help.ocs_12.5.1.comlib/html/comlib/X31916.htm

   http://infocenter.sybase.com/help/index.jsp?topic=/com.sybase.help.sdk_12.5.1.ctref/html/ctref/X55618.htm */

#include <ctpublic.h>
#include <cspublic.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

void mltds_ct_ctx_finalize(value ctx);
void mltds_ct_con_finalize(value conn);
void mltds_ct_cmd_finalize(value cmd);
void mltds_binding_buffer_finalize(value buf);

/* Definitions of custom structs */
#define DEFINE_CUSTOM_OPERATIONS(name, finalize)                \
  static struct custom_operations name##_operations = {         \
    "freetds/ct/cs_" #name,                                     \
    finalize,                                                   \
    custom_compare_default,                                     \
    custom_hash_default,                                        \
    custom_serialize_default,                                   \
    custom_deserialize_default                                  \
};

DEFINE_CUSTOM_OPERATIONS(context, mltds_ct_ctx_finalize)
DEFINE_CUSTOM_OPERATIONS(connection, mltds_ct_con_finalize)
DEFINE_CUSTOM_OPERATIONS(command, mltds_ct_cmd_finalize)
DEFINE_CUSTOM_OPERATIONS(binding_buffer, mltds_binding_buffer_finalize)

#define BUFFER_CONTENTS(buff,cast_to) (*(cast_to*)(buff->data))

struct binding_buffer {
    CS_DATAFMT fmt;
    CS_INT real_type;
    void* data; /* The max len is stored as fmt.maxlength */
    CS_INT copied;
    CS_SMALLINT indicator;
};

/* Helper functions */

/* Raises exceptions when the retval isn't what it should be */
void retval_inspect(char* whichfunc, CS_RETCODE retval)
{
    switch(retval)
    {
    case CS_SUCCEED:
        return;
    case CS_FAIL:
        failwith(whichfunc);

#ifdef CS_NOMSG
/* Not supported in (at least some versions of) freetds */
    case CS_NOMSG:
      failwith("Internal error - CS_NOMSG returned (This should never be possible)");
#endif

    case CS_END_RESULTS:
        raise_constant(*caml_named_value("cs_end_results"));
    case CS_END_DATA:
        raise_constant(*caml_named_value("cs_end_data"));
    case CS_CANCELED:
        raise_constant(*caml_named_value("cs_cancelled"));
    }
}

#define context_ptr(ctx) *(CS_CONTEXT**)Data_custom_val(ctx)
#define connection_ptr(conn) *(CS_CONNECTION**)Data_custom_val(conn)
#define command_ptr(cmd) *(CS_COMMAND**)Data_custom_val(cmd)
#define buffer_ptr(buf) *(struct binding_buffer**)Data_custom_val(buf)

/* Conversions to and from C structs */
CS_INT conprop_of_value(value field)
{
    CAMLparam1(field);

    if (field == hash_variant("Username"))      CAMLreturn(CS_USERNAME);
    else if (field == hash_variant("Password")) CAMLreturn(CS_PASSWORD);
    else /* if (field == hash_variant("Appname"))*/  CAMLreturn(CS_APPNAME);
}

CS_INT cmdtype_of_value(value cmdtype)
{
    CAMLparam1(cmdtype);
    
    if ( cmdtype == hash_variant("Lang") )
        CAMLreturn(CS_LANG_CMD);
    else /* if ( cmdtype == hash_variant("Rpc")) */
        CAMLreturn(CS_RPC_CMD);
}

int datatype_of_value(value datatype)
{
    CAMLparam1(datatype);
    
    if ( datatype == hash_variant("Char") )             CAMLreturn(CS_CHAR_TYPE);
    else if ( datatype == hash_variant("Int") )         CAMLreturn(CS_INT_TYPE);
    else if ( datatype == hash_variant("SmallInt") )    CAMLreturn(CS_SMALLINT_TYPE);
    else if ( datatype == hash_variant("TinyInt") )     CAMLreturn(CS_TINYINT_TYPE);
    else if ( datatype == hash_variant("Money") )       CAMLreturn(CS_MONEY_TYPE);
    else if ( datatype == hash_variant("DateTime") )    CAMLreturn(CS_DATETIME_TYPE);
    else if ( datatype == hash_variant("Numeric") )     CAMLreturn(CS_NUMERIC_TYPE);
    else if ( datatype == hash_variant("Decimal") )     CAMLreturn(CS_DECIMAL_TYPE);
    else if ( datatype == hash_variant("DateTime4") )   CAMLreturn(CS_DATETIME4_TYPE);
    else if ( datatype == hash_variant("Money4") )      CAMLreturn(CS_MONEY4_TYPE);
    else if ( datatype == hash_variant("Image") )       CAMLreturn(CS_IMAGE_TYPE);
    else if ( datatype == hash_variant("Binary") )      CAMLreturn(CS_BINARY_TYPE);
    else if ( datatype == hash_variant("Bit") )         CAMLreturn(CS_BIT_TYPE);
    else if ( datatype == hash_variant("Real") )        CAMLreturn(CS_REAL_TYPE);
    else if ( datatype == hash_variant("Float") )       CAMLreturn(CS_FLOAT_TYPE);
    else if ( datatype == hash_variant("Text") )        CAMLreturn(CS_TEXT_TYPE);
    else if ( datatype == hash_variant("VarChar") )     CAMLreturn(CS_VARCHAR_TYPE);
    else if ( datatype == hash_variant("VarBinary") )   CAMLreturn(CS_VARBINARY_TYPE);
    else if ( datatype == hash_variant("LongChar") )    CAMLreturn(CS_LONGCHAR_TYPE);
    else if ( datatype == hash_variant("LongBinary") )  CAMLreturn(CS_LONGBINARY_TYPE);
    else if ( datatype == hash_variant("Long") )        CAMLreturn(CS_LONG_TYPE);
    else if ( datatype == hash_variant("Illegal") )     CAMLreturn(CS_ILLEGAL_TYPE);
    else if ( datatype == hash_variant("Sensitivity") ) CAMLreturn(CS_SENSITIVITY_TYPE);
    else if ( datatype == hash_variant("Boundary") )    CAMLreturn(CS_BOUNDARY_TYPE);
    else if ( datatype == hash_variant("Void") )        CAMLreturn(CS_VOID_TYPE);
    else if ( datatype == hash_variant("UShort") )      CAMLreturn(CS_USHORT_TYPE);

#ifdef CS_UNIQUE_TYPE
/* Not supported in (at least some versions of) sybase */
    else if ( datatype == hash_variant("Unique") )      CAMLreturn(CS_UNIQUE_TYPE);
#endif

    CAMLreturn(CS_ILLEGAL_TYPE);
}

value value_of_indicator(CS_INT indicator)
{
    CAMLparam0 ();
    switch(indicator)
    {
    case CS_NULLDATA:
        CAMLreturn(hash_variant("NullData"));
    case CS_GOODDATA:
    default:
        CAMLreturn(hash_variant("GoodData"));
    }
        
}

value value_of_datatype(int datatype)
{
    CAMLparam0 ();
    switch(datatype)
    {
    case CS_CHAR_TYPE:        CAMLreturn(hash_variant("Char"));
    case CS_INT_TYPE:         CAMLreturn(hash_variant("Int"));
    case CS_SMALLINT_TYPE:    CAMLreturn(hash_variant("SmallInt"));
    case CS_TINYINT_TYPE:     CAMLreturn(hash_variant("TinyInt"));
    case CS_MONEY_TYPE:       CAMLreturn(hash_variant("Money"));
    case CS_DATETIME_TYPE:    CAMLreturn(hash_variant("DateTime"));
    case CS_NUMERIC_TYPE:     CAMLreturn(hash_variant("Numeric"));
    case CS_DECIMAL_TYPE:     CAMLreturn(hash_variant("Decimal"));
    case CS_DATETIME4_TYPE:   CAMLreturn(hash_variant("DateTime4"));
    case CS_MONEY4_TYPE:      CAMLreturn(hash_variant("Money4"));
    case CS_IMAGE_TYPE:       CAMLreturn(hash_variant("Image"));
    case CS_BINARY_TYPE:      CAMLreturn(hash_variant("Binary"));
    case CS_BIT_TYPE:         CAMLreturn(hash_variant("Bit"));
    case CS_REAL_TYPE:        CAMLreturn(hash_variant("Real"));
    case CS_FLOAT_TYPE:       CAMLreturn(hash_variant("Float"));
    case CS_TEXT_TYPE:        CAMLreturn(hash_variant("Text"));
    case CS_VARCHAR_TYPE:     CAMLreturn(hash_variant("VarChar"));
    case CS_VARBINARY_TYPE:   CAMLreturn(hash_variant("VarBinary"));
    case CS_LONGCHAR_TYPE:    CAMLreturn(hash_variant("LongChar"));
    case CS_LONGBINARY_TYPE:  CAMLreturn(hash_variant("LongBinary"));
    case CS_LONG_TYPE:        CAMLreturn(hash_variant("Long"));
    case CS_ILLEGAL_TYPE:     CAMLreturn(hash_variant("Illegal"));
    case CS_SENSITIVITY_TYPE: CAMLreturn(hash_variant("Sensitivity"));
    case CS_BOUNDARY_TYPE:    CAMLreturn(hash_variant("Boundary"));
    case CS_VOID_TYPE:        CAMLreturn(hash_variant("Void"));
    case CS_USHORT_TYPE:      CAMLreturn(hash_variant("UShort"));

#ifdef CS_UNIQUE_TYPE
/* Not supported in (at least some versions of) sybase */    
    case CS_UNIQUE_TYPE:      CAMLreturn(hash_variant("Unique"));
#endif
    }
    CAMLreturn(hash_variant("Illegal"));
}

/* TODO: inspect the option variant or something,
   or write a wrapper that returns CS_UNUSED
   CS_INT cmdoption_of_value(value cmdoption)
   {
   CAMLparam1(cmdoption);
   if ( cmdoption
   CAMLreturn(CS_UNUSED);
   if ( cmdoption == hash_variant("Recompile") )
   CAMLreturn(CS_RECOMPILE);
   else if ( cmdoption == hash_variant("NoRecompile") )
   CAMLreturn(CS_NORECOMPILE);
   }
*/

value value_of_restype(CS_INT restype)
{
    CAMLparam0 ();
    
    if ( restype == CS_ROW_RESULT )
        CAMLreturn( hash_variant("Row") );

    if ( restype == CS_PARAM_RESULT )
        CAMLreturn( hash_variant("Param") );
    
    if ( restype == CS_STATUS_RESULT )
        CAMLreturn( hash_variant("Status") );

    if ( restype == CS_CMD_DONE )
        CAMLreturn( hash_variant("Cmd_done") );
    
    if ( restype != CS_CMD_SUCCEED )
        raise_constant(*caml_named_value("cs_cmd_fail"));

    CAMLreturn(Val_unit);
}

value resinfo_type_of_value(value resinfo)
{
    CAMLparam1(resinfo);

    if ( resinfo == hash_variant("Row_count") )
        CAMLreturn(CS_ROW_COUNT);
    else if ( resinfo == hash_variant("Cmd_number") )
        CAMLreturn(CS_CMD_NUMBER);
    else /* if ( resinfo == hash_variant("Numdata") ) */
        CAMLreturn(CS_NUMDATA);
}


static value cons(value val1, value val2)
{
    CAMLparam2(val1,val2);
    CAMLlocal1(result);

    result = alloc(2, Tag_cons);
    Store_field(result, 0, val1);
    Store_field(result, 1, val2);
    
    CAMLreturn(result);
}

#define car(x) Field(x, 0)
#define cdr(x) Field(x, 1)

#if 0
/* Doesn't work on sybase (locales are abstract types), and doesn't seem to be needed anyway.
   In FreeTDS they are transparent types, but they aren't supposed to be, really
 */   

#define LOCALE_LANG    0
#define LOCALE_CHARSET 1
#define LOCALE_TIME    2
#define LOCALE_COLLATE 3
#define LOCALE_SIZE 4

value value_of_locale(CS_LOCALE locale)
{
    CAMLparam0 ();
    CAMLlocal1(result);
    
    result = alloc(LOCALE_SIZE, 0);
    Store_field(result, LOCALE_LANG, copy_string(locale.language));
    Store_field(result, LOCALE_CHARSET, copy_string(locale.charset));
    Store_field(result, LOCALE_TIME, copy_string(locale.time));
    Store_field(result, LOCALE_COLLATE, copy_string(locale.collate));

    CAMLreturn(result);
}

CS_LOCALE locale_of_value(value locale)
{
    CAMLparam1(locale);
    CS_LOCALE result;

    result.language = String_val(Field(locale, LOCALE_LANG));
    result.charset = String_val(Field(locale, LOCALE_CHARSET));
    result.time = String_val(Field(locale, LOCALE_TIME));
    result.collate = String_val(Field(locale, LOCALE_COLLATE));
        
    CAMLreturn(result);
}

#endif

value value_of_status_bitmask(CS_INT status)
{
    CAMLparam0 ();
    CAMLlocal1(result);
    
    result = Val_emptylist;

    if ( status & CS_CANBENULL )
        result = cons(hash_variant("CanBeNull"), result);
    
    if ( status & CS_NODATA )
        result = cons(hash_variant("NoData"), result);
    
    if ( status & CS_IDENTITY )
        result = cons(hash_variant("Identity"), result);

    if ( status & CS_RETURN )
        result = cons(hash_variant("Return"), result);

    CAMLreturn(result);
}

CS_INT status_of_value(value status)
{
    CAMLparam1(status);
    CS_INT result = 0;
    CAMLlocal1(stat);

    if ( status == Val_emptylist ) CAMLreturn(0);
    else {
        stat = car(status);
        
        if ( stat == hash_variant("CanBeNull") )
            result = CS_CANBENULL;
        else if ( stat == hash_variant("NoData") )
            result = CS_NODATA;
        else if ( stat == hash_variant("Identity") )
            result = CS_IDENTITY;
        else if ( stat == hash_variant("Return") )
            result = CS_RETURN;
        
        /* Not tail recursive, but the list should be brief */
        CAMLreturn(result | status_of_value(cdr(status)));
    }
}

#define COL_NAME   0
#define COL_STATUS 1
#define COL_BUFFER 2
#define COL_SIZE   3

value column_of_buffer(struct binding_buffer* buf)
{
    CAMLparam0 ();
    CAMLlocal2(result, buffer);
    
    buffer = alloc_custom(&binding_buffer_operations,
                          sizeof(struct binding_buffer*), 0, 1);
    buffer_ptr(buffer) = buf;

    result = alloc(COL_SIZE, 0);
    Store_field(result, COL_NAME, copy_string(buf->fmt.name));
    Store_field(result, COL_STATUS, value_of_status_bitmask(buf->fmt.format));
    Store_field(result, COL_BUFFER, buffer);
    
    CAMLreturn(result);
}

/*****************************/
/* The Bindings              */
/*****************************/

/*** Allocation ***/
CAMLprim value mltds_cs_ctx_create(value unit)
{
    CAMLparam1(unit);
    CS_CONTEXT* context;
    CAMLlocal1(result);
    
    retval_inspect( "cs_ctx_alloc", cs_ctx_alloc(CS_VERSION_100, &context) );
    retval_inspect( "ct_init", ct_init(context, CS_VERSION_100) );

    result = alloc_custom(&context_operations, sizeof(CS_CONTEXT*), 0, 1);
    context_ptr(result) = context;
    
    CAMLreturn(result);
}

CAMLprim value mltds_ct_con_alloc(value context)
{
    CAMLparam1(context);
    CS_CONNECTION* conn;
    CAMLlocal1(result);
    
    retval_inspect( "ct_con_alloc", ct_con_alloc(context_ptr(context), &conn) );

    retval_inspect( "ct_diag",
                    ct_diag(conn, CS_INIT, CS_UNUSED, CS_UNUSED, NULL) );

    result = alloc_custom(&connection_operations, sizeof(CS_CONNECTION*), 0, 1);
    connection_ptr(result) = conn;

    CAMLreturn(result);
}

CAMLprim value mltds_ct_cmd_alloc(value conn)
{
    CAMLparam1(conn);
    CS_COMMAND* command;
    CAMLlocal1(result);

    retval_inspect( "ct_cmd_alloc", 
                    ct_cmd_alloc(connection_ptr(conn), &command) );

    result = alloc_custom(&command_operations, sizeof(CS_COMMAND*), 0, 1);
    command_ptr(result) = command;
    
    CAMLreturn(result);
}




CAMLprim value mltds_ct_con_setstring(value conn, value field, value newval)
{
    CAMLparam3(conn, field, newval);
    
    retval_inspect( "ct_con_props",
                    ct_con_props(connection_ptr(conn), 
                                 CS_SET, 
                                 conprop_of_value(field), 
                                 String_val(newval), 
                                 string_length(newval), 
                                 NULL));
    
    CAMLreturn(Val_unit);
}

CAMLprim value mltds_ct_connect(value connection, value servername)
{
    CAMLparam2(connection, servername);

    retval_inspect( "ct_connect",
                    ct_connect(connection_ptr(connection), 
                               String_val(servername), 
                               string_length(servername)));
    
    CAMLreturn(Val_unit);
}



CAMLprim value mltds_ct_command(value cmd, value cmdtype, value option, value text)
{
    CAMLparam4(cmd, cmdtype, option, text);

    retval_inspect( "ct_command",
                    ct_command(command_ptr(cmd),
                               cmdtype_of_value(cmdtype),
                               String_val(text),
                               string_length(text),
                               CS_UNUSED /*cmdoption_of_value(option)*/) );
    CAMLreturn( Val_unit );
}


CAMLprim value mltds_ct_send(value cmd)
{
    CAMLparam1(cmd);
    retval_inspect( "ct_send", ct_send(command_ptr(cmd)));
    CAMLreturn( Val_unit );
}

CAMLprim value mltds_ct_results(value cmd)
{
    CAMLparam1(cmd);
    CS_INT restype;
    
    retval_inspect( "ct_results", ct_results(command_ptr(cmd), &restype) );

    CAMLreturn(value_of_restype(restype));
}
          
CAMLprim value mltds_ct_res_info(value cmd, value resinfo_type)
{
    CAMLparam2(cmd, resinfo_type);
    CS_INT resinfo;
    
    retval_inspect( "ct_res_info", ct_res_info(command_ptr(cmd),
                                               resinfo_type_of_value(resinfo_type),
                                               &resinfo,
                                               CS_UNUSED,
                                               NULL) );

    CAMLreturn ( Val_int(resinfo) );
}

CAMLprim value mltds_ct_bind( value cmd, value maxlen, value index )
{
    CAMLparam3(cmd, maxlen, index);
    CS_DATAFMT fmt = {{0,0,0,0,0,0,0,0,0,0,0}};
    struct binding_buffer* buf = malloc(sizeof(struct binding_buffer));
    buf->fmt = fmt;
    
    retval_inspect( "ct_describe",
                    ct_describe( command_ptr(cmd), Int_val(index), &(buf->fmt) ) );
    
    /* NOTE: the datatype in dataformat with coerce the ct library to
       convert the data to the appropriate type, so anything we don't want to
       cast to caml we let FreeTDS cast for us */
    buf->real_type = buf->fmt.datatype;
    
    switch(buf->fmt.datatype)
    {
    case CS_BIT_TYPE: 
        /* No change */
        break;

    case CS_TINYINT_TYPE:
    case CS_SMALLINT_TYPE:
    case CS_INT_TYPE:
        /* All become 32 bit ints, even though ocamldbi only uses 31 bit ints */
        buf->fmt.datatype = CS_INT_TYPE;
        break;

    case CS_REAL_TYPE:
    case CS_FLOAT_TYPE:
        buf->fmt.datatype = CS_FLOAT_TYPE;
        break;

    case CS_DATETIME_TYPE:
    case CS_DATETIME4_TYPE:
    case CS_MONEY_TYPE:
    case CS_MONEY4_TYPE:
    case CS_NUMERIC_TYPE:
    case CS_DECIMAL_TYPE:
        /* I don't really understand the DECIMAL type so I let FreetDS cast to a string
           and then load in the ocamldbi Decimal.t type */
    case CS_TEXT_TYPE:
    case CS_CHAR_TYPE:
    case CS_VARCHAR_TYPE:
        buf->fmt.datatype = CS_CHAR_TYPE;
        buf->fmt.maxlength = Int_val(maxlen);
        break;
            
    case CS_IMAGE_TYPE:
    case CS_BINARY_TYPE:
    case CS_VARBINARY_TYPE:
        buf->fmt.datatype = CS_BINARY_TYPE;
        buf->fmt.maxlength = Int_val(maxlen);
        break;
    }
    
    buf->data = malloc(maxlen);

    retval_inspect( "ct_bind",
                    ct_bind(command_ptr(cmd),
                            Int_val(index),
                            &(buf->fmt),
                            (buf->data),
                            &(buf->copied),
                            &(buf->indicator)));
    
    CAMLreturn(column_of_buffer(buf));
}

CAMLprim value mltds_buffer_contents( value buffer )
{
    CAMLparam1(buffer);
    CAMLlocal2(result, str);

    
    struct binding_buffer* buf = buffer_ptr(buffer);
    
    if ( buf->indicator == CS_NULLDATA )
    {
        CAMLreturn(hash_variant("Null"));
    }
    
    /* There are many more cases here than necessary, mostly because
       I wrote it before I learned about the ct-lib being able to do all
       the conversions for me */
    switch(buf->fmt.datatype)
    {
    case CS_BIT_TYPE:
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Bool"));
        Store_field(result, 1, Val_bool((int) BUFFER_CONTENTS(buf, CS_BIT)));
        CAMLreturn(result);

    case CS_TINYINT_TYPE:
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Tinyint"));
        Store_field(result, 1, Val_int((int) BUFFER_CONTENTS(buf, CS_TINYINT)));
        CAMLreturn(result);

    case CS_SMALLINT_TYPE:
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Smallint"));
        Store_field(result, 1, Val_int((int) BUFFER_CONTENTS(buf, CS_SMALLINT)));
        CAMLreturn(result);
        
    case CS_INT_TYPE:
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Int"));
        Store_field(result, 1, copy_int32((int) BUFFER_CONTENTS(buf, CS_INT)));
        CAMLreturn(result);
        
    case CS_FLOAT_TYPE:
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Float"));
        Store_field(result, 1, copy_double((double) BUFFER_CONTENTS(buf, CS_FLOAT)));
        CAMLreturn(result);

    case CS_REAL_TYPE:
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Real"));
        Store_field(result, 1, copy_double((double) BUFFER_CONTENTS(buf, CS_REAL)));
        CAMLreturn(result);

    case CS_TEXT_TYPE:
    case CS_CHAR_TYPE:
    case CS_VARCHAR_TYPE:
        switch (buf->real_type)
        {
        case CS_MONEY_TYPE:
        case CS_MONEY4_TYPE:
        case CS_NUMERIC_TYPE:
        case CS_DECIMAL_TYPE:
        case CS_FLOAT_TYPE:
        case CS_REAL_TYPE:
            str = alloc_string(buf->copied);
            strncpy(String_val(str), (char*)(buf->data), buf->copied);
            
            result = alloc(2, 0);
            Store_field(result, 0, hash_variant("Decimal"));
            Store_field(result, 1, str);
            CAMLreturn(result);

        case CS_TEXT_TYPE:
        case CS_CHAR_TYPE:
        case CS_VARCHAR_TYPE:
        default:
            str = alloc_string(buf->copied);
            strncpy(String_val(str), (char*)(buf->data), buf->copied);
            
            result = alloc(2, 0);
            Store_field(result, 0, hash_variant("String"));
            Store_field(result, 1, str);
            CAMLreturn(result);
            
        }
        break;

    case CS_IMAGE_TYPE:
    case CS_BINARY_TYPE:
    case CS_VARBINARY_TYPE:
    case CS_DATETIME_TYPE:
    case CS_DATETIME4_TYPE:
    case CS_MONEY_TYPE:
    case CS_MONEY4_TYPE:
    case CS_NUMERIC_TYPE:
    case CS_DECIMAL_TYPE:
    default:
        str = alloc_string(buf->copied);
        strncpy(String_val(str), (char*)(buf->data), buf->copied);
        
        result = alloc(2, 0);
        Store_field(result, 0, hash_variant("Binary"));
        Store_field(result, 1, str);
        CAMLreturn(result);
    }
}

CAMLprim value mltds_ct_fetch( value cmd )
{
    CAMLparam1(cmd);
    CS_INT rows_read;

    /* These are actually CS_UNUSED according to the docs! */
    retval_inspect( "ct_fetch",
                    ct_fetch(command_ptr(cmd),
                             CS_UNUSED,
                             CS_UNUSED,
                             CS_UNUSED,
                             &rows_read) );

    CAMLreturn( Int_val(rows_read) );
}

/* Since only one option is meaningful, simplify to a bool */
CAMLprim value mltds_ct_close( value conn, value force )
{
    CAMLparam2(conn, force);
    CS_INT option = CS_UNUSED;
    if ( Val_bool(force) ) option = CS_FORCE_CLOSE;
    
    retval_inspect( "ct_close",
                    ct_close(connection_ptr(conn),
                             option) );

    CAMLreturn(Val_unit);
}


/* Keep in sync with the OCaml definition of [severity]. */
value value_of_severity(CS_INT severity)
{
    CAMLparam0();

    switch(severity)
    {
    case CS_SV_INFORM:        CAMLreturn(Int_val(0));
    case CS_SV_API_FAIL:      CAMLreturn(Int_val(1));
    case CS_SV_RETRY_FAIL:    CAMLreturn(Int_val(2));
    case CS_SV_RESOURCE_FAIL: CAMLreturn(Int_val(3));
    case CS_SV_COMM_FAIL:     CAMLreturn(Int_val(4));
    case CS_SV_INTERNAL_FAIL: CAMLreturn(Int_val(5));
    /* CS_SV_FATAL */
    default:                  CAMLreturn(Int_val(6));
    }
}

static value get_client_message(CS_CONNECTION* conn, CS_INT msgno)
{
    CAMLparam0();
    CAMLlocal2(result,str);
    CS_CLIENTMSG msg;

    retval_inspect(
      "ct_diag", ct_diag(conn, CS_GET, CS_CLIENTMSG_TYPE, msgno, &msg) );

    /*str = caml_copy_string(msgtext);*/
    str = alloc_string(msg.msgstringlen);
    strncpy(String_val(str), msg.msgstring, msg.msgstringlen ); 
    
    result = alloc(2, 0);
    Store_field(result, 0, value_of_severity(msg.severity));
    Store_field(result, 1, str); 
    
    CAMLreturn(result);
}

static value get_server_message(CS_CONNECTION* conn, CS_INT msgno)
{
    CAMLparam0();
    CAMLlocal2(result,str);
    CS_SERVERMSG msg;

    retval_inspect(
      "ct_diag", ct_diag(conn, CS_GET, CS_SERVERMSG_TYPE, msgno, &msg) );

    /*str = caml_copy_string(msgtext);*/
    str = alloc_string(strnlen(msg.text, CS_MAX_MSG) + 1);
    strncpy(String_val(str), msg.text, strnlen(msg.text, CS_MAX_MSG));
    
    result = alloc(2, 0);
    Store_field(result, 0, value_of_severity(msg.severity));
    Store_field(result, 1, str);
    
    CAMLreturn(result);
}


CAMLexport value mltds_add_messages_client(value vconnection, value vlist)
{
  CAMLparam2(vconnection, vlist);
  CS_CONNECTION* conn = connection_ptr(vconnection);
  CS_INT msgcount;
  CS_INT msgno;
    
  retval_inspect(
    "ct_diag",
    ct_diag(conn, CS_STATUS, CS_CLIENTMSG_TYPE, CS_UNUSED, &msgcount));

  for(msgno = msgcount; msgno > 0; msgno--)
    vlist = cons(get_client_message(conn, msgno), vlist);

  retval_inspect(
    "ct_diag",
    ct_diag(conn, CS_CLEAR, CS_CLIENTMSG_TYPE, CS_UNUSED, NULL));
  CAMLreturn(vlist);
}

CAMLexport value mltds_add_messages_server(value vconnection, value vlist)
{
  CAMLparam2(vconnection, vlist);
  CS_CONNECTION* conn = connection_ptr(vconnection);
  CS_INT msgcount;
  CS_INT msgno;

  retval_inspect(
    "ct_diag",
    ct_diag(conn, CS_STATUS, CS_SERVERMSG_TYPE, CS_UNUSED, &msgcount) );

  for(msgno = msgcount; msgno > 0; msgno--)
    vlist = cons(get_server_message(conn, msgno), vlist);
        
  retval_inspect(
    "ct_diag",
    ct_diag(conn, CS_CLEAR, CS_SERVERMSG_TYPE, CS_UNUSED, NULL) );

  CAMLreturn(vlist);
}

/*** Deallocation ***/

/* FIXME: the programmer should not have to take care about memory. */
/* Note: the programmer must not allow the context to be
   GC'd until all connections are closed, or they'll be killed. */
void mltds_ct_ctx_finalize(value context)
{
    CS_CONTEXT* ctx = Data_custom_val(context);

    ct_exit(ctx, CS_FORCE_EXIT);
    cs_ctx_drop(ctx);
}

void mltds_ct_con_finalize(value connection)
{
    CS_CONNECTION* conn = Data_custom_val(connection);

    ct_close(conn, CS_FORCE_CLOSE);
    ct_con_drop(conn);
}

void mltds_ct_cmd_finalize(value cmd)
{
    ct_cmd_drop(Data_custom_val(cmd));
}

void mltds_binding_buffer_finalize(value buffer)
{
    struct binding_buffer* buf = buffer_ptr(buffer);
    free(buf->data);
    free(buf);
}
