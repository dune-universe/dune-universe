#include "sqlite3.h"

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <stdio.h>
#include <string.h>

#define WRAP(x) do{if ((x) != SQLITE_OK){\
    sqlite3_error(x);\
    return Val_unit;\
}}while(0)

void sqlite3_error (int i) {
    value *sqlite_error = caml_named_value("sqlite error");
    const char *err = sqlite3_errstr(i);
    if (err && sqlite_error){
        caml_raise_with_string(*sqlite_error, err);
    } else if (sqlite_error) {
        caml_raise_with_string(*sqlite_error, "unknown");
    } else {
        caml_failwith ("unknown sqlite error");
    }
}

// Db
value copy_s (sqlite3_value *blob, int isblob){
    int len = sqlite3_value_bytes(blob);
    value s = caml_alloc_string (len);
    const char *b = NULL;
    if (isblob){
        b = sqlite3_value_blob (blob);
    } else {
        b = sqlite3_value_text (blob);
    }

    if (b){
        memcpy(String_val(s), b, len);
    }

    return s;
}

value Null = Long_val(0);

value caml_of_sqlite (char const *data){
    sqlite3_value *v = (sqlite3_value*)data;
    value dst;

    if (v){
        switch (sqlite3_value_type(v)){
        case SQLITE_INTEGER:
            dst = caml_alloc(1, 3);
            Store_field(dst, 0, caml_copy_int64 (sqlite3_value_int64(v)));
            return dst;
        case SQLITE_FLOAT:
            dst = caml_alloc(1, 2);
            Store_field(dst, 0, caml_copy_double (sqlite3_value_double(v)));
            return dst;
        case SQLITE_BLOB:
            dst = caml_alloc(1, 0);
            Store_field(dst, 0, copy_s (v, 1));
            return dst;
        case SQLITE_TEXT:
            dst = caml_alloc(1, 1);
            Store_field(dst, 0, copy_s (v, 0));
            return dst;
        default:
            return Null;
        }
    }

    return Null;
}

void set_result (sqlite3_context *ctx, value inp){
    if (Is_long(inp)){
        sqlite3_result_null(ctx);
    } else {
        int length = 0;
        char *str;
        switch (Tag_val(inp)){
        case 0:
            length = caml_string_length (Field(inp, 0));
            sqlite3_result_blob (ctx, String_val(Field(inp, 0)), length, SQLITE_TRANSIENT);
            break;
        case 1:
            length = caml_string_length (Field(inp, 0));
            sqlite3_result_text (ctx, String_val(Field(inp, 0)), length, SQLITE_TRANSIENT);
            break;
        case 2:
            sqlite3_result_double (ctx, Double_val(Field(inp, 0)));
            break;
        case 3:
            sqlite3_result_int64 (ctx, Int64_val(Field(inp, 0)));
            break;
        default:
            caml_failwith("invalid conversion");
        }
    }
}

void call_function(sqlite3_context *ctx, int narg, sqlite3_value **args){
    value *fn = sqlite3_user_data(ctx);
    if (fn){
        value xargs = caml_alloc_array (caml_of_sqlite, (char const **)args);
        set_result (ctx, caml_callback (*fn, xargs));
    } else {
        caml_failwith("Invalid function name");
    }
}

void destroy_function (void *data){
    caml_remove_generational_global_root(data);
}

value _ezsqlite_db_create_function (value db, value name, value nargs){
    value *fn = caml_named_value (String_val(name));
    caml_register_generational_global_root(fn);
    WRAP(sqlite3_create_function_v2 ((sqlite3*)db, String_val(name), Int_val(nargs), SQLITE_UTF8, (void*)fn, &call_function, NULL, NULL, &destroy_function));
    return Val_unit;
}

int commit_hook_callback(void *data){
    value *v = caml_named_value("commit hook");

    if (v){
        return Val_int(caml_callback(*v, Val_unit));
    }

    return 0;
}

void update_hook_callback(void *data, int i, char const *a, char const *b, sqlite3_int64 rowid){
    value *v = caml_named_value("update hook");

    if (v){
        value args[] = {Val_int(i), caml_copy_string (a), caml_copy_string (b), caml_copy_int64(rowid)};
        caml_callbackN(*v,4, args);
    }
}

int auto_extension_callback(sqlite3 *db, char **err, void * _api){
    value *v = caml_named_value("auto extension");
    if (v){
        caml_callback(*v, (value)db);
    }

    sqlite3_commit_hook(db, commit_hook_callback, NULL);
    sqlite3_update_hook(db, update_hook_callback, NULL);

    return SQLITE_OK;
}

static int _inited = 0;
static void _init (){
    if (!_inited){
        sqlite3_auto_extension((void*)auto_extension_callback);
        _inited = 1;
    }
}

value _ezsqlite_db_load (value path){
    _init();
    CAMLparam1(path);
    sqlite3 *handle = NULL;
    WRAP(sqlite3_open_v2 (String_val(path), &handle, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL));
    CAMLreturn ((value)handle);
}

value _ezsqlite_db_free (value db){
    CAMLparam1(db);
    sqlite3 *handle = (sqlite3*)db;
    if (handle){
        sqlite3_close(handle);
    }
    CAMLreturn(Val_unit);
}

// Stmt
value _ezsqlite_stmt_prepare (value db, value s) {
    CAMLparam2(db, s);
    sqlite3 *handle = (sqlite3*)db;
    sqlite3_stmt *stmt = NULL;
    WRAP(sqlite3_prepare_v2(handle, String_val(s), caml_string_length(s), &stmt, NULL));
    CAMLreturn((value)stmt);
}

value _ezsqlite_stmt_finalize (value stmt) {
    CAMLparam1(stmt);
    if ((sqlite3_stmt*)stmt){
        sqlite3_finalize((sqlite3_stmt*)stmt);
    }
    CAMLreturn(Val_unit);
}

value _ezsqlite_stmt_step (value stmt){
    CAMLparam1 (stmt);
    int res = sqlite3_step ((sqlite3_stmt*)stmt);
    switch (res){
    case SQLITE_ROW:
        CAMLreturn(Val_true);
    case SQLITE_DONE:
        CAMLreturn(Val_false);
    default:
        WRAP(res);
        CAMLreturn(Val_false);
    }
}

value _ezsqlite_stmt_reset (value stmt) {
    CAMLparam1(stmt);
    WRAP(sqlite3_reset((sqlite3_stmt*)stmt));
    CAMLreturn(Val_unit);
}

value _ezsqlite_stmt_clear_bindings (value stmt) {
    CAMLparam1(stmt);
    WRAP(sqlite3_clear_bindings((sqlite3_stmt*)stmt));
    CAMLreturn(Val_unit);
}

value _ezsqlite_stmt_parameter_count (value stmt) {
    CAMLparam1(stmt);
    CAMLreturn(Val_int (sqlite3_bind_parameter_count ((sqlite3_stmt*)stmt)));
}

value _ezsqlite_stmt_parameter_index (value stmt, value name) {
    CAMLparam2(stmt, name);
    CAMLreturn (Val_int (sqlite3_bind_parameter_index ((sqlite3_stmt*)stmt, String_val(name))));
}

value _ezsqlite_bind_null (value stmt, value i){
    CAMLparam2(stmt, i);
    WRAP(sqlite3_bind_null((sqlite3_stmt*)stmt, Int_val(i)));
    CAMLreturn(Val_unit);
}

value _ezsqlite_bind_blob (value stmt, value i, value s){
    CAMLparam3(stmt, i, s);
    size_t len = caml_string_length(s);
    WRAP(sqlite3_bind_blob((sqlite3_stmt*)stmt, Int_val(i), String_val(s), len, SQLITE_TRANSIENT));
    CAMLreturn(Val_unit);
}

value _ezsqlite_bind_text (value stmt, value i, value s){
    CAMLparam3(stmt, i, s);
    size_t len = caml_string_length(s);
    WRAP(sqlite3_bind_text((sqlite3_stmt*)stmt, Int_val(i), String_val(s), len, SQLITE_TRANSIENT));
    CAMLreturn(Val_unit);
}

value _ezsqlite_bind_int64 (value stmt, value i, value b){
    CAMLparam3(stmt, i, b);
    WRAP(sqlite3_bind_int64((sqlite3_stmt*)stmt, Int_val(i), Int64_val(b)));
    CAMLreturn(Val_unit);
}

value _ezsqlite_bind_double (value stmt, value i, value b){
    CAMLparam3(stmt, i, b);
    WRAP(sqlite3_bind_double((sqlite3_stmt*)stmt, Int_val(i), Double_val(b)));
    CAMLreturn(Val_unit);
}

value _ezsqlite_bind_value (value stmt, value i, value b){
    CAMLparam3(stmt, i, b);
    WRAP(sqlite3_bind_value((sqlite3_stmt*)stmt, Int_val(i), (sqlite3_value*)b));
    CAMLreturn(Val_unit);
}

value _ezsqlite_data_count (value stmt){
    CAMLparam1(stmt);
    CAMLreturn(Val_int(sqlite3_data_count((sqlite3_stmt*)stmt)));
}

value _ezsqlite_column_type (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(Val_int(sqlite3_column_type((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_column_text (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLlocal1(s);
    int len = sqlite3_column_bytes ((sqlite3_stmt*)stmt, Int_val(i));
    s = caml_alloc_string (len);
    const char* txt = sqlite3_column_text ((sqlite3_stmt*)stmt, Int_val(i));
    if (txt){
        memcpy(String_val(s), txt, len);
    }
    CAMLreturn(s);
}

value _ezsqlite_column_blob (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLlocal1(s);
    int len = sqlite3_column_bytes ((sqlite3_stmt*)stmt, Int_val(i));
    s = caml_alloc_string (len);
    const char * blob = sqlite3_column_blob ((sqlite3_stmt*)stmt, Int_val(i));
    if (blob){
        memcpy(String_val(s), blob, len);
    }
    CAMLreturn(s);
}

value _ezsqlite_column_int64 (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(caml_copy_int64(sqlite3_column_int64((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_column_int (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(Val_int(sqlite3_column_int((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_column_double (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(caml_copy_double(sqlite3_column_double((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_column_value (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn((value)sqlite3_column_value((sqlite3_stmt*)stmt, Int_val(i)));
}

value _ezsqlite_column_name (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(caml_copy_string (sqlite3_column_name ((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_database_name (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(caml_copy_string (sqlite3_column_database_name ((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_table_name (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(caml_copy_string (sqlite3_column_table_name ((sqlite3_stmt*)stmt, Int_val(i))));
}

value _ezsqlite_origin_name (value stmt, value i){
    CAMLparam2(stmt, i);
    CAMLreturn(caml_copy_string (sqlite3_column_origin_name ((sqlite3_stmt*)stmt, Int_val(i))));
}

// Backup
value _ezsqlite_backup_init (value to, value toDb, value from, value fromDb){
    CAMLparam4(to, toDb, from, fromDb);
    sqlite3 *_to = (sqlite3*)to;
    sqlite3 *_from = (sqlite3*)from;

    sqlite3_backup *bup = sqlite3_backup_init(_to, String_val (toDb), _from, String_val(fromDb));
    if (bup == NULL){
        caml_raise_with_string(*caml_named_value("sqlite error"), sqlite3_errmsg(_to));
        CAMLreturn(Val_unit);
    }

    CAMLreturn((value)bup);
}

value _ezsqlite_backup_step (value backup, value n){
    CAMLparam2(backup, n);

    int res = sqlite3_backup_step ((sqlite3_backup*)backup, Int_val(n));

    if (res == SQLITE_OK || res == SQLITE_BUSY || res == SQLITE_LOCKED){
        CAMLreturn(Val_true);
    } else if (res == SQLITE_DONE) {
        CAMLreturn(Val_false);
    } else {
        WRAP(res);
        CAMLreturn(Val_false);
    }
}

value _ezsqlite_backup_finish(value backup){
    CAMLparam1(backup);
    WRAP(sqlite3_backup_finish ((sqlite3_backup*)backup));
    CAMLreturn(Val_unit);
}

value _ezsqlite_backup_remaining(value backup){
    CAMLparam1(backup);
    CAMLreturn(Val_int (sqlite3_backup_remaining ((sqlite3_backup*)backup)));
}

value _ezsqlite_backup_pagecount(value backup){
    CAMLparam1(backup);
    CAMLreturn(Val_int (sqlite3_backup_pagecount ((sqlite3_backup*)backup)));
}

value helper_blob_open(value db, value dbname, value tablename, value fieldname, value idx, int rw){
   sqlite3_blob *blob;
   WRAP(sqlite3_blob_open((sqlite3*)db, String_val(dbname), String_val(tablename), String_val(fieldname), Int64_val(idx), rw, &blob) != SQLITE_OK);

   return (value)blob;
}

value _ezsqlite_blob_open_rw(value db, value dbname, value tablename, value fieldname, value idx){
    CAMLparam5(db, dbname, tablename, fieldname, idx);
    CAMLreturn(helper_blob_open(db, dbname, tablename, fieldname, idx, 1));
}

value _ezsqlite_blob_open_ro(value db, value dbname, value tablename, value fieldname, value idx){
    CAMLparam5(db, dbname, tablename, fieldname, idx);
    CAMLreturn(helper_blob_open(db, dbname, tablename, fieldname, idx, 0));
}



value _ezsqlite_blob_close (value blob){
    CAMLparam1(blob);
    WRAP(sqlite3_blob_close((sqlite3_blob*)blob));
    CAMLreturn(Val_unit);
}

value _ezsqlite_blob_reopen (value blob, value idx){
    CAMLparam2(blob, idx);
    WRAP(sqlite3_blob_reopen((sqlite3_blob*)blob, Int64_val(idx)));
    CAMLreturn(Val_unit);
}

value _ezsqlite_blob_bytes (value blob){
    CAMLparam1(blob);
    CAMLreturn(Val_int (sqlite3_blob_bytes((sqlite3_blob*)blob)));
}

value _ezsqlite_blob_read (value blob, value s, value n, value offs){
    CAMLparam4(blob, s, n, offs);
    WRAP(sqlite3_blob_read ((sqlite3_blob*)blob, String_val(s), Int_val(n), Int_val(offs)));
    CAMLreturn(Val_unit);
}

value _ezsqlite_blob_write (value blob, value s, value n, value offs){
    CAMLparam4(blob, s, n, offs);
    WRAP(sqlite3_blob_write ((sqlite3_blob*)blob, String_val(s), Int_val(n), Int_val(offs)));
    CAMLreturn(Val_unit);
}
