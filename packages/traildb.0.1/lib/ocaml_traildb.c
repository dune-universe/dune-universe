#include <string.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <traildb.h>

/* -------------------- */
/* Working with errors. */
/* -------------------- */

/* Must be synchronized with TrailDB.error. */
static int const ERROR_CODES[] = {
    /* generic */

    TDB_ERR_NOMEM,
    TDB_ERR_PATH_TOO_LONG,
    TDB_ERR_UNKNOWN_FIELD,
    TDB_ERR_UNKNOWN_UUID,
    TDB_ERR_INVALID_TRAIL_ID,
    TDB_ERR_HANDLE_IS_NULL,
    TDB_ERR_HANDLE_ALREADY_OPENED,
    TDB_ERR_UNKNOWN_OPTION,
    TDB_ERR_INVALID_OPTION_VALUE,
    TDB_ERR_INVALID_UUID,

    /* io */

    TDB_ERR_IO_OPEN,
    TDB_ERR_IO_CLOSE,
    TDB_ERR_IO_WRITE,
    TDB_ERR_IO_READ,
    TDB_ERR_IO_TRUNCATE,
    TDB_ERR_IO_PACKAGE,

    /* tdb_open */

    TDB_ERR_INVALID_INFO_FILE,
    TDB_ERR_INVALID_VERSION_FILE,
    TDB_ERR_INCOMPATIBLE_VERSION,
    TDB_ERR_INVALID_FIELDS_FILE,
    TDB_ERR_INVALID_UUIDS_FILE,
    TDB_ERR_INVALID_CODEBOOK_FILE,
    TDB_ERR_INVALID_TRAILS_FILE,
    TDB_ERR_INVALID_LEXICON_FILE,
    TDB_ERR_INVALID_PACKAGE,

    /* tdb_cons */

    TDB_ERR_TOO_MANY_FIELDS,
    TDB_ERR_DUPLICATE_FIELDS,
    TDB_ERR_INVALID_FIELDNAME,
    TDB_ERR_TOO_MANY_TRAILS,
    TDB_ERR_VALUE_TOO_LONG,
    TDB_ERR_APPEND_FIELDS_MISMATCH,
    TDB_ERR_LEXICON_TOO_LARGE,
    TDB_ERR_TIMESTAMP_TOO_LARGE,
    TDB_ERR_TRAIL_TOO_LONG,

    /* querying */
    TDB_ERR_ONLY_DIFF_FILTER,
    TDB_ERR_NO_SUCH_ITEM,
    TDB_ERR_INVALID_RANGE,
    TDB_ERR_INCORRECT_TERM_TYPE
};

static int const ERR_UNKNOWN = (sizeof ERROR_CODES) / (sizeof ERROR_CODES[0]);

static CAMLprim
void raise_exception(tdb_error err)
{
  CAMLparam0();
  static value *exception_handler = NULL;

  if (exception_handler == NULL) {
    exception_handler = caml_named_value("traildb.error");
    if (exception_handler == NULL) {
      caml_failwith("Internal error: unknown exception handler: traildb.error");
    }
  }

  int i;
  for (i = 0; i < ERR_UNKNOWN; i++) {
    if (err == ERROR_CODES[i]) {
      break;
    }
  }

  if (i < ERR_UNKNOWN) caml_raise_with_arg(*exception_handler, Val_int(i));
  else caml_failwith("Internal error: unknown traildb error code");

  CAMLnoreturn;
}

extern CAMLprim
value ocaml_tdb_error_str(value caml_err) {
  CAMLparam1(caml_err);
  CAMLlocal1(caml_msg);

  caml_msg = caml_copy_string(tdb_error_str (Int_val(caml_err)));

  CAMLreturn(caml_msg);
}

/* --------------------------------------- */
/* Custom wrappers and operations.         */
/* --------------------------------------- */

#define Tdb_cons_val(v) (*((tdb_cons **) Data_custom_val(v)))

static void tdb_cons_custom_finalize(value v) {
  tdb_cons_close(Tdb_cons_val(v));
}

static struct custom_operations tdb_cons_operations = {
  "tdb_cons",
  tdb_cons_custom_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_tdb_cons_wrapper (tdb_cons* tdb) {
  value v = alloc_custom(&tdb_cons_operations, sizeof(tdb_cons*), 0, 1);
  Tdb_cons_val(v) = tdb;
  return v;
}

#define Tdb_val(v) (*((struct _tdb **) Data_custom_val(v)))

static void tdb_custom_finalize(value v) {
  tdb_close(Tdb_val(v));
}

static struct custom_operations tdb_operations = {
  "tdb",
  tdb_custom_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_tdb_wrapper (tdb* tdb) {
  value v = alloc_custom(&tdb_operations, sizeof(tdb), 0, 1);
  Tdb_val(v) = tdb;
  return v;
}

#define Tdb_cursor_val(v) (*((tdb_cursor **) Data_custom_val(v)))

static void tdb_cursor_custom_finalize(value v) {
  tdb_cursor_free(Tdb_cursor_val(v));
}

static struct custom_operations tdb_cursor_operations = {
  "tdb_cursor",
  tdb_cursor_custom_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_tdb_cursor_wrapper (tdb_cursor* tdb) {
  value v = alloc_custom(&tdb_cursor_operations, sizeof(tdb_cursor*), 0, 1);
  Tdb_cursor_val(v) = tdb;
  return v;
}


/* --------------------------------------- */
/* Helper functions.                       */
/* --------------------------------------- */

static unsigned int ocaml_list_len(value caml_list) {
  CAMLparam1(caml_list);

  unsigned int count = 0;
  while (caml_list != Val_emptylist) {
    ++ count;
    caml_list = Field(caml_list, 1);
  }
  return count;
}

static void copy_ocaml_string_list(value caml_list, char** strings)
{
  CAMLparam1(caml_list);

  while (caml_list != Val_emptylist) {
    *strings++ = String_val(Field(caml_list, 0));
    caml_list = Field(caml_list, 1);
  }
}

static void copy_ocaml_length_list(value caml_list, uint64_t* lengths)
{
  CAMLparam1(caml_list);

  while (caml_list != Val_emptylist) {
    *lengths++ = caml_string_length(Field(caml_list, 0));
    caml_list = Field(caml_list, 1);
  }
}

/* --------------------------------------- */
/* Working with TrailDB database builders. */
/* --------------------------------------- */

extern CAMLprim
value ocaml_tdb_cons_open(value caml_path, value caml_fields) {
  CAMLparam2(caml_path, caml_fields);
  CAMLlocal1(caml_tdb_cons);

  tdb_cons* tdb = tdb_cons_init();
  caml_tdb_cons = alloc_tdb_cons_wrapper(tdb);

  uint64_t num_ofields = ocaml_list_len(caml_fields);
  char* ofield_names[num_ofields];
  copy_ocaml_string_list(caml_fields, ofield_names);

  const char* root = String_val(caml_path);
  tdb_error err = tdb_cons_open(tdb, root, (const char **) ofield_names, num_ofields);
  if (err) raise_exception(err);

  CAMLreturn(caml_tdb_cons);
}

extern CAMLprim
value ocaml_tdb_cons_add(value caml_tdb_cons, value caml_uuid, value caml_timestamp, value caml_values) {
  CAMLparam4(caml_tdb_cons, caml_uuid, caml_timestamp, caml_values);
  
  tdb_cons* cons = Tdb_cons_val(caml_tdb_cons);
  const uint8_t* uuid = (const uint8_t*) String_val(caml_uuid);
  const uint64_t timestamp = Int64_val(caml_timestamp);
  uint64_t num_values = ocaml_list_len(caml_values);
  char* values[num_values];
  copy_ocaml_string_list(caml_values, values);
  uint64_t lengths[num_values];
  copy_ocaml_length_list(caml_values, lengths);

  tdb_error err = tdb_cons_add(cons, uuid, timestamp, (const char **) values, (const uint64_t *) lengths);
  if (err) raise_exception(err);

  CAMLreturn(Val_unit);
}

extern CAMLprim
value ocaml_tdb_cons_append(value caml_tdb_cons, value caml_tdb) {
  CAMLparam2(caml_tdb_cons, caml_tdb);
  
  tdb_cons* cons = Tdb_cons_val(caml_tdb_cons);
  tdb* tdb = Tdb_val(caml_tdb);

  tdb_error err = tdb_cons_append(cons, tdb);
  if (err) raise_exception(err);

  CAMLreturn(Val_unit);
}

extern CAMLprim
value ocaml_tdb_cons_finalize(value caml_tdb_cons) {
  CAMLparam1(caml_tdb_cons);
  
  tdb_cons *cons = Tdb_cons_val(caml_tdb_cons);
  tdb_error err = tdb_cons_finalize(cons);
  if (err) raise_exception(err);

  CAMLreturn(Val_unit);
}

/* ----------------------------------------- */
/* Working with TrailDB read-only databases. */
/* ----------------------------------------- */

extern CAMLprim
value ocaml_tdb_open(value caml_path) {
  CAMLparam1(caml_path);
  CAMLlocal1(caml_tdb);

  tdb* tdb = tdb_init();
  caml_tdb = alloc_tdb_wrapper(tdb);

  const char* root = String_val(caml_path);
  tdb_error err = tdb_open(tdb, root);
  if (err) raise_exception(err);

  CAMLreturn(caml_tdb);
}

extern CAMLprim
value ocaml_tdb_dontneed(value caml_tdb) {
  CAMLparam1(caml_tdb);
  
  tdb* tdb = Tdb_val(caml_tdb);
  tdb_dontneed(tdb);

  CAMLreturn(Val_unit);
}

extern CAMLprim
value ocaml_tdb_willneed(value caml_tdb) {
  CAMLparam1(caml_tdb);
  
  tdb* tdb = Tdb_val(caml_tdb);
  tdb_willneed(tdb);

  CAMLreturn(Val_unit);
}

extern CAMLprim
value ocaml_tdb_num_trails(value caml_tdb) {
  CAMLparam1(caml_tdb);
  CAMLlocal1(caml_num);

  tdb* tdb = Tdb_val(caml_tdb);
  uint64_t count = tdb_num_trails(tdb);
  caml_num = caml_copy_int64(count);

  CAMLreturn(caml_num);
}

extern CAMLprim
value ocaml_tdb_num_events(value caml_tdb) {
  CAMLparam1(caml_tdb);
  CAMLlocal1(caml_num);

  tdb* tdb = Tdb_val(caml_tdb);
  uint64_t count = tdb_num_events(tdb);
  caml_num = caml_copy_int64(count);

  CAMLreturn(caml_num);
}

extern CAMLprim
value ocaml_tdb_num_fields(value caml_tdb) {
  CAMLparam1(caml_tdb);
  CAMLlocal1(caml_num);

  tdb* tdb = Tdb_val(caml_tdb);
  uint64_t count = tdb_num_fields(tdb);
  caml_num = caml_copy_int64(count);

  CAMLreturn(caml_num);
}

extern CAMLprim
value ocaml_tdb_min_timestamp(value caml_tdb) {
  CAMLparam1(caml_tdb);
  CAMLlocal1(caml_timestamp);

  tdb* tdb = Tdb_val(caml_tdb);
  uint64_t timestamp = tdb_min_timestamp(tdb);
  caml_timestamp = caml_copy_int64(timestamp);

  CAMLreturn(caml_timestamp);
}

extern CAMLprim
value ocaml_tdb_max_timestamp(value caml_tdb) {
  CAMLparam1(caml_tdb);
  CAMLlocal1(caml_timestamp);

  tdb* tdb = Tdb_val(caml_tdb);
  uint64_t timestamp = tdb_max_timestamp(tdb);
  caml_timestamp = caml_copy_int64(timestamp);

  CAMLreturn(caml_timestamp);
}

/* --------------------- */
/* Working with uuids.   */
/* --------------------- */

extern CAMLprim
value ocaml_tdb_get_uuid(value caml_tdb, value caml_trail_id) {
  CAMLparam2(caml_tdb, caml_trail_id);
  CAMLlocal2(caml_option, caml_uuid);

  tdb* tdb = Tdb_val(caml_tdb);
  uint64_t trail = Int64_val(caml_trail_id);
  const uint8_t *uuid = tdb_get_uuid(tdb, trail);

  if (uuid) {
    caml_uuid = caml_alloc_string(16);
    memcpy(String_val(caml_uuid), uuid, 16);

    caml_option = caml_alloc(1,0); // Some(uuid)
    Store_field(caml_option, 0, caml_uuid);
  } else {
    caml_option = Val_int(0); // None
  }

  CAMLreturn(caml_option);
}

extern CAMLprim
value ocaml_tdb_get_trail_id(value caml_tdb, value caml_uuid) {
  CAMLparam2(caml_tdb, caml_uuid);
  CAMLlocal2(caml_option, caml_trail_id);

  tdb* tdb = Tdb_val(caml_tdb);
  const uint8_t* uuid = (const uint8_t*) String_val(caml_uuid);
  uint64_t trail_id;

  tdb_error err = tdb_get_trail_id(tdb, uuid, &trail_id);
  if (err) {
    caml_option = Val_int(0); // None
  } else {
    caml_option = caml_alloc(1,0); // Some(trail_id)
    Store_field(caml_option, 0, caml_copy_int64(trail_id));
  }

  CAMLreturn(caml_option);
}

/* --------------------- */
/* Working with fields. */
/* --------------------- */

extern CAMLprim
value ocaml_tdb_get_field(value caml_tdb, value caml_name) {
  CAMLparam2(caml_tdb, caml_name);
  CAMLlocal1(caml_field);

  tdb* tdb = Tdb_val(caml_tdb);
  const char *field_name = String_val(caml_name);
  tdb_field field;
  tdb_error err = tdb_get_field(tdb, field_name, &field);

  if (err) {
    caml_field = Val_int(0); // None
  } else {
    caml_field = caml_alloc(1,0); // Some(tdb_field)
    Store_field(caml_field, 0, caml_copy_int64(field));
  }

  CAMLreturn(caml_field);
}

extern CAMLprim
value ocaml_tdb_get_field_name(value caml_tdb, value caml_field) {
  CAMLparam2(caml_tdb, caml_field);
  CAMLlocal1(caml_name);

  tdb* tdb = Tdb_val(caml_tdb);
  tdb_field field = Int64_val(caml_field);
  const char *field_name = tdb_get_field_name(tdb, field);

  if (field_name) {
    caml_name = caml_alloc(1,0); // Some(str)
    Store_field(caml_name, 0, caml_copy_string(field_name));
  }
  else {
    caml_name = Val_int(0); // None
  } 
 
  CAMLreturn(caml_name);
}

extern CAMLprim
value ocaml_tdb_lexicon_size(value caml_tdb, value caml_field) {
  CAMLparam2(caml_tdb, caml_field);
  CAMLlocal1(caml_num);

  tdb* tdb = Tdb_val(caml_tdb);
  tdb_field field = Int64_val(caml_field);
  uint64_t count = tdb_lexicon_size(tdb, field);
  caml_num = caml_copy_int64(count);

  CAMLreturn(caml_num);
}

extern CAMLprim
value ocaml_tdb_item_field(value caml_item) {
  CAMLparam1(caml_item);
  CAMLlocal1(caml_field);

  tdb_item item = Int64_val(caml_item);
  tdb_field field = tdb_item_field(item); 
  caml_field = caml_copy_int64(field);

  CAMLreturn(caml_field);
}

extern CAMLprim
value ocaml_tdb_get_item_value(value caml_tdb, value caml_item) {
  CAMLparam2(caml_tdb, caml_item);
  CAMLlocal1(caml_string_value);

  tdb* tdb = Tdb_val(caml_tdb);
  tdb_item item = Int64_val(caml_item);
  uint64_t value_length;
  const char *string_value = tdb_get_item_value(tdb, item, &value_length);

  caml_string_value = caml_alloc_string(value_length);
  memcpy(String_val(caml_string_value), string_value, value_length);

  CAMLreturn(caml_string_value);
}

/* --------------------- */
/* Working with cursors. */
/* --------------------- */

extern CAMLprim
value ocaml_tdb_cursor_new(value caml_tdb) {
  CAMLparam1(caml_tdb);
  CAMLlocal1(caml_cursor);

  tdb* tdb = Tdb_val(caml_tdb);
  tdb_cursor* cursor = tdb_cursor_new(tdb);
  caml_cursor = alloc_tdb_cursor_wrapper(cursor);

  CAMLreturn(caml_cursor);
}

extern CAMLprim
value ocaml_tdb_get_trail(value caml_cursor, value caml_trail) {
  CAMLparam2(caml_cursor, caml_trail);
  
  tdb_cursor* cursor = Tdb_cursor_val(caml_cursor);
  uint64_t trail = Int64_val(caml_trail);

  tdb_error err = tdb_get_trail(cursor, trail);
  if (err) raise_exception(err);

  CAMLreturn(Val_unit);
}

extern CAMLprim
value ocaml_tdb_get_trail_length(value caml_cursor) {
  CAMLparam1(caml_cursor);
  CAMLlocal1(caml_num);

  tdb_cursor* cursor = Tdb_cursor_val(caml_cursor);
  uint64_t count = tdb_get_trail_length(cursor);
  caml_num = caml_copy_int64(count);

  CAMLreturn(caml_num);
}

value caml_copy_tdb_event(const tdb_event *event) {
  CAMLparam0();
  CAMLlocal5(caml_option, caml_event, caml_timestamp, caml_values, caml_cons);

  if (event) {
    caml_timestamp = caml_copy_int64(event->timestamp);
    caml_values = Val_int(0); // Nil

    uint64_t i = event->num_items;
    while (i--) {
      const tdb_item item = event->items[i];
      caml_cons = caml_alloc(2,0); // Cons(item, values)
      Store_field(caml_cons, 0, caml_copy_int64(item));
      Store_field(caml_cons, 1, caml_values);

      caml_values = caml_cons;
    }

    caml_event = caml_alloc(2,0); // {timestamp,values}
    Store_field(caml_event, 0, caml_timestamp);
    Store_field(caml_event, 1, caml_values);

    caml_option = caml_alloc(1,0); // Some(event)
    Store_field(caml_option, 0, caml_event);
  }
  else {
    caml_option = Val_int(0); // None
  }

  CAMLreturn(caml_option);
}

extern CAMLprim
value ocaml_tdb_cursor_next(value caml_cursor) {
  CAMLparam1(caml_cursor);
  CAMLlocal1(caml_event_option);

  tdb_cursor* cursor = Tdb_cursor_val(caml_cursor);
  const tdb_event *event = tdb_cursor_next(cursor);
  caml_event_option = caml_copy_tdb_event(event);

  CAMLreturn(caml_event_option);
}

extern CAMLprim
value ocaml_tdb_cursor_peek(value caml_cursor) {
  CAMLparam1(caml_cursor);
  CAMLlocal1(caml_event_option);

  tdb_cursor* cursor = Tdb_cursor_val(caml_cursor);
  const tdb_event *event = tdb_cursor_peek(cursor);
  caml_event_option = caml_copy_tdb_event(event);

  CAMLreturn(caml_event_option);
}
