#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/alloc.h>

#include <kclangc.h>

static
int const OPEN_FLAGS[] = {
  KCOREADER,
  KCOWRITER,
  KCOCREATE,
  KCOTRUNCATE,
  KCOAUTOTRAN,
  KCOAUTOSYNC,
  KCONOLOCK,
  KCOTRYLOCK,
  KCONOREPAIR
};

static
int decode_flags(value options, int const codes[])
{
  CAMLparam1(options);
  CAMLlocal1(head);
  int flags = 0;
  while (options!=Val_emptylist) {
    head = Field(options, 0);
    flags = flags | (codes[Int_val(head)]);
    options = Field(options, 1);
  }
  CAMLreturn(flags);
}

static
void RAISE(const char *error)
{
  static value *exception_handler = NULL;
  if (exception_handler == NULL) {
    exception_handler = caml_named_value("kyotocabinet.error");
    if (exception_handler == NULL) {
      caml_failwith(error);
    }
  }
  caml_raise_with_string(*exception_handler, error);
}

#define KCDB_val(v) *((KCDB **) &Field(v, 0))
#define KCCUR_val(v) *((KCCUR **) &Field(v, 0))

inline static KCDB* get_db(value caml_db)
{
  KCDB* db = KCDB_val(caml_db);
  if (! db) {
    RAISE("Database has been closed");
  }

  return db;
}

inline static KCCUR* get_cursor(value caml_cursor)
{
  KCCUR* cur = KCCUR_val(caml_cursor);
  if (! cur) {
    RAISE("Cursor has been closed");
  }

  return cur;
}


extern CAMLprim
value kc_open(value path, value options)
{
  CAMLparam2(path, options);

  KCDB* db = kcdbnew();
  if (! kcdbopen(db, String_val(path), decode_flags(options, OPEN_FLAGS))) {
     const char *error = kcdbemsg(db);
     kcdbdel(db);
     RAISE(error);
  }

  value caml_db = alloc_small(1, Abstract_tag);
  KCDB_val(caml_db) = db;
  CAMLreturn(caml_db);
}

extern CAMLprim
value kc_close(value caml_db)
{
  CAMLparam1(caml_db);
  
  KCDB* db = KCDB_val(caml_db);
  if (db) {
    if (! kcdbclose(db)) {
       const char *error = kcdbemsg(db);
       RAISE(error);
    }

    kcdbdel(db);
    KCDB_val(caml_db) = NULL;
  }

  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_count(value caml_db)
{
  CAMLparam1(caml_db);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  int64_t count = kcdbcount(db);
  val = copy_int64(count);
  
  CAMLreturn(val);
}

extern CAMLprim
value kc_size(value caml_db)
{
  CAMLparam1(caml_db);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  int64_t size = kcdbsize(db);
  val = copy_int64(size);
  
  CAMLreturn(val);
}

extern CAMLprim
value kc_path(value caml_db)
{
  CAMLparam1(caml_db);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  const char* path = kcdbpath(db);
  if (! path) {
     const char *error = kcdbemsg(db);
     RAISE(error);
  }

  val = caml_copy_string(path);
  kcfree((void*) path);
  
  CAMLreturn(val);
}

extern CAMLprim
value kc_status(value caml_db)
{
  CAMLparam1(caml_db);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  const char* status = kcdbstatus(db);
  if (! status) {
     const char *error = kcdbemsg(db);
     RAISE(error);
  }

  val = caml_copy_string(status);
  kcfree((void*) status);
  
  CAMLreturn(val);
}

extern CAMLprim
value kc_set(value caml_db, value key, value val)
{
  CAMLparam3(caml_db, key, val);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbset(db,
    String_val(key), caml_string_length(key),
    String_val(val), caml_string_length(val)
  )) {
     RAISE(kcdbemsg(db));
  }

  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_add(value caml_db, value key, value val)
{
  CAMLparam3(caml_db, key, val);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbadd(db,
    String_val(key), caml_string_length(key),
    String_val(val), caml_string_length(val)
  )) {
     if (kcdbecode(db) == KCEDUPREC) caml_invalid_argument("Entry already exists");
     else RAISE(kcdbemsg(db));
  }

  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_replace(value caml_db, value key, value val)
{
  CAMLparam3(caml_db, key, val);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbreplace(db,
    String_val(key), caml_string_length(key),
    String_val(val), caml_string_length(val)
  )) {
     if (kcdbecode(db) == KCENOREC) raise_not_found();
     else RAISE(kcdbemsg(db));
  }

  CAMLreturn(Val_unit);
}

static
const char* get_some_value(const char *kbuf, size_t ksiz, const char *vbuf, size_t vsiz, size_t *sp, void *opq)
{
  CAMLparam0();
  CAMLlocal1(str);

  str = caml_alloc_string(vsiz);
  memcpy(String_val(str), vbuf, vsiz);

  value *block = (value*) opq;
  *block = caml_alloc(1,0); // Some(str);
  Store_field(*block, 0, str); 

  return KCVISNOP;
}

static 
const char* get_no_value(const char *kbuf, size_t ksiz, size_t *sp, void *opq)
{
  value *val = (value*) opq;
  *val = Val_int(0); // None
  return KCVISNOP;
}

extern CAMLprim
value kc_get(value caml_db, value key)
{
  CAMLparam2(caml_db, key);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  if (! kcdbaccept(db,
    String_val(key), caml_string_length(key),
    get_some_value, get_no_value, &val, 0
  )) {
     RAISE(kcdbemsg(db));
  }
  
  CAMLreturn(val);
}

static
const char* get_the_value(const char *kbuf, size_t ksiz, const char *vbuf, size_t vsiz, size_t *sp, void *opq)
{
  CAMLparam0();
  CAMLlocal1(str);

  str = caml_alloc_string(vsiz);
  memcpy(String_val(str), vbuf, vsiz);

  value *block = (value*) opq;
  *block = str;

  return KCVISNOP;
}

static 
const char* found_no_value(const char *kbuf, size_t ksiz, size_t *sp, void *opq)
{
  char** res = (char**) opq;
  *res = NULL;
  return KCVISNOP;
}

extern CAMLprim
value kc_find(value caml_db, value key)
{
  CAMLparam2(caml_db, key);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  if (! kcdbaccept(db,
    String_val(key), caml_string_length(key),
    get_the_value, found_no_value, &val, 0
  )) {
     RAISE(kcdbemsg(db));
  }
  
  if ((char*)val == NULL) raise_not_found();
  CAMLreturn(val);
}

static
const char* exists_some_value(const char *kbuf, size_t ksiz, const char *vbuf, size_t vsiz, size_t *sp, void *opq)
{
  value *val = (value*) opq;
  *val = Val_true;
  return KCVISNOP;
}

static 
const char* exists_no_value(const char *kbuf, size_t ksiz, size_t *sp, void *opq)
{
  value *val = (value*) opq;
  *val = Val_false;
  return KCVISNOP;
}

extern CAMLprim
value kc_exists(value caml_db, value key)
{
  CAMLparam2(caml_db, key);
  CAMLlocal1(val);

  KCDB* db = get_db(caml_db);
  if (! kcdbaccept(db,
    String_val(key), caml_string_length(key),
    exists_some_value, exists_no_value, &val, 0
  )) {
     RAISE(kcdbemsg(db));
  }
  
  CAMLreturn(val);
}

extern CAMLprim
value kc_remove(value caml_db, value key)
{
  CAMLparam2(caml_db, key);

  KCDB* db = get_db(caml_db);
  if (! kcdbremove(db,
    String_val(key), caml_string_length(key)
  )) {
     if (kcdbecode(db) != KCENOREC) {
       RAISE(kcdbemsg(db));
     }
  }

  CAMLreturn(Val_unit);
}

static
KCCUR* open_cursor(KCDB* db)
{
  KCCUR* cur = kcdbcursor(db);
  if (! kccurjump(cur)) {
     if (kccurecode(cur) != KCENOREC) {
        const char *error = kccuremsg(cur);
        kccurdel(cur);
        cur = NULL;
        RAISE(error);
     }
  }

  return cur;
}

extern CAMLprim
value kc_cursor_open(value caml_db)
{
  CAMLparam1(caml_db);

  KCDB* db = get_db(caml_db);
  KCCUR* cur = open_cursor(db);

  value caml_cursor = alloc_small(1, Abstract_tag);
  KCCUR_val(caml_cursor) = cur;
  CAMLreturn(caml_cursor);
}

extern CAMLprim
value kc_cursor_close(value caml_cursor)
{
  CAMLparam1(caml_cursor);
  
  KCCUR* cur = KCCUR_val(caml_cursor);
  if (cur) {
    kccurdel(cur);
    KCCUR_val(caml_cursor) = NULL;
  }

  CAMLreturn(Val_unit);
}

static
const char* get_pair(const char *kbuf, size_t ksiz, const char *vbuf, size_t vsiz, size_t *sp, void *opq)
{
  CAMLparam0();
  CAMLlocal3(key,val,pair);

  key  = caml_alloc_string(ksiz);
  memcpy(String_val(key ), kbuf, ksiz);

  val = caml_alloc_string(vsiz);
  memcpy(String_val(val), vbuf, vsiz);

  pair = caml_alloc(2,0); // (tuple)
  Store_field(pair, 0, key); 
  Store_field(pair, 1, val); 

  value *block = (value*) opq;
  *block = pair;
  return KCVISNOP;
}

extern CAMLprim
value kc_cursor_next(value caml_cursor)
{
  CAMLparam1(caml_cursor);
  CAMLlocal2(val,pair);
  
  KCCUR* cur = get_cursor(caml_cursor);
  if (kccuraccept(cur, get_pair, &pair, 0, 1)) {
    val = caml_alloc(1,0); // Some(pair);
    Store_field(val, 0, pair); 
  }
  else {
     if (kccurecode(cur) == KCENOREC) {
       val = Val_int(0); // None
     }
     else {
       RAISE(kccuremsg(cur));
     }
  }
  
  CAMLreturn(val);
}

extern CAMLprim
value kc_cursor_jump(value caml_cursor, value key)
{
  CAMLparam2(caml_cursor, key);
  
  KCCUR* cur = get_cursor(caml_cursor);
  if (! kccurjumpkey(cur, String_val(key), caml_string_length(key))) {
     if (kccurecode(cur) != KCENOREC) {
       RAISE(kccuremsg(cur));
     }
  }
  
  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_is_prefix(value caml_prefix, value caml_string)
{
  CAMLparam2(caml_prefix, caml_string);
  CAMLlocal1(val);

  int prefix_len = caml_string_length(caml_prefix);
  int string_len = caml_string_length(caml_string);

  if (string_len < prefix_len) val = Val_bool(0);
  else val = Val_bool(strncmp(String_val(caml_prefix), String_val(caml_string), prefix_len) == 0);

  CAMLreturn(val);
};

extern CAMLprim
value kc_begin_tran(value caml_db)
{
  CAMLparam1(caml_db);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbbegintran(db, 0)) {
     const char *error = kcdbemsg(db);
     RAISE(error);
  }

  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_begin_tran_sync(value caml_db)
{
  CAMLparam1(caml_db);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbbegintran(db, 1)) {
     const char *error = kcdbemsg(db);
     RAISE(error);
  }

  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_commit_tran(value caml_db)
{
  CAMLparam1(caml_db);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbendtran(db, 1)) {
     const char *error = kcdbemsg(db);
     RAISE(error);
  }

  CAMLreturn(Val_unit);
}

extern CAMLprim
value kc_abort_tran(value caml_db)
{
  CAMLparam1(caml_db);
  
  KCDB* db = get_db(caml_db);
  if (! kcdbendtran(db, 0)) {
     const char *error = kcdbemsg(db);
     RAISE(error);
  }

  CAMLreturn(Val_unit);
}
