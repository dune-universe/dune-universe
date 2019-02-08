/* File: dblib_stubs.c

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

/* Binding to the DB-Library part of freetds.
   See http://www.freetds.org/userguide/samplecode.htm */

#include <assert.h>
#include <sybfront.h> /* sqlfront.h always comes first */
#include <sybdb.h>
#include <syberror.h>
#include <string.h>
#include <math.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/threads.h>

/* OCaml severity.  Keep in sync with OCaml [Dblib]. */
#define SEVERITY_PROGRAM Val_int(6)
#define SEVERITY_RESOURCE Val_int(7)
#define SEVERITY_FATAL Val_int(9)
#define SEVERITY_CONSISTENCY Val_int(10)

typedef struct User_data {
  value latest_exception;
  int have_lock;
} User_data;

static void userdata_free(DBPROCESS* proc)
{
  User_data* data = (User_data*)dbgetuserdata(proc);
  if (data == NULL)
    return;

  caml_remove_global_root(&(data->latest_exception));

  dbsetuserdata(proc, NULL);
  caml_stat_free(data);
}

static void userdata_setup(DBPROCESS* proc)
{
  userdata_free(proc);

  User_data* data = caml_stat_alloc(sizeof(User_data));
  data->latest_exception = Val_unit;
  data->have_lock = 1;
  caml_register_global_root(&(data->latest_exception));
  dbsetuserdata(proc, (BYTE*)data);
}

static int userdata_has_exception(DBPROCESS *proc)
{
  User_data* data = (User_data*)dbgetuserdata(proc);
  return data != NULL && data->latest_exception != Val_unit;
}

static void userdata_set_latest_exception(DBPROCESS *proc, value exn)
{
  CAMLparam1(exn);
  User_data* data = (User_data*)dbgetuserdata(proc);
  if(data == NULL)
    caml_raise(exn);

  data->latest_exception = exn;
  CAMLreturn0;
}

static void maybe_raise_userdata_exn(DBPROCESS* proc)
{
  CAMLparam0();
  CAMLlocal1(vexn);

  User_data* data = (User_data*)dbgetuserdata(proc);
  if(data == NULL || data->latest_exception == Val_unit)
    CAMLreturn0;

  vexn = data->latest_exception;
  data->latest_exception = Val_unit;
  caml_raise(vexn);
}

static void userdata_release_runtime_system(DBPROCESS *proc)
{
  User_data *data = (User_data*)dbgetuserdata(proc);

  // If userdata wasn't setup, then we're in the msg/error callback for dbopen
  if (data == NULL) {
    caml_release_runtime_system();
    return;
  }

  assert(data->have_lock);
  data->have_lock = FALSE;
  caml_release_runtime_system();
}

static int userdata_acquire_runtime_system(DBPROCESS *proc)
{
  User_data *data = (User_data *)dbgetuserdata(proc);

  // If userdata wasn't setup, then we're in the msg/error callback for dbopen
  if (data == NULL) {
    caml_acquire_runtime_system();
    return FALSE;
  }

  int had_lock = data->have_lock;
  if (!had_lock)
  {
    caml_acquire_runtime_system();
    data->have_lock = TRUE;
  }
  return had_lock;
}

static value make_dblib_error(value vseverity, char* msg)
{
  // See caml_raise_with_args()
  // https://github.com/ocaml/ocaml/blob/dda4ad6edd34656a48ce2343279736fe0fef0aba/runtime/fail_nat.c#L90
  CAMLparam1(vseverity);
  CAMLlocal2(vexn, vmsg);

  static value *exn = NULL;
  if (exn == NULL) {
    /* First time around, look up by name */
    exn = caml_named_value("Freetds.Dblib.Error");
  }

  vmsg = caml_copy_string(msg);
  vexn = caml_alloc_small (3, 0);
  Store_field(vexn, 0, *exn);
  Store_field(vexn, 1, vseverity);
  Store_field(vexn, 2, vmsg);
  CAMLreturn(vexn);
}

static void raise_error(value, char*) __attribute__ ((noreturn));

static void raise_error(value vseverity, char *msg)
{
  CAMLparam1(vseverity);
  CAMLlocal1(vexn);

  vexn = make_dblib_error(vseverity, msg);
  caml_raise(vexn);
}

static int convert_severity(int severity)
{
  /* Keep in sync with dblib.ml */
  severity -= EXINFO;
  if (severity >= 11) /* number of constructors on the OCaml side */
    severity = 9;
  return severity;
}

static int msg_handler(DBPROCESS *dbproc, DBINT msgno, int msgstate, int severity,
		       char *msgtext, char *srvname, char *procname, int line)
{
  enum { changed_database = 5701, changed_language = 5703 };

  if (msgno == changed_database || msgno == changed_language)
    return 0;

  int had_lock = userdata_acquire_runtime_system(dbproc);

  CAMLparam0();
  CAMLlocal4(vseverity, vline, vmsg, vres);
  static value *handler = NULL;

  if (handler == NULL) {
    /* First time around, look up by name */
    handler = caml_named_value("Freetds.Dblib.msg_handler");
  }

  vseverity = Val_int(convert_severity(severity));
  vline = Val_int(line);
  vmsg = caml_copy_string(msgtext);

  vres = caml_callback3_exn(*handler, vseverity, vline, vmsg);
  if (Is_exception_result(vres))
  {
    if (dbproc == NULL)
      caml_raise(Extract_exception(vres));
    else
      userdata_set_latest_exception(dbproc, Extract_exception(vres));
  }

  CAMLdrop;

  if (!had_lock)
    userdata_release_runtime_system(dbproc);

  return 0;
}

/* http://manuals.sybase.com/onlinebooks/group-cnarc/cng1110e/dblib/@Generic__BookTextView/16561;pt=39614 */
static int err_handler(DBPROCESS *dbproc, int severity, /* in syberror.h */
                       int dberr, /* in sybdb.h, SYBE* macros */
                       int oserr, /* in sybdb.h */
                       char *dberrstr, char *oserrstr)
{
  // For server messages, FreeTDS calls both the msg handler and err handler,
  // but the msg handler gets an actual message and the error handler gets a
  // useless message ("Check messages from the SQL Server")
  // Ensure that if the msghandler already threw an exception, we don't
  // overwrite that
  if (dberr == SYBESMSG && userdata_has_exception(dbproc))
  {
    return INT_CANCEL;
  }

  int had_lock = userdata_acquire_runtime_system(dbproc);
  CAMLparam0();
  CAMLlocal4(vseverity, vmsg, vres, vexn);

  static value *handler = NULL;
  if (handler == NULL) {
    /* First time around, look up by name */
    handler = caml_named_value("Freetds.Dblib.err_handler");
  }

  vseverity = Val_int(convert_severity(severity));
  vmsg = caml_copy_string(dberrstr);
  vres = caml_callback3_exn(*handler, vseverity, Val_int(dberr), vmsg);

  int have_exn = FALSE;
  if (Is_exception_result(vres))
  {
    vexn = Extract_exception(vres);
    have_exn = TRUE;
  }
  else if ((dbproc == NULL) || (DBDEAD(dbproc))) {
    /* Always raise an exception when the DB handle is unusable. */
    vexn = make_dblib_error(vseverity, dberrstr);
    have_exn = TRUE;
  }
  else if (oserr != DBNOERR && oserr != 0 /* Undefined error */) {
    vexn = make_dblib_error(vseverity, oserrstr);
    have_exn = TRUE;
  }

  if (have_exn) {
    if (dbproc == NULL)
      caml_raise(vexn);
    else
      userdata_set_latest_exception(dbproc, vexn);
  }

  CAMLdrop;

  if (!had_lock)
    userdata_release_runtime_system(dbproc);

  return INT_CANCEL;
}

CAMLexport value ocaml_freetds_dbinit(value unit)
{
  CAMLparam1(unit);

  if (dbinit() == FAIL) {
    raise_error(SEVERITY_FATAL, "Cannot initialize DB-lib!");
  }
  /* Install a error handler using exceptions, the default error
   * handler aborts the program under some circumstances. */
  dberrhandle(&err_handler);
  dbmsghandle(&msg_handler);
  CAMLreturn(Val_unit);
}


#define DBPROCESS_VAL(v) (* (DBPROCESS **) Data_custom_val(v))
#define DBPROCESS_ALLOC()                                       \
  alloc_custom(&dbprocess_ops, sizeof(DBPROCESS *), 1, 30)

static int dbprocess_compare(value v1, value v2)
{
  CAMLparam2(v1, v2);
  /* Compare pointers */
  if (DBPROCESS_VAL(v1) < DBPROCESS_VAL(v2)) CAMLreturn(-1);
  else if (DBPROCESS_VAL(v1) > DBPROCESS_VAL(v2)) CAMLreturn(1);
  else CAMLreturn(0);
}

static intnat dbprocess_hash(value v)
{
  CAMLparam1(v);
  /* The pointer will do a good hash and respect compare v1 v2 = 0 ==>
     hash(v1) = hash(v2) */
  CAMLreturn((intnat) DBPROCESS_VAL(v));
}

static struct custom_operations dbprocess_ops = {
  "freetds/dbprocess", /* identifier for serialization and deserialization */
  custom_finalize_default, /* one must call dbclose */
  &dbprocess_compare,
  &dbprocess_hash,
  custom_serialize_default,
  custom_deserialize_default
};


CAMLexport
value ocaml_freetds_dbopen(value vuser, value vpasswd, value vchar_set,
                           value vlanguage, value vapplication,
                           value vversion, value vserver)
{
  CAMLparam5(vuser, vpasswd, vapplication, vchar_set, vlanguage);
  CAMLxparam2(vversion, vserver);
  CAMLlocal1(vdbproc);
  LOGINREC *login;
  DBPROCESS *dbproc;
  DBINT version;

  if ((login = dblogin()) == NULL) {
    raise_error(SEVERITY_FATAL,
                "Freetds.Dblib.connect: cannot allocate the login structure");
  }
  if (Is_block(vuser)) /* <> None */
    DBSETLUSER(login, String_val(Field(vuser, 0)));
  if (Is_block(vpasswd)) /* <> None */
    DBSETLPWD(login, String_val(Field(vpasswd, 0)));
  if (Is_block(vchar_set))
    DBSETLCHARSET(login, String_val(Field(vchar_set, 0)));
  if (Is_block(vlanguage))
    DBSETLNATLANG(login, String_val(Field(vlanguage, 0)));
  if (Is_block(vapplication)) /* <> None */
    DBSETLAPP(login, String_val(Field(vapplication, 0)));
  if (Is_block(vversion)) /* <> None */ {
    /* Keep in sync with the def. of OCaml [version] type. */
    switch (Int_val(Field(vversion, 0))) {
    case 0: version = DBVERSION_42; break;
    case 1: version = DBVERSION_46; break;
    case 2: version = DBVERSION_70; break;
    case 3: version = DBVERSION_71; break;
    case 4: version = DBVERSION_72; break;
    case 5:
#ifdef DBVERSION_73
      version = DBVERSION_73; break;
#else
      raise_error(SEVERITY_PROGRAM, "Freetds.Dblib.connect: Your version "
                  "of FreeTDS does not support V73.  Please upgrade.");
#endif
    case 6:
#ifdef DBVERSION_74
      version = DBVERSION_74; break;
#else
      raise_error(SEVERITY_PROGRAM, "Freetds.Dblib.connect: Your version "
                  "of FreeTDS does not support V74.  Please upgrade.");
#endif
    default:
      raise_error(SEVERITY_CONSISTENCY, "Freetds.Dblib.connect: "
                  "Version not in sync with the OCaml definition. "
                  "Contact the authors of OCaml FreeTDS.");
    }
    if (DBSETLVERSION(login, version) == FAIL) {
      raise_error(SEVERITY_PROGRAM,
                  "Freetds.Dblib.connect: could not set version");
    }
  }

  // Can't access OCaml string data once we release the runtime lock, so we need
  // to copy this
  char *server = caml_stat_strdup(String_val(vserver));
  caml_release_runtime_system();
  dbproc = dbopen(login, server);
  caml_stat_free(server);
  dbloginfree(login); /* dbopen made => [login] no longer needed. */
  caml_acquire_runtime_system();

  if (dbproc == NULL) {
    raise_error(SEVERITY_FATAL,
                "Freetds.Dblib.connect: unable to connect to the database");
  }
  userdata_setup(dbproc);
  vdbproc = DBPROCESS_ALLOC();
  DBPROCESS_VAL(vdbproc) = dbproc;
  CAMLreturn(vdbproc);
}

CAMLexport value ocaml_freetds_dbopen_bc(value * argv, int argn)
{
  return ocaml_freetds_dbopen(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}


CAMLexport value ocaml_freetds_dbclose(value vdbproc)
{
  CAMLparam1(vdbproc);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);
  userdata_free(dbproc);
  caml_release_runtime_system();
  dbclose(dbproc);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

CAMLexport value ocaml_freetds_dbuse(value vdbproc, value vdbname)
{
  CAMLparam2(vdbproc, vdbname);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);

  char* dbname = caml_stat_strdup(String_val(vdbname));
  userdata_release_runtime_system(dbproc);
  RETCODE ret = dbuse(dbproc, dbname);
  caml_stat_free(dbname);
  assert(!userdata_acquire_runtime_system(dbproc));

  maybe_raise_userdata_exn(dbproc);
  if (ret == FAIL) {
    caml_raise_not_found(); /* More precise error on the OCaml side. */
  }
  CAMLreturn(Val_unit);
}

CAMLexport value ocaml_freetds_dbname(value vdbproc)
{
  CAMLparam1(vdbproc);
  CAMLlocal1(vname);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);

  char* name = dbname(dbproc);
  maybe_raise_userdata_exn(dbproc);
  vname = caml_copy_string(name);
  CAMLreturn(vname);
}

/* Executing SQL queries
**********************************************************************/

CAMLexport value ocaml_freetds_dbsqlexec(value vdbproc, value vsql)
{
  CAMLparam2(vdbproc, vsql);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);

  RETCODE ret = dbcmd(dbproc, String_val(vsql));
  maybe_raise_userdata_exn(dbproc);
  if (ret == FAIL)
  {
    raise_error(SEVERITY_RESOURCE, "Freetds.Dblib.sqlexec: cannot "
                "allocate memory to hold the SQL query");
  }

  /* Sending the query to the server resets the command buffer. */
  userdata_release_runtime_system(dbproc);
  ret = dbsqlexec(dbproc);
  assert(!userdata_acquire_runtime_system(dbproc));

  maybe_raise_userdata_exn(dbproc);
  if (ret == FAIL)
  {
    caml_raise_not_found(); /* More precise error on the OCaml side. */
  }
  CAMLreturn(Val_unit);
}

CAMLexport value ocaml_freetds_dbresults(value vdbproc)
{
  CAMLparam1(vdbproc);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);
  userdata_release_runtime_system(dbproc);
  RETCODE erc = dbresults(dbproc);
  assert(!userdata_acquire_runtime_system(dbproc));
  maybe_raise_userdata_exn(dbproc);
  if (erc == FAIL)
  {
    raise_error(SEVERITY_PROGRAM,
                "Freetds.Dblib.results: query was not processed "
                "successfully by the server");
  }
  CAMLreturn(Val_bool(erc == SUCCEED));
}

CAMLexport value ocaml_freetds_numcols(value vdbproc)
{
  /* noalloc */
  CAMLparam1(vdbproc);
  DBPROCESS* dbproc = DBPROCESS_VAL(vdbproc);
  int num_cols = dbnumcols(dbproc);
  maybe_raise_userdata_exn(dbproc);
  CAMLreturn(Val_int(num_cols));
}

CAMLexport value ocaml_freetds_dbcolname(value vdbproc, value vc)
{
  CAMLparam2(vdbproc, vc);
  CAMLlocal1(vname);
  DBPROCESS* dbproc = DBPROCESS_VAL(vdbproc);

  char *name = dbcolname(dbproc, Int_val(vc));
  maybe_raise_userdata_exn(dbproc);
  if (name == NULL)
    /* Raise an exception compatible with [coltype]. */
    raise_error(SEVERITY_PROGRAM,
                "FreeTDS.Dblib.colname: Column number out of range");

  vname = caml_copy_string(name);
  CAMLreturn(vname);
}

CAMLexport value ocaml_freetds_dbcoltype(value vdbproc, value vc)
{
  CAMLparam2(vdbproc, vc);
  DBPROCESS* dbproc = DBPROCESS_VAL(vdbproc);

  int ty = dbcoltype(dbproc, Int_val(vc));
  maybe_raise_userdata_exn(dbproc);

  /* Keep in sync with "type col_type" on the Caml side. */
  switch (ty) {
  case SYBCHAR:    CAMLreturn(Val_int(0));
  case SYBVARCHAR: CAMLreturn(Val_int(1));
  case SYBINTN: CAMLreturn(Val_int(2));
  case SYBINT1: CAMLreturn(Val_int(3));
  case SYBINT2: CAMLreturn(Val_int(4));
  case SYBINT4: CAMLreturn(Val_int(5));
  case SYBINT8: CAMLreturn(Val_int(6));
  case SYBFLT8: CAMLreturn(Val_int(7));
  case SYBFLTN: CAMLreturn(Val_int(8));
  case SYBNUMERIC: CAMLreturn(Val_int(9));
  case SYBDECIMAL: CAMLreturn(Val_int(10));
  case SYBDATETIME: CAMLreturn(Val_int(11));
  case SYBDATETIME4: CAMLreturn(Val_int(12));
  case SYBDATETIMN: CAMLreturn(Val_int(13));
  case SYBBIT: CAMLreturn(Val_int(14));
  case SYBTEXT: CAMLreturn(Val_int(15));
  case SYBIMAGE: CAMLreturn(Val_int(16));
  case SYBMONEY4: CAMLreturn(Val_int(17));
  case SYBMONEY: CAMLreturn(Val_int(18));
  case SYBMONEYN: CAMLreturn(Val_int(19));
  case SYBREAL: CAMLreturn(Val_int(20));
  case SYBBINARY: CAMLreturn(Val_int(21));
  case SYBVARBINARY: CAMLreturn(Val_int(22));
  }
  if (ty == -1)
    /* The handler catches this exception.  Raise a compatible one. */
    raise_error(SEVERITY_PROGRAM,
                "FreeTDS.Dblib.coltype: Column number out of range");
  else
    raise_error(SEVERITY_CONSISTENCY,
                "Freetds.Dblib.coltype: unknown column type "
                "(contact the author of the OCaml bindings)");
}

CAMLexport value ocaml_freetds_dbcancel(value vdbproc)
{
  CAMLparam1(vdbproc);
  DBPROCESS* dbproc = DBPROCESS_VAL(vdbproc);

  userdata_release_runtime_system(dbproc);
  dbcancel(dbproc);
  assert(!userdata_acquire_runtime_system(dbproc));
  maybe_raise_userdata_exn(dbproc);

  CAMLreturn(Val_unit);
}

CAMLexport value ocaml_freetds_dbcanquery(value vdbproc)
{
  CAMLparam1(vdbproc);
  DBPROCESS* dbproc = DBPROCESS_VAL(vdbproc);

  userdata_release_runtime_system(dbproc);
  dbcanquery(dbproc);
  assert(!userdata_acquire_runtime_system(dbproc));
  maybe_raise_userdata_exn(dbproc);

  CAMLreturn(Val_unit);
}

CAMLexport value ocaml_freetds_dbnextrow(value vdbproc)
{
  /* noalloc */
  CAMLparam1(vdbproc);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);

  userdata_release_runtime_system(dbproc);
  STATUS st = dbnextrow(dbproc);
  assert(!userdata_acquire_runtime_system(dbproc));
  maybe_raise_userdata_exn(dbproc);

  switch (st) {
  case REG_ROW:
    CAMLreturn(Val_int(st));
    break;

  case NO_MORE_ROWS:
    caml_raise_not_found();
    break;

  case FAIL:
    /* The handler should be triggered (this should not occur). */
    raise_error(SEVERITY_RESOURCE, "Freetds.Dblib.nextrow: failed");
    break;

  case BUF_FULL:
    raise_error(SEVERITY_RESOURCE, "Freetds.Dblib.nextrow: buffer full"
                " (report to the developer of this library)");
    break;

  default:
    CAMLreturn(Val_int(st));
  }
}

CAMLexport value ocaml_freetds_dbdata(value vdbproc, value vc)
{
  CAMLparam2(vdbproc, vc);
  CAMLlocal1(vdata);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);
  vdata = caml_alloc_small(1, Abstract_tag);
  Field(vdata, 0) = (value)dbdata(dbproc, Int_val(vc));
  maybe_raise_userdata_exn(dbproc);

  CAMLreturn(vdata);
}

CAMLexport value ocaml_freetds_is_null(value vdata)
{
  /* noalloc */
  CAMLparam1(vdata);
  CAMLreturn(Val_bool((BYTE *)Field(vdata, 0) == NULL));
}

CAMLexport value ocaml_freetds_dbdatlen(value vdbproc, value vc)
{
  /* noalloc */
  CAMLparam2(vdbproc, vc);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);

  int len = dbdatlen(dbproc, Int_val(vc));
  maybe_raise_userdata_exn(dbproc);

  CAMLreturn(Val_int(len));
}

CAMLexport value ocaml_freetds_get_data(value vdbproc, value vcol,
                                        value vdata)
{
  CAMLparam3(vdbproc, vcol, vdata);
  CAMLlocal2(vres, vconstructor);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);
  BYTE *data = (BYTE *)Field(vdata, 0);
  BYTE *data_byte;
  DBINT len, converted_len;
  int col = Int_val(vcol), ty, data_int;
  double data_double;
  DBDATEREC di;
  RETCODE ret;

/* Taken from the implementation of caml_copy_string */
#define COPY_STRING(res, s, len_bytes)           \
  res = caml_alloc_string(len_bytes);                  \
  memmove(String_val(res), s, len_bytes);

#define CONVERT_STRING(destlen)                                         \
  data_byte = caml_stat_alloc(destlen); /* printable size */            \
  converted_len =                                                       \
    dbconvert(dbproc, ty, data, len, SYBCHAR, data_byte, destlen);      \
  if (converted_len >= 0) {                                             \
    COPY_STRING(vres, (char *) data_byte, converted_len);               \
  }                                                                     \
  caml_stat_free(data_byte);                                            \
  maybe_raise_userdata_exn(dbproc);                                     \
  if (converted_len < 0) {                                              \
    raise_error(SEVERITY_RESOURCE,                                      \
                "Freetds.Dblib.nextrow: problem with copying strings. " \
                "Please contact the author of the Freetds bindings.");  \
  }

#define CONSTRUCTOR(tag, value)        \
  vconstructor = caml_alloc(1, tag);   \
  Store_field(vconstructor, 0, value)

  len = dbdatlen(dbproc, col);
  maybe_raise_userdata_exn(dbproc);
  if (len < 0)
  {
    // Column number out of range
    raise_error(SEVERITY_CONSISTENCY,
                "Freetds.Dblib.nextrow: column number not in range. "
                "Please write to the authors of the OCaml FreeTDS bindings.");
  }

  ty = dbcoltype(dbproc, col);
  maybe_raise_userdata_exn(dbproc);

  switch (ty) {
  case SYBCHAR:    /* fall-through */
  case SYBVARCHAR:
  case SYBTEXT:
    COPY_STRING(vres, data, len);
    CONSTRUCTOR(0, vres);
    break;
  case SYBIMAGE:
  case SYBBINARY:
  case SYBVARBINARY:
    COPY_STRING(vres, data, len);
    CONSTRUCTOR(10, vres);
    break;

  case SYBINT1:
    dbconvert(dbproc, ty, data, len,
              SYBINT4, (BYTE*) &data_int, sizeof(int));
    maybe_raise_userdata_exn(dbproc);
    CONSTRUCTOR(1, Val_int(data_int));
    break;
  case SYBINT2:
    dbconvert(dbproc, ty, data, len,
              SYBINT4, (BYTE*) &data_int, sizeof(int));
    maybe_raise_userdata_exn(dbproc);
    CONSTRUCTOR(2, Val_int(data_int));
    break;
  case SYBINTN:
  case SYBINT4:
    data_int = *((int*)data);
#if OCAML_WORD_SIZE == 32
    if (-1073741824 <= data_int && data_int < 1073741824)
      CONSTRUCTOR(3, Val_int(data_int));
    else /* require more than 31 bits allow for */
      CONSTRUCTOR(4, caml_copy_int32(data_int));
#else
    CONSTRUCTOR(3, Val_int(data_int));
#endif
    break;
  case SYBINT8:
    CONSTRUCTOR(5, caml_copy_int64(* (int64_t *) data));
    break;

  case SYBFLT8:
    CONSTRUCTOR(6, caml_copy_double(* (double *) data));
    break;
  case SYBFLTN:
  case SYBREAL:
    dbconvert(dbproc, ty, data, len,
              SYBFLT8, (BYTE*) &data_double, sizeof(double));
    maybe_raise_userdata_exn(dbproc);
    CONSTRUCTOR(6, caml_copy_double(data_double));
    break;

  case SYBNUMERIC:
    CONVERT_STRING(ceil(2.5 * len)); /* FIXME: max size ? */
    CONSTRUCTOR(11, vres);
    break;
  case SYBDECIMAL:
    CONVERT_STRING(ceil(2.5 * len)); /* FIXME: max size ? */
    CONSTRUCTOR(12, vres);
    break;

  case SYBBIT:
    CONSTRUCTOR(9, Val_bool(*data != '\0'));
    break;

  case SYBDATETIME:
  case SYBDATETIME4:
  case SYBDATETIMN:
    ret = dbdatecrack(dbproc, &di, (DBDATETIME *) data);
    maybe_raise_userdata_exn(dbproc);
    if(ret == FAIL) {
      raise_error(SEVERITY_CONSISTENCY,
                  "Freetds.Dblib.nextrow: date conversion failed. "
                  "Please contact the author of these bindings.");
    }
    vconstructor = caml_alloc(/* size: */ 8, /* tag: */ 7);
#ifdef MSDBLIB
    /* http://msdn.microsoft.com/en-us/library/aa937027%28SQL.80%29.aspx */
    Store_field(vconstructor, 0, Val_int(di.year));
    Store_field(vconstructor, 1, Val_int(di.month));
    Store_field(vconstructor, 2, Val_int(di.day));
    Store_field(vconstructor, 3, Val_int(di.hour));
    Store_field(vconstructor, 4, Val_int(di.minute));
    Store_field(vconstructor, 5, Val_int(di.second));
    Store_field(vconstructor, 6, Val_int(di.millisecond));
    Store_field(vconstructor, 7, Val_int(di.tzone));
#else
    /* From sybdb.h */
    Store_field(vconstructor, 0, Val_int(di.dateyear));
    Store_field(vconstructor, 1, Val_int(di.datemonth));
    Store_field(vconstructor, 2, Val_int(di.datedmonth));
    Store_field(vconstructor, 3, Val_int(di.datehour));
    Store_field(vconstructor, 4, Val_int(di.dateminute));
    Store_field(vconstructor, 5, Val_int(di.datesecond));
    Store_field(vconstructor, 6, Val_int(di.datemsecond));
    Store_field(vconstructor, 7, Val_int(di.datetzone));
#endif
    break;

  case SYBMONEY4:
  case SYBMONEY:
  case SYBMONEYN:
    dbconvert(dbproc, ty, data, len,
              SYBFLT8, (BYTE*) &data_double, sizeof(double));
    maybe_raise_userdata_exn(dbproc);
    CONSTRUCTOR(8, caml_copy_double(data_double));
    break;

  default:
    if (ty == -1)
      raise_error(SEVERITY_CONSISTENCY,
                  "Freetds.Dblib.nextrow: column number not in range. "
                  "Please write to the authors of the OCaml FreeTDS bindings.");
    else
      raise_error(SEVERITY_CONSISTENCY,
                  "Freetds.Dblib.nextrow: dbcoltype not handled (C stub)\n");
  }
  CAMLreturn(vconstructor);
}


CAMLexport value ocaml_freetds_dbcount(value vdbproc)
{
  CAMLparam1(vdbproc);
  CAMLlocal1(vcount);
  DBPROCESS *dbproc = DBPROCESS_VAL(vdbproc);
  vcount = dbcount(dbproc);
  maybe_raise_userdata_exn(dbproc);
  CAMLreturn(Val_int(vcount));
}


CAMLexport value ocaml_freetds_dbsettime(value vseconds)
{
  CAMLparam1(vseconds);
  /* http://manuals.sybase.com/onlinebooks/group-cnarc/cng1110e/dblib/@ebt-link;pt=16561?target=%25N%15_39241_START_RESTART_N%25 */
  if (dbsettime(Int_val(vseconds)) == FAIL) {
    raise_error(SEVERITY_PROGRAM, "Freetds.Dblib.settime");
  }
  CAMLreturn(Val_unit);
}
