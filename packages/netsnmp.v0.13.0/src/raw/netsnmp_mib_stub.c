#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/threads.h>
#include "netsnmp_stubs_mutex.h"

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include "netsnmp_helper_fun.h"

/** Interface between OCaml and the mutex protected library functions. Do
 *  not call Net-SNMP API functions from here, add a *_mutex version to
 *  the netsnmp_stubs_mutex.c file and call that.
 *
 *  The OCaml runtime lock is managed in this file
 */

CAMLprim value caml_netsnmp_init_mib(value unit __attribute__((unused)))
{
  caml_release_runtime_system();
  netsnmp_init_mib_mutex();
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_shutdown_mib(value unit __attribute__((unused)))
{
  caml_release_runtime_system();
  shutdown_mib_mutex();
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_add_mibdir(value v)
{
  const char *dirname = String_val(v);
  int res;

  caml_release_runtime_system();
  res = add_mibdir_mutex(dirname);
  caml_acquire_runtime_system();

  return Val_int(res);
}

CAMLprim value caml_netsnmp_read_module(value v)
{
  const char *name = String_val(v);

  caml_release_runtime_system();
  netsnmp_read_module_mutex(name);
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_read_mib(value v)
{
  const char *filename = String_val(v);

  caml_release_runtime_system();
  read_mib_mutex(filename);
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_read_all_mibs(value unit __attribute__((unused)))
{
  caml_release_runtime_system();
  read_all_mibs_mutex();
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_add_module_replacement(
  value vold_module, value vnew_module, value vtag, value vlen)
{
  const char *old_module = String_val(vold_module);
  const char *new_module = String_val(vnew_module);
  const char *tag = String_val(vtag);
  int len = Int_val(vlen);

  if (strlen(tag) == 0) tag = NULL;

  caml_release_runtime_system();
  add_module_replacement_mutex(old_module, new_module, tag, len);
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_snmp_set_mib_warnings(value v)
{
  caml_release_runtime_system();
  snmp_set_mib_warnings_mutex(Int_val(v));
  caml_acquire_runtime_system();

  return Val_unit;
}

CAMLprim value caml_snmp_set_save_descriptions(value v)
{
  caml_release_runtime_system();
  snmp_set_save_descriptions_mutex(Int_val(v));
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_snmp_set_mib_errors(value v)
{
  caml_release_runtime_system();
  snmp_set_mib_errors_mutex(Int_val(v));
  caml_acquire_runtime_system();
  return Val_unit;
}

CAMLprim value caml_read_objid(value v)
{
  CAMLparam1(v);
  CAMLlocal2(ml_oib, ml_objid);
  oid anOID[MAX_OID_LEN];
  size_t anOID_len;
  const char *input = String_val(v);
  int ret;

  anOID_len = MAX_OID_LEN;
  caml_release_runtime_system();
  ret = read_objid_mutex(input, anOID, &anOID_len);
  caml_acquire_runtime_system();

  if (ret == 1)
  {
    ml_objid = caml_alloc_string(anOID_len * (sizeof *anOID));
    ml_oib = caml_alloc(2, 0);
    memmove(Bytes_val(ml_objid), (char *)anOID, anOID_len * sizeof *anOID);
    Store_field(ml_oib, 0, ml_objid);
    Store_field(ml_oib, 1, Val_int(anOID_len));
    CAMLreturn(ml_oib);
  }
  else
    caml_raise_constant(*caml_named_value("Netsnmp_error_not_found"));
}

CAMLprim value caml_get_node(value v)
{
  CAMLparam1(v);
  CAMLlocal2(ml_oib, ml_objid);
  oid anOID[MAX_OID_LEN];
  size_t anOID_len;
  const char *input = String_val(v);
  int ret;

  anOID_len = MAX_OID_LEN;
  caml_release_runtime_system();
  ret = get_node_mutex(input, anOID, &anOID_len);
  caml_acquire_runtime_system();

  if (ret == 1)
  {
    ml_objid = caml_alloc_string(anOID_len * (sizeof *anOID));
    ml_oib = caml_alloc(2, 0);
    memmove(Bytes_val(ml_objid), (char *)anOID, anOID_len * sizeof *anOID);
    Store_field(ml_oib, 0, ml_objid);
    Store_field(ml_oib, 1, Val_int(anOID_len));
    CAMLreturn(ml_oib);
  }
  else
    caml_raise_constant(*caml_named_value("Netsnmp_error_not_found"));
}

CAMLprim value caml_get_module_node(value vobjid, value vmodule)
{
  CAMLparam2(vobjid, vmodule);
  CAMLlocal2(ml_oib, ml_objid);
  oid anOID[MAX_OID_LEN];
  size_t anOID_len;
  const char *objid = String_val(vobjid);
  const char *module = String_val(vmodule);
  int ret;

  anOID_len = MAX_OID_LEN;
  caml_release_runtime_system();
  ret = get_module_node_mutex(objid, module, anOID, &anOID_len);
  caml_acquire_runtime_system();

  if (ret == 1)
  {
    ml_objid = caml_alloc_string(anOID_len * (sizeof *anOID));
    ml_oib = caml_alloc(2, 0);
    memmove(Bytes_val(ml_objid), (char *)anOID, anOID_len * sizeof *anOID);
    Store_field(ml_oib, 0, ml_objid);
    Store_field(ml_oib, 1, Val_int(anOID_len));
    CAMLreturn(ml_oib);
  }
  else
    caml_raise_constant(*caml_named_value("Netsnmp_error_not_found"));
}

static FILE *dupfd2fp(const int fd)
{
  FILE *fp;
  int nfd;

  if ((nfd = dup(fd)) < 0)
    netsnmp_raise_ocaml_exception_system_error("caml_print_mib(dup): ");

  if ((fp = fdopen(nfd, "a")) == NULL)
    netsnmp_raise_ocaml_exception_system_error("caml_print_mib(fdopen): ");

  return fp;
}

CAMLprim value caml_print_mib(value vfd)
{
  int fd = Int_val(vfd);
  FILE *fp;

  fp = dupfd2fp(fd);
  caml_release_runtime_system();
  print_mib_mutex(fp);
  caml_acquire_runtime_system();
  fclose(fp);

  return Val_unit;
}

CAMLprim value caml_fprint_objid(value vfd, value ml_oid)
{
  int fd = Int_val(vfd);
  oid *objid = (oid *)String_val(Field(ml_oid, 0));
  int objid_len = Int_val(Field(ml_oid, 1));
  FILE *fp;

  fp = dupfd2fp(fd);
  caml_release_runtime_system();
  fprint_objid_mutex(fp, objid, objid_len);
  caml_acquire_runtime_system();
  fclose(fp);

  return Val_unit;
}

/** [oom_realloc] - increase the size of a memory region, on failure clean
 *  up and call [oom_error] to raise an error after aquiring the ocaml run
 *  time lock */
static void *oom_realloc(void *buf, size_t buflen)
{
  void *rbuf = (char *)realloc(buf, buflen);

  if (rbuf == NULL)
  {
    free(buf);
    caml_acquire_runtime_system();
    oom_error();
  }
  return rbuf;
}

CAMLprim value caml_snprint_objid(value ml_oid)
{
  CAMLparam1(ml_oid);
  CAMLlocal1(ml_s);
  oid *objid = (oid *)String_val(Field(ml_oid, 0));
  int objid_len = Int_val(Field(ml_oid, 1));
  int buflen = 256;
  char *buf = (char *)malloc(buflen);

  if (buf == NULL) oom_error();
  caml_release_runtime_system();
  while (snprint_objid_mutex(buf, buflen, objid, objid_len) == -1)
  {
    buflen *= 2;
    buf = (char *)oom_realloc(buf, buflen);
  }
  caml_acquire_runtime_system();

  ml_s = caml_copy_string(buf);
  free(buf);
  CAMLreturn(ml_s);
}

CAMLprim value caml_snprint_description(value ml_oid)
{
  CAMLparam1(ml_oid);
  CAMLlocal1(ml_s);
  oid *objid = (oid *)String_val(Field(ml_oid, 0));
  int objid_len = Int_val(Field(ml_oid, 1));
  int buflen = 1024;
  char *buf = (char *)malloc(buflen);

  if (buf == NULL) oom_error();
  caml_release_runtime_system();
  while (snprint_description_mutex(buf, buflen, objid, objid_len, 132) == -1)
  {
    buflen *= 2;
    buf = (char *)oom_realloc(buf, buflen);
  }
  caml_acquire_runtime_system();

  ml_s = caml_copy_string(buf);
  free(buf);
  CAMLreturn(ml_s);
}


CAMLprim value caml_counter64_to_string(value ml_counter)
{
  CAMLparam1(ml_counter);
  CAMLlocal1(ml_res);
  char buf[32];
  uint64_t v;
  u_long low;
  u_long high;

  high = Long_val(Field(ml_counter, 0));
  low = Long_val(Field(ml_counter, 1));
  v = (high << 32) | low;
  snprintf(buf, sizeof buf - 1, "%lu", v); buf[sizeof buf - 1] = '\0';

  ml_res = caml_copy_string(buf);
  CAMLreturn(ml_res);
}
