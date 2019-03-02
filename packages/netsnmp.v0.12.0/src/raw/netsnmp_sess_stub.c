#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/threads.h>

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include "netsnmp_error_helper.h"
#include "netsnmp_helper_fun.h"
#include "netsnmp_stubs_mutex.h"

/** Interface between OCaml and the mutex protected library functions. Do
 *  not call Net-SNMP API functions from here, add a *_mutex version to
 *  the netsnmp_stubs_mutex.c file and call that.
 *
 *  The OCaml runtime lock is managed in this file
 */

/* Not defined by mlvalues.h */
#define Val_unsigned_int(x) ((x<<1)+1)

#define AUTH_PROTO_IGNORE                 (0)
#define AUTH_PROTO_USMHMACMD5AUTHPROTOCOL (1)

static long ml_snmp_version_to_c(int v)
{
  switch (v)
  {
    case 0:  return SNMP_VERSION_1;
    case 1:  return SNMP_VERSION_2c;
    case 2:  return SNMP_VERSION_3;
    default: caml_failwith("unknown snmp version");
  }
}

static int ml_snmp_sec_auth_proto_to_c(int v)
{
  switch (v)
  {
    case 0:  return AUTH_PROTO_IGNORE;
    case 1:  return AUTH_PROTO_USMHMACMD5AUTHPROTOCOL;
    default: caml_failwith("unknown snmp auth protocol");
  }
}

CAMLprim value caml_snmp_sess_init(value unit __attribute__((unused)))
{
  CAMLparam0();
  CAMLlocal1(ml_session);
  netsnmp_session *session;


  ml_session = caml_alloc_string(sizeof *session);
  session = (netsnmp_session *)String_val(ml_session);
  caml_release_runtime_system();
  snmp_sess_init_mutex(session);
  caml_acquire_runtime_system();

  CAMLreturn(ml_session);
}

CAMLprim value caml_snmp_sess_open(value ml_session, value ml_session_cfg)
{
  CAMLparam2(ml_session, ml_session_cfg);
  CAMLlocal1(ml_session_handle);
  netsnmp_session *session;
  void *session_handle;
  const char *s;

 /* ml_session_cfg
  *
  *   version              : snmp_version         [0]
  * ; retries              : int                  [1]
  * ; timeout              : int                  [2]
  * ; peername             : string               [3]
  * ; localname            : string               [4]
  * ; local_port           : int                  [5]
  * v1/2c
  * ; community            : string               [6]
  * v3
  * ; securityName         : string               [7]
  * ; securityAuthProto    : snmp_sec_auth_proto  [8]
  * ; securityAuthPassword : string               [9]
  */

  session = (netsnmp_session *)String_val(ml_session);
  session->version = ml_snmp_version_to_c(Int_val(Field(ml_session_cfg,0)));

  if (Int_val(Field(ml_session_cfg,1)) > 0)
    session->retries = Int_val(Field(ml_session_cfg,1));

  if (Int_val(Field(ml_session_cfg,2)) > 0)
    session->timeout = Int_val(Field(ml_session_cfg,2));

  session->peername = (char *) String_val(Field(ml_session_cfg,3));

  s = String_val(Field(ml_session_cfg,4));
  if (strlen(s) == 0 || (strlen(s) == 1 && strcmp(s, "0")))
    session->localname = (char *) String_val(Field(ml_session_cfg,4));

  if (Int_val(Field(ml_session_cfg,5)) > 0)
    session->local_port = Int_val(Field(ml_session_cfg,5));

  switch (session->version)
  {
    case SNMP_VERSION_1:
    case SNMP_VERSION_2c:
      session->community = (u_char *)String_val(Field(ml_session_cfg,6));
      session->community_len = strlen((const char *)session->community);
      break;
    case SNMP_VERSION_3:
      session->securityName = (char *) String_val(Field(ml_session_cfg,7));
      session->securityNameLen = strlen(session->securityName);
      switch (ml_snmp_sec_auth_proto_to_c(Field(ml_session_cfg,8)))
      {
        case AUTH_PROTO_IGNORE:
          break;
        case AUTH_PROTO_USMHMACMD5AUTHPROTOCOL:
          session->securityAuthProto = usmHMACMD5AuthProtocol;
          session->securityAuthProtoLen = sizeof(usmHMACMD5AuthProtocol)/sizeof(oid);
          session->securityAuthKeyLen = USM_AUTH_KU_LEN;
          s = String_val(Field(ml_session_cfg,9));
          if (generate_Ku(session->securityAuthProto, session->securityAuthProtoLen,
                          (u_char *) s, strlen(s),
                          session->securityAuthKey,
                          &session->securityAuthKeyLen) != SNMPERR_SUCCESS) {
            caml_failwith("failed to generate Ku from auth pass phrase");
          }
          break;
        default: caml_failwith("unknown version 3 securityAuthProto");
      }
      break;
    default: caml_failwith("unknown snmp version when creating session");
  }
  caml_release_runtime_system();
  SOCK_STARTUP;
  session_handle = snmp_sess_open_mt( session );
  caml_acquire_runtime_system();

  if (!session_handle)
  {
    caml_release_runtime_system();
    SOCK_CLEANUP;
    caml_acquire_runtime_system();
    caml_failwith("snmp_sess_open failed");
  }

  p_to_ml_value(session_handle,ml_session_handle);
  CAMLreturn(ml_session_handle);
}

CAMLprim value caml_snmp_sess_close(value ml_handle)
{
  void *handle;
  ml_value_to_p(ml_handle, handle);
  caml_release_runtime_system();
  snmp_sess_close_mt(handle);
  SOCK_CLEANUP;
  caml_acquire_runtime_system();

  return Val_unit;
}

static value oid_value_data_uint(int tag, uint val)
{
  CAMLparam0();
  CAMLlocal1(ml_res);

  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, Val_int(val));

  CAMLreturn(ml_res);
}

static value oid_value_data_int(int tag, int val)
{
  CAMLparam0();
  CAMLlocal1(ml_res);

  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, Val_int(val));

  CAMLreturn(ml_res);
}

static value oid_value_data_int64(int tag, int64_t val)
{
  CAMLparam0();
  CAMLlocal1(ml_res);

  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, caml_copy_int64(val));

  CAMLreturn(ml_res);
}

static value oid_value_data_uint64(int tag, u_long high, u_long low)
{
  CAMLparam0();
  CAMLlocal2(ml_res, ml_val);

  ml_val = caml_alloc_tuple(2);
  Store_field(ml_val, 0, Val_unsigned_int(high));
  Store_field(ml_val, 1, Val_unsigned_int(low));

  high = 0;  high = Long_val(Field(ml_val, 0));

  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, ml_val);

  CAMLreturn(ml_res);
}

static value oid_value_data_double(int tag, double val)
{
  CAMLparam0();
  CAMLlocal1(ml_res);

  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, caml_copy_double(val));

  CAMLreturn(ml_res);
}

static value oid_value_data_string(int tag, char *strval, int str_len)
{
  CAMLparam0();
  CAMLlocal2(ml_res, ml_str);

  ml_res = caml_alloc(1, tag);
  ml_str = caml_alloc_string(str_len);
  memmove(Bytes_val(ml_str), strval, str_len);
  Store_field(ml_res, 0, ml_str);

  CAMLreturn(ml_res);
}

static value oid_value_data_oid(int tag, oid *oidval, int oid_len)
{
  CAMLparam0();
  CAMLlocal3(ml_res, ml_oid, ml_objid);

  ml_objid = caml_alloc_string(oid_len * (sizeof *oidval));
  memmove(Bytes_val(ml_objid), (char *)oidval, oid_len * (sizeof *oidval));
  ml_oid = caml_alloc(2, 0);
  Store_field(ml_oid, 0, ml_objid);
  Store_field(ml_oid, 1, Val_int(oid_len));

  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, ml_oid);

  CAMLreturn(ml_res);
}

static value oid_value_error(int tag, int err)
{
  CAMLparam0();
  CAMLlocal2(ml_res, ml_err);

  /*
     Snmp_nosuchobject         (0)
   | Snmp_nosuchinstance       (1)
   | Snmp_endofmibview         (2)
   | Snmp_unknown_error of int (0)
   */
  switch (err)
  {
    case SNMP_NOSUCHOBJECT:     ml_err = Val_int(0); break;
    case SNMP_NOSUCHINSTANCE:   ml_err = Val_int(1); break;
    case SNMP_ENDOFMIBVIEW:     ml_err = Val_int(2); break;
    default: ml_err = caml_alloc(1, 0); Store_field(ml_err, 0, Val_int(err)); break;
  }
  ml_res = caml_alloc(1, tag);
  Store_field(ml_res, 0, ml_err);

  CAMLreturn(ml_res);
}

CAMLprim value caml_snmp_sess_synch_response(value ml_handle, value ml_pdu)
{
  CAMLparam2(ml_handle, ml_pdu);
  CAMLlocal5(exn_msg, ml_err, ml_node_value, ml_values, cons);
  CAMLlocal3(ml_node, ml_objid, ml_oid);
  void *handle;
  netsnmp_pdu *pdu, **ppdu;
  netsnmp_pdu *response;
  int status;
  netsnmp_variable_list *var;
  u_char *ip;
  char buf[32];
  struct counter64 *c64;
  int64_t i64;

  ml_values = Val_emptylist;
  ml_value_to_p(ml_handle, handle);
  ppdu = (netsnmp_pdu **)Data_custom_val(ml_pdu);
  pdu = *ppdu;

  caml_release_runtime_system();
  status = snmp_sess_synch_response_mt(handle, pdu, &response);
  caml_acquire_runtime_system();
  *ppdu = NULL;

  if (status == STAT_SUCCESS && response->errstat == SNMP_ERR_NOERROR)
  {

    /*
    type ASN1_value =
     | ASN_Null                           (0)
     | ASN_error of oid_value_error       (0)
     | ASN_Integer of int                 (1)
     | ASN_Gauge of int                   (2)
     | ASN_Counter of int                 (3)
     | ASN_Timeticks of int               (4)
     | ASN_Uinteger of int                (5)
     | ASN_String of string               (6)
     | ASN_Opaque of string               (7)
     | ASN_Ipaddress of string            (8)
     | ASN_Objid of oid                   (9)
     | ASN_Counter64 of counter64         (10)
     | ASN_Bitstring of string            (11)
     | ASN_Opaque_counter64 of counter64  (12)
     | ASN_Opaque_u64 of counter64        (13)
     | ASN_Opaque_i64 of int64            (14)
     | ASN_Opaque_float of float          (15)
     | ASN_Opaque_double of float         (16)
    */

    for (var = response->variables; var; var = var->next_variable)
    {
      switch (var->type)
      {
       case ASN_INTEGER:
         ml_node_value = oid_value_data_int(1, *var->val.integer);
         break;
       case ASN_GAUGE:
         ml_node_value = oid_value_data_uint(2, *var->val.integer);
         break;
       case ASN_COUNTER:
         ml_node_value = oid_value_data_uint(3, *var->val.integer);
         break;
       case ASN_TIMETICKS:
         ml_node_value = oid_value_data_uint(4, *var->val.integer);
         break;
       case ASN_UINTEGER:
         ml_node_value = oid_value_data_uint(5, (unsigned int)*var->val.integer);
         break;
       case ASN_OCTET_STR:
         ml_node_value = oid_value_data_string(6, (char*)var->val.string, var->val_len);
         break;
       case ASN_OPAQUE:
         ml_node_value = oid_value_data_string(7, (char*)var->val.string, var->val_len);
         break;
       case ASN_IPADDRESS:
         ip = (u_char*)var->val.string;
         snprintf(buf, sizeof buf, "%d.%d.%d.%d", ip[0], ip[1], ip[2], ip[3]);
         buf[(sizeof buf)-1] = '\0';
         ml_node_value = oid_value_data_string(8, buf, strlen(buf));
         break;
       case ASN_NULL:
         ml_node_value = Val_int(0);
         break;
       case ASN_OBJECT_ID:
         ml_node_value = oid_value_data_oid(9, (oid *)var->val.objid, var->val_len/sizeof(oid));
         break;
       case ASN_COUNTER64:
         c64 = (struct counter64 *)var->val.counter64;
         ml_node_value = oid_value_data_uint64(10, c64->high, c64->low);
         break;
       case ASN_BIT_STR:
         ml_node_value = oid_value_data_string(11, (char*)var->val.bitstring, var->val_len);
         break;
    #ifdef NETSNMP_WITH_OPAQUE_SPECIAL_TYPES
       case ASN_OPAQUE_COUNTER64:
         c64 = (struct counter64 *)var->val.counter64;
         ml_node_value = oid_value_data_uint64(12, c64->high, c64->low);
         break;
       case ASN_OPAQUE_U64:
         c64 = (struct counter64 *)var->val.counter64;
         ml_node_value = oid_value_data_uint64(13, c64->high, c64->low);
         break;

       case ASN_OPAQUE_I64:
         c64 = (struct counter64 *)var->val.counter64;
         i64 = (((uint64_t)c64->high) << 32) | (uint64_t)c64->low;
         ml_node_value = oid_value_data_int64(14, i64);
         break;

       case ASN_OPAQUE_FLOAT:
         if (var->val.floatVal)
           ml_node_value = oid_value_data_double(15, (double)(*var->val.floatVal));
         else
           ml_node_value = oid_value_error(0, var->type);
         break;

       case ASN_OPAQUE_DOUBLE:
         if (var->val.doubleVal)
           ml_node_value = oid_value_data_double(16, *var->val.doubleVal);
         else
           ml_node_value = oid_value_error(0, var->type);
         break;
    #endif

       case SNMP_NOSUCHOBJECT:
       case SNMP_NOSUCHINSTANCE:
       case SNMP_ENDOFMIBVIEW:
       case ASN_NSAP:
       default:
          ml_node_value = oid_value_error(0, var->type);
          break;
      }

      ml_objid = caml_alloc_string(var->name_length * (sizeof *(var->name)));
      memmove(Bytes_val(ml_objid), (char *)var->name, var->name_length * (sizeof *(var->name)));
      ml_oid = caml_alloc(2, 0);
      Store_field(ml_oid, 0, ml_objid);
      Store_field(ml_oid, 1, Val_int(var->name_length));

      ml_node = caml_alloc(2, 0);
      Store_field(ml_node, 0, ml_oid);
      Store_field(ml_node, 1, ml_node_value);

      /* add to list */
      cons = caml_alloc(2, 0);
      Store_field(cons, 0, ml_node);
      Store_field(cons, 1, ml_values);
      ml_values = cons;
    }
  }
  else
  {
    if (status == STAT_SUCCESS)
    {
      ml_err = caml_alloc(2, 0);
      Store_field(ml_err, 0, Val_int(snmp_pdu_error_status_to_ml_variant(response->errstat)));
      Store_field(ml_err, 1, caml_copy_string(snmp_errstring(response->errstat)));
      netsnmp_raise_ocaml_exception("Netsnmp_response_error", ml_err);
    }
    else if (status == STAT_TIMEOUT)
    {
      caml_raise_constant(*caml_named_value("Netsnmp_request_timeout"));
    }
    else
    {
      int perror, psnmperr;
      char *perrstr;

      snmp_sess_error(handle, &perror, &psnmperr, &perrstr);
      ml_err = caml_alloc(3, 0);
      Store_field(ml_err, 0, Val_int(perror));
      Store_field(ml_err, 1, Val_int(snmperr_to_ml_variant(psnmperr)));
      Store_field(ml_err, 2, caml_copy_string(perrstr));
      free(perrstr);
      netsnmp_raise_ocaml_exception("Netsnmp_general_error", ml_err);
    }
  }

  if (response)
  {
    caml_release_runtime_system();
    snmp_free_pdu_mutex(response);
    caml_acquire_runtime_system();
  }

  CAMLreturn(ml_values);
}
