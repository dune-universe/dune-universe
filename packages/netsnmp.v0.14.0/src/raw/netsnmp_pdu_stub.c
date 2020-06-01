#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/threads.h>

#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "netsnmp_helper_fun.h"
#include "netsnmp_error_helper.h"
#include "netsnmp_helper_fun.h"
#include "netsnmp_stubs_mutex.h"

/** Interface between OCaml and the mutex protected library functions. Do
 *  not call Net-SNMP API functions from here, add a *_mutex version to
 *  the netsnmp_stubs_mutex.c file and call that.
 *
 *  The OCaml runtime lock is managed in this file
 */

int ml_pdu_type_to_c(int ml_type)
{
  switch (ml_type)
  {
    case 0: return SNMP_MSG_GET;
    case 1: return SNMP_MSG_GETNEXT;
    case 2: return SNMP_MSG_RESPONSE;
    case 3: return SNMP_MSG_SET;
    case 4: return SNMP_MSG_TRAP;
    case 5: return SNMP_MSG_GETBULK;
    case 6: return SNMP_MSG_INFORM;
    case 7: return SNMP_MSG_TRAP2;
    case 8: return SNMP_MSG_REPORT;
    default: caml_failwith("unknown snmp pdu type");
  }
}

static void pdu_finalize(value ml_value)
{
  netsnmp_pdu **ppdu;

  ppdu = (netsnmp_pdu **)Data_custom_val(ml_value);
  if (*ppdu != NULL)
  {
    snmp_free_pdu_mutex(*ppdu);
    *ppdu = NULL;
  }
}

static struct custom_operations pdu_custom_ops = {
  "pdu",
  pdu_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
#ifdef custom_fixed_length_default
  custom_fixed_length_default,
#endif
};

CAMLprim value caml_snmp_pdu_create(value ml_pdu_type)
{
  CAMLparam1(ml_pdu_type);
  CAMLlocal1(ml_netsnmp_pdu);
  netsnmp_pdu *pdu, **ppdu;
  int pdu_type = ml_pdu_type_to_c(Int_val(ml_pdu_type));

  caml_release_runtime_system();
  pdu = snmp_pdu_create_mutex(pdu_type);
  caml_acquire_runtime_system();

  ml_netsnmp_pdu = caml_alloc_custom(&pdu_custom_ops, sizeof pdu, 0, 1);
  ppdu = (netsnmp_pdu **)Data_custom_val(ml_netsnmp_pdu);
  *ppdu = pdu;

  CAMLreturn(ml_netsnmp_pdu);
}

CAMLprim value caml_snmp_add_null_var(value ml_pdu, value ml_oid)
{
  CAMLparam0();
  oid *objid = (oid *)String_val(Field(ml_oid, 0));
  int objid_len = Int_val(Field(ml_oid, 1));
  netsnmp_pdu *pdu;

  pdu = *((netsnmp_pdu **)Data_custom_val(ml_pdu));
  snmp_add_null_var_mutex(pdu, objid, objid_len);
  CAMLreturn(ml_pdu);
}

static int value_error_to_type(value ml_error)
{
  /*
     Snmp_nosuchobject         (0)
   | Snmp_nosuchinstance       (1)
   | Snmp_endofmibview         (2)
   | Snmp_unknown_error of int (0)
   */

  /* For known types the variant maps to the result, for unknown it's the
   * value contained in the variant
   */
  return Int_val(Is_block(ml_error) ? Field(ml_error, 0) : ml_error);
}

static struct counter64 asn_counter64_of_value(value ml_value)
{
  unsigned int high, low;
  struct counter64 c64;

  high = Field(ml_value, 0);
  low = Field(ml_value, 1);

  c64.high = high; c64.low = low;

  return c64;
}

static netsnmp_variable_list *local_snmp_pdu_add_variable(netsnmp_pdu *pdu,
  const oid * name, size_t name_length, u_char type, const void * value, size_t len)
{
  netsnmp_variable_list *ret;

  caml_release_runtime_system();
  ret = snmp_pdu_add_variable_mutex(pdu, name, name_length, type, value, len);
  caml_acquire_runtime_system();

  return ret;
}

CAMLprim value caml_snmp_add_variable(value ml_pdu, value ml_oid, value ml_value)
{
  CAMLparam0();
  value ml_v;
  oid *objid = (oid *)String_val(Field(ml_oid, 0));
  int objid_len = Int_val(Field(ml_oid, 1));
  netsnmp_pdu *pdu;
  oid *vobjid;
  int vobjid_len;
  int stype;
  int i;
  uint u;
  const char *s, *sip;
  int64_t i64;
  struct counter64 c64;
  u_char ip[4];
  double d;
  float f;

  pdu = *((netsnmp_pdu **)Data_custom_val(ml_pdu));

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

  if (Is_block(ml_value))
  {
    ml_v = Field(ml_value, 0);

    switch (Tag_val(ml_value))
    {
      case 0:
        stype = value_error_to_type(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, stype, NULL, 0);
        break;

      case 1:
        i = Int_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_INTEGER, &i, sizeof i);
        break;

      case 2:
        u = (uint)Int_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_GAUGE, &u, sizeof u);
        break;

      case 3:
        u = (uint)Int_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_COUNTER, &u, sizeof u);
        break;

      case 4:
        u = (uint)Int_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_TIMETICKS, &u, sizeof u);
        break;

      case 5:
        u = (uint)Int_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_UINTEGER, &u, sizeof u);
        break;

      case 6:
        s = String_val(ml_v);  i = caml_string_length(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OCTET_STR, s, i);
        break;

      case 7:
        s = String_val(ml_v);  i = caml_string_length(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OPAQUE, s, i);
        break;

      case 8:
        sip = String_val(ml_v);
        for (i = 0; i < 4; i++)
        {
          ip[i] = atoi(sip);
          if ((s = strchr(sip, '.')) != NULL) sip = s + 1;
          else sip = sip + strlen(sip);
        }
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_IPADDRESS, ip, sizeof ip);
        break;

      case 9:
        vobjid = (oid *)String_val(Field(ml_value, 0));
        vobjid_len = Int_val(Field(ml_value, 1));
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OBJECT_ID, vobjid, vobjid_len * sizeof(oid));
        break;

      case 10:
        c64 = asn_counter64_of_value(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_COUNTER64, &c64, sizeof c64);
        break;

      case 11:
        s = String_val(ml_v);  i = caml_string_length(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_BIT_STR, s, i);
        break;

      case 12:
        c64 = asn_counter64_of_value(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OPAQUE_COUNTER64, &c64, sizeof c64);
        break;

      case 13:
        c64 = asn_counter64_of_value(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OPAQUE_U64, &c64, sizeof c64);
        break;

      case 14:
        i64 = Int64_val(ml_v);
        c64.high = (((uint64_t)i64) >> 32) & ((uint)~0);
        c64.low = ((uint64_t)i64) & ((uint)~0);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OPAQUE_I64, &c64, sizeof c64);
        break;

      case 15:
        f = (float)Double_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OPAQUE_FLOAT, &f, sizeof f);
        break;

      case 16:
        d = Double_val(ml_v);
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_OPAQUE_DOUBLE, &d, sizeof d);
        break;

      default:
        caml_failwith("unknown snmp data type");
        break;
    }
  }
  else
  {
    switch (Int_val(ml_value))
    {
      case 0:
        local_snmp_pdu_add_variable(pdu, objid, objid_len, ASN_NULL, NULL, 0);
        break;
    }
  }
  CAMLreturn(ml_pdu);
}
