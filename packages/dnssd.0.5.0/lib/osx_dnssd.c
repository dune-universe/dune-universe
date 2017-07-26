#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <string.h>

#if !defined(__APPLE__)
#define bool int
#define true 1
#define false 0
#endif

#if defined(__APPLE__)
#include <dns_sd.h>

int table_DNSServiceType[] = {
    kDNSServiceType_A,
    kDNSServiceType_NS,
    kDNSServiceType_MD,
    kDNSServiceType_MF,
    kDNSServiceType_CNAME,
    kDNSServiceType_SOA,
    kDNSServiceType_MB,
    kDNSServiceType_MG,
    kDNSServiceType_MR,
    kDNSServiceType_NULL,
    kDNSServiceType_WKS,
    kDNSServiceType_PTR,
    kDNSServiceType_HINFO,
    kDNSServiceType_MINFO,
    kDNSServiceType_MX,
    kDNSServiceType_TXT,
    kDNSServiceType_RP,
    kDNSServiceType_AFSDB,
    kDNSServiceType_X25,
    kDNSServiceType_ISDN,
    kDNSServiceType_RT,
    kDNSServiceType_NSAP,
    kDNSServiceType_NSAP_PTR,
    kDNSServiceType_SIG,
    kDNSServiceType_KEY,
    kDNSServiceType_PX,
    kDNSServiceType_GPOS,
    kDNSServiceType_AAAA,
    kDNSServiceType_LOC,
    kDNSServiceType_NXT,
    kDNSServiceType_EID,
    kDNSServiceType_NIMLOC,
    kDNSServiceType_SRV,
    kDNSServiceType_ATMA,
    kDNSServiceType_NAPTR,
    kDNSServiceType_KX,
    kDNSServiceType_CERT,
    kDNSServiceType_A6,
    kDNSServiceType_DNAME,
    kDNSServiceType_SINK,
    kDNSServiceType_OPT,
    kDNSServiceType_APL,
    kDNSServiceType_DS,
    kDNSServiceType_SSHFP,
    kDNSServiceType_IPSECKEY,
    kDNSServiceType_RRSIG,
    kDNSServiceType_NSEC,
    kDNSServiceType_DNSKEY,
    kDNSServiceType_DHCID,
    kDNSServiceType_NSEC3,
    kDNSServiceType_NSEC3PARAM,

    kDNSServiceType_HIP,

    kDNSServiceType_SPF,
    kDNSServiceType_UINFO,
    kDNSServiceType_UID,
    kDNSServiceType_GID,
    kDNSServiceType_UNSPEC,

    kDNSServiceType_TKEY,
    kDNSServiceType_TSIG,
    kDNSServiceType_IXFR,
    kDNSServiceType_AXFR,
    kDNSServiceType_MAILB,
    kDNSServiceType_MAILA,
    kDNSServiceType_ANY
};
#endif

CAMLprim value stub_int_of_DNSServiceType(value ty) {
  CAMLparam1(ty);
  CAMLlocal1(ret);
#if defined(__APPLE__)
  int c_ty = Int_val(ty);
  if (c_ty >= sizeof(table_DNSServiceType) / sizeof(table_DNSServiceType[0])) {
    ret = Val_int(-1);
  } else {
    ret = Val_int(table_DNSServiceType[c_ty]);
  }
#else
  caml_failwith("Only implemented on macOS");
#endif
  CAMLreturn(ret);
}

#if defined(__APPLE__)
int table_kDNSServiceErr[] = {
  /* kDNSServiceErr_NoError, -- not in the OCaml type */
  kDNSServiceErr_Unknown,
  kDNSServiceErr_NoSuchName,
  kDNSServiceErr_NoMemory,
  kDNSServiceErr_BadParam,
  kDNSServiceErr_BadReference,
  kDNSServiceErr_BadState,
  kDNSServiceErr_BadFlags,
  kDNSServiceErr_Unsupported,
  kDNSServiceErr_NotInitialized,
  kDNSServiceErr_AlreadyRegistered,
  kDNSServiceErr_NameConflict,
  kDNSServiceErr_Invalid,
  kDNSServiceErr_Firewall,
  kDNSServiceErr_Incompatible,
  kDNSServiceErr_BadInterfaceIndex,
  kDNSServiceErr_Refused,
  kDNSServiceErr_NoSuchRecord,
  kDNSServiceErr_NoAuth,
  kDNSServiceErr_NoSuchKey,
  kDNSServiceErr_NATTraversal,
  kDNSServiceErr_DoubleNAT,
  kDNSServiceErr_BadTime,
  kDNSServiceErr_BadSig,
  kDNSServiceErr_BadKey,
  kDNSServiceErr_Transient,
  kDNSServiceErr_ServiceNotRunning,
  kDNSServiceErr_NATPortMappingUnsupported,
  kDNSServiceErr_NATPortMappingDisabled,
  kDNSServiceErr_NoRouter,
  kDNSServiceErr_PollingMode,
  kDNSServiceErr_Timeout,
};
#endif

CAMLprim value error_of_kDNSServiceErr(int c_ty) {
#if defined(__APPLE__)
  value ret = Val_int(1); /* Unknown */
  for (int i = 0; i < sizeof(table_kDNSServiceErr) / sizeof(table_kDNSServiceErr[0]); i++) {
    if (table_kDNSServiceErr[i] == c_ty) {
      ret = Val_int(i);
    }
  }
  return ret;
#else
  caml_failwith("Only implemented on macOS");
#endif
}

typedef struct _query {
#if defined(__APPLE__)
  DNSServiceRef serviceRef;
#endif
  void *context;
  bool finalized;
} query;

#define Query_val(x) ((query*)Data_custom_val(x))

static void finalize_query(value v) {
  query *q = Query_val(v);
  if (!q->finalized) {
#if defined(__APPLE__)
    DNSServiceRefDeallocate(q->serviceRef);
#endif
    free(q->context);
    q->context = NULL;
  }
  q->finalized = true;
}

static struct custom_operations query_custom_ops = {
    .identifier   = "DNSServiceRef query handling",
    .finalize     = finalize_query,
    .compare      = custom_compare_default,
    .hash         = custom_hash_default,
    .serialize    = custom_serialize_default,
    .deserialize  = custom_deserialize_default
};

#if defined(__APPLE__)
static void common_callback(DNSServiceRef sdRef, DNSServiceFlags flags, uint32_t interfaceIndex,
                       DNSServiceErrorType errorCode, const char *fullname, uint16_t rrtype,
                       uint16_t rrclass, uint16_t rdlen, const void *rdata, uint32_t ttl, void *context) {
  CAMLparam0();
  CAMLlocal4(result, record, raw, code);

  static value *ocaml_f = NULL;
  if (ocaml_f == NULL) {
      ocaml_f = caml_named_value("ocaml-osx-dnssd");
  }
  if (ocaml_f == NULL) abort();
  int c_token = *(int*)context;
  if (errorCode == kDNSServiceErr_NoError) {
    record = caml_alloc(5, 0);
    raw = caml_copy_string(fullname);
    Store_field(record, 0, raw);
    Store_field(record, 1, Val_int(rrtype));
    Store_field(record, 2, Val_int(rrclass));
    raw = caml_alloc_string(rdlen);
    memcpy(String_val(raw), rdata, rdlen);
    Store_field(record, 3, raw);
    Store_field(record, 4, ttl);
    result = caml_alloc(1, 0); /* Ok */
    Store_field(result, 0, record);
    caml_callback2(*ocaml_f, Val_int(c_token), result);
  } else {
    result = caml_alloc(1, 1); /* Error */
    code = error_of_kDNSServiceErr(errorCode);
    Store_field(result, 0, code);
    caml_callback2(*ocaml_f, Val_int(c_token), result);
  }
  CAMLreturn0;
}
#endif

CAMLprim value stub_query_record(value name, value ty, value token) {
  CAMLparam3(name, ty, token);
  CAMLlocal1(v);
  v = caml_alloc_custom(&query_custom_ops, sizeof(query), 0, 1);
  query *q = Query_val(v);
  char *c_name = String_val(name);
  int c_ty = Int_val(ty);
  q->context = malloc(sizeof(int));
  *(int*)(q->context) = Int_val(token);
#if defined(__APPLE__)
  DNSServiceQueryRecord(&q->serviceRef, kDNSServiceFlagsTimeout | kDNSServiceFlagsReturnIntermediates, 0, c_name, c_ty,
                        kDNSServiceClass_IN, common_callback, q->context);
#else
  caml_failwith("Only implemented on macOS");
#endif
  q->finalized = false;
  CAMLreturn(v);
}

CAMLprim value stub_query_fd(value v) {
  CAMLparam1(v);
  query *q = Query_val(v);
  int fd = -1;
#if defined(__APPLE__)
  fd = DNSServiceRefSockFD(q->serviceRef);
#else
  caml_failwith("Only implemented on macOS");
#endif
  if (fd == -1) caml_failwith("DNSServiceRefSocketFD");
  CAMLreturn(Val_int(fd));
}

CAMLprim value stub_query_process(value v) {
  CAMLparam1(v);
  query *q = Query_val(v);
#if defined(__APPLE__)
  DNSServiceProcessResult(q->serviceRef);
#else
  caml_failwith("Only implemented on macOS");
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value stub_query_deallocate(value v) {
  CAMLparam1(v);
  query *q = Query_val(v);
  if (!q->finalized) {
#if defined(__APPLE__)
    DNSServiceRefDeallocate(q->serviceRef);
#else
    caml_failwith("Only implemented on macOS");
#endif
    free(q->context);
    q->context = NULL;
  }
  q->finalized = true;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_is_supported_on_this_platform(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(ret);
  ret = Val_int(0);
#if defined(__APPLE__)
  ret = Val_int(1);
#endif
  CAMLreturn(ret);
}
