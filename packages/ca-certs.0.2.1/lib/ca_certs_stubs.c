#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/memory.h"

#ifdef _WIN32

#include <windows.h>

value ca_certs_iter_on_anchors(value v_f)
{
    CAMLparam1(v_f);
    CAMLlocal1(v_encoded_cert);

    HCERTSTORE hCertStore = CertOpenSystemStore(0, "ROOT");
    if (!hCertStore)
    {
        caml_failwith("ca_certs_iter_on_anchors: CertOpenSystemStore returned NULL");
    }

    PCCERT_CONTEXT pCertContext = NULL;
    while ((pCertContext = CertEnumCertificatesInStore(hCertStore, pCertContext)) != NULL)
    {
        if (!(pCertContext->dwCertEncodingType & X509_ASN_ENCODING))
        {
            caml_failwith("ca_certs_iter_on_anchors: certificate does not have expected encoding");
        }
        v_encoded_cert = caml_alloc_initialized_string(
            pCertContext->cbCertEncoded,
            pCertContext->pbCertEncoded);
        caml_callback(v_f, v_encoded_cert);
    }

    if (!CertCloseStore(hCertStore, 0))
    {
        caml_failwith("ca_certs_iter_on_anchors: CertCloseStore returned an error");
    }

    CAMLreturn(Val_unit);
}

#else

value ca_certs_iter_on_anchors(value v_unit)
{
    caml_failwith("ca_certs_iter_on_anchors: only implemented on Windows");
}

#endif
