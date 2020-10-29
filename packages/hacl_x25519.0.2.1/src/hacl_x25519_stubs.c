#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include "Hacl_Curve25519_51.h"
#include "Hacl_Ed25519.h"

CAMLprim value ml_Hacl_Curve25519_crypto_scalarmult (value pk, value sk, value basepoint) {
    CAMLparam3(pk, sk, basepoint);
    Hacl_Curve25519_51_scalarmult(Caml_ba_data_val(pk),
                                  Caml_ba_data_val(sk),
                                  Caml_ba_data_val(basepoint));
    CAMLreturn(Val_unit);
}

CAMLprim value ml_Hacl_Ed25519_secret_to_public (value out, value sk) {
    CAMLparam2(out, sk);
    Hacl_Ed25519_secret_to_public(Caml_ba_data_val(out), Caml_ba_data_val(sk));
    CAMLreturn(Val_unit);
}

CAMLprim value ml_Hacl_Ed25519_sign (value sig, value sk, value msg, value len) {
    CAMLparam4(sig, sk, msg, len);
    Hacl_Ed25519_sign(Caml_ba_data_val(sig),
                      Caml_ba_data_val(sk),
                      Int_val(len),
                      Caml_ba_data_val(msg));
    CAMLreturn(Val_unit);
}

CAMLprim value ml_Hacl_Ed25519_verify (value pub, value msg, value len, value sig) {
    CAMLparam4(pub, msg, len, sig);
    bool r;
    r = Hacl_Ed25519_verify(Caml_ba_data_val(pub),
		            Int_val(len),
                            Caml_ba_data_val(msg),
                            Caml_ba_data_val(sig));
    CAMLreturn(Val_bool(r));
}
