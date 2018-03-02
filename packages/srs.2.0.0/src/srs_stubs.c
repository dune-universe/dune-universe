#include <stdlib.h>
#include <string.h>

#include <srs2.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#define Val_char(x) Val_int(x)
#define Char_val(x) ((char)Int_val(x))

static void
srs_error(const char *err)
{
    caml_raise_with_string(*caml_named_value("Error"), err);
}

static void
finalize_srs(value srs_val)
{
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);
    srs_free(srs);
}

static struct custom_operations srs_ops = {
    "srs_t custom ops",
    finalize_srs,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
};

CAMLprim value
caml_srs_new(value unit)
{
    CAMLparam1(unit);
    CAMLlocal1(srs_val);
    srs_t *srs;

    srs = srs_new();
    if (srs == NULL)
        srs_error("SRS.new");

    srs_val = caml_alloc_custom(&srs_ops, sizeof(*srs), 0, 1);
    memcpy(Data_custom_val(srs_val), srs, sizeof(*srs));

    CAMLreturn(srs_val);
}

CAMLprim value
caml_srs_add_secret(value srs_val, value secret_val)
{
    CAMLparam2(srs_val, secret_val);
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);
    char *secret = String_val(secret_val);

    srs_add_secret(srs, secret);

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_srs_forward(value srs_val, value sender_val, value alias_val)
{
    CAMLparam3(srs_val, sender_val, alias_val);
    CAMLlocal1(res);
    int ret;
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);
    char *sender = String_val(sender_val);
    char *alias = String_val(alias_val);
    char *rewritten;

    ret = srs_forward_alloc(srs, &rewritten, sender, alias);
    if (ret != SRS_SUCCESS)
        srs_error(srs_strerror(ret));

    res = caml_copy_string(rewritten);
    free(rewritten);

    CAMLreturn(res);
}

CAMLprim value
caml_srs_reverse(value srs_val, value sender_val)
{
    CAMLparam2(srs_val, sender_val);
    CAMLlocal1(res);
    int ret;
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);
    char *sender = String_val(sender_val);
    char *original;

    ret = srs_reverse_alloc(srs, &original, sender);
    if (ret != SRS_SUCCESS)
        srs_error(srs_strerror(ret));

    res = caml_copy_string(original);
    free(original);

    CAMLreturn(res);
}

CAMLprim value
caml_srs_set_separator(value srs_val, value sep_val)
{
    CAMLparam2(srs_val, sep_val);
    int ret;
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);
    char sep = Int_val(sep_val);

    ret = srs_set_separator(srs, sep);
    if (ret != SRS_SUCCESS)
        srs_error(srs_strerror(ret));

    CAMLreturn(Val_unit);
}

CAMLprim value
caml_srs_get_separator(value srs_val)
{
    CAMLparam1(srs_val);
    int index;
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);

    switch (srs_get_separator(srs)) {
    case '+':
        index = 0;
    case '-':
        index = 1;
    case '=':
        index = 2;
    default:
        srs_error("unexpected separator");
    }
    CAMLreturn(Val_int(index));
}

#define CAML_SRS_PARAM_DEFINE(name, type, to_type, of_type) \
CAMLprim value                                              \
caml_srs_set_##name(value srs_val, value param_val)         \
{                                                           \
    CAMLparam2(srs_val, param_val);                         \
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);         \
    type param = to_type(param_val);                        \
                                                            \
    srs_set_##name(srs, param);                             \
                                                            \
    CAMLreturn(Val_unit);                                   \
}                                                           \
                                                            \
CAMLprim value                                              \
caml_srs_get_##name(value srs_val)                          \
{                                                           \
    CAMLparam1(srs_val);                                    \
    srs_t *srs = (srs_t *)Data_custom_val(srs_val);         \
    type param = srs_get_##name(srs);                       \
                                                            \
    CAMLreturn(of_type(param));                             \
}

CAML_SRS_PARAM_DEFINE(maxage,        int, Int_val,  Val_int);
CAML_SRS_PARAM_DEFINE(hashlength,    int, Int_val,  Val_int);
CAML_SRS_PARAM_DEFINE(hashmin,       int, Int_val,  Val_int);
CAML_SRS_PARAM_DEFINE(alwaysrewrite, int, Bool_val, Val_bool);
CAML_SRS_PARAM_DEFINE(noforward,     int, Bool_val, Val_bool);
CAML_SRS_PARAM_DEFINE(noreverse,     int, Bool_val, Val_bool);
