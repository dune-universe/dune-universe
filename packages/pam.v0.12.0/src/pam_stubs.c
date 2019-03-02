#include <security/pam_appl.h>
#include <security/pam_modules.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/threads.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* The following constants are defined to match the variants passed from the ocaml pam
   library. One should not change the values assigned below without updating the ocaml
   library, or vice versa.

   The constants below allow us to refer to the constants in [_pam_types.h] logically and
   do not need to worry about translating values between both worlds.
*/
#define num_of_flags(flags)  (sizeof(flags) / sizeof(int))

#define TRUE  1
#define FALSE 0

#define CAML_PAM_PROMPT_ECHO_OFF  0
#define CAML_PAM_PROMPT_ECHO_ON   1
#define CAML_PAM_ERROR_MSG        2
#define CAML_PAM_TEXT_INFO        3

#define CAML_PAM_BUF_ERR   0
#define CAML_PAM_CONV_ERR  1

#define CAML_TAG_OK     0
#define CAML_TAG_ERROR  1

const int CAML_SESSION_FLAGS[] = {
  PAM_SILENT
};

const int CAML_AUTH_FLAGS[] = {
  PAM_SILENT,
  PAM_DISALLOW_NULL_AUTHTOK,
};

const int CAML_ACCT_FLAGS[] = {
  PAM_SILENT,
  PAM_DISALLOW_NULL_AUTHTOK,
};

const int CAML_CRED_FLAGS[] = {
  PAM_ESTABLISH_CRED,
  PAM_DELETE_CRED,
  PAM_REINITIALIZE_CRED,
  PAM_REFRESH_CRED,
};

const int CAML_AUTHTOK_FLAGS[] = {
  PAM_SILENT,
  PAM_CHANGE_EXPIRED_AUTHTOK,
};

const int CAML_ITEM_TYPES[] = {
  PAM_SERVICE,
  PAM_USER,
  PAM_USER_PROMPT,
  PAM_TTY,
  PAM_RUSER,
  PAM_RHOST,
  PAM_AUTHTOK,
  PAM_OLDAUTHTOK,
  PAM_XDISPLAY,
  PAM_XAUTHDATA,
  PAM_AUTHTOK_TYPE,
  PAM_CONV,
  PAM_FAIL_DELAY,
};

/* [pam_context_t] holds both the [pam_handle_t] and last pam operation status so that the
   ocaml code does not need to keep track of the last pam operation status, which is
   required for the final [pam_end] operation */
typedef struct _pam_context_t {
  pam_handle_t *handle;
  int status;
} pam_context_t;

static struct custom_operations pam_context_ops = {
  "pam_context_handling",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

#define Pam_context_val(v) (pam_context_t *)Data_custom_val(v)

void* not_null(void* ptr) {
  if (!ptr) {
    caml_failwith("Not enough memory");
  }

  return ptr;
}

void* re_malloc(size_t size)
{
  return not_null(malloc(size));
}

void* re_calloc(size_t nmemb, size_t size)
{
  return not_null(calloc(nmemb, size));
}

char* re_strdup(const char *s)
{
  return not_null(strdup(s));
}

CAMLprim value
alloc_pam_context(const pam_context_t *ctx)
{
  CAMLparam0();
  CAMLlocal1(v);

  v = caml_alloc_custom(&pam_context_ops, sizeof(pam_context_t), 0, 1);
  memcpy(Data_custom_val(v), ctx, sizeof(pam_context_t));

  CAMLreturn(v);
}

CAMLprim value
alloc_simple(value v, int tag)
{
  CAMLparam1(v);
  CAMLlocal1(result);

  result = caml_alloc_small(1, tag);
  Field(result, 0) = v;

  CAMLreturn(result);
}

CAMLprim value
alloc_ok(value v)
{
  CAMLparam1(v);

  CAMLreturn(alloc_simple(v, CAML_TAG_OK));
}

CAMLprim value
alloc_error(value v)
{
  CAMLparam1(v);

  CAMLreturn(alloc_simple(v, CAML_TAG_ERROR));
}

CAMLprim value
alloc_unit_or_error(int status)
{
  CAMLparam0();

  CAMLreturn(status == PAM_SUCCESS
             ? alloc_ok(Val_unit)
             : alloc_error(Val_int(status)));
}

CAMLprim value
alloc_none()
{
  CAMLparam0();

  CAMLreturn(Val_int(0));
}

CAMLprim value
alloc_some(value v)
{
  CAMLparam1(v);

  CAMLreturn(alloc_simple(v, 0));
}

CAMLprim value
head(value list)
{
  CAMLparam1(list);

  CAMLreturn(Field(list, 0));
}

CAMLprim value
tail(value list)
{
  CAMLparam1(list);

  CAMLreturn(Field(list, 1));
}

int caml_list_length(value list) {
  CAMLparam1(list);

  if (Is_long(list)) {
    CAMLreturnT(int, 0);
  } else {
    CAMLreturnT(int, 1 + caml_list_length(Field(list, 1)));
  }
}

int caml_flag_to_int(value flag, const int all_flags[], const int num_of_flags)
{
  CAMLparam1(flag);

  int index = Int_val(flag);

  if (index >= num_of_flags) {
    caml_failwith("Unexpected flag index value");
  } else {
    CAMLreturnT(int, all_flags[index]);
  }
}

int caml_flag_list_to_int(value flags, const int all_flags[], const int num_of_flags)
{
  CAMLparam1(flags);

  int retval = 0;

  while (Is_block(flags)) {
    retval |= caml_flag_to_int(head(flags), all_flags, num_of_flags);
    flags = tail(flags);
  }

  CAMLreturnT(int, retval);
}

CAMLprim value
pam_message_to_value(const struct pam_message *m)
{
  CAMLparam0();
  CAMLlocal1(result);

  int msg_style = 0;
  char *buf = NULL;

  switch (m->msg_style) {
  case PAM_PROMPT_ECHO_OFF:
    msg_style = CAML_PAM_PROMPT_ECHO_OFF;
    break;
  case PAM_PROMPT_ECHO_ON:
    msg_style = CAML_PAM_PROMPT_ECHO_ON;
    break;
  case PAM_ERROR_MSG:
    msg_style = CAML_PAM_ERROR_MSG;
    break;
  case PAM_TEXT_INFO:
    msg_style = CAML_PAM_TEXT_INFO;
    break;
  default:
    buf = re_malloc(256);
    snprintf(buf, 256, "Unexpected pam message '%s', message style %d", m->msg, m->msg_style);
    result = caml_copy_string(buf);
    free(buf);

    CAMLreturn(alloc_error(result));
    break;
  }

  result = caml_alloc_small(2, 0);
  Field(result, 0) = Val_int(msg_style);
  Field(result, 1) = caml_copy_string(m->msg);

  CAMLreturn(alloc_ok(result));
}

CAMLprim value
pam_message_array_to_list(const struct pam_message **msg, int num_msg)
{
  CAMLparam0();
  CAMLlocal1(list);

  if (num_msg == 0) {
    CAMLreturn(Val_int(0));
  } else {
    list = caml_alloc_small(2, 0);
    Field(list, 0) = pam_message_to_value(*msg);
    Field(list, 1) = pam_message_array_to_list(msg + 1, num_msg - 1);

    CAMLreturn(list);
  }
}

void pam_response_list_to_array(struct pam_response **resp, value resp_list)
{
  CAMLparam1(resp_list);
  CAMLlocal2(resp_v, resp_msg);

  struct pam_response *r = *resp;

  if (Is_long(resp_list)) {
    CAMLreturn0;
  } else {
    resp_v   = Field(resp_list, 0);
    resp_msg = Field(resp_v, 0);

    if (Is_long(resp_msg)) {
      /* [resp_msg] is None and we do not set the return pointer */
      r = NULL;
    } else {
      /* [resp_msg] is [Some of string], extract from the block */
      r->resp = re_strdup(String_val(Field(resp_msg, 0)));
      r->resp_retcode = Int_val(Field(resp_v, 1));
    }

    pam_response_list_to_array(resp + 1, Field(resp_list, 1));

    CAMLreturn0;
  }
}

int conv_wrapper(int num_msg,
                 const struct pam_message **msg,
                 struct pam_response **resp,
                 void *appdata_ptr)
{
  CAMLparam0();
  CAMLlocal4(msg_list, cb, resp_or_error, resp_list);

  struct pam_response *temp_resp;

  msg_list = pam_message_array_to_list(msg, num_msg);
  cb = (value)appdata_ptr;

  resp_or_error = caml_callback(cb, msg_list);

  /* A lot of [break] statements below are not required given the [return]
     statements. They are put in place to avoid mistakes caused by future changes. */
  switch (Tag_val(resp_or_error)) {
  case CAML_TAG_OK:
    resp_list = Field(resp_or_error, 0);

    if (caml_list_length(resp_list) != num_msg) {
      CAMLreturnT(int, PAM_CONV_ERR);
    } else {
      *resp = re_calloc(num_msg, sizeof(*temp_resp));
      pam_response_list_to_array(resp, resp_list);

      CAMLreturnT(int, PAM_SUCCESS);
    }
    break;

  case CAML_TAG_ERROR:
    switch (Int_val(Field(resp_or_error, 0))) {
    case CAML_PAM_BUF_ERR:
      CAMLreturnT(int, PAM_BUF_ERR);
      break;

    case CAML_PAM_CONV_ERR:
      CAMLreturnT(int, PAM_CONV_ERR);
      break;

    default:
      CAMLreturnT(int, PAM_CONV_ERR);
      break;
    }

    break;

  default:
    /* We do not expect such value, hence return an error */
    CAMLreturnT(int, PAM_CONV_ERR);
    break;
  }
}

CAMLprim value
pam_execute_with_flags(value ctx,
                       int flags_c,
                       int (*func)(pam_handle_t *, int))
{
  CAMLparam1(ctx);

  pam_context_t *c = Pam_context_val(ctx);
  pam_handle_t  *h = c->handle;
  c->status = func(h, flags_c);

  CAMLreturn(alloc_unit_or_error(c->status));
}

CAMLprim value
caml_pam_start(value service, value user, value conv_func)
{
  CAMLparam3(service, user, conv_func);
  CAMLlocal2(ctx, result);

  int retval = 0;
  pam_context_t c;
  pam_handle_t *pamh = NULL;
  struct pam_conv conv;
  const char *user_c = String_val(user);

  conv.conv = conv_wrapper;
  conv.appdata_ptr = (void *)conv_func;

  retval = pam_start(String_val(service),
                     user_c,
                     &conv,
                     &pamh);

  c.handle = pamh;
  c.status = retval;

  ctx = alloc_pam_context(&c);

  if (c.status == PAM_SUCCESS) {
    CAMLreturn(alloc_ok(ctx));
  } else {
    result = caml_alloc_tuple(2);
    Store_field(result, 0, ctx);
    Store_field(result, 1, retval);
    CAMLreturn(alloc_error(result));
  }
}

CAMLprim value
caml_pam_end(value ctx)
{
  CAMLparam1(ctx);

  int retval;

  pam_context_t *c = Pam_context_val(ctx);
  c->status = pam_end(c->handle, 0);
  retval = c->status;

  CAMLreturn(alloc_unit_or_error(retval));
}

CAMLprim value
caml_pam_authenticate(value ctx, value flags)
{
  CAMLparam2(ctx, flags);

  int flags_c = caml_flag_list_to_int(flags, CAML_AUTH_FLAGS,
                                      num_of_flags(CAML_AUTH_FLAGS));

  CAMLreturn(pam_execute_with_flags(ctx, flags_c, &pam_authenticate));
}

CAMLprim value
caml_pam_acct_mgmt(value ctx, value flags)
{
  CAMLparam2(ctx, flags);

  int flags_c = caml_flag_list_to_int(flags, CAML_ACCT_FLAGS,
                                      num_of_flags(CAML_ACCT_FLAGS));


  CAMLreturn(pam_execute_with_flags(ctx, flags_c, &pam_acct_mgmt));
}

CAMLprim value
caml_pam_setcred(value ctx, value silent, value flag)
{
  CAMLparam3(ctx, silent, flag);

  int flags_c = caml_flag_to_int(flag, CAML_CRED_FLAGS,
                                 num_of_flags(CAML_ACCT_FLAGS));

  if (Bool_val(silent)) {
    flags_c |= PAM_SILENT;
  }

  CAMLreturn(pam_execute_with_flags(ctx, flags_c, &pam_setcred));
}

CAMLprim value
caml_pam_chauthtok(value ctx, value flags)
{
  CAMLparam2(ctx, flags);

  int flags_c = caml_flag_list_to_int(flags, CAML_AUTHTOK_FLAGS,
                                      num_of_flags(CAML_AUTHTOK_FLAGS));

  CAMLreturn(pam_execute_with_flags(ctx, flags_c, &pam_chauthtok));
}

CAMLprim value
caml_pam_open_session(value ctx, value flags)
{
  CAMLparam2(ctx, flags);

  int flags_c = caml_flag_list_to_int(flags, CAML_SESSION_FLAGS,
                                      num_of_flags(CAML_SESSION_FLAGS));

  CAMLreturn(pam_execute_with_flags(ctx, flags_c, &pam_open_session));
}

CAMLprim value
caml_pam_close_session(value ctx, value flags)
{
  CAMLparam2(ctx, flags);

  int flags_c = caml_flag_list_to_int(flags, CAML_SESSION_FLAGS,
                                      num_of_flags(CAML_SESSION_FLAGS));

  CAMLreturn(pam_execute_with_flags(ctx, flags_c, &pam_close_session));
}

CAMLprim value
caml_pam_getenv(value ctx, value key)
{
  CAMLparam2(ctx, key);
  CAMLlocal1(result);

  const char *data = NULL;

  const pam_context_t *c = Pam_context_val(ctx);
  data = pam_getenv(c->handle, String_val(key));

  if (data) {
    result = alloc_ok(caml_copy_string(data));
  } else {
    result = alloc_error(caml_copy_string("pam_getenv failed"));
  }

  CAMLreturn(result);
}

CAMLprim value
caml_pam_putenv(value ctx, value key_and_data)
{
  CAMLparam2(ctx, key_and_data);
  CAMLlocal1(result);

  pam_context_t *c = Pam_context_val(ctx);
  c->status = pam_putenv(c->handle, String_val(key_and_data));

  CAMLreturn(alloc_unit_or_error(c->status));
}

CAMLprim value
str_list_to_caml_list(char **str)
{
  CAMLparam0();
  CAMLlocal1(list);

  if (!*str) {
    CAMLreturn(Val_int(0));
  } else {
    list = caml_alloc_small(2, 0);
    Field(list, 0) = caml_copy_string(*str);
    Field(list, 1) = str_list_to_caml_list(str + 1);
    CAMLreturn(list);
  }
}

CAMLprim value
caml_pam_getenvlist(value ctx)
{
  CAMLparam1(ctx);
  CAMLlocal1(list);

  pam_context_t *c = Pam_context_val(ctx);
  char **pam_envs  = pam_getenvlist(c->handle);
  char **temp      = pam_envs;

  if (pam_envs) {
    list = str_list_to_caml_list(pam_envs);

    /* free pam_envs and all its items as pamlib will not */
    while (*temp) {
      free(*temp);
      ++temp;
    }
    free(pam_envs);

    CAMLreturn(alloc_ok(list));
  } else {
    CAMLreturn(alloc_error(caml_copy_string("pam_getenvlist failed")));
  }
}

void ensure_supported_item_type(int item_type_c)
{
  switch (item_type_c) {
  case PAM_SERVICE:
  case PAM_USER:
  case PAM_USER_PROMPT:
  case PAM_TTY:
  case PAM_RUSER:
  case PAM_RHOST:
  case PAM_AUTHTOK:
  case PAM_OLDAUTHTOK:
  case PAM_XDISPLAY:
  case PAM_AUTHTOK_TYPE:
    return;
  default:
    caml_failwith("Item type is not supported.");
  }
}

CAMLprim value
caml_pam_get_item(value ctx, value item_type)
{
  CAMLparam2(ctx, item_type);
  CAMLlocal1(result);

  pam_context_t *c = Pam_context_val(ctx);
  int item_type_c = caml_flag_to_int(item_type, CAML_ITEM_TYPES,
                                     num_of_flags(CAML_ITEM_TYPES));
  const void *item = NULL;

  ensure_supported_item_type(item_type_c);
  c->status = pam_get_item(c->handle, item_type_c, &item);

  if (c->status == PAM_SUCCESS) {
    if (item) {
      CAMLreturn(alloc_ok(alloc_some(caml_copy_string(item))));
    } else {
      CAMLreturn(alloc_ok(alloc_none()));
    }
  } else {
    CAMLreturn(alloc_error(Val_int(c->status)));
  }
}

CAMLprim value
caml_pam_set_item(value ctx, value item_type, value item)
{
  CAMLparam3(ctx, item_type, item);
  CAMLlocal1(result);

  pam_context_t *c = Pam_context_val(ctx);
  int item_type_c = caml_flag_to_int(item_type, CAML_ITEM_TYPES,
                                     num_of_flags(CAML_ITEM_TYPES));
  const void *item_c = NULL;

  ensure_supported_item_type(item_type_c);
  item_c = (void *)String_val(item);
  c->status = pam_set_item(c->handle, item_type_c, item_c);

  CAMLreturn(alloc_unit_or_error(c->status));
}

CAMLprim value
caml_pam_strerror(value ctx, value errnum)
{
  CAMLparam2(ctx, errnum);

  pam_context_t *c = (pam_context_t *)ctx;

  const char *err = pam_strerror(c->handle, Int_val(errnum));

  CAMLreturn(caml_copy_string(err));
}
