/*
 * Copyright (C) 2015 Unikernel Systems
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __APPLE__
#include <fcntl.h>
#include <unistd.h>
#include <asl.h>
#else
#define aslclient void*
#define aslmsg void*
static void asl_not_available(){
  caml_failwith("Apple system log is not available on this platform");
}
#endif

#define Asl_val(v) (*((aslclient *) Data_custom_val(v)))

#ifdef __APPLE__
static void client_finalize(value v) {
  asl_close(Asl_val(v));
}

static struct custom_operations client_ops = {
  "org.mirage.caml.asl.client",
  client_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_client(aslclient asl) {
  value v = alloc_custom(&client_ops, sizeof(aslclient), 0, 1);
  Asl_val(v) = asl;
  return v;
}
#endif

CAMLprim value stub_asl_open_null(){
  CAMLparam0();
  CAMLlocal1(result);
#ifdef __APPLE__
  result = alloc_client(NULL);
#else
  asl_not_available();
#endif
  CAMLreturn(result);
}

CAMLprim value stub_asl_open(value ident, value facility, value stderr, value no_delay, value no_remote) {
  CAMLparam5(ident, facility, stderr, no_delay, no_remote);
  CAMLlocal1(result);
#ifdef __APPLE__
  const char *c_ident = String_val(ident);
  const char *c_facility = String_val(facility);
  uint32_t options =
      (Bool_val(stderr)?ASL_OPT_STDERR:0)
    | (Bool_val(no_delay)?ASL_OPT_NO_DELAY:0)
    | (Bool_val(no_remote)?ASL_OPT_NO_REMOTE:0);
  aslclient asl = NULL;
  caml_release_runtime_system();
  asl = asl_open(c_ident, c_facility, options);
  caml_acquire_runtime_system();

  result = alloc_client(asl);
#else
  asl_not_available();
#endif
  CAMLreturn(result);
}

CAMLprim value stub_asl_add_output_file(
  value t, value fd, value msg_fmt, value time_fmt, value level_up_to
) {
  CAMLparam5(t, fd, msg_fmt, time_fmt, level_up_to);
  CAMLlocal1(result);
#ifdef __APPLE__
  aslclient c_asl = Asl_val(t);
  int c_descriptor = Int_val(fd); /* assume type Unix.file_descr = int */
  const char *c_msg_fmt = String_val(msg_fmt);
  const char *c_time_fmt = String_val(time_fmt);
  int c_filter = ASL_FILTER_MASK_UPTO(Int_val(level_up_to));
  int c_text_encoding = ASL_ENCODE_SAFE;
  result = Val_int(0); /* false */
  if (asl_add_output_file(c_asl, c_descriptor, c_msg_fmt, c_time_fmt, c_filter, c_text_encoding) == 0) {
    result = Val_int(1); /* true */
  }
#else
  asl_not_available();
#endif
  CAMLreturn(result);
}

#define Msg_val(v) (*((aslmsg *) Data_custom_val(v)))

#ifdef __APPLE__
static void message_finalize(value v) {
  asl_free(Msg_val(v));
}

static struct custom_operations message_ops = {
  "org.mirage.caml.asl.message",
  message_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};
#endif

#ifdef __APPLE__
static value alloc_message(aslmsg msg) {
  value v = Val_int(0);
  v = alloc_custom(&message_ops, sizeof(aslmsg), 0, 1);
  Msg_val(v) = msg;
  return v;
}
#endif

CAMLprim value stub_asl_new_msg() {
  CAMLparam0();
  CAMLlocal1(result);
#ifdef __APPLE__
  caml_release_runtime_system();
  aslmsg msg = asl_new(ASL_TYPE_MSG);
  caml_acquire_runtime_system();
  result = alloc_message(msg);
#else
  asl_not_available();
#endif
  CAMLreturn(result);
}

CAMLprim value stub_asl_set(value m, value key, value val) {
  CAMLparam3(m, key, val);
  const char *c_key = strdup(String_val(key));
  const char *c_val = strdup(String_val(val));
#ifdef __APPLE__
  caml_release_runtime_system();
  asl_set(Msg_val(m), c_key, c_val);
  caml_acquire_runtime_system();
#else
  asl_not_available();
#endif
  free((void*)c_key);
  free((void*)c_val);
  CAMLreturn(0);
}

#ifdef __APPLE__
#define GENERATE_ASL_SET(name) \
CAMLprim value stub_asl_set_##name(value m, value string) { \
  CAMLparam2(m, string); \
  const char *c_string = strdup(String_val(string)); \
  caml_release_runtime_system(); \
  asl_set(Msg_val(m), ASL_KEY_##name, c_string); \
  caml_acquire_runtime_system(); \
  free((void*)c_string); \
  CAMLreturn(0); \
}
#else
#define GENERATE_ASL_SET(name) \
CAMLprim value stub_asl_set_##name(value m, value string) { \
  CAMLparam2(m, string); \
  const char *c_string = strdup(String_val(string)); \
  asl_not_available(); \
  free((void*)c_string); \
  CAMLreturn(0); \
}
#endif

GENERATE_ASL_SET(TIME)
GENERATE_ASL_SET(HOST)
GENERATE_ASL_SET(SENDER)
GENERATE_ASL_SET(FACILITY)
GENERATE_ASL_SET(PID)
GENERATE_ASL_SET(UID)
GENERATE_ASL_SET(GID)
GENERATE_ASL_SET(LEVEL)
GENERATE_ASL_SET(MSG)

#ifdef __APPLE__
#define GENERATE_ASL_LEVEL(name) \
CAMLprim value stub_get_asl_level_##name(){ \
  CAMLparam0(); \
  CAMLreturn(Val_int(ASL_LEVEL_##name)); \
}
#else
#define GENERATE_ASL_LEVEL(name) \
CAMLprim value stub_get_asl_level_##name(){ \
  CAMLparam0(); \
  CAMLreturn(Val_int(0)); \
}
#endif
GENERATE_ASL_LEVEL(EMERG)
GENERATE_ASL_LEVEL(ALERT)
GENERATE_ASL_LEVEL(CRIT)
GENERATE_ASL_LEVEL(ERR)
GENERATE_ASL_LEVEL(WARNING)
GENERATE_ASL_LEVEL(NOTICE)
GENERATE_ASL_LEVEL(INFO)
GENERATE_ASL_LEVEL(DEBUG)

CAMLprim value stub_asl_log(value client, value message, value level, value txt) {
  CAMLparam4(client, message, level, txt);
#ifdef __APPLE__
  int c_level = Int_val(level);
  aslclient asl = Asl_val(client);
  aslmsg msg = Msg_val(message);
  const char *c_message = strdup(String_val(txt));
  caml_release_runtime_system();
  asl_log(asl, msg, c_level, "%s", c_message);
  caml_acquire_runtime_system();
  free((void*)c_message);
#else
  asl_not_available();
#endif
  CAMLreturn(0);
}
