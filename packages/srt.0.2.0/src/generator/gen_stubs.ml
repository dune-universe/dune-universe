let c_headers =
  {|
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <pthread.h>
#include <srt/srt.h>
#include <stdio.h>

static pthread_key_t ocaml_c_thread_key;
static pthread_once_t ocaml_c_thread_key_once = PTHREAD_ONCE_INIT;

static void ocaml_srt_on_thread_exit(void *key) { caml_c_thread_unregister(); }

static void ocaml_srt_make_key() {
  pthread_key_create(&ocaml_c_thread_key, ocaml_srt_on_thread_exit);
}

void ocaml_srt_register_thread() {
  static int initialized = 1;
  void *ptr;
  pthread_once(&ocaml_c_thread_key_once, ocaml_srt_make_key);
  if (caml_c_thread_register() && !pthread_getspecific(ocaml_c_thread_key))
    pthread_setspecific(ocaml_c_thread_key, (void *)&initialized);
}

static value log_handler = (value)NULL;

void ocaml_srt_log_handler(void *opaque, int level, const char *file, int line,
                           const char *area, const char *message) {
  value _file, _area, _message, _ret;
  value _args[5];
  int nargs = 5;

  ocaml_srt_register_thread();
  caml_acquire_runtime_system();

  _args[0] = Val_int(level);
  _file = caml_copy_string(file);

  caml_register_generational_global_root(&_file);

  _args[1] = _file;
  _args[2] = Val_int(line);
  _area = caml_copy_string(area);

  caml_register_generational_global_root(&_area);

  _args[3] = _area;
  _message = caml_copy_string(message);

  caml_register_generational_global_root(&_message);

  _args[4] = _message;
  _ret = caml_callbackN(log_handler, nargs, _args);

  caml_remove_generational_global_root(&_file);
  caml_remove_generational_global_root(&_area);
  caml_remove_generational_global_root(&_message);
  caml_release_runtime_system();
}

value ocaml_srt_clear_log_handler(value unit) {
  srt_setloghandler(NULL, NULL);
  caml_remove_generational_global_root(&log_handler);
  return Val_unit;
}

CAMLprim value ocaml_srt_register_log_handler(value handler) {
  CAMLparam1(handler);
  log_handler = handler;
  caml_register_generational_global_root(&log_handler);
  srt_setloghandler(NULL, &ocaml_srt_log_handler);
  CAMLreturn(Val_unit);
}
|}

let locked_c_headers = {|
#include <string.h>
|}

let () =
  let mode = Sys.argv.(1) in
  let fname = Sys.argv.(2) in
  let locked = Array.length Sys.argv > 3 in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  let fn =
    match mode with
      | "ml" -> Cstubs.write_ml
      | "c" ->
          if locked then Format.fprintf format "%s@\n" locked_c_headers
          else Format.fprintf format "%s@\n" c_headers;
          Cstubs.write_c
      | _ -> assert false
  in
  if locked then fn format ~prefix:"ocaml_srt" (module Srt_stubs_locked.Def)
  else
    fn ~concurrency:Cstubs.unlocked format ~prefix:"ocaml_srt"
      (module Srt_stubs.Def);
  Format.pp_print_flush format ();
  close_out oc
