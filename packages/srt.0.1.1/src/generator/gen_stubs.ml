let c_headers =
  "\n\
   #include <srt/srt.h>\n\n\
   #include <caml/alloc.h>\n\
   #include <caml/callback.h>\n\
   #include <caml/fail.h>\n\
   #include <caml/memory.h>\n\
   #include <caml/threads.h>\n\n\
   #include <stdio.h>\n\
   #include <pthread.h>\n\n\
   static pthread_key_t ocaml_c_thread_key;\n\
   static pthread_once_t ocaml_c_thread_key_once = PTHREAD_ONCE_INIT;\n\n\
   static void ocaml_srt_on_thread_exit(void *key) {\n\
  \  caml_c_thread_unregister();\n\
   }\n\n\
   static void ocaml_srt_make_key() {\n\
  \  pthread_key_create(&ocaml_c_thread_key, ocaml_srt_on_thread_exit);\n\
   }\n\n\
   void ocaml_srt_register_thread() {\n\
  \  static int initialized = 1;\n\
  \  void *ptr;\n\n\
  \  pthread_once(&ocaml_c_thread_key_once, ocaml_srt_make_key);\n\n\
  \  if (caml_c_thread_register() && !pthread_getspecific(ocaml_c_thread_key)) \n\
  \    pthread_setspecific(ocaml_c_thread_key,(void*)&initialized);\n\
   }\n\n\
   static value log_handler = (value)NULL;\n\n\
   void ocaml_srt_log_handler(void* opaque, int level, const char* file, int \
   line, const char* area, const char* message) {\n\
  \  value _file, _area, _message, _ret;\n\
  \  value _args[5];\n\
  \  int nargs = 5;\n\n\
  \  ocaml_srt_register_thread();\n\n\
  \  caml_acquire_runtime_system();\n\n\
  \  _args[0] = Val_int(level);\n\n\
  \  _file = caml_copy_string(file);\n\
  \  caml_register_generational_global_root(&_file);\n\
  \  _args[1] = _file;\n\n\
  \  _args[2] = Val_int(line);\n\n\
  \  _area = caml_copy_string(area);\n\
  \  caml_register_generational_global_root(&_area);\n\
  \  _args[3] = _area;\n\n\
  \  _message = caml_copy_string(message);\n\
  \  caml_register_generational_global_root(&_message);\n\
  \  _args[4] = _message;\n\n\
  \  _ret = caml_callbackN(log_handler, nargs, _args);\n\n\
  \  caml_remove_generational_global_root(&_file);\n\
  \  caml_remove_generational_global_root(&_area);\n\
  \  caml_remove_generational_global_root(&_message);\n\n\
  \  caml_release_runtime_system();\n\
   }\n\n\
   value ocaml_srt_clear_log_handler(value unit) {\n\
  \  srt_setloghandler(NULL, NULL);\n\
  \  caml_remove_generational_global_root(&log_handler);\n\
  \  return Val_unit;\n\
   }\n\n\
   CAMLprim value ocaml_srt_register_log_handler(value handler) {\n\
  \  CAMLparam1(handler);\n\
  \  log_handler = handler;\n\
  \  caml_register_generational_global_root(&log_handler);\n\
  \  srt_setloghandler(NULL, &ocaml_srt_log_handler);\n\
  \  CAMLreturn(Val_unit);\n\
   }\n"

let locked_c_headers = "\n#include <string.h>\n"

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
