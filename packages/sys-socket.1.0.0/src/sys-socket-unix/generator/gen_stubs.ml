let c_headers = "
#include <arpa/inet.h>
#include <string.h>
"

let () =
  let mode = Sys.argv.(1) in
  let fname = Sys.argv.(2) in
  let oc = open_out_bin fname in
  let format =
    Format.formatter_of_out_channel oc
  in
  let fn =
    match mode with
      | "ml" -> Cstubs.write_ml
      | "c"  ->
         Format.fprintf format "%s@\n" c_headers;
         Cstubs.write_c
      | _    -> assert false
  in
  fn format ~prefix:"sys_socket_unix" (module Sys_socket_unix_stubs.Def);
  Format.pp_print_flush format ();
  close_out oc
