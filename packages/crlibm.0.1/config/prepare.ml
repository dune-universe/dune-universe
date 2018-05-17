(* Change the CRlibm files so they can all be in the same directory —
   Dune requires that. *)

let read_all fn =
  let fh = open_in_bin fn in
  let len = in_channel_length fh in
  let content = Bytes.create len in
  really_input fh content 0 len;
  close_in fh;
  Bytes.unsafe_to_string content

let write_all fn ~data =
  let fh = open_out_bin fn in
  output_string fh data;
  close_out fh

let rm_scs_lib fn =
  let data = read_all fn in
  (* write_all (fn ^ ".bak") ~data; *)
  let data = Str.global_replace (Str.regexp_string "scs_lib/") "" data in
  write_all fn ~data

let () =
  rm_scs_lib "src/crlibm/crlibm_private.h";
  rm_scs_lib "src/crlibm/triple-double.h";
  (* Create an empty crlibm_config.h, use "c_flags" for configuration
     (see discover.ml). *)
  write_all "src/crlibm/crlibm_config.h"
    ~data:"/* Configuration done using -D command line arguments. */";
