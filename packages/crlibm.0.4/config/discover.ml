module C = Configurator.V1

(* This script is run in _build/<context>/src/ *)
let crlibm_dir = "../../../src/crlibm"

let copy fn0 fn1 =
  let fh0 = open_in_bin fn0 in
  let fh1 = open_out_bin fn1 in
  let b = Bytes.create 4096 in
  let n = ref 0 in
  while n := input fh0 b 0 4096;  !n > 0 do
    output fh1 b 0 !n
  done;
  close_in fh0;
  close_out fh1

let has_header c h =
  try ignore(C.C_define.import c ~includes:[h] []); true
  with _ (* Fail to compile *) -> false

let is_prefix ~prefix s =
  (* naive *)
  let n = String.length prefix in
  n <= String.length s && String.sub s 0 n = prefix

let add_has_header_flag c name ~cflags =
  if has_header c (name ^ ".h") then
    ("-DHAVE_" ^ String.uppercase_ascii name ^ "_H") :: cflags
  else cflags

let conf_crlibm c =
  (* Based on the "configure.ac" in crlibm but restricted to the
     platforms supported by OCaml (see the "configure" script in the
     OCaml distribution). *)
  let system = C.ocaml_config_var_exn c "system" in
  let arch = C.ocaml_config_var_exn c "architecture" in
  let cflags = add_has_header_flag c "fenv" ~cflags:[] in
  let cflags = add_has_header_flag c "float" ~cflags in
  let cflags = add_has_header_flag c "inttypes" ~cflags in
  let cflags = add_has_header_flag c "stdlib" ~cflags in
  let cflags = add_has_header_flag c "strings" ~cflags in
  let cflags = add_has_header_flag c "unistd" ~cflags in
  let cflags = if system = "cygwin" || system = "mingw" || system = "mingw64" then
                 "-DCRLIBM_TYPEOS_CYGWIN" :: cflags
               else if is_prefix system ~prefix:"bsd"
                       || List.mem system ["netbsd"; "freebsd"; "macosx"] then
                 "-DCRLIBM_TYPEOS_BSD" :: cflags
               else cflags in
  let cflags = match arch with
    | "power" -> "-DCRLIBM_TYPECPU_POWERPC" :: cflags
    | "i386" -> "-DCRLIBM_TYPECPU_X86" :: cflags
    | "amd64" -> "-DCRLIBM_TYPECPU_AMD64" :: cflags
    | _ -> cflags in
  let has_ia32_de = arch = "i386" in (* double extended *)
  (* let has_ia64_de = arch = "amd64" in *)
  let has_fpu_control = try C.C_define.import c ~includes:["fpu_control.h"]
                              ["_FPU_SETCW", C.C_define.Type.Switch]
                            <> []
                        with _ -> false in
  let cflags = if has_fpu_control then "-DCRLIBM_HAS_FPU_CONTROL" :: cflags
               else cflags in
  let cflags = (* Default values *)
    "-DSCS_NB_BITS=30" :: "-DSCS_NB_WORDS=8" :: cflags in
  let use_hardware_de = has_ia32_de && has_fpu_control in
  let copy fn0 fn1 = copy (Filename.concat crlibm_dir fn0) fn1 in
  if use_hardware_de then (
    copy "log-de.c" "log-selected.c";
  )
  else (
    copy "log.c" "log-selected.c";
  );
  cflags

let () =
  let c = C.create "crlibm" in
  let cflags = conf_crlibm c in
  C.Flags.write_sexp "c_flags.sexp" cflags
