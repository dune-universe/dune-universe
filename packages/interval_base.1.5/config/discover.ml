module C = Configurator.V1

let start_with s prefix =
  let len = String.length prefix in
  String.length s >= len && String.sub s 0 len = prefix

(* Activate the right code (through C flags) according to whether the
   architecture is Intel. *)
let cflags_intel c =
  let arch = C.ocaml_config_var_exn c "architecture" in
  let cc = try C.ocaml_config_var_exn c "native_c_compiler" with _ -> "" in
  (* For now, the ASM is only for GCC, not MSVC. *)
  if (arch = "amd64" || arch = "i386") && start_with cc "gcc" then
    ["-DINTEL_ARCH"]
  else []


let () =
  let c = C.create "interval" in
  let cflags = cflags_intel c in
  C.Flags.write_sexp "c_flags.sexp" cflags;


