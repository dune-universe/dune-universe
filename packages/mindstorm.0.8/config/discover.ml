open Printf
module C = Configurator.V1
module P = C.Pkg_config

let merge_pkg p1 p2 = match p2 with
  | Some p2 -> { P.cflags = p1.P.cflags @ p2.P.cflags;
                 P.libs = p1.P.libs @ p2.P.libs }
  | None -> p1

let get_bluetooth c =
  let sys = C.ocaml_config_var_exn c "system"in
  let ms_sdk = "\"C:\\Program Files\\Microsoft Platform SDK\\Include\"" in
  if sys = "linux" then (
    match P.get c with
    | None -> None
    | Some p -> P.query p ~package:"bluez"
  )
  else if sys = "msvc" || sys = "win64" then
    Some { P.cflags = ["/nologo"; "/DWIN32"; "/I" ^ ms_sdk];  libs = []}
  else if sys = "mingw64" then
    Some { P.cflags = ["-I"; ms_sdk];  libs = [] }
  else if sys = "macosx" then
    let mac_bt = "/System/Library/Frameworks/IOBluetooth.framework/Headers" in
    Some { P.cflags = ["-DMACOSX"; "-I"; mac_bt];  libs = [] }
  else
    C.die "System %S currently not supported.  Please contact the \
           mindstorm developers." sys

let get_usb c =
  match P.get c with
  | None -> None
  | Some p ->
     match P.query p ~package:"libusb-1.0" with
     | None -> None
     | Some p -> Some { p with P.cflags = "-DHAS_USB" :: p.P.cflags }

let has_usb_set_option ?c_flags ?link_flags c =
  C.c_test c ?c_flags ?link_flags
    "#include <libusb.h>
     int main() {
       libusb_context *ctx = NULL;
       libusb_set_option(ctx, LIBUSB_OPTION_LOG_LEVEL, LIBUSB_LOG_LEVEL_INFO);
       return 0;
     }"

let discover c =
  let p = if Sys.word_size = 64 then { P.cflags = ["-DARCH64"]; libs = [] }
          else { P.cflags = []; libs = [] } in
  let p = merge_pkg p (get_bluetooth c) in
  let p, has_usb = match get_usb c with
    | None -> (p, false)
    | Some _ as usb -> (merge_pkg p usb, true) in
  let c_flags =
    match Sys.getenv "MINDSTORM_CFLAGS" with
    | exception Not_found -> p.P.cflags
    | alt_cflags -> C.Flags.extract_blank_separated_words alt_cflags in
  let libs =
    match Sys.getenv "MINDSTORM_LIBS" with
    | exception Not_found -> p.P.libs
    | alt_libs -> "-lm" :: C.Flags.extract_blank_separated_words alt_libs in
  let c_flags =
    if has_usb && has_usb_set_option c ~c_flags ~link_flags:libs then
      "-DHAS_USB_SET_OPTION" :: c_flags
    else c_flags in
  C.Flags.write_sexp "c_flags.sexp" c_flags;
  C.Flags.write_sexp "c_library_flags.sexp" libs

let cppo file c =
  let ocaml_version = C.ocaml_config_var_exn c "version" in
  let system = C.ocaml_config_var_exn c "system" in
  let arch = C.ocaml_config_var_exn c "architecture" in
  let has_usb = get_usb c <> None in
  let cmd = sprintf "cppo -D %s -D %s%s -V OCAML:%s %s"
                       system arch (if has_usb then " -D HAS_USB" else "")
                       ocaml_version (Filename.quote file) in
  ignore(Sys.command cmd)

let () =
  let cppo_file = ref "" in
  let specs = [
      ("--cppo", Arg.Set_string cppo_file,
       " run cppo with the right arguments")] in
  Arg.parse specs (fun _ -> raise(Arg.Bad "no anonymous arg")) "discover";
  C.main ~name:"mindstorm"
    (if !cppo_file <> "" then cppo !cppo_file else discover)
