open Base

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Haskell's [($)]. *)

let (!%) fmt = Printf.sprintf fmt

module Configurator = struct
  include Configurator.V1

  let ( ^/ ) = Caml.Filename.concat

  let path_sep =
    if Sys.win32 then
      ';'
    else
      ':'

  let exe = if Sys.win32 then ".exe" else ""

  let get_path () =
    match Caml.Sys.getenv "PATH" with
    | exception Not_found -> []
    | s -> String.split ~on:path_sep s

  let find_file_in bases dirs =
    List.find_map dirs ~f:(fun dir ->
      List.find_map bases ~f:(fun base ->
        let path = dir ^/ base in
        if Caml.Sys.file_exists path then Some path else None))

  let find_program prog =
    let prog = prog ^ exe in
    find_file_in [prog] & get_path ()

  let find_ocaml_program prog =
    let prog_opt = prog ^ ".opt" ^ exe in
    let prog_byte = prog ^ ".byte" in (* XXX need to check in Windows *)
    find_file_in [prog_opt; prog; prog_byte ] & get_path ()

  let () = Findlib.init ()

  let find_ocaml_package n =
    match Findlib.package_directory n with
    | s -> 
        (* findlib 1.7.3 installs META file for graphics 
           even when there is no graphics library installed. *)
        let dest = Caml.Filename.temp_file "test" ".cma" in
        let res = match Caml.Sys.command & !% "ocamlfind ocamlc -package %s -o %s -linkpkg" n dest with
          | 0 -> Some s
          | _ -> None
          | exception _ -> None
        in
        (try Caml.Sys.remove dest with _ -> ());
        res
    | exception Findlib.No_such_package _ -> None

  module Package_conf = struct
    type t
      = Pkg_config.package_conf
      = { libs   : string list
        ; cflags : string list
        }

    let options_of {libs; cflags} = String.concat ~sep:" " (libs @ cflags)

    let merge c1 c2 =
      { libs = c1.libs @ c2.libs; cflags = c1.cflags @ c2.cflags }

    let empty = { libs= []; cflags= [] }
  end
end

open Configurator

type item =
  | Pkg_config   of unit option
  | File         of string option
  | Program      of string option
  | Library      of Package_conf.t option
  | OCamlLibrary of string option

module Make(A : sig val name : string end) = struct
  let t = create A.name

  let log fmt = Caml.Format.eprintf fmt

  module Package_conf = Package_conf
  open Package_conf

  let extract_package_conf xs =
    Caml.List.fold_left merge empty
      (Caml.List.map (fun item -> match item with
           | Library (Some pkc) -> pkc
           | _ -> empty) xs)

  let write_package_conf_sexps ~prefix xs =
    let open Base in
    let open Stdio in
    let package_conf = extract_package_conf xs in
    Out_channel.write_all (!% "%sc_flags.sexp" prefix)
      ~data:(Base.Sexp.to_string
               (sexp_of_list sexp_of_string package_conf.cflags));
    Out_channel.write_all (!% "%sc_library_flags.sexp" prefix)
      ~data:(Base.Sexp.to_string
               (sexp_of_list sexp_of_string package_conf.libs))


  let pkcfg, pkg_config = 
    log "Checking pkg-config in $PATH... ";
    let pkcfg = Pkg_config.get t in
    (pkcfg,
     match pkcfg with
     | None -> 
         log "(not found)@.";
         Pkg_config None
     | Some _p -> 
         log "(found)@.";
         Pkg_config (Some ())
    )

  let find_program n = 
    log "Checking program %s in $PATH... " n;
    match find_program n with
    | None -> 
        log "(not found)@.";
        Program None
    | Some n -> 
        log "(found : %s)@." n;
        Program (Some n)

  let find_ocaml_program n = 
    log "Checking OCaml program %s in $PATH... " n;
    match find_ocaml_program n with
    | None -> 
        log "(not found)@.";
        Program None
    | Some n -> 
        log "(found : %s)@." n;
        Program (Some n)

  let find_ocaml_package n = 
    log "Checking OCaml library %s... " n;
    match find_ocaml_package n with
    | Some s -> 
        log "(found : %s)@." s;
        OCamlLibrary (Some s)
    | None -> 
        log "(not found)@.";
        OCamlLibrary None

  let by_pkg_config package () = 
    log "Checking pkg-config package %s... " package;
    match pkcfg with
    | None -> 
        log "(not found : needs pkg-config)@.";
        None
    | Some pkgcfg ->
        match Pkg_config.query pkgcfg ~package with
        | None ->
            log "(not found)@.";
            None
        | Some conf ->
            log "(found : %s)@." (Package_conf.options_of conf);
            Some conf

  let by_cc ~c_flags ~link_flags ~headers ~functions:fnames () =
    log "Checking library %s by using C compiler... " (String.concat ~sep:" " link_flags);
    let headers = "stdio.h" :: headers in
    let includes = Caml.List.map (!% "#include <%s>") headers in
    let fcalls = Caml.List.map (!% "  ( (void(*)()) (%s) )();") fnames in
    let code = 
      String.concat ~sep:"\n" 
      & includes 
        @ [ "int main(int argc, char **argv) {" ]
        @ fcalls
        @ [ "return 0; }"
          ; "\n"
          ]
    in
    if 
      c_test t 
        ~c_flags 
        ~link_flags
        code
    then begin
      let conf = { libs= link_flags; cflags= c_flags } in
      log "(found : %s)@." (Package_conf.options_of conf);
      Some conf
    end else begin
      log "(not found)@.";
      None
    end

  let find_library tests =
    Library (List.find_map tests (fun t -> t ()))

  let find_file base ~dirs =
    log "Checking file %s..." base;
    match find_file_in [base] dirs with
    | None ->
        log "(not found)@.";
        File None
    | Some p ->
        log "(found : %s)@." p;
        File (Some p)

  open C_define
  open C_define.Value

  let bool k o =
    let b = match o with Some _ -> true | None -> false in
    !% "BOOL_%s" k, String (if b then "true" else "false")

  let has k o =
    let b = match o with Some _ -> true | None -> false in
    !% "HAS_%s" k, Switch b

  (* Configurator does not have #define X code... *)
  let path k o =
    [ bool k o;  has k o ] @ 
    match o with 
    | Some p -> [ !% "PATH_%s" k, String p
                (* ; !% "PATH_OPTION_%s" k, String (!% "(Some %s)" p) *) ]
    | None -> [ (* !% "PATH_OPTION_%s" k, String "None" *) ]

  let library k o =
    [ bool k o;  has k o ] @ 
    match o with
    | Some { libs; cflags } ->
        [ !% "CFLAGS_%s" k, String (String.concat ~sep:" " cflags )
        ; !% "LDFLAGS_%s" k, String (String.concat ~sep:" " libs )
        ]
    | None -> []

  let make_header ~fname kitems =
    let kvs = List.concat_map kitems ~f:(fun (k,item) -> match item with
        | Pkg_config o -> [ bool k o; has k o ] 
        | File o 
        | Program o 
        | OCamlLibrary o -> path k o
        | Library o -> library k o)
    in
    gen_header_file t ~fname kvs
end

