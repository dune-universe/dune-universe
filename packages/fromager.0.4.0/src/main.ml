open Core

(* configuration *)

let fromager_config = "fromage.toml"

type config = {
  ocamlformat_version : string option;
  ignored_files : string list;
  ignored_dirs : string list;
}

let parse_toml filename : config option =
  match Sys.file_exists filename with
  | `No | `Unknown -> None
  | `Yes ->
      let toml =
        match Toml.Parser.from_filename filename with
        | `Error _ -> failwith "could not parse fromage.toml"
        | `Ok toml -> toml
      in
      let config =
        match Toml.Types.Table.find_opt (Toml.Min.key "config") toml with
        | Some (TTable config) -> config
        | _ -> failwith "fromage.toml has no config table"
      in
      let ocamlformat_version =
        match
          Toml.Types.Table.find_opt (Toml.Min.key "ocamlformat_version") config
        with
        | Some (TString ocamlformat_version) -> Some ocamlformat_version
        | _ -> None
      in
      let ignored_files =
        match
          Toml.Types.Table.find_opt (Toml.Min.key "ignored_files") config
        with
        | Some (TArray (NodeString ignored_files)) -> ignored_files
        | _ -> []
      in
      let ignored_dirs =
        match
          Toml.Types.Table.find_opt (Toml.Min.key "ignored_dirs") config
        with
        | Some (TArray (NodeString ignored_dirs)) -> ignored_dirs
        | _ -> []
      in
      Some { ocamlformat_version; ignored_files; ignored_dirs }

(* interaction with ocamlformat *)

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let ocamlformat args =
  let args = String.concat ~sep:" " args in
  let command = "ocamlformat " ^ args in
  run command

let ocamlformat_version = ocamlformat [ "--version" ]

let run_ocamlformat ~(write : bool) (filename : string) =
  let args = if write then [ "-i" ] else [ "" ] in
  (* write in-place *)
  let args = args @ [ filename ] in
  let _ = ocamlformat args in
  ()

(* recursion *)

let is_ignored_dir (path : string) ~config =
  let basename = Filename.basename path in
  let first_letter = basename.[0] in
  (* ignore _* and .* folders *)
  if
    Char.(first_letter = '_')
    || (String.length basename > 1 && Char.(first_letter = '.'))
  then true
  else
    match config with
    | None -> false
    | Some config -> List.mem ~equal:String.( = ) config.ignored_dirs path

let is_ignored_file path ~config =
  let basename = Filename.basename path in
  let first_letter = basename.[0] in
  (* ignore .* files *)
  if Char.(first_letter = '.') then true
  else
    match config with
    | None -> false
    | Some config -> List.mem ~equal:String.( = ) config.ignored_files path

(* apply [~f] on every `.ml` and `.mli` file found in [folders] *)
let rec visit folders ~config ~f =
  match folders with
  | [] -> ()
  | file :: rest -> (
      let is_dir = Sys.is_directory file in
      match is_dir with
      | `Yes ->
          let rest =
            let ignored = is_ignored_dir ~config file in
            if not ignored then
              let inside_dirs = Sys.ls_dir file in
              let inside_dirs =
                List.map inside_dirs ~f:(fun x -> Filename.concat file x)
              in
              inside_dirs @ rest
            else rest
          in
          visit rest ~config ~f
      | `No | `Unknown ->
          let ignored = is_ignored_file ~config file in
          (if not ignored then
           match Filename.split_extension file with
           | _, Some "ml" | _, Some "mli" -> f file
           | _ -> ());
          visit rest ~config ~f)

(* main *)

let enforce_ocamlformat_version config =
  match config with
  | None -> ()
  | Some config -> (
      match config.ocamlformat_version with
      | None -> ()
      | Some expected_version ->
          let version = String.rstrip ocamlformat_version in
          if String.(version = expected_version) then ()
          else failwith "unexpected ocamlformat version according to config")

let () =
  let cwd = Sys.getcwd () in
  (* parse fromage.toml *)
  let config = parse_toml fromager_config in
  (* enforce ocamlformat version *)
  enforce_ocamlformat_version config;
  (* create an empty .ocamlformat if there are none *)
  let ocamlformat_file = Filename.concat cwd ".ocamlformat" in
  let ocamlformat_already_exists =
    match Sys.file_exists ocamlformat_file with `Yes -> true | _ -> false
  in
  if not ocamlformat_already_exists then
    Out_channel.write_all ocamlformat_file ~data:"";
  (* format everything *)
  visit [ "." ] ~config ~f:(run_ocamlformat ~write:true);
  (* remove the .ocamlformat if there were none *)
  if not ocamlformat_already_exists then Unix.remove ocamlformat_file
