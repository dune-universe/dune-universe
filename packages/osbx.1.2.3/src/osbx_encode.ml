open Cmdliner
open Encode
open Sbx_specs

exception Packaged_exn of string

let encode (silent:Progress_report.silence_level) (force_out:bool) (no_meta:bool) (ver:version) (uid:string option) (hash_type:string option) (in_filename:string) (provided_out_filename:string option) : unit =
  Dynamic_param.Common.set_silence_settings silent;
  try
    let uid : bytes option =
      match uid with
      | None     -> None
      | Some str ->
        match Conv_utils.hex_string_to_bytes str with
        | Ok uid  -> Some uid
        | Error _ -> raise (Packaged_exn (Printf.sprintf "Uid %s is not a valid hex string" str)) in
    let out_filename : string =
      let supposed_out_filename =
        match provided_out_filename with
        | Some str -> str
        | None     -> String.concat "" [in_filename; ".sbx"] in
      let out_file_exists = Sys.file_exists supposed_out_filename in
      if out_file_exists then
        begin
          if Sys.is_directory supposed_out_filename then
            (* if a directory is given, then try DIR/INFILE.sbx *)
            let in_filename_no_path       = Misc_utils.path_to_file in_filename in
            let inferred_filename         = String.concat "" [in_filename_no_path; ".sbx"] in
            let inferred_full_path        = Misc_utils.make_path [supposed_out_filename; inferred_filename] in
            let inferred_full_path_exists = Sys.file_exists inferred_full_path in
            match (inferred_full_path_exists, force_out) with
            | (true , false) -> raise (Packaged_exn (Printf.sprintf "File %s already exists" inferred_full_path))
            | (_    , true )
            | (false, _    ) -> inferred_full_path
          else
            begin
              if force_out then
                supposed_out_filename
              else
                raise (Packaged_exn (Printf.sprintf "File %s already exists" supposed_out_filename))
            end
        end
      else
        supposed_out_filename in
    let hash =
      match hash_type with
      | Some str -> str
      | None     -> "SHA256" in
    match Process.encode_file ~uid ~want_meta:(not no_meta) ~ver ~hash ~in_filename ~out_filename with
    | Ok stats  -> Stats.print_stats stats
    | Error msg -> raise (Packaged_exn msg)
  with 
  | Packaged_exn str -> Printf.printf "%s\n" str
;;

let uid =
  let doc = "Alternative file uid in hex (by default uid is randomly generated). Uid must be exactly 6 bytes(12 hex digits) in length." in
  Arg.(value & opt (some string) None & info ["uid"] ~docv:"UID-HEX" ~doc)
;;

let no_meta =
  let doc = "Do not put metadata block in the sbx container" in
  Arg.(value & flag & info ["no-meta"] ~doc)
;;

let in_file =
  let doc = "File to encode" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_file =
  let doc = "Sbx container name (defaults to INFILE.sbx).
  If $(docv) is a directory(DIR), then the final file will be stored as DIR/INFILE.sbx." in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"OUT" ~doc)
;;
