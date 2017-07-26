open Cmdliner
open Decode

exception Packaged_exn of string

let decode (force_out:bool) (in_filename:string) (provided_out_filename:string option) : unit =
  try
    let stored_out_filename : string option =
      match Process.fetch_out_filename ~in_filename with
      | Ok name   -> name
      | Error msg -> raise (Packaged_exn msg) in
    let final_out_path : string =
      match provided_out_filename with
      | None     -> (
          match stored_out_filename with
          | None     ->
            raise (Packaged_exn "No original filename was found in sbx container and no output file name is provided")
          | Some str ->
            str
        )
      | Some provided_path ->
          let provided_path_exists = Sys.file_exists provided_path in
          let provided_path_is_dir =
            try Sys.is_directory provided_path with | Sys_error _ -> false in
          match (provided_path_exists, provided_path_is_dir, stored_out_filename) with
          |     (true                , false               , _                  )
            -> provided_path
          |     (true                , true                , Some str           )
            -> let stored_out_filename_no_path = Misc_utils.path_to_file str in
            Misc_utils.make_path [provided_path; stored_out_filename_no_path]
          |     (true                , true                , None               )
            -> raise (Packaged_exn "No original filename was found in sbx container and output file name is a directory")
          |     (false               , _                   , _                  )
            -> provided_path in
    if Sys.file_exists final_out_path && not force_out then
      raise (Packaged_exn (Printf.sprintf "File %s already exists" final_out_path))
    else
      begin
        Printf.printf "Output file name                               : %s\n" final_out_path;
        match Process.decode_file ~in_filename ~out_filename:(Some final_out_path) with
        | Ok stats  -> Stats.print_stats stats
        | Error msg -> raise (Packaged_exn msg)
      end
  with
  | Packaged_exn msg -> Printf.printf "%s\n" msg

let in_file =
  let doc = "Sbx container to decode" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_file =
  let doc = "Decoded file name.
  If $(docv) is not provided, then name stored in sbx container is used if present.
  If $(docv) is provided and is a directory(DIR) then output file is stored as DIR/STORED_NAME.
  If $(docv) is provided and is not a directory, then it is used directly." in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"OUT" ~doc)
;;
