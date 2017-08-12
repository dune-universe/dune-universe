open Cmdliner
open Decode

exception Packaged_exn of string

let decode (silent:Progress_report.silence_level option) (force_out:bool) (no_meta:bool) (show_max:int64 option) (in_filename:string) (provided_out_filename:string option) : unit =
  Param.Decode.set_failure_list_max_length_possibly show_max;
  Param.Common.set_silence_settings silent;
  try
    let ref_block =
      let prefer = if no_meta then `Any else `Meta in
      match Process.fetch_ref_block ~prefer ~in_filename with
      | Ok res    -> res
      | Error msg -> raise (Packaged_exn msg) in
    let stored_out_filename =
      Sbx_block_helpers.try_block_to_filename ref_block in
    let final_out_path : string =
      match provided_out_filename with
      | None     -> (
          match stored_out_filename with
          | None     ->
            raise (Packaged_exn "No original filename was found in sbx container and no output file name was provided")
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
        Printf.printf "Output file name                                 : %s\n" final_out_path;
        match Process.decode_file ~ref_block ~in_filename ~out_filename:(Some final_out_path) with
        | Ok stats  -> Stats.print_stats stats
        | Error msg -> raise (Packaged_exn msg)
      end
  with
  | Packaged_exn msg -> Printf.printf "%s\n" msg
;;

let show_max =
  let doc = Printf.sprintf "Show up to $(docv)(defaults to %Ld) failing positions.
  Negative values are treated as 0." !Param.Decode.failure_list_max_length in
  Arg.(value & opt (some int64) None & info ["show-fail-max"] ~docv:"SHOW-FAIL-MAX" ~doc)
;;

let no_meta =
  let doc = "Use first whatever valid block as reference block. Use this when the container does not have metadata block or when you are okay with using a data block as reference block." in
  Arg.(value & flag & info ["no-meta"] ~doc)
;;

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
