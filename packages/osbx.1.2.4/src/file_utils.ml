open Stdint
open Stream_file

exception File_access_error

let getmtime ~(filename:string) : float =
  (* Unix.stat should also be usable on windows
   * Reference :
   *  https://caml.inria.fr/pub/docs/manual-ocaml/libunix.html (Accessed on 2017-06-29)
   *    See table shown at bottom, Unix.stat is not in the table
   *)
  try
    let { Unix.st_mtime = mtime; _ } = Unix.stat filename in
    mtime
  with
  | _ -> raise File_access_error
;;

let getmtime_uint64 ~(filename:string) : uint64 =
  Uint64.of_float (getmtime ~filename)
;;

module Processor = struct
  let file_size_getter : int64 Stream.in_processor =
    (fun in_file ->
       LargeFile.in_channel_length in_file
    )
  ;;
end

let getsize ~(filename:string) : int64 =
  match Stream.process_in ~in_filename:filename Processor.file_size_getter with
  | Ok size -> size
  | Error _ -> raise File_access_error
;;

let getsize_uint64 ~(filename:string) : uint64 =
    Uint64.of_int64 (getsize ~filename)
;;
