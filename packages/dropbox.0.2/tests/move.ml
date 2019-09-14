open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is from_path, Sys.argv.(1) is to_path.*)

let main t args =
  match args with
  | [from_path; to_path] ->
     D.move t from_path to_path
     >>= (function
           | `Some m -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
           | `None -> Lwt_io.printlf "No file %s" from_path
           | `Invalid s -> Lwt_io.printlf "Invalid: %s" s
           | `Too_many_files -> Lwt_io.printlf "Too many files involved")
  | _ -> Lwt_io.printlf "%s <from_path> <to_path>" Sys.argv.(0)

let () =
  Common.run main
