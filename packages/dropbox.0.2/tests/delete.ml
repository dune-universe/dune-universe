open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the path and Sys.argv.(1) the root *)

let string_to_root a = match a with
  | "auto" -> `Auto
  | "dropbox" -> `Dropbox
  | "sandbox" -> `Sandbox
  | _ -> invalid_arg "root must be auto, dropbox or sandbox"

let delete t ?root path =
  D.delete t ?root path
  >>= function
  | `Some m -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
  | `None -> Lwt_io.printlf "No file %s" path
  | `Too_many_files -> Lwt_io.printlf "Too many files involved"

let main t args =
  match args with
  | [path] -> delete t path
  | [path; root] -> delete t ~root:(string_to_root root) path
  | _ -> Lwt_io.printf "%s <path> [root]\n" Sys.argv.(0)

let () =
  Common.run main
