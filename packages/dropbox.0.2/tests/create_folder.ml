open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the path of the folder and Sys.argv.(1) is the root *)

let string_to_root a = match a with
  | "auto" -> `Auto
  | "dropbox" -> `Dropbox
  | "sandbox" -> `Sandbox
  | _ -> invalid_arg "root must be auto, dropbox or sandbox"

let create_folder t ?root path =
  D.create_folder t ?root path
  >>= function
  | `Some m -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
  | `Invalid s -> Lwt_io.printlf "Invalid: %s" s

let main t args =
  match args with
  | [path] -> create_folder t path
  | [path; root] -> create_folder t ~root:(string_to_root root) path
  | _ -> Lwt_io.printlf "%s <path> [root]\n" Sys.argv.(0)

let () =
  Common.run main
