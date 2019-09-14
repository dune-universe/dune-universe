open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the path of the file and Sys.argv.(1) is the rev of the file.
    You can get a rev of the file by using {!revisions}. *)

let main t args =
  match args with
  | [fn; rev] ->
     D.restore t ~rev fn >>=
       (function
         | Some meta -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata meta)
         | None -> Lwt_io.printlf "No file %s with rev %s." fn rev)
  | _ -> Lwt_io.printf "Usage: %s <path-to-file> <revision>\n" Sys.argv.(0)

let () =
  Common.run main
