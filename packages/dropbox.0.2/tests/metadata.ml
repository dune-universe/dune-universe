open Lwt
module D = Dropbox_lwt_unix

let metadata t fn =
  D.metadata t fn ~include_media_info:true >>= function
  | Some metadata ->
     Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata metadata)
  | None ->
     Lwt_io.printlf "No file %S." fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (metadata t) args

let () =
  Common.run main
