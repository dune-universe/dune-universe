open Lwt
module D = Dropbox_lwt_unix

let media t fn =
  D.media t fn >>= function
  | Some media -> Lwt_io.printlf "%s" (Dropbox_j.string_of_link media)
  | None -> Lwt_io.printlf "No file %s" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (media t) args

let () =
  Common.run main
