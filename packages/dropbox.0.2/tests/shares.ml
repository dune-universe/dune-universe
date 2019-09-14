open Lwt
module D = Dropbox_lwt_unix

let shares t fn =
  D.shares t fn >>= function
  | Some shared_l -> Lwt_io.printlf "%s"
                     (Dropbox_j.string_of_shared_link shared_l)
  | None -> Lwt_io.printlf "No file %s" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (shares t) args

let () =
  Common.run main
