open Lwt
module D = Dropbox_lwt_unix

let copy_ref t fn =
  D.copy_ref t fn >>= function
  | Some copy_ref -> Lwt_io.printlf "%s" (Dropbox_j.string_of_copy_ref copy_ref)
  | None -> Lwt_io.printlf "No file %s" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (copy_ref t) args

let () =
  Common.run main
