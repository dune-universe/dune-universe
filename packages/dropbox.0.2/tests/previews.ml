open Lwt
module D = Dropbox_lwt_unix

let download t fn =
  D.previews t fn >>= function
  | None -> Lwt_io.printlf "No file named %S." fn
  | Some(c_t, _c_l, stream) ->
    let format = match c_t with
      | "application/pdf" -> ".pdf"
      | _ -> ".html" in
    let fn = Filename.basename fn in
    let fname = "preview_of_" ^ (Filename.chop_extension fn) ^ format in
    Lwt_unix.(openfile fname [O_WRONLY; O_CREAT; O_TRUNC] 0o664)
    >>= fun fd ->
    let write s =
      let s = Bytes.of_string s in
      Lwt_unix.write fd s 0 (Bytes.length s) >>= fun _ ->
      return_unit in
    Lwt_stream.iter_s write stream >>= fun () ->
    Lwt_unix.close fd >>= fun () ->
    Lwt_io.printlf "Wrote \"%s\"." fname

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | _ ->
     (* You can replace Lwt_list.iter_p by Lwt_list.iter_s for a
        sequential download and see how much slower it is. *)
     Lwt_list.iter_p (download t) args

let () =
  Common.run main
