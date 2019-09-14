open Lwt
module D = Dropbox_lwt_unix

let rec really_read fd buffer ofs len =
  Lwt_unix.read fd buffer ofs len >>= fun r ->
  if r < len then really_read fd buffer (ofs + r) (len - r)
  else return()

let upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0)
  >>= fun fd -> Lwt_unix.stat fn >>= fun u ->
  let size = u.Lwt_unix.st_size in
  let buffer = Bytes.create size in
  really_read fd buffer 0 size >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  D.files_put t fn (`String (Bytes.to_string buffer)) >>= fun m ->
  assert(m.D.bytes = size);
  Lwt_io.printlf "Send: %s\nMetadata: %s\n"
                 fn (Dropbox_j.string_of_metadata m)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
