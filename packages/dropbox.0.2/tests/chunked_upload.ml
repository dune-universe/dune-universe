open Lwt
module D = Dropbox_lwt_unix

let upload t fn =
  (* We first open the file and get its size *)
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  Lwt_unix.stat fn >>= fun u ->
  let size = u.Lwt_unix.st_size in

  (* Use a 1Mb buffer to exert the upload in several chunks on file of
     "small" size. *)
  let buff_len = 1024 * 1024 in
  let buffer = Bytes.create buff_len in

  (* Send the whole file in chunks. *)
  let rec send_chunks ?id ?(ofs=0) remaining =
    if remaining > 0 then (
      Lwt_unix.read fd buffer 0 (min buff_len remaining) >>= fun read ->
      let chunk = if read = buff_len then Bytes.to_string buffer
                  else Bytes.sub_string buffer 0 read in
      D.chunked_upload t (`String chunk) ?id ~ofs >>= fun chkd_upld ->
      Lwt_io.printlf "Uploaded: %i bytes → id: %s, ofs: %i"
                     read (chkd_upld.D.id :> string) chkd_upld.D.ofs
      >>= fun () ->
      send_chunks ~id:chkd_upld.D.id ~ofs:chkd_upld.D.ofs
                  (remaining - read)
    )
    else
      return id in

  send_chunks size >>= function
  | Some id ->
     (* Finally, we finish the upload of the file by using
        commit_chunked_upload. *)
     D.commit_chunked_upload t id fn >>= fun metadata ->
     Lwt_unix.close fd >>= fun () ->
     assert(metadata.D.bytes = size);
     Lwt_io.printlf "Sent: %S\nMetadata:\n%s"
                    fn (Dropbox_j.string_of_metadata metadata)
  | None ->
     Lwt_io.printlf "Nothing uploaded."


let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
