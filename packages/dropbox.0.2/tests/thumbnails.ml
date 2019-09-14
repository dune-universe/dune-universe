open Lwt
module D = Dropbox_lwt_unix

(** If there is two entries, we call thumbnails with the path as
       the first arg and the param format as the second.
    If there is three entries, we add the param size as the third argument *)

let string_to_size = function
  | "xs" -> `Xs
  | "s" -> `S
  | "m" -> `M
  | "l" -> `L
  | "xl" -> `Xl
  | _ -> invalid_arg "Size must be xs, s, m, l or xl."

let string_to_format = function
  | "jpeg" -> `Jpeg
  | "jpg" -> `Jpeg
  | "png" -> `Png
  | "bmp" -> `Bmp
  | _ -> invalid_arg "Format must be jpeg, jpg, png or bmp."


let download t ?(format="jpeg") ?(size="s") fn =
  D.thumbnails t ~size:(string_to_size size)
               ~format:(string_to_format format) fn
  >>= function
    | None -> Lwt_io.printlf "No image named %S." fn
    | Some(metadata, stream) ->
  (* Save stream to the disk *)
  let fn = Filename.basename fn in
  let fname = "thumbnail_of_" ^ (Filename.chop_extension fn) ^ "." ^ format in
  Lwt_unix.(openfile fname [O_WRONLY; O_CREAT; O_TRUNC] 0o664)
  >>= fun fd ->
  let write s =
    let s = Bytes.of_string s in
    Lwt_unix.write fd s 0 (Bytes.length s) >>= fun _ ->
    return_unit in
  Lwt_stream.iter_s write stream >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Lwt_io.printlf "Wrote a thumbnail as %S\n%s" fname
                 (Dropbox_j.string_of_metadata metadata)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | [fn] -> download t fn
  | [fn; size] -> download t ~size fn
  | [fn; size; format] -> download t ~format ~size fn
  | _ -> Lwt_io.printlf "%s <path> [size] [format]" Sys.argv.(0)

let () =
  Common.run main
