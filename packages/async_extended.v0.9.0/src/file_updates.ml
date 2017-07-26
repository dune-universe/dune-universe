open Core
open Async

module Update = struct
  type 'a t =
    | Update of 'a
    | Error of ([`Transform | `Read] * exn)
end

let updates ?(poll_interval=sec 5.) filename ~transform =
  let f =
    match transform with
    | `Of_sexp f -> (fun s -> f (Sexp.of_string s))
    | `Of_raw f  -> f
  in
  let r,w = Pipe.create () in
  let rec loop last_mtime =
    let loop new_mtime =
      Clock.with_timeout poll_interval (Pipe.closed r)
      >>> function
      | `Result () -> ()
      | `Timeout   -> loop new_mtime
    in
    let error e = Pipe.write w (Update.Error e) >>> (fun () -> loop None) in
    try_with (fun () -> Unix.stat filename)
    >>> function
    | Error e -> error (`Read, e)
    | Ok stat ->
      let new_mtime = stat.Unix.Stats.mtime in
      if Option.value_map last_mtime  ~default:true ~f:(fun mt -> Time.(<>) new_mtime mt)
      then begin
        try_with (fun () -> Reader.file_contents filename)
        >>> function
        | Error e -> error (`Read, e)
        | Ok contents ->
          begin match Result.try_with (fun () -> f contents) with
          | Ok transformed_contents ->
            Pipe.write w (Update.Update transformed_contents) >>> fun () ->
            loop (Some new_mtime)
          | Error e -> error (`Transform, e)
          end
      end else loop last_mtime
  in
  loop None;
  r


