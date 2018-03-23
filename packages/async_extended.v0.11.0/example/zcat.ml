open Core
open Async
open Async_extended.Std

let main file () =
  let buf = Bytes.create 1024 in
  let finished = Ivar.create () in
  let stdout = Lazy.force Writer.stdout in
  Reader.with_gzip_file file ~f:(fun reader ->
    let rec loop () =
      upon (Reader.read reader buf) (function
        | `Eof -> Ivar.fill finished ()
        | `Ok len ->
          Writer.write_substring stdout (Substring.create buf ~pos:0 ~len);
          loop ())
    in
    loop ();
    Ivar.read finished)
  >>= fun () -> Writer.flushed stdout

let () =
  Command.async ~summary:"cat a compressed file"
    (let open Command.Let_syntax in
     let%map_open file = anon ("FILE-NAME" %: file) in
     main file)
  |> Command.run
