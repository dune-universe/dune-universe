module type IO = sig
  include Plist_xml.S

  val opendir : string -> Unix.dir_handle io
  val readdir : Unix.dir_handle -> string io
  val closedir : Unix.dir_handle -> unit io

  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io

  val with_stream : string -> ((char, s) Markup.stream -> 'a io) -> 'a io

  val catch : (unit -> 'a io) -> (exn -> 'a io) -> 'a io

  val protect : finally:(unit -> unit io) -> (unit -> 'a io) -> 'a io

  val prerr_endline : string -> unit io

  val to_stderr : (char, Markup.sync) Markup.stream -> unit io
end

module Make (IO : IO) = struct
  let ( let* ) = IO.bind

  let test_pass () =
    let* handle = IO.opendir "pass" in
    IO.protect (fun () ->
        let rec loop () =
          let* str = IO.readdir handle in
          let* _ =
            if str <> Filename.parent_dir_name
            && str <> Filename.current_dir_name then (
              let* () = IO.prerr_endline ("Testing " ^ str) in
              let* plist = IO.with_stream ("pass/" ^ str) IO.parse_exn in
              plist
              |> Plist_xml.signals
              |> Markup.pretty_print
              |> Markup.write_xml
              |> IO.to_stderr
            ) else
              IO.return ()
          in loop ()
        in
        IO.catch loop (function
            | End_of_file -> IO.return ()
            | exn -> raise exn
          )
      ) ~finally:(fun () -> IO.closedir handle)

  let test_fail () =
    let* handle = IO.opendir "fail" in
    IO.protect (fun () ->
        let rec loop () =
          let* str = IO.readdir handle in
          let* () =
            if str <> Filename.parent_dir_name
            && str <> Filename.current_dir_name then (
              prerr_endline ("Testing " ^ str);
              IO.catch
                (fun () ->
                   let* _ = IO.with_stream ("fail/" ^ str) IO.parse_exn in
                   failwith ("Test " ^ str ^ " parsed"))
                (function
                  | Plist_xml.Parse_error _ -> IO.return ()
                  | exn -> raise exn)
            ) else (
              IO.return ()
            )
          in loop ()
        in
        IO.catch loop (function
            | End_of_file -> IO.return ()
            | exn -> raise exn
          )
      ) ~finally:(fun () -> IO.closedir handle)
end
