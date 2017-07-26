let%lwt () =
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input "README.md" in
  [%defer.lwt Lwt_io.close ic];
  let%lwt bytes = Lwt_io.read ic in
  Lwt_io.printl bytes
