let promise =
  [%defer.lwt Lwt_io.write Lwt_io.stdout " world"];
  Lwt_io.write Lwt_io.stdout "Hello"

let () = Lwt_main.run promise
