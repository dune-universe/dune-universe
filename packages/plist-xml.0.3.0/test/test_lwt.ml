module Lwt_eff : Test_common.IO with type 'a io = 'a Lwt.t = struct
  include Plist_xml_lwt
  include Lwt_unix

  type s = Markup.async
  type 'a io = 'a Lwt.t

  let bind = Lwt.bind
  let return = Lwt.return

  let ( let* ) = Lwt.bind

  let with_stream str f =
    let* chan = Lwt_io.open_file ~mode:Lwt_io.input str in
    Lwt.finalize (fun () ->
        let stream = Markup_lwt_unix.channel chan in
        f stream
      ) (fun () -> Lwt_io.close chan)

  let catch = Lwt.catch

  let protect ~finally f = Lwt.finalize f finally

  let prerr_endline = Lwt_io.write_line Lwt_io.stderr

  let to_stderr = Markup_lwt_unix.to_channel Lwt_io.stderr
end

open Test_common.Make(Lwt_eff)

let () =
  prerr_endline "Test pass...";
  Lwt_main.run (test_pass ());
  prerr_endline "Test fail...";
  Lwt_main.run (test_fail ())
