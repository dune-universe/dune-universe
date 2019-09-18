open Srt

let () =
  startup ();
  Log.set_handler (fun {Log.message;_} -> print_endline message);
  Log.setloglevel `Warning;
  let s = socket Unix.PF_INET Unix.SOCK_DGRAM 0 in 
  Printf.printf "Messageapi: %b\n%!" (getsockflag s messageapi);
  Printf.printf "Setting transtype to file..\n%!";
  setsockflag s transtype `File;
  Printf.printf "Messageapi: %b\n%!" (getsockflag s messageapi);
  setsockflag s rcvsyn true;
  Printf.printf "Rcvsyn: %b\n%!" (getsockflag s rcvsyn);
  setsockflag s rcvsyn false;
  Printf.printf "Rcvsyn: %b\n%!" (getsockflag s rcvsyn);
  setsockflag s payloadsize 1234;
  Printf.printf "Payloadsize: %d\n%!" (getsockflag s payloadsize);
  close s;
  cleanup()
