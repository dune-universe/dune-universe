

let () =
  Printf.printf "connecting…\n%!";
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 2526));
  let ic = Unix.in_channel_of_descr sock in
  let oc = Unix.out_channel_of_descr sock in
  Printf.printf "socket open…\n%!";
  let transport = new TChannelTransport.t (ic,oc) in
  let proto1 = new TBinaryProtocol.t transport in
  let proto2 = new TBinaryProtocol.t transport in
  let c = new Calculator.client proto1 proto2 in
  Printf.printf "ping server\n%!";
  c#ping;
  Printf.printf "try to add 2 and 2\n%!";
  let n = c#add 2l 2l in
  Printf.printf "… 2+2 = %ld\n%!" n;
  ()
