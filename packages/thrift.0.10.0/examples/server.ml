
let () =
  let iface = object
    inherit Calculator.iface
    method ping =
      Printf.printf "server: got ping\n%!";
      print_endline "ping"

    method add i j =
      Printf.printf "server: got add\n%!";
      match i, j with
      | Some i, Some j -> Int32.add i j
      | _ -> failwith "missing arg"

    method calculate _ _ = failwith "unimplemented"
  end
  in
  let processor = new Calculator.processor iface in
  let server =
    new TThreadedServer.t processor (new TServerSocket.t 2526)
      (object method getTransport t = t end)
      (new TBinaryProtocol.factory)
      (new TBinaryProtocol.factory)
  in
  server#serve
