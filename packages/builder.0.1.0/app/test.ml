
let () =
  let job = Builder.{ name = "test" ; script = "# nothing to do here" } in
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let out = [ (0, "hello") ; (3, "world") ; (10, "end") ] in
  let now = Ptime_clock.now () in
  let finished = match Ptime.add_span now (Ptime.Span.of_int_s 300) with Some t -> t | None -> assert false in
  let res = Builder.Exited 0 in
  let data =
    [ (Fpath.v "hello", "world") ; (Fpath.v "bin/hello.hvt", "random data, this is supposed to be an ELF file") ]
  in
  let cs = Builder.Asn.exec_to_cs (job, uuid, out, now, finished, res, data) in
  let encoded = Base64.encode_string (Cstruct.to_string cs) in
  print_endline encoded
