let _ =
  let file = "2017-05-02.log" in
  let chan = open_in file in
  let msgs = Opazl.Parser.from_channel chan in
  assert (List.length msgs = 384) ;
  Format.printf "Tests are OK !@."
