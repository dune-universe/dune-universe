let () =
  let buflen = 1024 in
  let buf = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout buflen in
  let conv = Samplerate.create Samplerate.Conv_linear 1 in
  let outbuf =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (2 * buflen)
  in
  let i, _ = Samplerate.process_ba conv 2. buf outbuf in
  Printf.printf "Converted %d out of %d.\n%!" i buflen
