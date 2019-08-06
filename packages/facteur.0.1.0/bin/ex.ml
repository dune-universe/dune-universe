let () =
  let magic = Magic.magic_init (Bytes.make 8 '\000') [ Magic.MIME_TYPE ] in
  Magic.magic_load magic None ;
  print_endline (Magic.magic_file magic Sys.argv.(1)) ;
  Magic.magic_close magic
