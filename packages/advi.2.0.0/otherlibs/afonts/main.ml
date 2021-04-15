let x_fonts_table =
  Fonts_table.get ();;

Hashtbl.iter
  (fun k _v ->
   prerr_endline (Printf.sprintf "Font %s" k)
  )
  x_fonts_table;
  print_newline ()
;;

