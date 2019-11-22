let _ =
  if Array.length Sys.argv < 2 then
    failwith (Format.sprintf "usage: %s <file>" Sys.argv.(0)) ;
  let file = Sys.argv.(1) in
  let chan = open_in file in
  let lines = ref [] in
  let keep_alpha s =
    let to_keep = [' '; '\''; '-'] in
    let to_keep = List.map (fun el -> Uchar.of_char el) to_keep in
    let b = Buffer.create 256 in
    let add_alpha () _ = function
      | `Malformed _ ->
          Uutf.Buffer.add_utf_8 b Uutf.u_rep
      | `Uchar u ->
          if Uucp.Alpha.is_alphabetic u || List.mem u to_keep then
            Uutf.Buffer.add_utf_8 b u
    in
    Uutf.String.fold_utf_8 add_alpha () s ;
    Buffer.contents b
  in
  ( try
      while true do
        let l = String.trim (input_line chan) in
        let l = keep_alpha l in
        lines := l :: !lines
      done
    with End_of_file -> close_in chan ) ;
  let lines = List.rev !lines in
  let generator = Omg.init () in
  List.iter (fun el -> Omg.feed generator el) lines ;
  for _ = 0 to 30 do
    Format.fprintf Format.std_formatter "GEN: %s@."
      (Omg.generate_markov_text generator 50 (None, None) false)
  done
