open Ast_mapper

module Make(A : sig
  val name : string
  val mapper : Ast_mapper.mapper
end) = struct

  open A
    
  let handle_error f =
    try f () with 
(*
    | Syntaxerr.Error e ->
        Format.eprintf "%a@." Syntaxerr.report_error e;
        exit 2
*)
    | e ->
        Format.eprintf "%a@." Location.report_exception e;
        exit 2
    
  let impl mapper fname =
    handle_error @@ fun () ->
      let str = Pparse.parse_implementation ~tool_name:name fname in
      let str = mapper.structure mapper str in
      Pprintast.structure Format.std_formatter str;
      Format.fprintf Format.std_formatter "@."
  
  let intf mapper fname =
    handle_error @@ fun () ->
      let sg = Pparse.parse_interface ~tool_name:name fname in
      let sg = mapper.signature mapper sg in
      Pprintast.signature Format.std_formatter sg;
      Format.fprintf Format.std_formatter "@."
      
  let anonymous mapper fname = 
    if Filename.check_suffix fname ".ml" then impl mapper fname (* .mlt ? *)
    else if Filename.check_suffix fname ".mli" then intf mapper fname 
    else assert false
  
  let () =
    let debug = ref false in
    let rev_files = ref [] in 
    Arg.parse 
      [ "-debug", Arg.Set debug, "debug mode which can take .ml/.mli then print the result"
      ]
      (fun s -> rev_files := s :: !rev_files)
      name;
    try
      match !debug, List.rev !rev_files with
      | true, files ->
          List.iter (anonymous mapper) files
      | false, [infile; outfile] ->
          Ast_mapper.apply ~source:infile ~target:outfile mapper
      | _ -> 
          failwith @@ name ^ " infile outfile"
    with
    | e -> Location.report_exception Format.err_formatter e
end
  
