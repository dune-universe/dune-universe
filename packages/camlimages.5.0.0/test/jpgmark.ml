(***********************************************************************)
(*                                                                     *)
(*                           CamlImages                                *)
(*                                                                     *)
(*                            Jun Furuse                               *)
(*                                                                     *)
(*  Copyright 1999-2013,                                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: test.ml,v 1.32.2.1 2010/05/13 13:14:47 furuse Exp $ *)

let dump path = 
  Format.eprintf "File: %s@." path;
  let markers = Jpeg.read_markers path in
  prerr_endline "markers loaded";
  List.iter (fun t ->
    Format.eprintf "  %a@." Jpeg.Marker.format t)
    markers
  
let images =
  let images = ref [] in
  Arg.parse [] (fun x -> images := x :: !images) "test images";
  List.rev !images

let main () =
  try 
    for _i = 0 to 10000 do
      List.iter dump images
    done
  with
  | Exit -> exit 0
  | End_of_file -> exit 0
  | Sys.Break -> exit 2

let () = main ()
