(***********************************************************************)
(*                                                                     *)
(*                           CamlImages                                *)
(*                                                                     *)
(*                           Jun Furuse                                *)
(*                                                                     *)
(*  Copyright 1999-2013,                                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: test.ml,v 1.32.2.1 2010/05/13 13:14:47 furuse Exp $ *)

(* CR jfuruse: move to Spotlib *)
let _dump_hex s = 
  for i = 0 to String.length s - 1 do
    Printf.printf "%02x " (Char.code s.[i]);
    if i mod 16 = 15 then Printf.printf "\n";
  done

let dump path = 
  Format.eprintf "File: %s@." path;
  List.iter (fun mrk ->
    Format.eprintf "  %a@." Jpeg.Marker.format mrk;
    match mrk with
    | Jpeg.Marker.App (1, cont) when
        (try String.sub cont 0 6 = "Exif\000\000" with _ -> false) ->
        let exif = Exif.Data.from_string cont in
        Exif.Data.dump exif;
        Format.eprintf "%a@." Exif.Data.format exif;
        Exif.Data.dump exif;
    | _ -> ()
  ) (Jpeg.read_markers path)
  
let images =
  let images = ref [] in
  Arg.parse [] (fun x -> images := x :: !images) "test images";
  List.rev !images

let main () =
  try List.iter dump images
  with
  | Exit -> exit 0
  | End_of_file -> exit 0
  | Sys.Break -> exit 2

let () = main ()
