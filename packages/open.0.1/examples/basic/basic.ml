let extensions = ["pdf"; "png"; "svg"];;

List.iter (fun ext ->
  Format.printf "attempting to open %s file...\n%!" ext;
  let file = "ocaml." ^ ext in
  Open.in_default_app file
) extensions;;

Format.printf "done..?\n";;
