let extensions = ["pdf"; "png"; "svg"];;

List.iter (fun ext ->
  Format.printf "attempting to open %s file...%!" ext;
  let file = "ocaml logo." ^ ext in
  let ok = Open.in_default_app file in
  if ok then Format.printf "ok.\n%!" else Format.printf "failed.\n%!"
) extensions;;

Format.printf "Done.\n";;
