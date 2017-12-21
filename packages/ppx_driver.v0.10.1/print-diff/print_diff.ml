open StdLabels

let patdiff_cmd ~use_color =
  let args =
    List.concat [
      ["-keep-whitespace"];
      ["-location-style omake"];
      (if use_color then ["-unrefined"] else ["-ascii"]);
    ]
  in
  String.concat ~sep:" " ("patdiff" :: args)
;;

let print ?diff_command ?(use_color=false) ~file1 ~file2 () =
  let exec cmd =
    let cmd =
      Printf.sprintf "%s %s %s 1>&2" cmd (Filename.quote file1) (Filename.quote file2)
    in
    match Sys.command cmd with
    | 0 -> true
    | 1 -> false
    | n -> Printf.eprintf "%S exited with code %d\n" cmd n; exit 2
  in
  match diff_command with
  | Some s -> ignore (exec s : bool)
  | None ->
    if exec (patdiff_cmd ~use_color) then (
      Printf.eprintf "File \"%s\", line 1, characters 0-0:\n%!" file1;
      ignore (exec "diff -u" : bool);
    )
;;
