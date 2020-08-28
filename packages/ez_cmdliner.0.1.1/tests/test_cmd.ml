open Ezcmd.TYPES

(* Options common to all commands *)

let () =
  let cmd_args =
    [
      ( [ "m"; "patch-name" ],
        Arg.String (fun s -> Printf.printf "patch-name: %s\n" s),
        Ezcmd.info ~docv:"NAME" "Name of the patch." );
      ( [ "A"; "author" ],
        Arg.String (fun s -> Printf.printf "author: %s\n" s),
        Ezcmd.info ~docv:"EMAIL" "Specifies the author's identity." );
      ( [ "a"; "all" ],
        Arg.Bool (fun bool -> Printf.printf "all: %b\n" bool),
        Ezcmd.info "Answer yes to all patches." );
      ( [ "ask-deps" ],
        Arg.Bool (fun bool -> Printf.printf "ask-deps: %b\n" bool),
        Ezcmd.info "Ask for extra dependencies." );
      ( [],
        Anons
          (fun files ->
            Printf.printf "Number of files: %d\n" (List.length files)),
        Ezcmd.info ~docv:"FILE or DIR" "Print info on $(docv)" );
    ]
  in
  let cmd_doc = "create a patch from unrecorded changes" in
  let cmd_man =
    [
      `S Ezcmd.MANPAGE.s_description;
      `P
        "Creates a patch from changes in the working tree. If you specifya set \
         of files ...";
    ]
  in
  let cmd_action () = print_endline "record" in
  let cmd = { cmd_name = "record"; cmd_args; cmd_action; cmd_man; cmd_doc } in
  Ezcmd.main ~version:"1.2.3" cmd
