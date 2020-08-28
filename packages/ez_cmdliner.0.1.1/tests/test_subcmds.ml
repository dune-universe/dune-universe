open Ezcmd.TYPES

(* Options common to all commands *)

let debug = ref false

let prehook = ref None

let copts_t =
  let docs = Ezcmd.MANPAGE.s_common_options in
  [
    ( [ "debug" ],
      Arg.Bool (fun bool -> debug := bool),
      Ezcmd.info ~docs "Give only debug output." );
    (*
    let doc = "Suppress informational output." in
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
     *)
    ( [ "prehook" ],
      Arg.String (fun s -> prehook := Some s),
      Ezcmd.info ~docs "Specify command to run before this $(mname) command." );
  ]

(* Commands *)

let help_secs =
  [
    `S Ezcmd.MANPAGE.s_common_options;
    `P "These options are common to all commands.";
    `S "MORE HELP";
    `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
    `Noblank;
    `P "Use `$(mname) help patterns' for help on patch matching.";
    `Noblank;
    `P "Use `$(mname) help environment' for help on environment variables.";
    `S Ezcmd.MANPAGE.s_bugs;
    `P "Check bug reports at http://bugs.example.org.";
  ]

let initialize_cmd =
  let repodir = ref None in
  let cmd_action () =
    match !repodir with None -> () | Some repodir -> print_endline repodir
  in
  let cmd_args =
    [
      ( [ "repodir" ],
        Arg.File (fun file -> repodir := Some file),
        Ezcmd.info ~docv:"DIR"
          "Run the program in repository directory $(docv)." );
    ]
    @ copts_t
  in
  let cmd_man =
    [
      `S Ezcmd.MANPAGE.s_description;
      `P
        "Turns the current directory into a Darcs repository. Anyexisting \
         files and subdirectories become ...";
      `Blocks help_secs;
    ]
  in
  {
    cmd_name = "init";
    cmd_args;
    cmd_action;
    cmd_doc = "make the current directory a repository";
    cmd_man;
  }

let record_cmd =
  let cmd_args =
    [
      ( [ "m"; "patch-name" ],
        Arg.String (fun _s -> assert false),
        Ezcmd.info ~docv:"NAME" "Name of the patch." );
      ( [ "A"; "author" ],
        String (fun _s -> assert false),
        Ezcmd.info ~docv:"EMAIL" "Specifies the author's identity." );
      ( [ "a"; "all" ],
        Bool (fun _bool -> assert false),
        Ezcmd.info "Answer yes to all patches." );
      ( [ "ask-deps" ],
        Bool (fun _bool -> assert false),
        Ezcmd.info "Ask for extra dependencies." );
      ( [],
        Anons (fun _files -> assert false),
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
      `Blocks help_secs;
    ]
  in
  let cmd_action () = print_endline "record" in
  { cmd_name = "record"; cmd_args; cmd_action; cmd_man; cmd_doc }

let () =
  let name = "cop" in
  let version = "1.2.3" in
  let doc = "a generic build system" in
  let man = [ `P "Hello" ] @ help_secs in
  Ezcmd.main_with_subcommands ~name ~version ~doc ~man
    [ initialize_cmd; record_cmd ]
