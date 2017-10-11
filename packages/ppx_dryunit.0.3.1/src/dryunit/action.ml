open Util

let init () =
    App.init ();
    `Ok ()

let help man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
      let topics = "topics" :: "patterns" :: "environment" :: cmds in
      let conv, _ =
        Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      ( match conv topic with
        | `Error e -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
        | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
        | `Ok t ->
            let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
            `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)
      )


type gen_opts =
  { nocache   : bool
  ; framework : string
  ; cache_dir : string option
  ; ignore    : string option
  ; filter    : string option
  ; targets   : string list
  }

let catch f () =
  try
    f ();
    `Ok ()
  with
   Failure e -> `Error (false, e)

let gen { nocache; framework; cache_dir; ignore; filter; targets} =
  let cache_dir = unwrap_or "_build/.dryunit" cache_dir in
  let ignore = unwrap_or "" ignore in
  let filter = unwrap_or "" filter in
  catch  (fun () -> App.gen ~nocache ~framework ~cache_dir ~ignore ~filter ~targets) ()
