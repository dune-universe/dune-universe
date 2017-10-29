open Core_normalization
open Core_runtime
open Core_util

let init_ext () =
    Core_serializer.init_extension ();
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

(*
  This options are used generate the code to:
    - activate ppx_dryunit
    - generate the final source code with bootstrap code
*)
type gen_opts =
  { nocache    : bool
  ; framework  : string option
  ; cache_dir  : string option
  ; only       : string option
  ; ignore     : string option
  ; ignore_path: string option
  ; targets    : string list
  }

type init_opts =
  { framework: string }

let create_init_options framework : init_opts =
  { framework }

let catch f () =
  try
    f ();
    `Ok ()
  with
   Failure e -> `Error (false, e)

(* move this to App.gen_extension *)
let gen_extension { nocache; framework; cache_dir; ignore; only; ignore_path; targets} =
  let cache_dir = unwrap_or "_build/.dryunit" cache_dir in
  let ignore = unwrap_or "" ignore in
  let only = unwrap_or "" only in
  let ignore_path = unwrap_or "" ignore_path in
  let framework = unwrap_or "alcotest" framework in
  catch
    ( fun () ->
      App.gen_extension ~nocache ~framework ~cache_dir ~ignore ~only ~ignore_path ~targets
    ) ()

let gen_executable { nocache; framework; cache_dir; ignore; only; ignore_path; targets} =
  let cache_dir = unwrap_or "_build/.dryunit" cache_dir in
  let ignore = unwrap_or "" ignore in
  let only = unwrap_or "" only in
  let ignore_path = unwrap_or "" ignore_path in
  let framework = TestFramework.of_string (unwrap_or "alcotest" framework) in
  let targets = if List.length targets == 0 then [ "main.ml" ] else targets in
  List.iter
    ( fun target ->
        let suites = App.get_suites ~nocache ~framework ~cache_dir ~ignore ~only ~targets
          ~ignore_path ~detection:"dir" ~main:target in
        App.gen_executable framework suites target
    )
    targets;
  `Ok ()

let init_executable { framework; } =
  Core_serializer.init_default (TestFramework.of_string framework);
  `Ok ()

let init_framework f =
  Core_serializer.init_default f;
  `Ok ()
