open! Import

type t =
  | Create of {
      source : Source.t;
      switch_name : Switch_name.t option;
      configure_command : Bos.Cmd.t option;
    }

let eval runner github_client = function
  | Create { source; switch_name; configure_command } ->
      Op.create runner github_client source switch_name ~configure_command

let configure_command =
  let open Cmdliner.Arg in
  let conv = conv (Bos.Cmd.of_string, Bos.Cmd.pp) in
  value
    (opt (some conv) None
       (info ~doc:"Use this instead of \"./configure\"." ~docv:"COMMAND"
          [ "configure-command" ]))

module Create = struct
  module Source_with_original = struct
    type t = { source : Source.t; original : string }

    let of_string s =
      match Source.parse s with
      | Ok source -> Ok { source; original = s }
      | Error `Unknown -> Rresult.R.error_msgf "Invalid source: %S" s

    let pp ppf { original; _ } = Format.pp_print_string ppf original

    let conv = Cmdliner.Arg.conv (of_string, pp)
  end

  let source =
    let open Cmdliner.Arg in
    required
      (pos 0
         (some Source_with_original.conv)
         None
         (info ~doc:"Where to fetch the compiler." ~docv:"SOURCE" []))

  let switch_name =
    let open Cmdliner.Arg in
    let conv = conv (Switch_name.parse, Switch_name.pp) in
    value
      (opt (some conv) None
         (info ~docv:"SWITCH_NAME"
            ~doc:
              "Use this name for the switch. If omitted, a name is inferred \
               from the source."
            [ "switch" ]))

  let man =
    [
      `S Cmdliner.Manpage.s_description;
      `P "There are several ways to specify where to find a compiler:";
      `I ("Github branch", "user/repo:branch");
      `I
        ( "Github branch (short form)",
          "user:branch (repo defaults to \"ocaml\")" );
      `I ("Github pull request", "user/repo#number");
      `I
        ( "Github pull request (short form)",
          "#number (repo defaults to \"ocaml/ocaml\")" );
    ]

  let term =
    let open Let_syntax.Cmdliner in
    let+ { Source_with_original.source; _ } = source
    and+ switch_name = switch_name
    and+ configure_command = configure_command in
    Create { source; switch_name; configure_command }

  let info =
    Cmdliner.Term.info ~man ~doc:"Create a switch from a compiler source"
      "create"

  let command = (term, info)
end

let default =
  let open Cmdliner.Term in
  (ret (pure (`Help (`Auto, None))), info "opam-compiler")

let main () =
  let result = Cmdliner.Term.eval_choice default [ Create.command ] in
  ( match result with
  | `Ok op ->
      eval Runner.real Github_client.real op |> Rresult.R.failwith_error_msg
  | `Version -> ()
  | `Help -> ()
  | `Error _ -> () );
  Cmdliner.Term.exit result
