(* SPDX-License-Identifier: MIT *)

module Term = Cmdliner.Term
module Arg = Cmdliner.Arg
module Manpage = Cmdliner.Manpage

let ( $ ) = Cmdliner.Term.( $ )
let ( & ) = Cmdliner.Arg.( & )

let main regexp_arg main_regexp =
  match regexp_arg, main_regexp with
  | Some _, Some _ -> `Error (true, "Two regexps given. This is not supported yet") (* TODO *)
  | None, None -> `Error (true, "No regexp given. This is required")
  | Some regexp, None
  | None, Some regexp -> `Ok (fun () -> Grep.search ~regexp)

let regexp_arg =
  let doc = "" in (* TODO *)
  Arg.value &
  Arg.opt (Arg.some Arg.string) None &
  Arg.info ["regexp"] ~docv:"REGEXP" ~doc

let main_regexp =
  let doc = "" in (* TODO *)
  Arg.value &
  Arg.pos ~rev:true 0 (Arg.some Arg.string) None &
  Arg.info [] ~docv:"REGEXP" ~doc

let cmd =
  let doc = "greps anything in the sources of the latest version of every opam packages" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = [] in (* TODO *)
  Term.ret (Term.const main $ regexp_arg $ main_regexp),
  Term.info "opam-grep" ~version:Config.version ~doc ~sdocs ~exits ~man

let () =
  Term.exit @@ match Term.eval cmd with
  | `Ok f ->
      begin try f (); `Ok () with
      | Grep.OpamGrepError msg -> prerr_endline ("Error: "^msg); exit 1
      end
  | (`Error _ | `Version | `Help) as x -> x
