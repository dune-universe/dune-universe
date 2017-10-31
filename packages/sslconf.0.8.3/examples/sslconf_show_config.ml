(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

open Cmdliner
open Rresult

(* dump all testcases to config files in an empty directory *)

let cmd path =
  let conf = Sslconf.create () in
  match Sslconf.conf_load_file conf path with
  | Ok () ->
    `Ok (Sslconf.sexp_of_conf conf |>
         Sexplib.Sexp.to_string_mach |>
         print_endline)
  | Error err -> `Error (false, Sslconf.string_of_error err)

let path =
  let doc = "Path to config file." in
  let docv = "path" in
  Arg.(required & pos ~rev:true 0 (some non_dir_file) None & info [] ~docv ~doc)

let cmd =
  let doc = "Show Sslconf config as sexp:\n" ^
   "((<section stacks>)(<alist>)) =\n" ^
   "(((section <alist>) ...)((<name> value) ...)) =\n" ^
   "(((section ((name value) ...))...)(((section name) value)...)" in
  let man_xrefs =
    [ `Tool "sslconf_show_section"; `Tool "sslconf_show_value" ] in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_bugs;
      `P "Email them to <tony.wuersch at gmail.com>." ]
  in
  Term.(ret (const cmd $ path)),
  Term.info
    "sslconf_show_config"
    ~version:"0.8.3"
    ~doc
    ~exits
    ~man
    ~man_xrefs

let () = Term.(exit ~term_err:1 @@ eval cmd)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
