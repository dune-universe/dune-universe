(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

open Cmdliner
open Rresult

(* dump all testcases to config files in an empty directory *)

let cmd path section name =
  match path with
  | Some path -> (
      let conf = Sslconf.create () in
      match Sslconf.conf_load_file conf path with
      | Ok () -> (
          match Sslconf.conf_get_value ~conf:conf ?section:section name with
          | Some s -> `Ok (print_endline s)
          | None -> `Error (false, "no value found")
        )
      | Error err -> `Error (false, Sslconf.string_of_error err)
    )
  | None -> (
      match section with
      | Some _ ->  `Error (false, "section requires a config")
      | None -> (
          match Sslconf.conf_get_value name with
          | Some s -> `Ok (print_endline s)
          | None -> `Error (false, "no value found")
        )
    )

let path =
  let doc = "Path to config file (optional)." in
  let docv = "filename" in
  Arg.(value & opt (some non_dir_file) None & info ["f"; "cnf"] ~docv ~doc)

let section =
  let doc = "Section in config file (optional)." in
  let docv = "section" in
  Arg.(value & opt (some string) None & info ["s"; "sect"; "section"] ~docv ~doc)

let key =
  let doc = "name key." in
  let docv = "name" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv ~doc)

let cmd =
  let doc = "Show value of name in Sslconf config file" in
  let man_xrefs =
    [ `Tool "sslconf_show_config"; `Tool "sslconf_show_section" ] in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_bugs;
      `P "Email them to <tony.wuersch at gmail.com>." ]
  in
  Term.(ret (const cmd $ path $ section $ key)),
  Term.info
    "sslconf_show_value"
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
