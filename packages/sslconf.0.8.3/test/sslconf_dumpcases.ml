(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

(* dump all testcases to config files in an empty directory *)

open Cmdliner
open Testcase

let cmd dest =
  match (
    if not (Sys.file_exists dest)
    then
      let cmd =
        let mkdir = if Sys.os_type = "Win32" then "mkdir" else "mkdir -p" in
        mkdir ^ " " ^ dest in
      if Sys.command cmd != 0
      then
        `Error (false, "failed to create " ^ dest)
      else
        `Ok ()
    else if not (Sys.is_directory dest) || Array.length (Sys.readdir dest) != 0
    then
      `Error (false, dest ^ " is not an empty directory")
    else
      `Ok ()
  ) with `Error (b, s) -> `Error (b, s) | `Ok () ->
  let open Fpath in
  let path = v dest in
  let write_case n case =
    let name = "test" ^ (string_of_int n) in
    let input, desc = match case with
    | Parse r | ParseLoad r -> r.input, r.desc
    | _ -> None, None in
    match input with
    | Some input -> (
        let path = path / name in
        let add_cnf = add_ext "cnf" in
        let oc = path |> add_cnf |> normalize |> to_string |> open_out in
        (
          match desc with
          | Some desc ->
              let desc = "# " ^ name ^ "\n# " ^ desc ^ "\n" in
              output_string oc desc
          | None -> ()
        );
        output_string oc input;
        close_out oc
      )
    | None -> ()
  in
    List.iteri write_case cases;
    `Ok ()

let dest =
  let doc = "Directory to write test cases to." in
  let docv = "DEST" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv ~doc)

let cmd =
  let doc = "Write test cases to directory" in
  let man_xrefs = [] in
  (* [ `Tool "mv"; `Tool "scp"; `Page ("umask", 2); `Page ("symlink", 7) ] *)
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_bugs;
      `P "Email them to <tony.wuersch at gmail.com>."; ]
  in
  Term.(ret (const cmd $ dest)),
  Term.info "dumpcases" ~version:"v0.8.0" ~doc ~exits ~man ~man_xrefs

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
