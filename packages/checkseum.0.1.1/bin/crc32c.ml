open Common

let digest input =
  let encoder, close_input =
    let default = Checkseum.Crc32c.default in
    match input with
    | `Std -> (X.encoder X.crc32c ~default (`Channel stdin), fun () -> ())
    | `File file ->
        let ic = open_in file in
        (X.encoder X.crc32c ~default (`Channel ic), fun () -> close_in ic)
  in
  let go () =
    match X.encode encoder with
    | `Await -> assert false (* XXX(dinosaure): iff [`Manual]. *)
    | `End value ->
        close_input () ;
        Fmt.pr "%a\n%!" Checkseum.Crc32c.pp value
  in
  go ()

open Cmdliner

type flow = [`File of string | `Std]

let input_flow =
  let parser s =
    if Sys.file_exists s then Ok (`File s)
    else Rresult.R.(error (msgf "%s not found" s))
  in
  let pp ppf = function
    | `File file -> Fmt.pf ppf "<%s>" file
    | `Std -> Fmt.string ppf "#stdin"
  in
  Arg.conv (parser, pp)

let input =
  let doc = "Input file." in
  Arg.(value & opt input_flow `Std & info ["i"; "input"] ~doc ~docv:"<file>")

let cmd =
  let doc = "CRC32C digest" in
  let exits = Term.default_exits in
  let man =
    [`S Manpage.s_description; `P "Tool to digest a input with CRC32C."]
  in
  ( Term.(const digest $ input)
  , Term.info "crc32c" ~version:"0.1" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
