open Common

let digest with_value input =
  let default = match with_value with
    | Some value -> Optint.of_int value
    | None -> Checkseum.Adler32.default in
  let encoder, close_input =
    match input with
    | `Std -> (X.encoder X.adler32 ~default (`Channel stdin), fun () -> ())
    | `File file ->
        let ic = open_in file in
        (X.encoder X.adler32 ~default (`Channel ic), fun () -> close_in ic)
  in
  let go () =
    match X.encode encoder with
    | `Await -> assert false (* XXX(dinosaure): iff [`Manual]. *)
    | `End value ->
        close_input () ;
        Fmt.pr "%a\n%!" Checkseum.Adler32.pp value
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

let with_value =
  let doc = "With specific value." in
  Arg.(value & opt (some int) None & info ["w"; "with"] ~doc)

let cmd =
  let doc = "ADLER32 digest" in
  let exits = Term.default_exits in
  let man =
    [`S Manpage.s_description; `P "Tool to digest a input with ADLER32."]
  in
  ( Term.(const digest $ with_value $ input)
  , Term.info "adler32" ~version:"0.1" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
