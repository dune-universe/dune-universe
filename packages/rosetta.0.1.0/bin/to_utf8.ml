let () = Printexc.record_backtrace true

let map kind input output =
  let decoder, close_input = match input with
    | `Std ->
      Rosetta.decoder kind (`Channel stdin),
      (fun () -> ())
    | `File file ->
      let ic = open_in file in
      Rosetta.decoder kind (`Channel ic),
      (fun () -> close_in ic) in
  let encoder, close_output, flush_output = match output with
    | `Std ->
      Uutf.encoder `UTF_8 (`Channel stdout),
      (fun () -> ()),
      (fun () -> flush stdout)
    | `File file ->
      let oc = open_out file in
      Uutf.encoder `UTF_8 (`Channel oc),
      (fun () -> close_out oc),
      (fun () -> flush oc) in
  let rec go () = match Rosetta.decode decoder with
    | `Await -> assert false (* XXXX(dinosaure): iff [`Manual]. *)
    | `End ->
      let[@warning "-8"] `Ok : [ `Ok | `Partial ] = Uutf.encode encoder `End in

        flush_output ()
      ; close_input ()
      ; close_output ()
      ; `Ok ()
    | `Malformed err -> `Error (false, err)
    | `Uchar u ->
      match Uutf.encode encoder (`Uchar u) with
      | `Ok -> go ()
      | `Partial -> assert false in (* XXX(dinosaure): iff [`Manual]. *)
  go ()

open Cmdliner

let kind =
  let parser s =
    try Ok (Rosetta.encoding_of_string s)
    with Invalid_argument err -> Error (`Msg err) in
  let pp = Fmt.using Rosetta.encoding_to_string Fmt.string in
  Arg.conv (parser, pp)

let kind =
  let doc = "Kind of encoding." in
  Arg.(required & opt (some kind) None & info ["k"; "kind"] ~doc ~docv:"<kind>")

type flow = [ `File of string | `Std ]

let input_flow =
  let parser s =
    if Sys.file_exists s
    then Ok (`File s) else Rresult.R.(error (msgf "%s not found" s)) in
  let pp ppf = function
    | `File file -> Fmt.pf ppf "<%s>" file
    | `Std -> Fmt.string ppf "#stdin" in
  Arg.conv (parser, pp)

let output_flow =
  let parser s = Ok (`File s) in
  let pp ppf = function
    | `File file -> Fmt.pf ppf "<%s>" file
    | `Std -> Fmt.string ppf "#stdout" in
  Arg.conv (parser, pp)

let input =
  let doc = "Input file." in
  Arg.(value & opt input_flow `Std & info ["i"; "input"] ~doc ~docv:"<file>")

let output =
  let doc = "Output file." in
  Arg.(value & opt output_flow `Std & info ["o"; "output"] ~doc ~docv:"<file>")

let cmd =
  let doc = "Rosetta mapper tool" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Tool to map a input to UTF-8." ] in
  Term.(const map $ kind $ input $ output),
  Term.info "to_utf8" ~version:"0.1" ~doc ~exits ~man

let () = Term.(exit @@ eval cmd)
