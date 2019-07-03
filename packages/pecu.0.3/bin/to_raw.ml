let () = Printexc.record_backtrace true

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length

let map input output =
  let decoder, close_input =
    match input with
    | `Std -> (Pecu.decoder (`Channel stdin), fun () -> ())
    | `File file ->
        let ic = open_in file in
        (Pecu.decoder (`Channel ic), fun () -> close_in ic)
  in
  let output_line, output_string, close_output, flush_output =
    match output with
    | `Std ->
        ( (fun line -> output_string stdout line ; output_byte stdout 0x000A)
        , (fun data -> output_string stdout data)
        , (fun () -> ())
        , fun () -> flush stdout )
    | `File file ->
        let oc = open_out file in
        ( (fun line -> output_string oc line ; output_byte oc 0x000A)
        , (fun data -> output_string oc data)
        , (fun () -> close_out oc)
        , fun () -> flush oc )
  in
  let rec go () =
    match (Pecu.decode decoder, Pecu.decoder_dangerous decoder) with
    | `Line line, dangerous ->
        if dangerous then Fmt.epr "Input does not respect 80 columns rule.\n%!" ;
        output_line line ;
        go ()
    | `Data data, dangerous ->
        if dangerous then Fmt.epr "Input does not respect 80 columns rule.\n%!" ;
        output_string data ;
        go ()
    | `Malformed data, dangerous ->
        if dangerous then Fmt.epr "Input does not respect 80 columns rule.\n%!" ;
        Fmt.epr "%a\n%!" pp_string data
    | `End, dangerous ->
        if dangerous then Fmt.epr "Input does not respect 80 columns rule.\n%!" ;
        close_input () ;
        flush_output () ;
        close_output ()
    | `Await, _ -> assert false
  in
  go ()

open Cmdliner

type flow = [`Std | `File of string]

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

let output_flow =
  let parser s = Ok (`File s) in
  let pp ppf = function
    | `File file -> Fmt.pf ppf "<%s>" file
    | `Std -> Fmt.string ppf "#stdout"
  in
  Arg.conv (parser, pp)

let input =
  let doc = "Input file." in
  Arg.(value & opt input_flow `Std & info ["i"; "input"] ~doc ~docv:"<file>")

let output =
  let doc = "Output file." in
  Arg.(value & opt output_flow `Std & info ["o"; "output"] ~doc ~docv:"<file>")

let cmd =
  let doc = "Pecu mapper tool" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Tool to map an Quoted-Printable input to ASCII" ]
  in
  ( Term.(const map $ input $ output)
  , Term.info "to_raw" ~version:"0.1" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
