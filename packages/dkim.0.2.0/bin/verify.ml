module Unix_scheduler = Dkim.Sigs.Make (struct
  type +'a t = 'a
end)

module Caml_flow = struct
  type backend = Unix_scheduler.t

  type flow = { ic : in_channel; buf : Buffer.t }

  let input flow buf off len =
    let len = Stdlib.input flow.ic buf off len in
    Buffer.add_string flow.buf (Bytes.sub_string buf off len) ;
    Unix_scheduler.inj len
end

module Dns = struct
  include Dns_client_unix

  type backend = Unix_scheduler.t

  let getaddrinfo t `TXT domain_name =
    match getaddrinfo t Dns.Rr_map.Txt domain_name with
    | Ok (_ttl, txtset) ->
        Unix_scheduler.inj (Ok (Dns.Rr_map.Txt_set.elements txtset))
    | Error _ as err -> Unix_scheduler.inj err
end

let unix =
  let open Unix_scheduler in
  { Dkim.Sigs.bind = (fun x f -> f (prj x)); return = inj }

module Flow = struct
  type backend = Unix_scheduler.t

  type flow = { ic : in_channel; buffer : Buffer.t; close : bool }

  let of_input = function
    | `Input -> { ic = stdin; buffer = Buffer.create 0x1000; close = false }
    | `Path path ->
        let ic = open_in (Fpath.to_string path) in
        { ic; buffer = Buffer.create 0x1000; close = true }

  let close { ic; close; _ } = if close then close_in ic

  let input flow buf off len =
    let len = Stdlib.input flow.ic buf off len in
    Buffer.add_subbytes flow.buffer buf off len ;
    Unix_scheduler.inj len
end

let ( <.> ) f g x = f (g x)

let show_result ~valid:v_valid ~invalid:v_invalid =
  let valid dkim =
    Fmt.pr "[%a]: %a\n%!"
      Fmt.(styled `Green string)
      "ok" Domain_name.pp (Dkim.domain dkim) in
  let invalid dkim =
    Fmt.pr "[%a]: %a (%a)\n%!"
      Fmt.(styled `Red string)
      "er" Domain_name.pp (Dkim.domain dkim) Domain_name.pp (Dkim.selector dkim)
  in
  List.iter valid v_valid ;
  List.iter invalid v_invalid

let exit_success = 0

let exit_failure = 1

let run quiet src newline nameserver =
  let nameserver =
    match nameserver with
    | Some (inet_addr, port) ->
        Some (`TCP, (Ipaddr_unix.of_inet_addr inet_addr, port))
    | None -> None in
  let dns = Dns_client_unix.create ?nameserver () in
  let flow = Flow.of_input src in
  let open Rresult in
  Unix_scheduler.prj (Dkim.extract_dkim flow unix (module Flow))
  >>= fun extracted ->
  (R.ok <.> Unix_scheduler.prj)
    (Dkim.extract_body ~newline flow unix
       (module Flow)
       ~prelude:extracted.Dkim.prelude)
  >>= fun body ->
  let f (valid, invalid) (dkim_field_name, dkim_field_value, m) =
    let fiber =
      let ( >>= ) = unix.Dkim.Sigs.bind in
      let return = unix.Dkim.Sigs.return in
      let ( >>? ) x f =
        x >>= function Ok x -> f x | Error err -> return (Error err) in
      Dkim.post_process_dkim m |> return >>? fun dkim ->
      Dkim.extract_server dns unix (module Dns) dkim >>? fun n ->
      Dkim.post_process_server n |> return >>? fun server ->
      return (Ok (dkim, server)) in
    match Unix_scheduler.prj fiber with
    | Ok (dkim, server) ->
        let correct =
          Dkim.verify extracted.Dkim.fields
            (dkim_field_name, dkim_field_value)
            dkim server body in
        if correct then (dkim :: valid, invalid) else (valid, dkim :: invalid)
    | Error _ -> (valid, invalid) in
  let valid, invalid = List.fold_left f ([], []) extracted.Dkim.dkim_fields in
  if not quiet then show_result ~valid ~invalid ;
  if List.length invalid = 0 then Ok exit_success else Ok exit_failure

let run quiet src newline nameserver =
  match run quiet src newline nameserver with
  | Ok v -> v
  | Error (`Msg err) ->
      Fmt.epr "%s: @[@%a@]@." Sys.argv.(0) Fmt.string err ;
      exit_failure

open Cmdliner

let input =
  let parser = function
    | "-" -> Ok `Input
    | v ->
    match Fpath.of_string v with
    | Ok path when Sys.file_exists v && not (Sys.is_directory v) ->
        Ok (`Path path)
    | Ok path -> Rresult.R.error_msgf "%a does not exist" Fpath.pp path
    | Error _ as err -> err in
  let pp ppf = function
    | `Input -> Fmt.string ppf "-"
    | `Path path -> Fpath.pp ppf path in
  Arg.conv (parser, pp)

let newline =
  let parser str =
    match String.lowercase_ascii str with
    | "lf" -> Ok Dkim.LF
    | "crlf" -> Ok Dkim.CRLF
    | _ -> Rresult.R.error_msgf "Invalid newline specification: %S" str in
  let pp ppf = function
    | Dkim.LF -> Fmt.string ppf "lf"
    | Dkim.CRLF -> Fmt.string ppf "crlf" in
  Arg.conv (parser, pp)

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Arg.env_var "SIGN_LOGS" in
  Logs_cli.level ~env ~docs:common_options ()

let renderer =
  let env = Arg.env_var "SIGN_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

(* XXX(dinosaure): if [None], [-q] is used. *)

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let inet_addr =
  let parser str =
    try
      match String.split_on_char ':' str with
      | [ ns ] -> Ok (Unix.inet_addr_of_string ns, 53)
      | [ ns; port ] -> Ok (Unix.inet_addr_of_string ns, int_of_string port)
      | _ -> Rresult.R.error_msgf "Invalid nameserver IP: %S" str
    with _exn ->
      Rresult.R.error_msgf "Nameserver must be a valid IPv4: %S" str in
  let pp ppf (inet_addr, port) =
    Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port in
  Arg.conv (parser, pp)

let nameserver =
  let doc = "IP of nameserver to use." in
  Arg.(value & opt (some inet_addr) None & info [ "nameserver" ] ~doc)

let src =
  let doc =
    "The email to verify, if it's omitted, we expect something into the \
     standard input." in
  Arg.(value & pos ~rev:true 0 input `Input & info [] ~docv:"<input>" ~doc)

let newline =
  let doc =
    "Depending on the transmission, an email can use the $(i,CRLF) end-of-line \
     (network transmission) or the LF end-of-line (UNIX transmission). By \
     default, we assume an UNIX transmission (LF character)." in
  Arg.(value & opt newline Dkim.LF & info [ "newline" ] ~doc)

let verify =
  let doc = "Verify DKIM-Signature of the given email." in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(b,verify) does the DKIM verification process. It checks signatures\n\
        \          and does the DNS request to verify these signatures. Then, \
         it shows\n\
        \          which signature is valid which is not.";
    ] in
  ( Term.(const run $ setup_logs $ src $ newline $ nameserver),
    Term.info "sign" ~doc ~exits ~man )

let () = Term.(exit_status @@ eval verify)
