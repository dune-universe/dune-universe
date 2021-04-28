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

let ( <.> ) f g x = f (g x)

let unix =
  let open Unix_scheduler in
  { Dkim.Sigs.bind = (fun x f -> f (prj x)); return = inj }

let priv_of_seed seed =
  let g =
    let seed = Cstruct.of_string seed in
    Mirage_crypto_rng.(create ~seed (module Fortuna)) in
  Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()

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

let run _ src dst newline private_key seed selector hash canon domain_name =
  match (private_key, seed) with
  | None, None -> `Error (true, "A private key or a seed is required")
  | _ -> (
      let key =
        match (private_key, seed) with
        | Some (`RSA pk), _ -> pk
        | None, Some (`Seed seed) -> priv_of_seed seed
        | _ -> assert false
        (* see below *) in
      let flow = Flow.of_input src in
      let dkim = Dkim.v ~selector ?hash ?canonicalization:canon domain_name in
      let dkim =
        Unix_scheduler.prj
          (Dkim.sign ~key ~newline flow unix (module Flow) dkim) in
      match dst with
      | `Output ->
          let new_line =
            match newline with Dkim.LF -> "\n" | Dkim.CRLF -> "\r\n" in
          Fmt.pr "%s" (Prettym.to_string ~new_line Dkim.Encoder.as_field dkim) ;
          Fmt.pr "%s" (Buffer.contents flow.buffer) ;
          Flow.close flow ;
          `Ok ()
      | `Path path ->
          let oc = open_out (Fpath.to_string path) in
          let new_line =
            match newline with Dkim.LF -> "\n" | Dkim.CRLF -> "\r\n" in
          output_string oc
            (Prettym.to_string ~new_line Dkim.Encoder.as_field dkim) ;
          output_string oc (Buffer.contents flow.buffer) ;
          close_out oc ;
          Flow.close flow ;
          `Ok ())

let contents_of_path path =
  let ic = open_in (Fpath.to_string path) in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Bytes.unsafe_to_string rs

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

let output =
  let parser str =
    let open Rresult in
    Fpath.of_string str >>| fun v -> `Path v in
  let pp ppf = function
    | `Output -> Fmt.string ppf "-"
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

let private_key =
  let parser str =
    let open Rresult in
    match
      Base64.decode ~pad:true str
      >>| Cstruct.of_string
      >>= X509.Private_key.decode_der
    with
    | Ok _ as v -> v
    | Error _ ->
    match Fpath.of_string str with
    | Ok path when Sys.file_exists str && not (Sys.is_directory str) ->
        let contents = contents_of_path path in
        X509.Private_key.decode_pem (Cstruct.of_string contents)
    | Ok path -> R.error_msgf "%a does not exist" Fpath.pp path
    | Error _ as err -> err in
  let pp ppf pk =
    let contents = X509.Private_key.encode_pem pk in
    Fmt.pf ppf "%s%!" (Cstruct.to_string contents) in
  Arg.conv (parser, pp)

let domain_name =
  let parser = Domain_name.of_string in
  let pp = Domain_name.pp in
  Arg.conv (parser, pp)

let hash =
  let parser str =
    match Astring.String.trim (String.lowercase_ascii str) with
    | "sha1" -> Ok `SHA1
    | "sha256" -> Ok `SHA256
    | _ -> Rresult.R.error_msgf "Invalid hash: %S" str in
  let pp ppf = function
    | `SHA1 -> Fmt.string ppf "sha1"
    | `SHA256 -> Fmt.string ppf "sha256" in
  Arg.conv (parser, pp)

let canon =
  let parser str =
    let v = Astring.String.trim str in
    let v = String.lowercase_ascii v in
    match (Astring.String.cut ~sep:"/" v, v) with
    | Some ("simple", "simple"), _ | None, "simple" -> Ok (`Simple, `Simple)
    | Some ("simple", "relaxed"), _ -> Ok (`Simple, `Relaxed)
    | Some ("relaxed", "simple"), _ -> Ok (`Relaxed, `Simple)
    | Some ("relaxed", "relaxed"), _ | None, "relaxed" -> Ok (`Relaxed, `Relaxed)
    | _ -> Rresult.R.error_msgf "Invalid canonicalization specification: %S" str
  in
  let pp ppf = function
    | `Simple, `Simple -> Fmt.string ppf "simple"
    | `Relaxed, `Relaxed -> Fmt.string ppf "relaxed"
    | `Simple, `Relaxed -> Fmt.string ppf "simple/relaxed"
    | `Relaxed, `Simple -> Fmt.string ppf "relaxed/simple" in
  Arg.conv (parser, pp)

let seed =
  let parser str =
    match Base64.decode ~pad:true str with
    | Ok v -> Ok (`Seed v)
    | Error _ as err -> err in
  let pp ppf (`Seed v) = Fmt.string ppf (Base64.encode_exn ~pad:true v) in
  Arg.conv (parser, pp)

let src =
  let doc =
    "The email to sign, if it's omitted, we expect something into the standard \
     input." in
  Arg.(value & pos ~rev:true 0 input `Input & info [] ~docv:"<input>" ~doc)

let dst =
  let doc =
    "The output file where we will store the signed email. If it's omitted, we \
     write on the standard output." in
  Arg.(value & opt output `Output & info [ "o" ] ~doc)

let newline =
  let doc =
    "Depending on the transmission, an email can use the $(i,CRLF) end-of-line \
     (network transmission) or the LF end-of-line (UNIX transmission). By \
     default, we assume an UNIX transmission (LF character)." in
  Arg.(value & opt newline Dkim.LF & info [ "newline" ] ~doc)

let private_key =
  let doc = "The X.509 PEM encoded private key used to sign the email." in
  Arg.(value & opt (some private_key) None & info [ "p" ] ~doc)

let seed =
  let doc =
    "Seed to generate a private key. Instead to pass a private-key, the user \
     can give a seed used then by a Fortuna random number generator to \
     generate a RSA private-key. From the seed, the user is able to reproduce \
     the same RSA private-key (and the public-key). " in
  Arg.(value & opt (some seed) None & info [ "seed" ] ~doc)

let selector =
  let doc =
    "DKIM selector. A domain (see $(b,domain)) can store several public-key. \
     Each of them are identified by a $(i,selector) such as the public-key is \
     stored into $(i,selector)._domainkey.$(i,domain). It can refer to a date, \
     a location or an user." in
  Arg.(required & opt (some domain_name) None & info [ "s"; "selector" ] ~doc)

let hash =
  let doc =
    "Hash algorithm to digest header's fields and body. User can digest with \
     SHA1 or SHA256 algorithm." in
  Arg.(value & opt (some hash) None & info [ "hash" ] ~doc)

let canon =
  let doc =
    "Canonicalization algorithm used to digest header's fields and body. \
     Default value is $(i,relaxed/relaxed). A $(i,simple) canonicalization can \
     be\n\
    \             used. The format of the argument is: $(i,canon)/$(i,canon) \
     or $(i,canon) to\n\
    \             use the same canonicalization for both header's fields and \
     body." in
  Arg.(value & opt (some canon) None & info [ "c" ] ~doc)

let hostname =
  let doc =
    "The domain where the DNS TXT record is available (which contains the \
     public-key)." in
  Arg.(required & opt (some domain_name) None & info [ "h"; "hostname" ] ~doc)

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
  Logs.set_reporter (reporter Fmt.stderr)

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let sign =
  let doc =
    "Sign an email with a private-key and re-export the given email with a \
     DKIM-Signature." in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(b,sign) permits to sign with a private-key (RSA) an email and \
         re-export it with a proper DKIM-Signature. The $(i,hostname) \
         specified with its $(i,selector) must be reachable by a DNS client \
         and it must contain a TXT record with, at least, the public-key.";
      `P
        "The output can used as is to a command which is able to send an email \
         (such as $(b,sendmail)).";
    ] in
  ( Term.(
      ret
        (const run
        $ setup_logs
        $ src
        $ dst
        $ newline
        $ private_key
        $ seed
        $ selector
        $ hash
        $ canon
        $ hostname)),
    Term.info "sign" ~doc ~exits ~man )

let () = Term.(exit @@ eval sign)
