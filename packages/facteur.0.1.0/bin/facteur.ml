module X509_handler = X509_handler
module Sendmail_handler = Sendmail_handler

let ( <.> ) f g = fun x -> f (g x)

module Result = struct
  let fold l =
    let open Rresult.R in
    List.fold_left (fun a x -> match a, x with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok a, Ok x -> Ok (x :: a))
      (Ok []) l >>| List.rev
end

module Option = struct
  let some x = Some x
end

let domain_of_reverse_path = function
  | None -> Rresult.R.error_msgf "reverse-path is empty"
  | Some { Colombe.Path.domain= domain; _ } -> Ok domain

let to_domain_name = function
  | Colombe.Domain.Domain l -> Domain_name.of_strings l
  | domain -> Rresult.R.error_msgf "Invalid domain: %a" Colombe.Domain.pp domain

let same_domain ~hostname domain =
  if Domain_name.is_subdomain ~subdomain:hostname ~domain
  then Ok domain
  else Rresult.R.error_msgf
      "Domain of sender does not correspond to domain of submission server (sender: %a, submission: %a)"
      Domain_name.pp domain Domain_name.pp hostname

let stream_of_ic ?(close_in= close_in) ic =
  let buf = Bytes.create 4096 in
  let closed = ref false in
  let go () =
    if !closed then None
    else match input ic buf 0 4096 with
      | 0 ->
        if not !closed then ( close_in ic ; closed := true )
        ; None
      | n -> Some (Bytes.unsafe_to_string buf, 0, n)
      | exception End_of_file ->
        if not !closed then ( close_in ic ; closed := true )
        ; None in
  go

let attachment_to_part ~magic (e, path) =
  let mime_type = Magic.info magic (Fpath.to_string path) in
  let parser =
    let open Angstrom in
    let open Mrmime.Rfc2045 in
    ty <* char '/' >>= subty in
  match Angstrom.parse_string parser mime_type with
  | Error _ -> Rresult.R.error_msgf "Unrecognize file %a" Fpath.pp path
  | Ok (ty_v, subty_v) ->
    let content =
      let open Mrmime.Content in
      let c =
        let open Mrmime.Content_type in
        make ty_v subty_v Parameters.empty in
      make ~encoding:e c in
    let content_disposition = Rfc2183.content_disposition (Fpath.basename path) in
    let content = Mrmime.Content.add content_disposition content in
    let ic = open_in (Fpath.to_string path) in
    Ok (Mrmime.Mt.part ~content (stream_of_ic ~close_in:(fun _ -> ()) ic))

let mail ~date ~zone ~from ~sender ~subject ~recipients ~encoding:e ~parameters:p ~body ~attachments =
  let open Mrmime in
  let date = match date with
    | Some (ptime, None) -> Date.of_ptime ~zone ptime
    | Some (ptime, Some tz) ->
      let hh, mm = tz / 3600, abs (tz mod 3600 / 60) in
      Date.of_ptime ~zone:(Rresult.R.get_ok Date.Zone.(tz hh mm)) ptime
    | None -> Date.of_ptime ~zone (Ptime_clock.now ()) in
  let header =
    let open Header in
    Field.(Date $ date)
    & Field.(Subject $ subject)
    & Field.(From $ [ from ])
    & Field.(Sender $ sender)
    & Field.(To $ List.map Address.mailbox recipients)
    & empty in
  let stream = match body with
    | `Stdin -> stream_of_ic stdin
    | `File filename -> stream_of_ic (open_in (Fpath.to_string filename)) in
  let content =
    let open Content in
    let c =
      let open Content_type in
      make `Text (Subtype.iana_exn `Text "plain") p in
    make ~encoding:e c in
  let magic = Magic.make [ Magic.MIME_TYPE; Magic.ERROR ] in
  let attachments = List.map (attachment_to_part ~magic) attachments in
  let attachments = List.fold_left
      (fun a x -> match a, x with
         | (Error _ as err), _ -> err
         | _, (Error _ as err) -> err
         | Ok a, Ok x -> Ok (x :: a))
      (Ok []) attachments in
  let part0 = Mt.part ~content stream in

  let open Rresult.R in
  attachments >>| function
  | [] -> Mt.make header Mt.simple part0
  | attachments ->
    let multipart = Mt.multipart ~rng:Mt.rng (part0 :: List.rev attachments) in
    (* XXX(dinosaure): default [content] is mixed. *)
    Mt.make header Mt.multi multipart

let stream_map f s =
  fun () -> match s () with
  | Some v -> Some (f v)
  | None -> None

let ask_password () =
  print_string "password: " ;
  flush stdout ;

  match
    let default = Unix.tcgetattr Unix.stdin in
    let silent = { default with c_echo= false
                              ; c_echoe= false
                              ; c_echok= false
                              ; c_echonl= false } in
    Some (default, silent) with
  | None ->
    input_line stdin
  | Some (default, silent) ->
    Unix.tcsetattr Unix.stdin Unix.TCSANOW silent ;
    ( try let s = input_line stdin in
        Unix.tcsetattr Unix.stdin Unix.TCSANOW default ; s
      with x -> Unix.tcsetattr Unix.stdin Unix.TCSANOW default ; raise x )
  | exception _ -> input_line stdin

let run
    level
    style_renderer
    logger
    username password
    date zone sender subject recipients
    hostname port
    with_starttls
    encoding parameters body
    attachments
    ca_file ca_path =
  let () = Fmt_tty.setup_std_outputs ?style_renderer () in
  let () = Logs.set_level level in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let logger = match logger with
    | Some name -> Some (Logs.src_log (Logs.Src.create name))
    | None -> None in
  let auth = match username, password with
    | None, None -> Ok None
    | Some username, Some password -> Ok (Some (Auth.make ~username password))
    | Some username, None -> let password = ask_password () in print_newline () ; Ok (Some (Auth.make ~username password))
    | None, Some _ -> Rresult.R.error_msgf "A password must be associated with an username" in
  let authenticator = match ca_file, ca_path with
    | Some ca_file, None -> Ok (`Ca_file ca_file)
    | None, Some ca_directory -> Ok (`Ca_directory ca_directory)
    | None, None -> Ok `No_authentication
    | Some _, Some _ -> Rresult.R.error_msgf "Impossible to load both CA file and CA directory." in
  let port = match port, with_starttls with
    | None, true -> 587 (* XXX(dinosaure): Submission port. *)
    | None, false -> 465
    | Some v, _ -> v in
  let mail =
    let open Rresult.R in
    Emile_mrmime.to_mrmime sender >>= fun sender ->
    List.map Emile_mrmime.to_mrmime recipients |> Result.fold >>= fun recipients ->
    mail
      ~date ~zone ~from:sender ~sender ~subject ~recipients
      ~encoding ~parameters ~body
      ~attachments in
  let process =
    let open Rresult.R in
    Emile_mrmime.to_mrmime sender >>= Colombe_mrmime.to_reverse_path >>= fun from ->
    domain_of_reverse_path from >>= fun domain ->
    to_domain_name domain >>= same_domain ~hostname >>= fun _ ->
    authenticator >>= fun tls ->
    auth >>= fun authenticator ->
    List.map (fun x -> Emile_mrmime.to_mrmime x >>= Colombe_mrmime.to_forward_path) recipients |> Result.fold >>= fun recipients ->
    mail >>= fun mail ->
    let stream = Mrmime.Mt.to_stream mail in
    let fiber =
      let open Lwt.Infix in
      X509_handler.authenticator tls >>= fun tls ->
      if with_starttls
      then Sendmail_handler.run_with_starttls ?logger ~hostname ~port ~domain ~authenticator ~tls ~from ~recipients stream
      else Sendmail_handler.run ?logger ~hostname ~port ~domain ~authenticator ~tls ~from ~recipients stream in
    Ok fiber in
  match process with
  | Error (`Msg err) -> `Error (false, err)
  | Ok fiber -> Lwt_main.run fiber

let default_content_type_parameters =
  let open Mrmime.Content_type.Parameters in
  of_list [ k "charset", v "utf-8" ]

let default_zone =
  Zone.init () ;
  let tz = Zone.get () in
  let hh, mm = tz / 3600, abs (tz mod 3600 / 60) in
  Rresult.R.get_ok (Mrmime.Date.Zone.tz hh mm)

open Cmdliner

let zone =
  let parser = Mrmime.Date.Zone.of_string in
  let pp = Mrmime.Date.Zone.pp in
  Arg.conv ~docv:"<zone>" (parser, pp)

let domain_name =
  let parser = Domain_name.(host <.> of_string_exn) in
  let pp = Domain_name.pp in
  Arg.conv ~docv:"<domain-name>" (parser, pp)

let domain =
  let parser = Colombe.Domain.of_string in
  let pp = Colombe.Domain.pp in
  Arg.conv ~docv:"<domain>" (parser, pp)

let mailbox = Emile_cmdliner.mailbox

let date =
  let parser x = match Ptime.(rfc3339_error_to_msg <.> of_rfc3339 ~strict:true ~sub:false ~start:0) x with
    | Ok (ptime, tz, _) -> Ok (ptime, tz)
    | Error _ as err -> err in
  let pp ppf (t, tz_offset_s) = Ptime.pp_rfc3339 ~space:true ?tz_offset_s () ppf t in
  Arg.conv ~docv:"<date>" (parser, pp)

let parameters =
  let parser x =
    let l = Astring.String.cuts ~empty:true ~sep:"," x in
    let l = List.map (fun x -> match Astring.String.cut ~sep:"=" x with
        | Some (k, v) -> Ok (k, v)
        | None -> Rresult.R.error_msgf "Key %S is alone" x) l in
    let l =
      let open Mrmime.Content_type in
      List.map (function
          | Ok (k, v) -> let open Rresult.R in
            Parameters.key k >>= fun k ->
            Parameters.value v >>| fun v -> (k, v)
          | Error _ as err -> err) l in
    let open Rresult.R in
    List.fold_left
      (fun a p -> match a, p with
         | (Error _ as err), _ -> err
         | _, (Error _ as err) -> err
         | Ok a, Ok p -> Ok (p :: a))
      (Ok []) l >>| Mrmime.Content_type.Parameters.of_list in
  let pp = Mrmime.Content_type.Parameters.pp in
  Arg.conv ~docv:"<parameters>" (parser, pp)

let mechanism =
  let parser x = match String.lowercase_ascii x with
    | "plain" -> Ok Auth.plain
    | _ -> Rresult.R.error_msgf "Invalid authentication mechanism: %s" x in
  let pp = Auth.pp_mechanism in
  Arg.conv ~docv:"<SASL mechanism>" (parser, pp)

let existing_file =
  let parser x = match Fpath.of_string x with
    | Ok v when Sys.file_exists x -> Ok v
    | Ok v -> Rresult.R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<filename>" (parser, pp)

let path =
  let parser = Fpath.of_string in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<path>" (parser, pp)

let existing_file_or_stdin =
  let parser = function
    | "-" -> Ok `Stdin
    | x ->
      let open Rresult.R in
      Arg.conv_parser existing_file x >>| fun v -> `File v in
  let pp ppf = function
    | `Stdin -> Fmt.string ppf "-"
    | `File v -> Fpath.pp ppf v in
  Arg.conv (parser, pp)

let directory =
  let parser x = match Fpath.of_string x with
    | Ok v when Sys.is_directory x -> Ok v
    | Ok v -> Rresult.R.error_msgf "%a is not a directory or does not exist" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<directory>" (parser, pp)

let unstructured =
  let parser x = match Mrmime.Unstructured.of_string x with
    | v -> Ok v
    | exception (Failure err) -> Rresult.R.error_msg err in
  let pp = Mrmime.Unstructured.pp in
  Arg.conv ~docv:"<unstructured>" (parser, pp)

let encoding =
  let parser = Mrmime.Content_encoding.of_string in
  let pp = Mrmime.Content_encoding.pp in
  Arg.conv ~docv:"<encoding>" (parser, pp)

let utf8_string =
  let parser x =
    Uutf.String.fold_utf_8 (fun r _ c -> match r, c with
        | Error _ as err, _ -> err
        | Ok _, `Malformed err -> Error (`Msg err)
        | Ok _ as v, `Uchar _ -> v) (Ok x) x in
  let pp ppf x =
    if Fmt.utf_8 Fmt.stdout
    then Fmt.string ppf x
    else Fmt.pf ppf "%S" x in
  Arg.conv ~docv:"<utf8-string>" (parser, pp)

let existing_file_with_encoding =
  let parser x =
    match Astring.String.cut ~sep:":" x with
    | None when Sys.file_exists x ->
      let open Rresult.R in
      Fpath.of_string x >>| fun v -> (`Base64, v)
    | None -> Rresult.R.error_msgf "%s does not exists" x
    | Some (encoding, v) -> match String.lowercase_ascii encoding, Fpath.of_string v with
      | "quoted-printable", Ok v ->
        if Sys.file_exists (Fpath.to_string v)
        then Ok (`Quoted_printable, v)
        else Rresult.R.error_msgf "%a does not exist" Fpath.pp v
      | "base64", Ok v ->
        if Sys.file_exists (Fpath.to_string v)
        then Ok (`Base64, v)
        else Rresult.R.error_msgf "%a does not exist" Fpath.pp v
      | encoding, _ -> Rresult.R.error_msgf "Invalid encoding mechanism: %S" encoding in
  let pp ppf (encoding, v) = Fmt.pf ppf "%a:%a" Mrmime.Content_encoding.pp encoding Fpath.pp v in
  Arg.conv ~docv:"<encoding:filename>" (parser, pp)

(* ----- *)

let logs = Logs_cli.level ~env:(Arg.env_var "FACTEUR_LOGGER") ()
let fmt = Fmt_cli.style_renderer ~env:(Arg.env_var "FACTEUR_FMT") ()

(* ----- *)

let date =
  let doc = "Date of the mail (rfc3339 format)" in
  Arg.(value & opt (some date) None & info [ "d"; "date" ] ~doc)

let sender =
  let doc = "Address of sender (rfc822 format)" in
  Arg.(required & opt (some mailbox) None & info [ "s"; "sender" ] ~doc)

let username =
  let doc = "Username" in
  Arg.(value & opt (some utf8_string) None & info [ "u"; "username" ] ~doc)

let password =
  let doc = "Password" in
  Arg.(value & opt (some utf8_string) None & info [ "p"; "password" ] ~doc)

let subject =
  let doc = "Subject of the mail" in
  Arg.(required & opt (some unstructured) None & info [ "subject" ] ~doc)

let recipients =
  let doc = "Recipients of the mail (rfc822 format)" in
  Arg.(value & pos_left ~rev:true 0 mailbox [] & info [] ~docv:"<recipients>" ~doc)

let hostname =
  let doc = "Hostname of SMTP server" in
  Arg.(required & opt (some domain_name) None & info [ "h"; "hostname" ] ~doc)

let port =
  let doc = "Port of SMTP server" in
  Arg.(value & opt (some int) None & info [ "port" ] ~doc)

let with_starttls =
  let doc = "Use STARTTLS extension" in
  Arg.(value & flag & info [ "with-starttls" ] ~doc)

let zone =
  let doc = "Time-zone used to send mail" in
  Arg.(value & opt zone default_zone & info [ "z"; "zone" ] ~doc)

let encoding =
  let doc = "Encoding of main body" in
  Arg.(value & opt encoding `Quoted_printable & info [ "e"; "encoding" ] ~doc)

let parameters =
  let doc = "Parameters of the main Content-Type" in
  Arg.(value & opt parameters default_content_type_parameters & info [ "parameters" ] ~doc)

let attachments =
  let doc = "File attachment" in
  Arg.(value & opt_all existing_file_with_encoding [] & info [ "f"; "file" ] ~doc)

let body =
  let doc = "Main body of the mail" in
  Arg.(value & pos ~rev:true 0 existing_file_or_stdin `Stdin & info [] ~doc)

let ca_file =
  let doc = "PEM format file of CA's" in
  Arg.(value & opt (some existing_file) None & info [ "ca-file"] ~doc)

let ca_path =
  let doc = "PEM format directory of CA's" in
  Arg.(value & opt (some directory) None & info [ "ca-path"] ~doc)

let logger =
  let doc = "Name of SMTP logger" in
  Arg.(value & opt (some string) None & info [ "l"; "logger" ] ~doc)

let command =
  let doc = "Facteur is a tool to send a mail in OCaml." in
  let exits = Term.default_exits in
  let man =
    [ `S "Description"
    ; `P "." ] in
  Term.(pure run $ logs $ fmt $ logger $ username $ password $ date $ zone $ sender $ subject $ recipients
        $ hostname $ port $ with_starttls $ encoding $ parameters $ body $ attachments
        $ ca_file $ ca_path),
  Term.info "facteur" ~exits ~doc ~man

let () = Term.(exit @@ eval command)
