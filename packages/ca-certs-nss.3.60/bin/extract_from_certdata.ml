(* input is a certdata.txt from nss, output is a ml file with the trust anchors *)

(* ideas from FreeBSD's security/ca-root-nss perl script, available at:
   https://github.com/freebsd/freebsd-ports/blob/master/security/ca_root_nss/files/MAca-bundle.pl.in *)
open Rresult.R.Infix

let until_end data =
  let rec go acc = function
    | [] -> invalid_arg "unexpected end of input (expected END)"
    | "END" :: tl -> (List.rev acc, tl)
    | x :: tl -> go (x :: acc) tl
  in
  go [] data

let decode_octal data =
  let nums = Astring.String.cuts ~empty:false ~sep:"\\" data in
  let numbers = List.map (fun s -> int_of_string ("0o" ^ s)) nums in
  let out = Bytes.create (List.length nums) in
  List.iteri (fun i x -> Bytes.set out i (char_of_int x)) numbers;
  Bytes.unsafe_to_string out

let label_token = "CKA_LABEL UTF8 "

let serial_token = "CKA_SERIAL_NUMBER MULTILINE_OCTAL"

let label_serial id serial = function
  | x :: tl when Astring.String.is_prefix ~affix:label_token x ->
      let llen = String.length label_token in
      let id = Astring.String.drop ~min:llen ~max:llen x in
      (Some id, serial, tl)
  | x :: tl when x = serial_token ->
      let serial, rest = until_end tl in
      let serial = decode_octal (String.concat "" serial) in
      (id, Some serial, rest)
  | _ :: tl -> (id, serial, tl)
  | [] -> assert false

let get_id_serial id serial =
  let id = match id with None -> invalid_arg "no ID" | Some id -> id
  and serial =
    match serial with None -> invalid_arg "no serial" | Some s -> s
  in
  (id, serial)

let grab_cert input =
  let rec go id serial cert = function
    | [] -> (
        let id, serial = get_id_serial id serial in
        match cert with
        | None -> invalid_arg "missing certificate"
        | Some x -> (id, serial, x) )
    | "CKA_VALUE MULTILINE_OCTAL" :: tl ->
        let cert, tl = until_end tl in
        go id serial (Some cert) tl
    | tl ->
        let id, serial, tl = label_serial id serial tl in
        go id serial cert tl
  in
  go None None None input

let trust_ok_token = "CKT_NSS_TRUSTED_DELEGATOR"

and not_trusted_token = "CKT_NSS_NOT_TRUSTED"

and verify_token = "CKT_NSS_MUST_VERIFY_TRUST"

let web_server_token = "CKA_TRUST_SERVER_AUTH"

and email_token = "CKA_TRUST_EMAIL_PROTECTION"

and code_signing_token = "CKA_TRUST_CODE_SIGNING"

let ck_trust_token = " CK_TRUST "

let extract_trust x =
  let open Astring.String in
  let is_trusted x =
    if is_suffix ~affix:trust_ok_token x then `Trusted
    else if is_suffix ~affix:not_trusted_token x then `Not_trusted
    else if is_suffix ~affix:verify_token x then `Must_verify
    else invalid_arg "unknown trust setting"
  in
  if is_prefix ~affix:(web_server_token ^ ck_trust_token) x then
    Some (`Web, is_trusted x)
  else if is_prefix ~affix:(email_token ^ ck_trust_token) x then
    Some (`Email, is_trusted x)
  else if is_prefix ~affix:(code_signing_token ^ ck_trust_token) x then
    Some (`Code_signing, is_trusted x)
  else None

let grab_trust input =
  let rec go id serial trust = function
    | [] ->
        let id, serial = get_id_serial id serial in
        (id, serial, trust)
    | x :: tl -> (
        match extract_trust x with
        | None ->
            let id, serial, tl = label_serial id serial (x :: tl) in
            go id serial trust tl
        | Some y -> go id serial (y :: trust) tl )
  in
  go None None [] input

let add (certs, trust) mode acc =
  match mode with
  | Some `Cert -> (List.rev acc :: certs, trust)
  | Some `Trust -> (certs, List.rev acc :: trust)
  | None -> (certs, trust)

let rec split_into_certs_and_trust dbs mode acc = function
  | [] ->
      let certs, trust = add dbs mode acc in
      (List.rev certs, List.rev trust)
  | "CKA_CLASS CK_OBJECT_CLASS CKO_CERTIFICATE" :: tl ->
      let dbs = add dbs mode acc in
      split_into_certs_and_trust dbs (Some `Cert) [] tl
  | "CKA_CLASS CK_OBJECT_CLASS CKO_NSS_TRUST" :: tl ->
      let dbs = add dbs mode acc in
      split_into_certs_and_trust dbs (Some `Trust) [] tl
  | x :: tl -> split_into_certs_and_trust dbs mode (x :: acc) tl

module M = Map.Make (struct
  type t = string * string

  let compare (lbl, serial) (lbl', serial') =
    match String.compare lbl lbl' with
    | 0 -> String.compare serial serial'
    | y -> y
end)

let to_hex s =
  let (`Hex serial) = Hex.of_string s in
  serial

let decode data =
  let certs, trust = split_into_certs_and_trust ([], []) None [] data in
  let db =
    List.fold_left
      (fun db data ->
        let id, serial, cert = grab_cert data in
        let db =
          M.update (id, serial)
            (function
              | None -> Some (Some cert, None)
              | Some (None, x) -> Some (Some cert, x)
              | Some (Some _, _) ->
                  Logs.warn (fun m ->
                      m "cert with %s (serial %s) already present" id
                        (to_hex serial));
                  invalid_arg "duplicate certificate")
            db
        in
        db)
      M.empty certs
  in
  List.fold_left
    (fun db data ->
      let id, serial, trust = grab_trust data in
      let db =
        M.update (id, serial)
          (function
            | None -> Some (None, Some trust)
            | Some (x, None) -> Some (x, Some trust)
            | Some (_, Some _) ->
                Logs.warn (fun m ->
                    m "trust with %s (serial %s) already present" id
                      (to_hex serial));
                invalid_arg "duplicate trust")
          db
      in
      db)
    db trust

let filter_trusted ?(purpose = fun (_, _) -> true) db =
  let is_trusted ys =
    let y = List.filter purpose ys in
    List.exists (function _, `Trusted -> true | _ -> false) y
    && List.for_all (function _, `Not_trusted -> false | _ -> true) ys
  in
  M.fold
    (fun (id, serial) (cert, trust) (acc, untrusted) ->
      match (cert, trust) with
      | None, _ ->
          Logs.debug (fun m ->
              m "ignoring %s (serial %s), no corresponding certificate" id
                (to_hex serial));
          (acc, untrusted)
      | Some _, None ->
          Logs.warn (fun m ->
              m "ignoring %s (serial %s), no corresponding trust" id
                (to_hex serial));
          (acc, succ untrusted)
      | Some cert, Some t ->
          if is_trusted t then (M.add (id, serial) cert acc, untrusted)
          else (
            Logs.warn (fun m ->
                m "Untrusted certificate %s (serial %s)" id (to_hex serial));
            (acc, succ untrusted) ))
    db (M.empty, 0)

let header =
  "(* automatically extracted from certdata.txt by ca-certs-nss v3.60. *)"

let stats ucount tcount dcount =
  Fmt.strf "(* processed %d certificates, %d untrusted, %d trusted. *)%s"
    (ucount + tcount + dcount)
    ucount tcount
    ( if dcount > 0 then
      "\n(* Omitted " ^ string_of_int dcount ^ " certificates (decoding). *)"
    else "" )

let to_ml untrusted db =
  let certs, decoding_issues =
    M.fold
      (fun (lbl, _) cert (acc, dec) ->
        let der = decode_octal (String.concat "" cert) in
        match X509.Certificate.decode_der (Cstruct.of_string der) with
        | Ok _cert ->
            (("(* " ^ lbl ^ " *) \"" ^ String.escaped der ^ "\"") :: acc, dec)
        | Error (`Msg msg) ->
            Logs.warn (fun m -> m "failed to decode certificate: %s" msg);
            (acc, succ dec))
      db ([], 0)
  in
  String.concat "\n"
    [
      header;
      stats untrusted (List.length certs) decoding_issues;
      "";
      "let certificates = [";
      "  " ^ String.concat ";\n  " (List.rev certs);
      "]";
      "";
    ]

let jump () filename output =
  Bos.OS.File.read_lines (Fpath.v filename) >>= fun data ->
  let certs = decode data in
  let trusted_certs, untrusted = filter_trusted certs in
  Logs.debug (fun m ->
      m "found %d certificates (%d total):" (M.cardinal trusted_certs)
        (M.cardinal certs));
  let out = to_ml untrusted trusted_certs in
  let fn = match output with None -> "-" | Some filename -> filename in
  Bos.OS.File.write (Fpath.v fn) out

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let input =
  let doc = "Full path to certdata.txt." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CERTDATA.TXT")

let output =
  let doc = "Output filename (defaults to stdout)." in
  Arg.(value & opt (some string) None & info [ "output" ] ~doc)

let cmd =
  let doc = "Extract NSS certdata.txt into OCaml code" in
  ( Term.(term_result (const jump $ setup_log $ input $ output)),
    Term.info "extract-from-certdata" ~version:"3.60" ~doc )

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
