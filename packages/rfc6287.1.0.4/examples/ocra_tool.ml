open Sexplib
open Sexplib.Conv

(** ocra credentials *)
type t =
  { s: Rfc6287.t  (** parsed suite *)
  ; k: Cstruct.t  (** key *)
  ; c: int64 option  (** counter *)
  ; p: Cstruct.t option  (** pinhash *)
  ; cw: int option  (** counter_window *)
  ; tw: int option  (** timestamp_window *) }

(** utility functions *)
let to_hex cs =
  let (`Hex s) = Hex.of_cstruct cs in
  s

(** serialize/unserialize *)
let sexp_of_t t =
  let to_hex_o = function None -> None | Some cs -> Some (to_hex cs) in
  let record kvs =
    Sexp.List List.(map (fun (k, v) -> Sexp.List [Sexp.Atom k; v]) kvs)
  in
  record
    [ ("s", sexp_of_string (Rfc6287.string_of_t t.s))
    ; ("k", sexp_of_string (to_hex t.k))
    ; ("c", sexp_of_option sexp_of_int64 t.c)
    ; ("p", sexp_of_option sexp_of_string (to_hex_o t.p))
    ; ("cw", sexp_of_option sexp_of_int t.cw)
    ; ("tw", sexp_of_option sexp_of_int t.tw) ]

let t_of_sexp = function
  | Sexp.List l -> (
    match
      List.fold_left
        (fun (s, k, c, p, cw, tw) -> function
          | Sexp.List [Sexp.Atom "s"; v] ->
              let open Rresult in
              let suite =
                match Rfc6287.t_of_string (string_of_sexp v) with
                | Error _ -> failwith "broken credentials file"
                | Ok x -> x
              in
              (Some suite, k, c, p, cw, tw)
          | Sexp.List [Sexp.Atom "k"; v] ->
              let key = Cstruct.of_hex (string_of_sexp v) in
              (s, Some key, c, p, cw, tw)
          | Sexp.List [Sexp.Atom "c"; v] ->
              let counter = option_of_sexp int64_of_sexp v in
              (s, k, counter, p, cw, tw)
          | Sexp.List [Sexp.Atom "p"; v] ->
              let pin =
                match option_of_sexp string_of_sexp v with
                | None -> None
                | Some x -> Some (Cstruct.of_hex x)
              in
              (s, k, c, pin, cw, tw)
          | Sexp.List [Sexp.Atom "cw"; v] ->
              let counter_window = option_of_sexp int_of_sexp v in
              (s, k, c, p, counter_window, tw)
          | Sexp.List [Sexp.Atom "tw"; v] ->
              let timestamp_window = option_of_sexp int_of_sexp v in
              (s, k, c, p, cw, timestamp_window)
          | _ -> failwith "broken credentials file" )
        (None, None, None, None, None, None)
        l
    with
    | Some s, Some k, c, p, cw, tw -> {s; k; c; p; cw; tw}
    | _ -> failwith "broken credentials file" )
  | _ -> failwith "broken credentials file"

(** storage *)
let of_file f =
  try
    let stat = Unix.stat f in
    let buf = Bytes.create stat.Unix.st_size in
    let fd = Unix.openfile f [Unix.O_RDONLY] 0 in
    let _read_b = Unix.read fd buf 0 stat.Unix.st_size in
    let () = Unix.close fd in
    t_of_sexp (Sexp.of_string (Bytes.unsafe_to_string buf))
  with Unix.Unix_error (e, _, _) -> failwith (Unix.error_message e)

let file_of f t =
  try
    let fd = Unix.openfile f [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
    let s = Bytes.unsafe_of_string (Sexp.to_string_hum (sexp_of_t t)) in
    let _written_bytes = Unix.single_write fd s 0 (Bytes.length s) in
    let () = Unix.close fd in
    `Ok ()
  with Unix.Unix_error (e, _, _) -> failwith (Unix.error_message e)

let cred_file = function
  | None -> failwith "credential_file required"
  | Some x -> x

(** "init" command *)
let initx i_f i_s i_k i_c i_p i_ph i_cw i_tw =
  let open Rfc6287 in
  let open Rresult in
  let e s = failwith s in
  try
    let strip_0x s =
      match Astring.String.with_range s ~len:2 with
      | "0x" -> Astring.String.with_range s ~first:3
      | _ -> s
    in
    let s =
      match i_s with
      | None -> failwith "suite_string required"
      | Some x -> (
        match t_of_string x with
        | Error _ -> failwith "invalid suite"
        | Ok y -> y )
    in
    let k =
      match i_k with
      | None -> e "key required"
      | Some x -> (
        try Cstruct.of_hex (strip_0x x) with Invalid_argument _ -> e "invalid key" )
    in
    let di = di_of_t s in
    let _ =
      match di.s with
      | None -> ()
      | Some _ -> e "suite requires unsupported session parameter (S)"
    in
    let c =
      match (di.c, i_c) with
      | false, None -> None
      | false, Some _ ->
          e
            "suite does not require counter parameter: -c <...> must not be set"
      | true, None -> e "suite requires counter parameter: -c <...> missing"
      | true, Some x -> Some x
    in
    let p =
      match (di.p, i_p, i_ph) with
      | None, None, None -> None
      | Some _, None, None ->
          e "suite requires pin parameter: -p <...> missing"
      | None, Some _, _ ->
          e "suite does not require pin parameter: -p <...> must not be set"
      | None, None, Some _ ->
          e "suite does not require pin parameter: -P <...> must not be set"
      | Some _, Some _, Some _ -> e "only on of -p|-P must be set"
      | Some dgst, Some x, None ->
          Some (Mirage_crypto.Hash.digest dgst (Cstruct.of_string x))
      | Some dgst, None, Some x ->
          let w =
            try Cstruct.of_hex (strip_0x x) with Invalid_argument _ ->
              e "invalid pin_hash"
          in
          if Cstruct.len w = Mirage_crypto.Hash.digest_size dgst then Some w
          else e "invalid pin_hash"
    in
    let cw =
      match (di.c, i_cw) with
      | _, None -> None
      | true, Some x when x > 0 -> Some x
      | true, Some _ -> e "invalid counter_window value"
      | false, Some _ ->
          e
            "suite does not require counter parameter: -w <...> must not be set"
    in
    let tw =
      match (di.t, i_tw) with
      | _, None -> None
      | Some _, Some x when x > 0 -> Some x
      | Some _, Some _ -> e "invalid timestamp_window value"
      | None, Some _ ->
          e
            "suite does not require timestamp parameter: -t <...> must not be \
             set"
    in
    file_of (cred_file i_f) {s; k; c; p; cw; tw}
  with Failure f -> `Error (false, f)

(** "info" command *)
let infox i_f =
  let print_t t =
    Printf.printf "suite:            %s\n" (Rfc6287.string_of_t t.s) ;
    Printf.printf "key:              0x%s\n" (to_hex t.k) ;
    ( match t.c with
    | None -> ()
    | Some c -> Printf.printf "counter:          0x%Lx\n" c ) ;
    ( match t.p with
    | None -> ()
    | Some p -> Printf.printf "pinhash:          0x%s\n" (to_hex p) ) ;
    ( match t.cw with
    | None -> ()
    | Some cw -> Printf.printf "counter_window:   %d\n" cw ) ;
    ( match t.tw with
    | None -> ()
    | Some cw -> Printf.printf "timestamp_window: %d\n" cw ) ;
    `Ok ()
  in
  try print_t (of_file (cred_file i_f)) with Failure e -> `Error (false, e)

(** "challenge" command *)
let challengex i_f =
  let () = Mirage_crypto_rng_unix.initialize () in
  try
    let t = of_file (cred_file i_f) in
    let _ = Printf.printf "%s\n" (Rfc6287.challenge t.s) in
    `Ok ()
  with Failure e -> `Error (false, e)

(** code shared between verify and response *)
let vr_aux i_f i_q =
  let q =
    match i_q with None -> failwith "challenge required" | Some x -> x
  in
  let f = cred_file i_f in
  let t = of_file f in
  let suite = t.s in
  let p = match t.p with None -> None | Some x -> Some (`Digest x) in
  let di = Rfc6287.di_of_t suite in
  let ts = match di.Rfc6287.t with None -> None | Some _ -> Some `Now in
  (q, f, t, suite, p, ts, None, t.c, t.k)

(** "verify" command *)
let verifyx i_f i_q i_a =
  try
    let a =
      match i_a with
      | None -> failwith "response required"
      | Some x -> Cstruct.of_string x
    in
    let q, f, t, suite, p, ts, s, c, key = vr_aux i_f i_q in
    let cw, tw = (t.cw, t.tw) in
    let open Rresult in
    let open Rfc6287 in
    match Rfc6287.verify1 ~c ~p ~s ~t:ts ~cw ~tw ~key ~q ~a suite with
    | Ok (true, next) -> (
        let _ = Printf.printf "success\n" in
        match next with
        | None -> `Ok ()
        | Some newc -> file_of f {t with c= Some newc} )
    | Ok (false, None) ->
        let _ = Printf.printf "failure\n" in
        `Ok ()
    | Error (Window s) -> failwith s
    | Error (DataInput s) -> failwith s
    | _ -> failwith "do not know"
  with Failure e -> `Error (false, e)

(** "response" command *)
let responsex i_f i_q =
  try
    let q, f, t, suite, p, ts, s, c, key = vr_aux i_f i_q in
    let open Rresult in
    let open Rfc6287 in
    match Rfc6287.gen1 ~c ~p ~s ~t:ts ~key ~q suite with
    | Ok x -> (
        let _ = Printf.printf "%s\n" (Cstruct.to_string x) in
        match t.c with
        | None -> `Ok ()
        | Some x -> file_of f {t with c= Some (Int64.add x 1L)} )
    | Error (DataInput e) -> failwith e
    | _ -> failwith "do not know"
  with Failure e -> `Error (false, e)

(** command line interface *)
open Cmdliner

let copts_sect = "COMMON OPTIONS"

let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `S "BUGS"
  ; `P "sure." ]

let i_f =
  let docs = copts_sect in
  let doc = "the OCRA credential file." in
  Arg.(
    value
    & opt (some string) None
    & info ["f"] ~docv:"credential_file" ~doc ~docs)

let init_cmd =
  let i_s =
    let doc = "OCRA suite string." in
    Arg.(value & opt (some string) None & info ["s"] ~docv:"suite_string" ~doc)
  in
  let i_k =
    let doc = "specified as hexadecimal string." in
    Arg.(value & opt (some string) None & info ["k"] ~docv:"key" ~doc)
  in
  let i_c =
    let doc =
      "If the suite_string requires a counter parameter,\n      \
       $(docv) is the initial counter value."
    in
    Arg.(value & opt (some int64) None & info ["c"] ~docv:"counter" ~doc)
  in
  let i_p =
    let doc =
      "If the suite_string requires a pin-hash parameter, it is\n      \
       calculated from $(docv) using the pin-hash algorith in suite_string"
    in
    Arg.(value & opt (some string) None & info ["p"] ~docv:"pin" ~doc)
  in
  let i_ph =
    let doc =
      "If the suite_string requires a pin-hash parameter, $(docv) will\n      \
       be used. $(docv) must encoded as hexadecimal string."
    in
    Arg.(value & opt (some string) None & info ["P"] ~docv:"pin_hash" ~doc)
  in
  let i_cw =
    let doc =
      "If the suite_string requires a counter parameter,\n      \
       $(docv) specifies the maximum number of verify attempts\n      \
       (incrementing the counter value). This parameter is optional."
    in
    Arg.(value & opt (some int) None & info ["w"] ~docv:"counter_window" ~doc)
  in
  let i_tw =
    let doc =
      "If the suite_string requires a timestamp parameter,\n      \
       $(docv) specifies the number of timestamp steps that will be made while\n      \
       verifying a response. The verify process will start at (now() -\n      \
       $(docv)) and end fail at (now() + $(docv) +1).\n      \
       This parameter is optional."
    in
    Arg.(
      value & opt (some int) None & info ["t"] ~docv:"timestamp_window" ~doc)
  in
  let doc = "Initialize OCRA credential file." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Parse suite string; serialize key, additional DataInput and \
         verification\n    \
         options to credential file ..." ]
    @ help_secs
  in
  ( Term.(ret (pure initx $ i_f $ i_s $ i_k $ i_c $ i_p $ i_ph $ i_cw $ i_tw))
  , Term.info "init" ~sdocs:copts_sect ~doc ~man )

let info_cmd =
  let doc = "Show content of OCRA credential file." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Show suite string, key, additional DataInput and verification\n     \
         options in credential file ..." ]
    @ help_secs
  in
  (Term.(ret (pure infox $ i_f)), Term.info "info" ~doc ~sdocs:copts_sect ~man)

let challenge_cmd =
  let doc = "Generate OCRA challenge" in
  let man =
    [ `S "Description"
    ; `P
        "Generate OCRA challenge according to the challenge format specified in\n      \
         the credential file ..." ]
    @ help_secs
  in
  ( Term.(ret (pure challengex $ i_f))
  , Term.info "challenge" ~doc ~sdocs:copts_sect ~man )

let verify_cmd =
  let i_q =
    Arg.(value & opt (some string) None & info ["q"] ~docv:"challenge")
  in
  let i_a =
    Arg.(value & opt (some string) None & info ["a"] ~docv:"response")
  in
  let doc = "Verify ORCA response" in
  let man =
    [ `S "Description"
    ; `P
        "Verify ORCA response with challenge and credentials from\n      \
         credential file."
    ; `P
        "Successful verification will write the next valid counter to the\n      \
         credential file if the OCRA suite specifies C." ]
    @ help_secs
  in
  ( Term.(ret (pure verifyx $ i_f $ i_q $ i_a))
  , Term.info "verify" ~doc ~sdocs:copts_sect ~man )

let response_cmd =
  let i_q =
    Arg.(value & opt (some string) None & info ["q"] ~docv:"challenge")
  in
  let doc = "Generate OCRA response" in
  let man =
    [ `S "Description"
    ; `P "Calculate OCRA response to challenge"
    ; `P
        "If OCAR suite specifies C, the counter in the credential_file will be\n          \
         incremented." ]
    @ help_secs
  in
  ( Term.(ret (pure responsex $ i_f $ i_q))
  , Term.info "response" ~doc ~sdocs:copts_sect ~man )

let default_cmd =
  let doc =
    "create and view OCRA credential files, generate challenges,\n             \
     calculate and verify responses"
  in
  let man = help_secs in
  ( Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ i_f))
  , Term.info "ocra_tool" ~sdocs:copts_sect ~doc ~man )

let cmds = [init_cmd; info_cmd; challenge_cmd; verify_cmd; response_cmd]

let () =
  match Term.eval_choice ~catch:false default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
