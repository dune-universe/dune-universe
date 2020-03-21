let with_faraday (f : Faraday.t -> unit) : string =
  let buf = Faraday.create 1024 in
  f buf;
  Faraday.serialize_to_string buf


module Request : Alcotest.TESTABLE with type t = Ssh_agent.any_ssh_agent_request = struct
  type t = Ssh_agent.any_ssh_agent_request
  let pp fmt (t : Ssh_agent.any_ssh_agent_request) =
    Fmt.string fmt
      (Ssh_agent.sexp_of_any_ssh_agent_request t
       |> Sexplib.Sexp.to_string)

  let equal (x1 : t) (x2 : t) =
    let open Ssh_agent in
    match x1, x2 with
    | Any_request Ssh_agentc_request_identities,
      Any_request Ssh_agentc_request_identities ->
      true
    | Any_request (Ssh_agentc_sign_request (pubkey1,data1,flags1)),
      Any_request (Ssh_agentc_sign_request (pubkey2,data2,flags2)) ->
      pubkey1 = pubkey2
      && data1 = data2
      && flags1 = flags2
    (* FIXME: unordered list *)
    | Any_request (Ssh_agentc_add_identity { privkey = p1; key_comment = c1; }),
      Any_request (Ssh_agentc_add_identity { privkey = p2; key_comment = c2; }) ->
      p1 = p2 && c1 = c2
    | Any_request (Ssh_agentc_remove_identity id1),
      Any_request (Ssh_agentc_remove_identity id2) ->
      id1 = id2
    | Any_request Ssh_agentc_remove_all_identities,
      Any_request Ssh_agentc_remove_all_identities ->
      true
    | Any_request (Ssh_agentc_add_smartcard_key { smartcard_id = id1; smartcard_pin = pin1 }),
      Any_request (Ssh_agentc_add_smartcard_key { smartcard_id = id2; smartcard_pin = pin2 }) ->
      id1 = id2 && pin1 = pin2
    | Any_request (Ssh_agentc_remove_smartcard_key { smartcard_reader_id = id1; smartcard_reader_pin = pin1 }),
      Any_request (Ssh_agentc_remove_smartcard_key { smartcard_reader_id = id2; smartcard_reader_pin = pin2 }) ->
      id1 = id2 && pin1 = pin2
    | Any_request (Ssh_agentc_lock p1),
      Any_request (Ssh_agentc_lock p2) ->
      p1 = p2
    | Any_request (Ssh_agentc_unlock p1),
      Any_request (Ssh_agentc_unlock p2) ->
      p1 = p2
    | Any_request (Ssh_agentc_add_id_constrained
                     { privkey = p1; key_comment = c1; key_constraints = constraints1}),
      Any_request (Ssh_agentc_add_id_constrained
                     { privkey = p2; key_comment = c2; key_constraints = constraints2}) ->
      p1 = p2 && c1 = c2 && constraints1 = constraints2
      (* FIXME: unordered list in the constraints *)
    | Any_request (Ssh_agentc_add_smartcard_key_constrained
                     { smartcard_id = t1; smartcard_pin = pin1; smartcard_constraints = c1 }),
      Any_request (Ssh_agentc_add_smartcard_key_constrained
                     { smartcard_id = t2; smartcard_pin = pin2; smartcard_constraints = c2 }) ->
      t1 = t2 && pin1 = pin2 && c1 = c2
      (* FIXME: unordered list in the constraints *)
    | Any_request (Ssh_agentc_extension { extension_type = t1; extension_contents = c1 }),
      Any_request (Ssh_agentc_extension { extension_type = t2; extension_contents = c2 }) ->
      t1 = t2 && c1 = c2
    | _, _ -> false
end

module Response : Alcotest.TESTABLE with type t = Ssh_agent.any_ssh_agent_response = struct
  type t = Ssh_agent.any_ssh_agent_response
  let pp fmt (t : Ssh_agent.any_ssh_agent_response) =
    Fmt.string fmt
      (Ssh_agent.sexp_of_any_ssh_agent_response t
       |> Sexplib.Sexp.to_string)
  let equal (x1 : t) (x2 : t) =
    let open Ssh_agent in
    match x1, x2 with
    | Any_response Ssh_agent_failure, Any_response Ssh_agent_failure -> true
    | Any_response Ssh_agent_success, Any_response Ssh_agent_success -> true
    | Any_response Ssh_agent_extension_failure, Any_response Ssh_agent_extension_failure -> true
    | Any_response (Ssh_agent_extension_blob d1),
      Any_response (Ssh_agent_extension_blob d2) ->
      d1 = d2
    | Any_response (Ssh_agent_identities_answer ids1),
      Any_response (Ssh_agent_identities_answer ids2) ->
      ids1 = ids2
    | Any_response (Ssh_agent_sign_response sig1),
      Any_response (Ssh_agent_sign_response sig2) ->
      sig1 = sig2
    | _, _ -> false
end

let m_request = (module Request
                  : Alcotest.TESTABLE with type t = Ssh_agent.any_ssh_agent_request)
let m_response = (module Response
                   : Alcotest.TESTABLE with type t = Ssh_agent.any_ssh_agent_response)


let () = Mirage_crypto_rng_unix.initialize ()
let privkey = Mirage_crypto_pk.Rsa.generate ~bits:1024 ()
let pubkey = Mirage_crypto_pk.Rsa.pub_of_priv privkey
let privkey = Ssh_agent.Privkey.Ssh_rsa privkey
let pubkey = Ssh_agent.Pubkey.Ssh_rsa pubkey

let serialize_parse s request =
  Alcotest.(check m_request) s (Ssh_agent.Any_request request)
    (let r = with_faraday (fun t ->
         Ssh_agent.Serialize.write_ssh_agent_request t request) in
     match Angstrom.parse_string Ssh_agent.Parse.ssh_agentc_message r with
     | Result.Ok req -> req
     | Result.Error e -> failwith e)

let serialize_parse_request_identities () =
  serialize_parse "serialize_parse_request_identities"
    Ssh_agent.Ssh_agentc_request_identities

let serialize_parse_sign_request () =
  serialize_parse "serialize_parse_sign_request"
    (Ssh_agent.Ssh_agentc_sign_request (pubkey, "KEY COMMENT", []))

let serialize_parse_add_identity () =
  serialize_parse "serialize_parse_add_identity"
    (Ssh_agent.Ssh_agentc_add_identity { privkey; key_comment = "KEY COMMENT" })

let serialize_parse_add_id_constrained_empty () =
  serialize_parse "serialize_parse_add_id_constrained"
    (Ssh_agent.Ssh_agentc_add_id_constrained
       { privkey; key_comment = "KEY COMMENT"; key_constraints = [] })

let serialize_parse_add_id_constrained_both () =
  serialize_parse "serialize_parse_add_id_constrained"
    (Ssh_agent.Ssh_agentc_add_id_constrained
       { privkey; key_comment = "KEY COMMENT"; key_constraints = [Confirm; Lifetime 5l] })

let serialize_parse_remove_identity () =
  serialize_parse "serialize_parse_remove_identity"
    (Ssh_agent.Ssh_agentc_remove_identity pubkey)

let serialize_parse_remove_all_identities () =
  serialize_parse "serialize_parse_remove_all_identities"
    Ssh_agent.Ssh_agentc_remove_all_identities

let serialize_parse_lock () =
  serialize_parse "serialize_parse_lock"
    (Ssh_agent.Ssh_agentc_lock "your favorite passphrase")

let serialize_parse_unlock () =
  serialize_parse "serialize_parse_unlock"
    (Ssh_agent.Ssh_agentc_unlock "your favorite passphrase")

let serialize_parse_extension () =
  serialize_parse "serialize_parse_extension"
    (Ssh_agent.Ssh_agentc_extension { extension_type = "query"; extension_contents = "" })

let serialize_parse_client = [
  "request_identities", `Quick, serialize_parse_request_identities;
  "sign_request", `Quick, serialize_parse_sign_request;
  "add_identity", `Quick, serialize_parse_add_identity;
  "add_id_constrained_empty", `Quick, serialize_parse_add_id_constrained_empty;
  "add_id_constrained_both", `Quick, serialize_parse_add_id_constrained_both;
  "remove_identity", `Quick, serialize_parse_remove_identity;
  "remove_all_identities", `Quick, serialize_parse_remove_all_identities;
  "lock", `Quick, serialize_parse_lock;
  "unlock", `Quick, serialize_parse_unlock;
  "extension", `Quick, serialize_parse_extension;
]

let serialize_parse_response s (response : 'a Ssh_agent.ssh_agent_response) =
  let extension = match Ssh_agent.Any_response response with
    | Ssh_agent.Any_response Ssh_agent.Ssh_agent_extension_failure ->
      true
    | Ssh_agent.Any_response (Ssh_agent.Ssh_agent_extension_blob _) ->
      true
    | _ -> false in
  Alcotest.(check m_response) s (Ssh_agent.Any_response response)
    (let r = with_faraday (fun t ->
         Ssh_agent.Serialize.write_ssh_agent_response t response) in
     match Angstrom.parse_string
             (Ssh_agent.Parse.ssh_agent_message ~extension)
             r with
     | Result.Ok req -> req
     | Result.Error e -> failwith e)

let serialize_parse_failure () =
  serialize_parse_response "serialize_parse_failure"
    Ssh_agent.Ssh_agent_failure

let serialize_parse_success () =
  serialize_parse_response "serialize_parse_success"
    Ssh_agent.Ssh_agent_success

let serialize_parse_extension_failure () =
  serialize_parse_response "serialize_parse_extension_failure"
    Ssh_agent.Ssh_agent_extension_failure

let serialize_parse_extension_blob () =
  serialize_parse_response "serialize_parse_extension_blob"
    (Ssh_agent.Ssh_agent_extension_blob "Hello, World!")
    (* You should probably never write an extension like this *)

let serialize_parse_identities_answer () =
  serialize_parse_response "serialize_parse_identities_answer"
    (Ssh_agent.Ssh_agent_identities_answer
       [ { pubkey; comment = "KEY COMMENT" } ])

let serialize_parse_sign_response () =
  serialize_parse_response "serialize_parse_sign_response"
    (Ssh_agent.Ssh_agent_sign_response "This is definitely a signature")

let serialize_parse_server = [
  "failure", `Quick, serialize_parse_failure;
  "extension_failure", `Quick, serialize_parse_extension_failure;
  "extension_blob", `Quick, serialize_parse_extension_blob;
  "identities_answer", `Quick, serialize_parse_identities_answer;
  "sign_response", `Quick, serialize_parse_sign_response;
]

let () =
  Alcotest.run "Serialize |> Parse identity" [
    "serialize_parse_client", serialize_parse_client;
    "serialize_parse_server", serialize_parse_server;
  ]
