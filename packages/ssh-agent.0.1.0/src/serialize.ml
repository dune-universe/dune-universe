open Types

module Wire = struct
  let write_byte t byte =
    Faraday.write_uint8 t byte

  let write_boolean t b =
    Faraday.write_uint8 t (if b then 1 else 0)

  let write_uint32 t uint32 =
    Faraday.BE.write_uint32 t uint32

  let write_uint64 t uint64 =
    Faraday.BE.write_uint64 t uint64

  let write_string t s =
    Faraday.BE.write_uint32 t (String.length s |> Int32.of_int);
    Faraday.write_string t s

  let write_mpint t mpint =
    if mpint = Z.zero
    then write_uint32 t 0l
    else
      let mpint = Nocrypto.Numeric.Z.to_cstruct_be mpint in
      let mpint_padded =
        if Cstruct.get_uint8 mpint 0 land 0x80 <> 0
        then Cstruct.append (Cstruct.of_string "\000") mpint
        else mpint in
      write_string t (Cstruct.to_string mpint_padded)

  let write_name_list t name_list =
    write_string t (String.concat "," name_list)
end

let with_faraday (f : Faraday.t -> unit) : string =
  let buf = Faraday.create 1024 in
  f buf;
  Faraday.serialize_to_string buf

let write_privkey t key =
  let open Privkey in
  match key with
  | Ssh_dss { p; q; gg; x; y } ->
    Wire.write_string t "ssh-dss";
    Wire.write_mpint t p;
    Wire.write_mpint t q;
    Wire.write_mpint t gg;
    Wire.write_mpint t y;
    Wire.write_mpint t x
  | Ssh_rsa { e; d; n; p; q; dp; dq; q' } ->
    (* iqmp (inverse of q modulo p) is q' *)
    Wire.write_string t "ssh-rsa";
    Wire.write_mpint t n;
    Wire.write_mpint t e;
    Wire.write_mpint t d;
    Wire.write_mpint t q';
    Wire.write_mpint t p;
    Wire.write_mpint t q
  | Blob { key_type; key_blob } ->
    Wire.write_string t key_type;
    Faraday.write_string t key_blob

let write_pubkey t key =
  let open Pubkey in
  match key with
  | Ssh_dss { p; q; gg; y } ->
    Wire.write_string t "ssh-dss";
    Wire.write_mpint t p;
    Wire.write_mpint t q;
    Wire.write_mpint t gg;
    Wire.write_mpint t y
  | Ssh_rsa { e; n } ->
    Wire.write_string t "ssh-rsa";
    Wire.write_mpint t e;
    Wire.write_mpint t n
  | Blob { key_type; key_blob } ->
    Wire.write_string t key_type;
    Faraday.write_string t key_blob

let write_protocol_number t ssh_agent =
  Wire.write_byte t (Protocol_number.ssh_agent_to_int ssh_agent)

let write_sign_flags t sign_flags =
  let flags = List.fold_left (fun acc sign_flag ->
      Protocol_number.sign_flag_to_int sign_flag lor acc)
      0 sign_flags in
  flags |> Int32.of_int |> Wire.write_uint32 t

let write_key_constraints t constraints =
  List.iter (function
      | Lifetime secs ->
        Faraday.write_uint8 t 1;
        Wire.write_uint32 t secs
      | Confirm ->
        Faraday.write_uint8 t 2)
    constraints

let write_ssh_agent_request t (type a) (req : a ssh_agent_request) =
  let message = with_faraday (fun t ->
      match req with
      | Ssh_agentc_request_identities ->
        write_protocol_number t SSH_AGENTC_REQUEST_IDENTITIES
      | Ssh_agentc_sign_request (pubkey, data, flags) ->
        write_protocol_number t SSH_AGENTC_SIGN_REQUEST;
        Wire.write_string t (with_faraday (fun t -> write_pubkey t pubkey));
        Wire.write_string t data;
        write_sign_flags t flags
      | Ssh_agentc_add_identity { privkey; key_comment } ->
        write_protocol_number t SSH_AGENTC_ADD_IDENTITY;
        write_privkey t privkey;
        Wire.write_string t key_comment
      | Ssh_agentc_remove_identity pubkey ->
        write_protocol_number t SSH_AGENTC_REMOVE_IDENTITY;
        Wire.write_string t (with_faraday (fun t -> write_pubkey t pubkey))
      | Ssh_agentc_remove_all_identities ->
        write_protocol_number t SSH_AGENTC_REMOVE_ALL_IDENTITIES
      | Ssh_agentc_add_smartcard_key { smartcard_id; smartcard_pin } ->
        write_protocol_number t SSH_AGENTC_ADD_SMARTCARD_KEY;
        Wire.write_string t smartcard_id;
        Wire.write_string t smartcard_pin
      | Ssh_agentc_remove_smartcard_key { smartcard_reader_id; smartcard_reader_pin } ->
        write_protocol_number t SSH_AGENTC_REMOVE_SMARTCARD_KEY;
        Wire.write_string t smartcard_reader_id;
        Wire.write_string t smartcard_reader_pin
      | Ssh_agentc_lock passphrase ->
        write_protocol_number t SSH_AGENTC_LOCK;
        Wire.write_string t passphrase
      | Ssh_agentc_unlock passphrase ->
        write_protocol_number t SSH_AGENTC_UNLOCK;
        Wire.write_string t passphrase
      | Ssh_agentc_add_id_constrained { privkey; key_comment; key_constraints } ->
        write_protocol_number t SSH_AGENTC_ADD_ID_CONSTRAINED;
        write_privkey t privkey;
        Wire.write_string t key_comment;
        write_key_constraints t key_constraints
      | Ssh_agentc_add_smartcard_key_constrained { smartcard_id; smartcard_pin;
                                                   smartcard_constraints } ->
        write_protocol_number t SSH_AGENTC_ADD_SMARTCARD_KEY_CONSTRAINED;
        Wire.write_string t smartcard_id;
        Wire.write_string t smartcard_pin;
        write_key_constraints t smartcard_constraints
      | Ssh_agentc_extension { extension_type; extension_contents } ->
        write_protocol_number t SSH_AGENTC_EXTENSION;
        Wire.write_string t extension_type;
        Faraday.write_string t extension_contents
    ) in
  Wire.write_uint32 t (Int32.of_int (String.length message));
  Faraday.write_string t message

let write_ssh_agent_response t (type a) (resp : a ssh_agent_response) =
  let message = with_faraday (fun t ->
      match resp with
      | Ssh_agent_failure ->
        write_protocol_number t SSH_AGENT_FAILURE
      | Ssh_agent_success ->
        write_protocol_number t SSH_AGENT_SUCCES
      | Ssh_agent_extension_failure ->
        write_protocol_number t SSH_AGENT_EXTENSION_FAILURE
      | Ssh_agent_extension_blob data ->
        Faraday.write_string t data
      | Ssh_agent_identities_answer ids ->
        write_protocol_number t SSH_AGENT_IDENTITIES_ANSWER;
        Wire.write_uint32 t (Int32.of_int (List.length ids));
        List.iter (fun { pubkey; comment } ->
            Wire.write_string t (with_faraday (fun t -> write_pubkey t pubkey));
            Wire.write_string t comment)
          ids
      | Ssh_agent_sign_response signature ->
        write_protocol_number t SSH_AGENT_SIGN_RESPONSE;
        Wire.write_string t signature)
  in
  Wire.write_uint32 t (Int32.of_int (String.length message));
  Faraday.write_string t message
