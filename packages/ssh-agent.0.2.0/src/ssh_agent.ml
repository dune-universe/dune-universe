include Types

module Parse = Parse
module Serialize = Serialize

let is_extension_request (type a) (req : a ssh_agent_request) =
  match req with
  | Ssh_agentc_extension _ -> true
  | _ -> false

let unpack_any_response (type a) (request : a ssh_agent_request) (response : any_ssh_agent_response)
  : (a ssh_agent_response, string) result =
  let success_or_fail (resp : any_ssh_agent_response)
    : ([`Ssh_agentc_successable] ssh_agent_response, string) result =
    match resp with
    | Any_response (Ssh_agent_success as r) -> Ok r
    | Any_response (Ssh_agent_failure as r) -> Ok r
    | Any_response _ -> Error "Illegal response type"
  in
  match request with
  | Ssh_agentc_request_identities ->
    begin match response with
      | Any_response (Ssh_agent_identities_answer _ as r) ->
        Ok r
      | Any_response (Ssh_agent_failure as r) ->
        Ok r
      | _ ->
        Error "Illegal response type"
    end
  | Ssh_agentc_sign_request (_,_,_) ->
    begin match response with
      | Any_response (Ssh_agent_sign_response _ as r) ->
        Ok r
      | Any_response (Ssh_agent_failure as r) ->
        Ok r
      | _ ->
        Error "Illegal response type"
    end
  | Ssh_agentc_extension _ ->
    begin match response with
      | Any_response (Ssh_agent_extension_failure as r) ->
        Ok r
      | Any_response (Ssh_agent_extension_blob _ as r) ->
        Ok r
      | Any_response (Ssh_agent_failure as r) ->
        Ok r
      | _ ->
        Error "Illegal response type"
    end
  | Ssh_agentc_add_identity _ -> success_or_fail response
  | Ssh_agentc_remove_identity _ -> success_or_fail response
  | Ssh_agentc_remove_all_identities -> success_or_fail response
  | Ssh_agentc_add_smartcard_key _ -> success_or_fail response
  | Ssh_agentc_remove_smartcard_key _ -> success_or_fail response
  | Ssh_agentc_lock _ -> success_or_fail response
  | Ssh_agentc_unlock _ -> success_or_fail response
  | Ssh_agentc_add_id_constrained _ -> success_or_fail response
  | Ssh_agentc_add_smartcard_key_constrained _ -> success_or_fail response

let sign priv (sign_flags : Protocol_number.sign_flag list) blob =
  match priv with
  | Privkey.Ssh_rsa key | Privkey.Ssh_rsa_cert (key, _) ->
    let alg_string, to_sign =
      if List.mem Protocol_number.SSH_AGENT_RSA_SHA2_512 sign_flags
      then let digest = Nocrypto.Hash.SHA512.digest (Cstruct.of_string blob) in
        "rsa-sha2-512", Cstruct.append Util.id_sha512 digest
      else if List.mem Protocol_number.SSH_AGENT_RSA_SHA2_256 sign_flags
      then let digest = Nocrypto.Hash.SHA256.digest (Cstruct.of_string blob) in
        "rsa-sha2-256", Cstruct.append Util.id_sha256 digest
      else let digest = Nocrypto.Hash.SHA1.digest (Cstruct.of_string blob) in
        "ssh-rsa", Cstruct.append Util.id_sha1 digest in
    let signed = Nocrypto.Rsa.PKCS1.sig_encode ~key to_sign in
    Serialize.(with_faraday (fun t ->
        Wire.write_string t alg_string;
        Wire.write_string t (Cstruct.to_string signed)))
  | Privkey.Ssh_dss _
  | Privkey.Blob _ ->
    failwith "Not implemented :-("

let string_of_tbs tbs =
  Serialize.with_faraday (fun t -> Serialize.write_ssh_rsa_cert_tbs t tbs)
