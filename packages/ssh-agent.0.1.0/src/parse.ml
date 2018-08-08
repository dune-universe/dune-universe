open Types

module Wire = struct
  open Angstrom

  let byte =
    any_uint8

  let boolean =
    any_char >>| ((<>) '\000')

  let uint32 =
    BE.any_int32

  let uint64 =
    BE.any_int64

  (* XXX: int32 -> int coercion *)
  let string =
    BE.any_int32 >>= fun string_len ->
    take (Int32.to_int string_len)

  (* XXX: int32 -> int coercion *)
  (* FIXME: negative numbers *)
  let mpint =
    BE.any_int32
    >>= fun mpint_len ->
    if mpint_len = 0l
    then return Z.zero
    else take (Int32.to_int mpint_len)
      >>= fun mpint ->
      return (Nocrypto.Numeric.Z.of_cstruct_be (Cstruct.of_string mpint))

  let name_list =
    string >>|
    String.split_on_char ','
end

(* Angstrom helpers *)
let take32 n =
  Angstrom.take (Int32.to_int n)

let count32 n =
  Angstrom.count (Int32.to_int n)

let parse_lift p1 p2 =
  let open Angstrom in
  p1 >>= fun s ->
  match parse_string p2 s with
  | Ok a -> Angstrom.return a
  | Error e -> Angstrom.fail e

let ssh_dss =
  let open Angstrom in
  Wire.mpint >>= fun p ->
  Wire.mpint >>= fun q ->
  Wire.mpint >>= fun gg ->
  Wire.mpint >>= fun y ->
  Wire.mpint >>= fun x ->
  return (Privkey.Ssh_dss { p; q; gg; y; x })

let ssh_rsa =
  let open Angstrom in
  Wire.mpint >>= fun n ->
  Wire.mpint >>= fun e ->
  Wire.mpint >>= fun d ->
  Wire.mpint >>= fun iqmp ->
  Wire.mpint >>= fun p ->
  Wire.mpint >>= fun q ->
  (* FIXME: How do the parameters correspond to Nocrypto.Rsa.priv ? *)
  return (Privkey.Ssh_rsa (Nocrypto.Rsa.priv_of_primes ~e ~p ~q))

let blob key_type =
  let open Angstrom in
  take_while (fun _ -> true) >>= fun key_blob ->
  return (Privkey.Blob { key_type; key_blob })

let privkey =
  let open Angstrom in
  Wire.string >>= function
  | "ssh-dss" ->
    ssh_dss
  | "ssh-rsa" ->
    ssh_rsa
  | key_type ->
    blob key_type

let pub_ssh_dss =
  let open Angstrom in
  Wire.mpint >>= fun p ->
  Wire.mpint >>= fun q ->
  Wire.mpint >>= fun gg ->
  Wire.mpint >>= fun y ->
  return (Pubkey.Ssh_dss { p; q; gg; y })

let pub_ssh_rsa =
  let open Angstrom in
  Wire.mpint >>= fun e ->
  Wire.mpint >>= fun n ->
  return (Pubkey.Ssh_rsa { e; n })

let pub_blob key_type =
  Angstrom.(take_while (fun _ -> true) >>= fun key_blob ->
            return @@ Pubkey.Blob { key_type; key_blob; })

let pubkey =
  let open Angstrom in
  Wire.string >>= function
  | "ssh-dss" ->
    pub_ssh_dss
  | "ssh-rsa" ->
    pub_ssh_rsa
  | key_type ->
    pub_blob key_type


let comment = Wire.string

let id_entry =
  let open Angstrom in
  parse_lift Wire.string pubkey >>= fun pubkey ->
  Wire.string >>= fun comment ->
  return { pubkey; comment }

let ssh_agent_identities_answer =
  let open Angstrom in
  BE.any_int32 >>= fun nkeys ->
  count32 nkeys id_entry

let ssh_agent_sign_response =
  let open Angstrom in
  Wire.string >>= fun signature ->
  return (Ssh_agent_sign_response signature)

let ssh_agent_extension_failure =
  let open Angstrom in
  Angstrom.any_uint8 >>|
  Protocol_number.int_to_ssh_agent >>=
  let open Protocol_number in function
    | Some SSH_AGENT_FAILURE ->
      return (Any_response Ssh_agent_failure)
    | Some SSH_AGENT_EXTENSION_FAILURE ->
      return (Any_response Ssh_agent_extension_failure)
    | _ -> fail "Goto extension blob"

let ssh_agent_message_type extension =
  let open Angstrom in
  if extension
  then
    ssh_agent_extension_failure <|>
    (take_while (fun _ -> true) >>= fun data ->
     return (Any_response (Ssh_agent_extension_blob data)))
  else
    Angstrom.any_uint8 >>|
    Protocol_number.int_to_ssh_agent >>=
    let open Protocol_number in function
      | Some SSH_AGENT_FAILURE ->
        return (Any_response Ssh_agent_failure)
      | Some SSH_AGENT_SUCCES ->
        return (Any_response Ssh_agent_success)
      | Some SSH_AGENT_IDENTITIES_ANSWER ->
        ssh_agent_identities_answer >>| fun identities ->
        Any_response (Ssh_agent_identities_answer identities)
      | Some SSH_AGENT_SIGN_RESPONSE ->
        ssh_agent_sign_response >>| fun r ->
        Any_response r
      | Some SSH_AGENT_EXTENSION_FAILURE ->
        return (Any_response (Ssh_agent_extension_failure))
      | Some protocol_number ->
        fail ("Unimplemeted protocol number: " ^
              ssh_agent_to_string protocol_number)
      | None ->
        fail "Unknown ssh-agent protocol number"


let ssh_agent_message ~extension =
  let open Angstrom in
  BE.any_int32 >>= fun msg_len ->
  parse_lift (take32 msg_len)
    (ssh_agent_message_type extension)

let ssh_agentc_sign_request =
  let open Angstrom in
  parse_lift Wire.string pubkey >>= fun pubkey ->
  Wire.string >>= fun data ->
  Wire.uint32 >>= fun mask ->
  let flags = Protocol_number.mask_to_sign_flags (Int32.to_int mask) in
  return (Ssh_agentc_sign_request (pubkey, data, flags))

let key_constraint =
  let open Angstrom in
  any_uint8 >>= function
  | 1 ->
    Wire.uint32 >>= fun secs -> return (Lifetime secs)
  | 2 ->
    return Confirm
  | _ ->
    fail "Unsupported key constraint type"

let ssh_agentc_add_identity =
  let open Angstrom in
  privkey >>= fun privkey ->
  Wire.string >>= fun key_comment ->
  return (Ssh_agentc_add_identity { privkey; key_comment })

let ssh_agentc_add_id_constrained =
  let open Angstrom in
  privkey >>= fun privkey ->
  Wire.string >>= fun key_comment ->
  many key_constraint >>= fun key_constraints ->
  return (Ssh_agentc_add_id_constrained { privkey; key_comment; key_constraints })

let ssh_agentc_remove_identity =
  let open Angstrom in
  parse_lift Wire.string pubkey >>= fun pubkey ->
  return (Ssh_agentc_remove_identity pubkey)

let ssh_agentc_add_smartcard_key =
  let open Angstrom in
  Wire.string >>= fun smartcard_id ->
  Wire.string >>= fun smartcard_pin ->
  return (Ssh_agentc_add_smartcard_key { smartcard_id; smartcard_pin })

let ssh_agentc_add_smartcard_key_constrained =
  let open Angstrom in
  Wire.string >>= fun smartcard_id ->
  Wire.string >>= fun smartcard_pin ->
  many key_constraint >>= fun smartcard_constraints ->
  return (Ssh_agentc_add_smartcard_key_constrained
            { smartcard_id; smartcard_pin; smartcard_constraints })

let ssh_agentc_remove_smartcard_key =
  let open Angstrom in
  Wire.string >>= fun smartcard_reader_id ->
  Wire.string >>= fun smartcard_reader_pin ->
  return (Ssh_agentc_remove_smartcard_key { smartcard_reader_id; smartcard_reader_pin })

let ssh_agentc_lock =
  let open Angstrom in
  Wire.string >>= fun passphrase ->
  return (Ssh_agentc_lock passphrase)

let ssh_agentc_unlock =
  let open Angstrom in
  Wire.string >>= fun passphrase ->
  return (Ssh_agentc_unlock passphrase)

let ssh_agentc_extension =
  let open Angstrom in
  Wire.string >>= fun extension_type ->
  take_while (fun _ -> true) >>= fun extension_contents ->
  return (Ssh_agentc_extension { extension_type; extension_contents })


let ssh_agentc_message_type =
  let open Angstrom in
  let req p = p >>| fun r -> Any_request r in
  any_uint8 >>|
  Protocol_number.int_to_ssh_agent >>=
  let open Protocol_number in function
    | Some SSH_AGENTC_REQUEST_IDENTITIES ->
      return (Any_request Ssh_agentc_request_identities)
    | Some SSH_AGENTC_SIGN_REQUEST ->
      req ssh_agentc_sign_request
    | Some SSH_AGENTC_ADD_IDENTITY ->
      req ssh_agentc_add_identity
    | Some SSH_AGENTC_REMOVE_IDENTITY ->
      req ssh_agentc_remove_identity
    | Some SSH_AGENTC_REMOVE_ALL_IDENTITIES ->
      return (Any_request Ssh_agentc_remove_all_identities)
    | Some SSH_AGENTC_ADD_SMARTCARD_KEY ->
      req ssh_agentc_add_smartcard_key
    | Some SSH_AGENTC_REMOVE_SMARTCARD_KEY ->
      req ssh_agentc_remove_smartcard_key
    | Some SSH_AGENTC_LOCK ->
      req ssh_agentc_lock
    | Some SSH_AGENTC_UNLOCK ->
      req ssh_agentc_unlock
    | Some SSH_AGENTC_ADD_ID_CONSTRAINED ->
      req ssh_agentc_add_id_constrained
    | Some SSH_AGENTC_ADD_SMARTCARD_KEY_CONSTRAINED ->
      req ssh_agentc_add_smartcard_key_constrained
    | Some SSH_AGENTC_EXTENSION ->
      req ssh_agentc_extension
    | None | Some _ ->
      fail "Not an ssh-agent request"

let ssh_agentc_message =
  let open Angstrom in
  BE.any_int32 >>= fun msg_len ->
  parse_lift (take32 msg_len)
    ssh_agentc_message_type
