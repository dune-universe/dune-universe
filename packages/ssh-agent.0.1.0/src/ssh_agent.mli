module Privkey : sig
  type ssh_dss = Nocrypto.Dsa.priv
  [@@deriving sexp_of]

  type ssh_rsa = Nocrypto.Rsa.priv
  [@@deriving sexp_of]

  type t =
    | Ssh_dss of ssh_dss
    | Ssh_rsa of ssh_rsa
    | Blob of {
        key_type : string;
        key_blob : string;
      }
    (** [Blob] is an unknown ssh wire string-unwrapped private key of type
     * [key_type]. *)
  [@@deriving sexp_of]
end

module Pubkey : sig
  type ssh_dss = Nocrypto.Dsa.pub
  [@@deriving sexp_of]

  type ssh_rsa = Nocrypto.Rsa.pub
  [@@deriving sexp_of]

  type t =
    | Ssh_dss of ssh_dss
    | Ssh_rsa of ssh_rsa
    | Blob of {
        key_type : string;
        key_blob : string;
      }
    (** [Blob] is an unknown ssh wire string-unwrapped public key of type
     * [key_type]. *)
  [@@deriving sexp_of]
end

type identity = {
  pubkey : Pubkey.t;
  comment : string;
}
(** [identity]s are returned when querying for identities, i.e.
 * in [Ssh_agent_identities_answer] when responding to
 * [Ssh_agentc_request_identities]. *)
[@@deriving sexp_of]

(** Flags for what hashing algorithm is desired when doing a signing request.
 * SHA1 is assumed otherwise. *)
type sign_flag = Protocol_number.sign_flag =
  | SSH_AGENT_RSA_SHA2_256
  | SSH_AGENT_RSA_SHA2_512
[@@deriving sexp_of]

type key_constraint =
  | Lifetime of int32 (* uint32 *)
  | Confirm
  (* Extensions are not implemented because the extension-specific data has
   * unknown length. This requires making the parser extensible for key
   * constraints. *)
[@@deriving sexp_of]

(** [ssh_agent_request_type] is used in the below GADTs for enforcing protocol
 * semantics. It represents types of requests. The [`Ssh_agentc_successable]
 * type is a generalization of all requests that expect either success or
 * failure. *)
type ssh_agent_request_type = [
  | `Ssh_agentc_request_identities
  | `Ssh_agentc_sign_request
  | `Ssh_agentc_extension
  | `Ssh_agentc_successable
]

type _ ssh_agent_request =
  | Ssh_agentc_request_identities :
      [`Ssh_agentc_request_identities] ssh_agent_request
  | Ssh_agentc_sign_request :
      Pubkey.t * string * Protocol_number.sign_flag list
    -> [`Ssh_agentc_sign_request] ssh_agent_request
  | Ssh_agentc_add_identity :
      { privkey : Privkey.t; key_comment : string }
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_remove_identity :
      Pubkey.t
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_remove_all_identities :
      [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_add_smartcard_key :
      { smartcard_id : string; smartcard_pin : string }
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_remove_smartcard_key :
      { smartcard_reader_id : string; smartcard_reader_pin : string }
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_lock :
      string
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_unlock :
      string
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_add_id_constrained :
      { privkey : Privkey.t; key_comment : string; key_constraints : key_constraint list }
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_add_smartcard_key_constrained :
      { smartcard_id : string; smartcard_pin : string;
        smartcard_constraints : key_constraint list }
    -> [`Ssh_agentc_successable] ssh_agent_request
  | Ssh_agentc_extension :
      { extension_type : string; extension_contents : string }
    -> [`Ssh_agentc_extension] ssh_agent_request
[@@deriving sexp_of]

type any_ssh_agent_request =
  Any_request : 'a ssh_agent_request -> any_ssh_agent_request
[@@deriving sexp_of]

type _ ssh_agent_response =
  | Ssh_agent_failure : [<ssh_agent_request_type] ssh_agent_response
  | Ssh_agent_success : [`Ssh_agentc_successable] ssh_agent_response
  | Ssh_agent_extension_failure : [`Ssh_agentc_extension] ssh_agent_response
  | Ssh_agent_extension_blob : string
    -> [`Ssh_agentc_extension] ssh_agent_response
  (** Generic uninterpreted response - it's up to the library user to interpret
   *  the message body. *)
  | Ssh_agent_identities_answer : identity list
    -> [`Ssh_agentc_request_identities] ssh_agent_response
  | Ssh_agent_sign_response : string
    -> [`Ssh_agentc_sign_request] ssh_agent_response
[@@deriving sexp_of]

type any_ssh_agent_response =
  Any_response : 'a ssh_agent_response -> any_ssh_agent_response
[@@deriving sexp_of]

type request_handler =
  { handle : 'a . 'a ssh_agent_request -> 'a ssh_agent_response; }
(** Any function that takes a request and returns a valid response for the
 * request type *)

module Parse : sig
  val ssh_agent_message : extension:bool -> any_ssh_agent_response Angstrom.t
  (** [ssh_agentc_message ~extension] parses an ssh-agent response. If
   * [extension] is [true], then the message is parsed as a response to a
   * [Ssh_agentc_extension] request. *)
  val ssh_agentc_message : any_ssh_agent_request Angstrom.t
  (** A parser for ssh-agent requests *)
end

module Serialize : sig
  val write_ssh_agent_response
    : Faraday.t -> 'a ssh_agent_response -> unit
  val write_ssh_agent_request
    : Faraday.t -> 'a ssh_agent_request -> unit
end

val is_extension_request
  : 'a ssh_agent_request -> bool
(** [is_extension_request request] returns true if [request] is
 * [Ssh_agentc_extension]. Useful for passing [~extension] to
 * [ssh_agent_message]. *)

val unpack_any_response
  : 'a ssh_agent_request -> any_ssh_agent_response
  -> ('a ssh_agent_response, string) result
(** [unpack_any_response request response] unpacks [response] if it is a valid
 * response type with regard to [request], otherwise [Error] is returned. *)

(** [sign privkey flags blob] returns a signature of [blob] using [privkey]
 * respecting the hashing algorithms specified by [flags]. Currently, only RSA
 * signatures are supported. *)
val sign : Privkey.t -> Protocol_number.sign_flag list -> string -> string
