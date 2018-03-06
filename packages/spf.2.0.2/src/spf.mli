(** OCaml bindings to {{:http://www.libspf2.org/} libspf2}. It is strongly
    recommended that a {{:https://github.com/andrenth/libspf2} patched libspf2
    with fixes sent to the SPF-devel mailing list} is used instead of the
    current official libspf2 release (version 1.2.9).
*)

(** The type of SPF server instances. *)
type server

(** The DNS scheme to be used by SPF. *)
type dns = Dns_resolv
         | Dns_cache
         | Dns_zone

(** The type of SPF requests. *)
type request

(** The type of SPF responses. *)
type response

(** The type of SPF comments. *)
type comments

(** The type of SPF reason. *)
type reason

(** The type of SPF results. *)
type result
  = Invalid
  | Neutral of comments
  | Pass
  | Fail of comments
  | Softfail of comments
  | None
  | Temperror
  | Permerror

exception Error of string
  (** Exception raised by SPF functions in case of errors. *)

val server : ?debug:bool -> dns -> server
  (** Allocates a new SPF server instance. *)

val free_server : server -> unit
  (** Frees memory allocated to the given {!server}. *)

val request : server -> request
  (** Create a new SPF request. *)

val free_request : request -> unit
  (** Frees memory allocated to the given {!request}. *)

val check_helo : server -> Unix.inet_addr -> string -> response
  (** [check_helo spf addr helo] performs an SPF check on address [addr]
      on the SPF record for the HELO name [helo] using SPF server [spf]. *)

val check_from : server -> Unix.inet_addr -> string -> response
  (** [check_helo spf addr from] performs an SPF check on address [addr]
      on the SPF record of the domain of the MAIL FROM address [from]. *)

val result : response -> result
  (** Extracts an {!result} from an {!response}. *)

val reason : response -> reason
  (** Extracts an {!reason} from an {!response}. *)

val received_spf : response -> string

val received_spf_value : response -> string
  (** Extracts the [Received-SPF] header value from an {!response}. *)

val header_comment : response -> string
  (** Extracts the comment part of the [Received-SPF] header value from an
      {!response}. *)

val smtp_comment : comments -> string
  (** Extracts the SMTP comment part from {!comments}. *)

val explanation : comments -> string
  (** Extracts the explanation part from {!comments}. *)

val string_of_result : result -> string
  (** Converts an {!result} to a string. *)

val string_of_reason : reason -> string
  (** Converts an {!reason} to a string. *)
