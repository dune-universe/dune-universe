(** OCaml bindings to {{:http://www.libsrs2.org/} libsrs2}. *)

type t
  (** The type of an SRS instance. *)

exception Error of string
  (** The exception raised by SRS functions in case of errors. *)

val create : unit -> t
  (** Creates a new SRS instance using [=] as the SRS separator, [21] as the
      SRS address validity period, [4] as the SRS hash length and hash
      minimum length. *)

val add_secret : t -> string -> unit
  (** Adds a new secret to an SRS instance. Only the secret used in the
      first call to [add_secret] is used for forward-rewriting addresses.
      All secrets are attempted for reverse-rewriting addresses. Must be
      called at least once. *)

val forward : t -> string -> string -> string
  (** [forward srs sender fwd] rewrites [sender] to look like an address
      from domain [fwd] using the information from SRS instance [srs]. *)

val reverse : t -> string -> string
  (** [reverse srs sender] rewrites an SRS-rewritten address [sender] back
      into its original form using the information from SRS instance [srs]. *)

val set_separator : t -> char -> unit
  (** Sets the character used as the initial SRS separator, that is, the one
      immediately after the [SRS0] or [SRS1] tags. Valid separators are [=],
      [+] and [-]. *)

val get_separator : t -> char
  (** Gets the character used as the initial SRS separator. *)

val set_max_age : t -> int -> unit
  (** Sets the amount of time (in days) during which SRS-rewritten addresses
      are valid. *)

val get_max_age : t -> int
  (** Gets the maximum SRS address age. *)

val set_hash_length : t -> int -> unit
  (** Sets the length of the hash part of SRS-rewritten addresses. *)

val get_hash_length : t -> int
  (** Gets the length of the hash part of SRS-rewritten addresses. *)

val set_hash_min : t -> int -> unit
  (** Sets the minimum length of an SRS hash. *)

val get_hash_min : t -> int
  (** Gets the minimum length of an SRS hash. *)

val set_no_forward : t -> bool -> unit
  (** If set to true, the SRS instance will not perform forward rewritting. *)

val get_no_forward : t -> bool
  (** Gets the value set by {!set_no_forward}. *)

val set_no_reverse : t -> bool -> unit
  (** If set to true, the SRS instance will not perform reverse rewritting. *)

val get_no_reverse : t -> bool
  (** Gets the value set by {!set_no_reverse}. *)

val make : string * string list -> int -> int -> char -> t
  (** Helper function to create and initialize an SRS instance in one step.
      [make (secret, secrets) max_age hash_len sep] will create an SRS
      instance using [secret] as the initial secret and [secrets] as additional
      secrets. The address validity duration will be set to [max_age], the
      SRS hash length will be set to [hash_len] and the SRS separator will be
      set to [sep]. *)
