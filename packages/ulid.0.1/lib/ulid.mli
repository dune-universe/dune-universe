(** ULID implementation.

    ULIDs are Universally Unique Lexicographically Sortable Identifier
    See the spec at https://github.com/ulid/spec
    
    {e Release %%VERSION%% - %%PKG_HOMEPAGE%%} *)

val get_nocrypto_rng : unit -> int -> int
(** A getter for the initialized nocrypto RNG in function form int -> int. *)

val encode_time : int -> int -> string
(** Encodes a UNIX timestamp in milliseconds into Base32. *)

val encode_random : int -> (int -> int) -> string
(** Encodes specified number of random ints into Base32 using specified PRNG. *)

val increment_base_32 : string -> string
(** Increments a Base32 encoded string by 1 *)

val ulid : ?seed_time:int -> unit -> string
(** Returns a randomly generated ULID using the current time or an optional seed time *)

val ulid_factory :
  ?prng:(int -> int) -> unit -> ?seed_time:int -> unit -> string
(** Returns a function to generate ULIDs using an optionally specified PRNG *)

val monotonic_factory :
  ?prng:(int -> int) -> unit -> ?seed_time:int -> unit -> string
(** Returns a function to generate monotonic ULIDs using an optionally specified PRNG *)
