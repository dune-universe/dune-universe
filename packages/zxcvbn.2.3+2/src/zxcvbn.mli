(** Bindings to the zxcvbn password strength estimation library *)

module Match : sig
  type kind =
    | Non_match
    | Brute_match
    | Dictionnary_match
    | Dict_leet_match
    | User_match
    | User_leet_match
    | Repeats_match
    | Sequence_match
    | Spatial_match
    | Date_match
    | Year_match
    | Unknown_match of int
  [@@deriving eq, ord, show]

  type t =
    { beginning: int
    ; length: int
    ; entropy: float
    ; kind: kind
    ; multipart: bool
    ; multipart_entropy: float
    }
  [@@deriving eq, ord, show]
end

(** Return the overall password entropy estimation and the list of matches.
    The underlying C function expects a null terminated string so keep in mind
    that if the given string contains null chars, only the substring until the
    first null char will be evaluated. *)
val matches : string -> float * Match.t list
