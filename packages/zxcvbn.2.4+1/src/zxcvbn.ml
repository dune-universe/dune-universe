module Match = struct
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

external matches : string -> float * Match.t list = "zxcvbn_match_caml"
