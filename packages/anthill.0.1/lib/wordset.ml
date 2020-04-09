open Core

module S = Set.Make(String)

include S

let to_lower set =
  fold set ~init:empty ~f:(fun s i -> add s (String.lowercase i));;
