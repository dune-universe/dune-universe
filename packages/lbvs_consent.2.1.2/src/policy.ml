
(* A consensus is parameterized by a policy/strategy that dictates how the
   consensus is built *)

module Log = Dolog.Log

type t =
  (* control strategies are not consensus queries per se *)
  | Single (* control *)
  | Opportunist (* control *)
  | Optimist (* v *)
  | Realist (* % *)
  | Knowledgeable (* activity-weighted % *)

let of_string = function
  | "sing" -> Single
  | "oppo" -> Opportunist
  | "opti" -> Optimist
  | "real" -> Realist
  | "know" -> Knowledgeable
  | other ->
    let () = Log.fatal "of_string: unknown strat: %s" other in
    exit 1

let to_string = function
  | Single -> "sing"
  | Opportunist -> "oppo"
  | Optimist -> "opti"
  | Realist -> "real"
  | Knowledgeable -> "know"
