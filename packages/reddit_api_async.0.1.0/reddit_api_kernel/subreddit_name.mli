(** [Subreddit_name] is a string identifier module that does some normalization:

    - Hashes and comparisons are caseless.
    - "r/" and "/r/" prefixes are dropped.
    - "u/" and "/u/" prefixes cause the string to be converted to the user's subreddit.
*)

open! Core_kernel
include Identifiable.S

val user_subreddit : Username.t -> t
val all : t
val combine : t list -> t
