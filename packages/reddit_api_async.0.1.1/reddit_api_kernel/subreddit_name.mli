(** [Subreddit_name] is a string identifier module that does some normalization:

    - Hashes and comparisons are caseless.
    - "r/" and "/r/" prefixes are dropped.
    - "u/" and "/u/" prefixes cause the string to be converted to the user's subreddit.
*)

open! Core_kernel

type t

(** [all] is /r/all, a special subreddit that includes items from most subreddits.

    Exceptions:

    {ol
    {- Some subreddits are excluded by Reddit administrators.}
    {- Some subreddits opt out of inclusion.}
    {- Users can filter individual subreddits out of their view of /r/all.}}
*)
val all : t

(** [combine l] is a subreddit name representing the combination of the
    subreddits named in [l]. In general, when [combine l] is used as an API
    parameter the response contains items from all the subreddits in [l].
*)
val combine : t list -> t

(** [user_subreddit user] is the name of the special subreddit associated with
    the user's profile. Only the user can post to this subreddit, and on the
    desktop site links in this subreddit are associated with the user rather
    than with any visible subreddit. *)
val user_subreddit : Username.t -> t

include Identifiable.S with type t := t
