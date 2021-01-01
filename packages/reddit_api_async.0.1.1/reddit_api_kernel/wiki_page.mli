open! Core_kernel

module Id : sig
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

module Revision : sig
  type t [@@deriving sexp]

  include Json_object.S_with_fields with type t := t

  module Id : sig
    type t [@@deriving sexp]

    include Stringable with type t := t

    val to_uuid : t -> Uuid.t
    val of_uuid : Uuid.t -> t
  end

  val page_name : t -> string
  val id : t -> Id.t
  val reason : t -> string option
  val timestamp : t -> Time_ns.t
  val hidden : t -> bool
  val author : t -> Thing.User.t option
end

module Edit_conflict : sig
  type t [@@deriving sexp]

  include Json_object.S_with_fields with type t := t

  val diff : t -> string
  val message : t -> string
  val new_content : t -> string
  val new_revision : t -> Revision.Id.t
  val reason : t -> string option
end

module Permissions : sig
  type t [@@deriving sexp]

  include Json_object.S_with_fields with type t := t
  include Json_object.S_with_kind with type t := t

  module Level : sig
    type t =
      | Use_subreddit_wiki_permissions
      | Only_approved_contributors_for_this_page
      | Only_moderators
    [@@deriving sexp]

    val of_int_exn : int -> t
    val to_int : t -> int
  end

  val level : t -> Level.t
  val contributors : t -> Thing.User.t list
  val listed : t -> bool
end

type t [@@deriving sexp]

include Json_object.S_with_fields with type t := t
include Json_object.S_with_kind with type t := t

val may_revise : t -> bool
val revision_id : t -> Revision.Id.t
val revision_by : t -> Thing.User.t
val content : t -> [ `markdown | `HTML ] -> string
val revision_time : t -> Time_ns.t
val revision_reason : t -> string option
