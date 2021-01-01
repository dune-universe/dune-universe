(** An inbox item is either a {!type:Thing.Message.t} or else a representation 
    of a comment with different fields from a {!Thing.Comment.t}.
*)

open! Core_kernel

module Comment : sig
  module Type : sig
    type t =
      | Comment_reply
      | Link_reply
    [@@deriving sexp]
  end

  type t [@@deriving sexp]

  include Json_object.S_with_fields with type t := t
  include Json_object.S_with_kind with type t := t

  val id : t -> Thing.Comment.Id.t
  val body : t -> [ `markdown | `HTML ] -> string
  val author : t -> Username.t option
  val subreddit : t -> Subreddit_name.t
  val creation_time : t -> Time_ns.t
  val score : t -> int
  val parent_id : t -> [ `Comment of Thing.Comment.Id.t | `Link of Thing.Link.Id.t ]
  val new_ : t -> bool
  val type_ : t -> Type.t
  val link_id : t -> Thing.Link.Id.t
  val link_title : t -> string
  val num_comments_in_thread : t -> int
end

type t =
  | Message of Thing.Message.t
  | Comment of Comment.t
[@@deriving sexp]

val of_json : Json.t -> t
