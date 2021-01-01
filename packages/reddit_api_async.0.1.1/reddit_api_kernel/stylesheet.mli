open! Core_kernel

module Image : sig
  type t

  val url : t -> Uri.t
  val link : t -> string
  val name : t -> string
end

type t

include Json_object.S_with_fields with type t := t
include Json_object.S_with_kind with type t := t

val images : t -> Image.t list
val subreddit_id : t -> Thing.Subreddit.Id.t
val stylesheet_text : t -> string
