open! Core_kernel

type t [@@deriving sexp]

include Json_object.S_with_fields with type t := t

module Rule : sig
  type t [@@deriving sexp]

  include Json_object.S_with_fields with type t := t

  module Kind : sig
    type t =
      | Link
      | Comment
      | All
    [@@deriving sexp]
  end

  val kind : t -> Kind.t
  val description : t -> [ `markdown | `HTML ] -> string
  val short_name : t -> string
  val report_reason : t -> string
  val creation_time : t -> Time_ns.t
  val priority : t -> int
end

val subreddit_rules : t -> Rule.t list
val site_rules : t -> Json.t
val site_rules_flow : t -> Json.t
