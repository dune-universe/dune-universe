open! Core_kernel

module type S = sig
  type t

  val of_json : Json.t -> t
  val to_json : t -> Json.t
end

module type S_with_fields = sig
  include S

  val get_field : t -> string -> Json.t option
  val get_field_exn : t -> string -> Json.t
  val field_map : t -> Json.t String.Map.t
end

module type S_with_kind = sig
  include S

  val kind : string
end

module type Kinded_param = sig
  type t

  val of_data_field : Json.t -> t
  val to_data_field : t -> Json.t
  val kind : string
end

module type Json_object = sig
  module type S = S
  module type S_with_fields = S_with_fields
  module type S_with_kind = S_with_kind

  module Utils : sig
    include S_with_fields with type t = Json.t String.Map.t
    include Sexpable.S with type t := t

    val optional_field : string -> (Json.t -> 'a) -> t -> 'a option
    val required_field : string -> (Json.t -> 'a) -> t -> 'a
    val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    val or_null : (Json.t -> 'a) -> Json.t -> 'a option
    val int : Json.value -> int
    val float : Json.value -> float
    val bool : Json.value -> bool
    val string : Json.value -> string
    val username : Json.value -> Username.t
    val subreddit_name : Json.value -> Subreddit_name.t
    val time : Json.value -> Time_ns.t
    val uri : Json.value -> Uri.t
  end

  module Make_kinded (Param : Kinded_param) : S_with_kind with type t := Param.t

  module Make_kinded_simple (Param : sig
    val kind : string
  end) : S_with_kind with type t := Utils.t
end
