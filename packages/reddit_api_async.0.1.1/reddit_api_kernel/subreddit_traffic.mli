open! Core_kernel

type t [@@deriving sexp]

include Json_object.S_with_fields with type t := t

module By_date : sig
  type t =
    { date : Date.t
    ; uniques : int
    ; pageviews : int
    ; subscriptions : int
    }
  [@@deriving sexp]
end

module By_month : sig
  type t =
    { year : int
    ; month : Month.t
    ; uniques : int
    ; pageviews : int
    }
  [@@deriving sexp]
end

module By_hour : sig
  type t =
    { hour : Time_ns.t
    ; uniques : int
    ; pageviews : int
    }
  [@@deriving sexp]
end

val by_date : t -> By_date.t list
val by_month : t -> By_month.t list
val by_hour : t -> By_hour.t list
