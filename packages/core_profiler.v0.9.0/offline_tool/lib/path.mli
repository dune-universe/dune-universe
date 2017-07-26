open! Core
open Core_profiler

(** The first and last points are stored explicitly, not least to ensure that there are
    two of them. The list of points in the middle is stored in reverse order for
    convenience when checking the path.

    In a path, we might demand that we go from A -> B directly, or A -> B possibly via
    some other points. This is represented as  [{ first = Direct_point a; last = b; ... }]
    and [{ first = Point a; last = b; ... }] respectively; that is, whether or not it must
    go directly is stored on the source, not the destination.

    A final example:
      [{ first = Point a; middle_rev = [Direct_point b]; last = c }]
    matches any sequence of points that starts at a, goes (possibly via some other
    distinct points) to b, and then directly from b to c.

    This and the [Event_generator.t] do not have the full power of regular expressions
    Notably, the same point may not appear in a path twice, except for when its
    second appearance is the last point in the path.
*)

type 'a point =
  | Direct_point of 'a
  | Point of 'a
[@@deriving sexp, compare]

type 'a t =
  { first : 'a point
    (* i.e., penultimate point first *)
  ; rest_rev : 'a point list
  ; last : 'a
  }
[@@deriving sexp, compare]

module I : sig
  type id_path = Probe_id.t t [@@deriving sexp, compare]
  type t = id_path      [@@deriving sexp, compare]
  include Comparable.S with type t := t
  include Hashable  .S with type t := t
end

val string_t_of_string : string -> string t option
val string_t_to_string : string t -> string

val examples : string t list

val readme : string Lazy.t

val lookup_ids : string t -> Util.Name_map.group -> Probe_id.t t
val lookup_names : Probe_id.t t -> Reader.Header.t -> string t

val id_t_to_string : Probe_id.t t -> ?with_group:string -> Reader.Header.t -> string

(** Get [t.first], discarding whether the first point is [Direct_point] or a [Point] *)
val first : 'a t -> 'a
val last : 'a t -> 'a
