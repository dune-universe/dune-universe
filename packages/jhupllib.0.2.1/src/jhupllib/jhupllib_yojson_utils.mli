(**
   This module contains utility functions for writing Yojson codec generators.
*)

open Batteries;;
open Yojson.Safe;;

(** Provides a serialization mechanism for a list data structure. *)
val list_to_yojson : ('t -> json) -> 't list -> json

(** Provides a serialization mechanism for a set data structure. *)
val set_to_yojson : ('t -> json) -> ('s -> 't Enum.t) -> 's -> json

(** Provides a serialization mechanism for a map data structure. *)
val map_to_yojson :
  ('k -> json) -> ('v -> json) -> ('m -> ('k * 'v) Enum.t) -> 'm -> json

(** The type of modules which give a Yojson serializer. *)
module type To_yojson_type =
sig
  type t
  val to_yojson : t -> json
end

(** A functor which generates a Yojson serializer for an existing functorized
    set module. *)
module Set_to_yojson :
  functor(S : Set.S)(Y : To_yojson_type with type t = S.elt) ->
  sig
    val to_yojson : S.t -> json
  end

(** A functor which generates a Yojson serializer for an existing functorized
    map module. *)
module Map_to_yojson :
  functor(M : Map.S)(Y : To_yojson_type with type t = M.key) ->
  sig
    val to_yojson : ('v -> json) -> 'v M.t -> json
  end
