open Hashcons

module VisitorsHashcons = struct

  (* We CAN implement the method [visit_hash_consed], but this method requires
     a hash-consing table. We assume that this table is stored in the field
     [_table], which we declare virtual. *)

  (* A key subtlety is that the method [visit_hash_consed] must be monomorphic
     in ['b]. Indeed, we cannot hope to build values of type ['b hash_consed]
     for every ['b]. We can only hope to build values of type ['b hash_consed]
     for a fixed ['b], where the hash-consing table has type ['b Hashcons.t].
     For now, the type ['b] is undetermined. It will be fixed in a subclass,
     where the field [_table] is initialized. *)

  class virtual ['self] map = object (_ : 'self)
    val virtual _table: 'b Hashcons.t
    method visit_hash_consed: 'env 'a .
      ('env -> 'a -> 'b) ->
      'env -> 'a hash_consed -> 'b hash_consed
    = fun visit_'a env { node = e; _ } ->
        hashcons _table (visit_'a env e)
  end

end

(* This allows us to define the types [expr] and [hexpr] and generate a
   visitor class for them. *)

type 'expr oexpr =
  | EConst of int
  | EAdd of 'expr * 'expr

and hexpr =
  H of hexpr oexpr hash_consed [@@unboxed]

[@@deriving visitors { variety = "map"; polymorphic = ["'expr"];
                       ancestors = ["VisitorsHashcons.map"] }]

(* Once the type [hexpr] is defined, we can allocate a table. *)

let table : hexpr oexpr Hashcons.t =
  Hashcons.create 128

(* Inheriting [map] and defining [_table] yields a working visitor. *)

let id : hexpr -> hexpr =
  let o = object
    inherit [_] map
    val _table = table
  end in
  o # visit_hexpr ()
