open Base

(** A subset of the operations of an {{!modtype:Base.Applicative.S}applicative}.
*)
module type Partial_applicative_S = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t
end

(** A subset of the operations of an {{!modtype:Base.Applicative.S2}applicative}
    with two type parameters.
*)
module type Partial_applicative_S2 = sig
  type ('a, 'e) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

  val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
end

(** The types used internally for building records by folding over fields
    — you shouldn't need to know them, but they must be exposed to allow
    type-checking to succeed.
*)
module type Make_creator_types = sig
  (** An internal state which is folded through the fields. *)
  type ('out, 'all_fields, 'extra) accum

  (** A step of the fold over one or more fields. *)
  type ('field, 'head, 'tail, 'all_fields, 'extra) fold_step =
    ('head, 'all_fields, 'extra) accum
    -> ('all_fields -> 'field) * ('tail, 'all_fields, 'extra) accum

  (** A step of the fold over one field.

      Each argument to [Fields.make_creator] should take that field
      as an argument and return something of this type.
  *)
  type ('field, 'tail, 'all_fields, 'extra) handle_one_field =
    ( 'field
    , ('field, 'tail) Hlist.cons
    , 'tail
    , 'all_fields Hlist.nonempty
    , 'extra
    ) fold_step

  (** A step of the fold over all fields of a record.

      The application of [Fields.make_creator] to all of the fields,
      but without an initial value for the fold, should have this type
      and be passed to {!Record_builder_S.build_for_record}.
  *)
  type ('record, 'all_fields, 'extra) handle_all_fields =
    ( 'record
    , 'all_fields Hlist.nonempty
    , Hlist.nil
    , 'all_fields Hlist.nonempty
    , 'extra
    ) fold_step
end

(** Modules of this type are used to traverse a record using a specific
    applicative. They can be made using the functor {!Record_builder.Make}.

    For example, we could traverse a record to create a quickcheck generator
    for it.

    {[
      type t = {
        forename: string;
        surname: string;
        birthday: Date.t;
      } [@@deriving fields]

      module G = Quickcheck.Generator

      let form : t G.t =
        let module B = Record_builder.Make(G) in
        let labelled_string field =
          B.field (G.map String.gen ~f:(fun str ->
            sprintf "%s:%s" (Field.name field) str))
        in
        B.build_for_record (
          Fields.make_creator
            ~forename:labelled_string
            ~surname:labelled_string
            ~birthday:(B.field Date.gen))
    ]}

    Is equivalent to: {[
      let form : t G.t =
        let labelled_string field =
          G.map String.gen ~f:(fun str ->
            sprintf "%s:%s" (Field.name field) str)
        in
        G.both (labelled_string Field.forename)
          (G.both (labelled_string Field.surname) Date.gen)
        |> G.map ~f:(fun (forename, (surname, birthday)) ->
          { forename; surname; birthday; })
    ]}
*)
module type Record_builder_S = sig
  type 'a applicative

  (** The types used internally for building records by folding over fields
      — you shouldn't need to know them, but they must be exposed to allow
      type-checking to succeed.
  *)
  module Make_creator_types : Make_creator_types

  (** Supply the term for one field.

      The type of this function is designed to match up with [Fields.make_creator]
      (see the example).
  *)
  val field : 'field applicative
    -> ('record, 'field) Field.t
    -> ('field, _, _, _) Make_creator_types.handle_one_field

  (** Build the overarching applicative for the whole record.

      This takes a partial application of [Fields.make_creator] as its argument,
      which should supply no initial value but use {!field} to supply a term
      for every field of the record.

      The type of this is designed to match up with [Fields.make_creator]
      (see the example).
  *)
  val build_for_record
    : ('record, _, _) Make_creator_types.handle_all_fields
    -> 'record applicative
end

module type Record_builder_S2 = sig
  type ('a, 'e) applicative

  module Make_creator_types : Make_creator_types

  val field : ('field, 'e) applicative
    -> ('record, 'field) Field.t
    -> ('field, _, _, 'e) Make_creator_types.handle_one_field

  val build_for_record
    : ('record, _, 'e) Make_creator_types.handle_all_fields
    -> ('record, 'e) applicative
end
