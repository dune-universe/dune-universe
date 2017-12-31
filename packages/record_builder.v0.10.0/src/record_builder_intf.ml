open Core_kernel

(** A subset of the operations of an applicative.

    This is only the operations we need to have for building a record.
*)
module type Partial_applicative_S = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t
end

module type Partial_applicative_S2 = sig
  type ('a, 'e) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

  val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
end

(** These are the types used for building records by folding over fields.

    You should never have to think about them, and should be able to
    skip straight on to [Record_builder]. They are exposed
    solely because they must be so that the
    application of [Fields.make_creator] will type-check.
*)
module type Make_creator_types = sig
    (** An internal state which is folded through the fields. *)
    type ('out, 'all_fields, 'extra) accum

    (** Each part of the fold has a type of this form. *)
    type ('field, 'head, 'tail, 'all_fields, 'extra) fold_step =
      ('head, 'all_fields, 'extra) accum
      -> ('all_fields -> 'field) * ('tail, 'all_fields, 'extra) accum

    (** A step of the fold over a single field has this type.

        Each argument to [Fields.make_creator] should take that field
        as an argument and return something of this type (see [field] below).
    *)
    type ('field, 'tail, 'all_fields, 'extra) handle_one_field =
      ( 'field
      , ('field, 'tail) Hlist.cons
      , 'tail
      , 'all_fields Hlist.nonempty
      , 'extra
      ) fold_step

    (** The overall fold of multiple steps created by applying
        [Fields.make_creator] without an initial value should have a type
        of this form. You then supply it as an argument to [build_for_record] below.
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
    applicative. They can be made using the functor [Record_builder.Make].

    This can be used to build all sorts of things, e.g. building
    a catalog form for a record type, or building a creator function
    which uses an applicative or monad like deferred to create the
    content for each field, perhaps useful when combined with
    [Quickcheck.Generator].

    e.g. {[
      module G = Quickcheck.Generator

      let form : t G.t =
        let module B = Record_builder.Make(G) in
        let labelled_string field =
          B.field (G.map String.gen ~f:(fun str -> sprintf "%s:%s" (Field.name field) str))
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
          G.map String.gen ~f:(fun str -> sprintf "%s:%s" (Field.name field) str)
        in
        G.map
          (G.both (labelled_string Field.forename) (G.both (labelled_string Field.surname) Date.gen)
             ~f:(fun (forename, (surname, birthday)) -> { forename; surname; birthday; })
    ]}
*)
module type Record_builder_S = sig
  type 'a applicative

  (** These types have to be exposed so that the typechecker
      can tell that the use of [Fields.make_creator] is well-typed.

      However, you shouldn't really have to think about them.
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
      which supplies the applicative terms to use for each field of the record. It
      performs the mapping and combining of these terms automatically.

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
