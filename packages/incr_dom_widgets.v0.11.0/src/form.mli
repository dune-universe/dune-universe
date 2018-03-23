open Core_kernel
(** Module for creating and manipulating web forms. *)

(** {1 Typed Id} *)
(** [Id.t] is an identifier for form elements *)
module Id : sig
  type 'a t
end

(** [Block.t Id.t] is an identifier for a block in a form. *)
module Block : sig
  type t
end

(** [List_id.t] is an identifier for a list of form elements in a form. *)
module List_id : sig
  type 'a t
end

(** [Variant_id.t] is an identifier for a variant selection in a form. *)
module Variant_id : sig
  type 'a t
end

(** {1 Form validation} *)
module Form_error : sig
  (** Represents an error created during form validation. *)
  type t

  (** Create an error attached to the given [Id.t]. *)
  val create : Error.t -> id:'a Id.t -> t
end

(** {1 Form Description} *)

(** Module to create a form blueprint by taking basic building blocks
    (like bool or string fields) and combining them into more complex
    structures which can be themselves combined with other structures. *)
module Description : sig

  (** Describes how to edit a type as a series of form elements.

      The two type parameters ['s] and ['g] represent respectively
      the type that will be edited (set), and the type of the returned edited
      value (get), so they will be the same when a form is actually used.

      The type parameter ['ids] represents the different form elements used
      by the form for editing, and contains (in some tuple structure) all of
      the ids that should be used for those form elements.
  *)
  type ('g, 's, 'ids) t

  val not_editable : default:'a -> ('a, 'a, unit) t

  val bool : (bool, bool, bool Id.t) t
  val string : (string, string, string Id.t) t

  val sexp : of_sexp:(Sexp.t -> 'a) -> sexp_of:('a -> Sexp.t) -> ('a, 'a, string Id.t) t

  (** Raises an exception if the provided list is empty or contains duplicate values. *)
  val variant : 'a list -> equal:('a -> 'a -> bool) -> ('a, 'a, 'a Variant_id.t) t

  (** Combine two [t]'s. *)
  val both : ('a, 's, 'a_ids) t -> ('b, 's, 'b_ids) t -> ('a * 'b , 's, 'a_ids * 'b_ids) t

  val map : ('a, 's, 'ids) t -> f:('a -> 'b) -> ('b, 's, 'ids) t
  val contra_map : ('g, 'b, 'ids) t -> f:('a -> 'b) -> ('g, 'a, 'ids) t

  (** Perform a conversion which also validates the data, and so may have errors. *)
  val conv
    :  ('a, 's, 'ids) t
    -> f:('a -> 'ids -> block_id:Block.t Id.t -> ('b, Form_error.t list) Result.t)
    -> ('b, 's, Block.t Id.t * 'ids) t

  val conv_without_block
    :  ('a, 's, 'ids) t
    -> f:('a -> 'ids -> ('b, Form_error.t list) Result.t)
    -> ('b, 's, 'ids) t

  val list :
    ('g, 's, 'ids) t -> ('g list, 's list, 'ids list * 's List_id.t) t

  module Let_syntax : sig
    module Let_syntax : sig
      val map : ('a, 's, 'ids) t -> f:('a -> 'b) -> ('b, 's, 'ids) t
      val both : ('a, 's, 'a_id) t -> ('b, 's, 'b_id) t -> ('a * 'b , 's, 'a_id * 'b_id) t

      module Open_on_rhs : sig
        (** Infix alias for [map]. *)
        val (^<) : ('a -> 'b) -> ('a, 's, 'ids) t -> ('b, 's, 'ids) t
        (** Infix alias for [contra_map]. *)
        val (<^) : ('g, 'b, 'ids) t -> ('a -> 'b) -> ('g, 'a, 'ids) t

        val bool : (bool, bool, bool Id.t) t
        val string : (string, string, string Id.t) t
        val list :
          ('g, 's, 'ids) t -> ('g list, 's list, 'ids list * 's List_id.t) t
        val variant : 'a list -> equal:('a -> 'a -> bool) -> ('a, 'a, 'a Variant_id.t) t
      end
    end
  end

  (** Build a form for a record directly, in the same manner as [Record_builder]
      or [Profunctor].

      e.g. {[
        let animal_form : (Animal.t, Animal.t, string Id.t * (bool Id.t * unit)) t =
          Form.Description.Of_record.build_for_record (
            Animal.Fields.make_creator
              ~species:(field Form.Description.string)
              ~can_quack:(field Form.Description.bool))
        ;;
      ]}

      Is equivalent† to: {[
        let animal_form : (Animal.t, Animal.t, string Id.t * (bool Id.t * unit)) t =
          let open Form.Description in
          map (
            both (contra_map ~f:Animal.species string) @@
            both (contra_map ~f:Animal.can_quack bool) @@
            Hlist.nil)
            ~f:(fun (species, (can_quack, ())) -> { Animal.species; can_quack; })
        ;;
      ]}

      † except that there is no risk of name shadowing as there would be here,
      but I wrote the local open only for brevity.
  *)
  module Of_record : sig
    (** These are the types used for building a form by combining record fields.

        You should never have to think about them, but they have to be exposed
        for type-checking to work correctly.
    *)
    module Make_creator_types : sig
      type ('tail, 'tail_ids, 'all_fields, 'all_ids, 'record) accum

      type ('field, 'head, 'head_ids, 'tail, 'tail_ids, 'all_fields, 'all_ids, 'record) fold_step =
        ('head, 'head_ids, 'all_fields, 'all_ids, 'record) accum
        -> ('all_fields -> 'field) * ('tail, 'tail_ids, 'all_fields, 'all_ids, 'record) accum

      type ('field, 'field_ids, 'tail, 'tail_ids, 'all_fields, 'all_ids, 'record) handle_one_field =
        ( 'field
        , ('field, 'tail) Record_builder.Hlist.cons, ('field_ids, 'tail_ids) Record_builder.Hlist.cons
        , 'tail, 'tail_ids
        , 'all_fields Record_builder.Hlist.nonempty, 'all_ids Record_builder.Hlist.nonempty
        , 'record
        ) fold_step

      type ('all_fields, 'all_ids, 'record) handle_all_fields =
        ( 'record
        , 'all_fields Record_builder.Hlist.nonempty, 'all_ids Record_builder.Hlist.nonempty
        , Record_builder.Hlist.nil, Record_builder.Hlist.nil
        , 'all_fields Record_builder.Hlist.nonempty, 'all_ids Record_builder.Hlist.nonempty
        , 'record) fold_step
    end

    (** Handle one field of a record using the supplied editor.

        This should be used as an argument to [Fields.make_creator], see the example.
    *)
    val field : ('field, 'field, 'field_ids) t
      -> ('record, 'field) Field.t
      -> ('field, 'field_ids, _, _, _, _, 'record) Make_creator_types.handle_one_field

    (** Build the form for a whole record.

        This is used with an application of [Fields.make_creator] as shown in the example.
    *)
    val build_for_record
      : (_, 'ids, 'record) Make_creator_types.handle_all_fields
      -> ('record, 'record, 'ids) t
  end
end

type ('a, 'ids) t
type ('a, 'ids) form = ('a, 'ids) t

(** Create a form from a form description. The name will only be used to provide
    better error messages. *)
val create : name:string -> ('a, 'a, 'ids) Description.t -> ('a, 'ids) t

val to_description : ('a, 'ids) t -> ('a, 'a, 'ids) Description.t

(** {1 Form state} *)
module State : sig
  (** Represent the state of a form.
      A state does not have any type parameter so that it's easier to embed in other data
      structure (like [Incr_dom.App_intf.Simple.Model.t]).

      A state is used in conjunction with the form it was created from. If you
      use a state with any other form an exception will be thrown. *)
  type t [@@deriving compare]

  (** Raises an exception if any initial value for variants is not a part of the options
      available in [form]. *)
  val create : ?init:'a -> ('a, _) form -> t

  (** Returns all ids defined in the given form. *)
  val field_ids : t -> (_, 'ids) form -> 'ids

  (** Try to read the value contained in a form.
      Errors, if any, will be stored in the returned state. *)
  val read_value : t -> ('a, _) form -> 'a option * t

  (** Return the error attached to a given id, if any. *)
  val error : t -> _ Id.t -> Error.t option

  (** Return all errors associated with the form state *)
  val errors : t -> Error.t list

end

(** {1 List module} *)

(** Provides operations related to lists of forms. *)
module List : sig

  type 'a list_modifier = {
    transform : 'elt. 'elt list -> create_new_form:(?init:'a -> unit -> 'elt) -> 'elt list
  }

  (** Modify list with a supplied [list_modifier] function.

      Raises an exception if the resulting list contains duplicated instances of
      previously existing elements (although duplicating new elements is fine). *)
  val modify_list : State.t -> 'a List_id.t -> f:'a list_modifier -> State.t

  (** Add one empty form in front of the list *)
  val cons   : State.t -> ?init:'a -> 'a List_id.t -> State.t

  (** Add one empty form to the end of the list. *)
  val append : State.t -> ?init:'a -> 'a List_id.t -> State.t

  (** Remove n-th form from the list.
      If [int] is invalid index (i.e. [int] < 0 or [int] >= length of the
      list), the list remains unchanged and no exceptions are raised. *)
  val remove_nth : State.t -> 'a List_id.t -> int -> State.t

end

(** {1 Input module} *)

(** Wrappers to create VDom input fields using field ids. *)
module Input : sig

  open Incr_dom.Vdom

  val text     : State.t -> string    Id.t -> Attr.t list -> Node.t

  val textarea : State.t -> string    Id.t -> Attr.t list -> Node.t

  val checkbox : State.t -> bool      Id.t -> Attr.t list -> Node.t

  val radio_button : State.t -> 'a Variant_id.t -> Attr.t list -> ('a * Node.t) list

  (** Dropdown for all initially provided values optionally sorted using the provided
      compare function.  *)
  val dropdown
    : State.t
    -> 'a Variant_id.t
    -> Attr.t list
    -> ?compare:('a -> 'a -> int)
    -> ('a -> string)
    -> Node.t

end
