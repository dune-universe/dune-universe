open Core_kernel

type 'a t

(** This is a module for building forms that allow the user to edit complicated types
    (for example, configs).

    ['a Sexp_form.t] gives you a form for editing values of type ['a].

    It is called [Sexp_form] because it relies on the fact that ['a] can be represented
    as a sexp. Specifically, it is capable of parsing the sexp representation of ['a]
    to fill out the default values for the fields of the form automatically.

    This means that if you have a type ['a] which defines [sexp_of_a], you can
    specify a default value of type ['a] when rendering the ['a Sexp_form.t].

    [Sexp_form] has other useful functionality beyond just parsing default values.

    For example, [ list : 'a Sexp_form.t -> 'a list Sexp_form.t ] converts a form
    for editing ['a] into a form for editing ['a list].

    This gives the user buttons to add and delete elements and remembers deleted elements
    so that the user can undo.

    Note: If you can't convert ['a] to a sexp, you can still construct a ['a Sexp_form.t].
    You will just be unable to specify default values for the form.
*)
module Init : sig
  type 'a t

  val simple : 'a t

  (** Used to display [default] as the initial value of your sexp_form.
      Also displays [diff] for the user to understand how the values entered into
      the form differ from the [default] value.
      [between] is a node which will be displayed between the form and the diff.

  *)
  val with_default
    :  sexp_of_t:('a -> Sexp.t)
    -> default:'a
    -> diff:(original:Sexp.t -> updated:Sexp.t -> Incr_dom.Vdom.Node.t)
    -> ?between:Incr_dom.Vdom.Node.t
    -> unit
    -> 'a t

  (** [no_diff] produces an empty [Node.t]. *)
  val no_diff : original:Sexp.t -> updated:Sexp.t -> Incr_dom.Vdom.Node.t
end

val to_interactive : init:'a Init.t -> 'a t -> 'a Or_error.t Incr_dom_interactive.t

(** A ['a Case.t] is implemented as a ['a Sexp_form.t].
    When you create a case from a variant constructor for variant [Foo.t],
    say a constructor for type [int * string], you get a
    [(int -> string -> Foo.t) Case.t].

    For more information, see the documentation for [variant].
*)
module Case : sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

(** [('record, 'a) Record_builder.t] is implemented as a ['a Sexp_form.t].
    The ['record] only exists for additional type safety.

    For more information, see the documentation for [record].
*)
module Record_builder : sig
  type ('record, 'a) t
end

(** [('record, 'a) Record_field.t] is implemented as a ['a Sexp_form.t].
    The ['record] only exists for additional type safety.

    For more information, see the documentation for [record].
*)
module Record_field : sig
  type ('record, 'a) t
end

module Primitives : sig
  val string : ?placeholder:string -> ?width:int -> unit -> string t
  val nonempty_string : ?placeholder:string -> ?width:int -> unit -> string t
  val int : int t
  val positive_int : int t
  val non_negative_int : int t
  val option : 'a t -> 'a option t
  val bool_true_false : bool t
  val bool_yes_no : bool t

  (** [element_name] is used for buttons. If you provide ~element_name:"foo", then the
      button will say "Add foo" instead of "Add".

      [gated_deletion] means that the user must check a checkbox to enable the delete
      buttons. This is not strictly necessary, since the list maintains a "deleted stack"
      internally so that if you delete an element, the next time you press "add" it will
      add back the deleted element. However, users might not be aware of this
      functionality.

      [order] indicates whether the order of the list elements is important. If it's not
      important, we only need one add button; if it's important, we need an add button
      between every pair of elements.
  *)
  val list
    :  ?element_name:string
    -> ?gated_deletion:unit
    -> ?max_size:int
    -> ?add_and_remove_button_attrs:Incr_dom.Vdom.Attr.t list
    -> ?editor_message_attr:Incr_dom.Vdom.Attr.t
    -> order:[ `Ordered | `Unordered ]
    -> 'a t
    -> 'a list t

  val assoc_map
    :  ?element_name:string
    -> ?gated_deletion:unit
    -> ?max_size:int
    -> key:'a t
    -> data:'b t
    -> of_alist_exn:(('a * 'b) list -> 'c)
    -> unit
    -> 'c t

  val set
    :  ?element_name:string
    -> ?gated_deletion:unit
    -> ?max_size:int
    -> of_list:('a list -> 'c)
    -> 'a t
    -> 'c t

  (** Building a record works like this: let's say you have a record ['r] which has n
      fields of types ['f1], ..., ['fn].

      First you call [record ~create], supplying a create function of type
      ['f1 -> ... -> 'fn -> 'r]. In most cases, your create function can delegate to
      [Fields.create] as in the usage example below.

      This gives you a [('r, 'f1 -> ... -> 'fn -> 'r) Record_builder.t]. The first type
      parameter ['r] is a phantom type which exists only for additional type safety.

      You turn this into a [('r, 'r) Record_builder.t] by applying the <.*> operator once
      for each field, in order. First, you call it with a [('r, 'f1) Record_field.t] to
      get a [('r, 'f2 -> .. -> 'fn -> 'r) Record_builder.t], then with a
      [('r, 'f2) Record_field.t] to get a
      [('r, 'f3 -> ... -> 'fn -> 'r) Record_builder.t], and so on until you end up with a
      [('r, 'r) Record_builder.t]. You can obtain a [Record_field.t] using the [field]
      function.

      Finally you convert your [('r, 'r) Record_field.t] into a ['r Sexp_form.t] using
      [finish_record].

      Usage example:

      {[
        module Foo = struct
          type t =
            { a : string
            ; b : int
            }
          [@@deriving fields, sexp_of]
        end

        let foo : Foo.t Sexp_form.t =
          let open Sexp_form.Primitives in
          let module Fields = Foo.Fields in
          record ~create:(fun a b -> Fields.create ~a ~b)
          <.*> field (string ()) Fields.a
          <.*> field int         Fields.b
          |> finish_record
      ]}
  *)
  val record : create:('a -> 'b) -> ('record, 'a -> 'b) Record_builder.t

  (** See documentation for [record]. *)
  val field : 'a t -> ('record, 'a) Field.t -> ('record, 'a) Record_field.t

  (** See documentation for [record]. *)
  val sexp_option_field
    :  'a t
    -> ('record, 'a option) Field.t
    -> ('record, 'a option) Record_field.t

  (** See documentation for [record]. *)
  val ( <.*> )
    :  ('record, 'a -> 'b) Record_builder.t
    -> ('record, 'a) Record_field.t
    -> ('record, 'b) Record_builder.t

  (** See documentation for [record]. *)
  val finish_record : ('record, 'record) Record_builder.t -> 'record t

  (** This has some special behaviour, namely that if the input string contains
      no parentheses or quotes then we will treat it as an atom -- so the user can enter
      [foo bar] when a direct conversion using [Sexp.of_string] followed by [t_of_sexp]
      would only accept ["foo bar"]. It can also override any error produced by ppx_sexp
      using [on_error], since the errors produced by ppx_sexp are not always easy for the
      user to understand.
  *)
  val from_ppx_sexp
    :  t_of_sexp:(Sexp.t -> 'a)
    -> ?on_error:(Error.t -> Error.t)
    -> unit
    -> 'a t

  (** [variant] takes a list of [Case.t]s. [Case.t]s are created by [case] and [case_raw].
      When you create a case from a variant constructor for variant ['v],
      say a case with arguments of types ['a1], ..., ['an], you get a
      [('a1 -> ... -> 'an -> 'v) Case.t].
      You turn this into a ['v Case.t] by applying the <|*> operator once per argument.
      Then you turn a ['v Case.t list] into a ['v Sexp_form.t] using [variant].

      Usage example:

      {[
        module Foo = struct
          type t =
            | A
            | B of int
            | C of int * string
          [@@deriving variants, sexp_of, compare]
        end

        let foo : Foo.t Sexp_form.t =
          let open Sexp_form.Primitives in
          variant
            (Variants.fold
               ~init:[]
               ~a:(fun acc a ->  case a                          :: acc)
               ~b:(fun acc b -> (case b <|*> int)                :: acc)
               ~c:(fun acc c -> (case c <|*> int <|*> string ()) :: acc))
      ]}

      By default, the error message lists all the possible variants, as in
      "Please select an option: Foo | Bar". If there are a lot of variants
      and you don't want this, you can use [don't_state_options_in_error].
  *)
  val variant : ?don't_state_options_in_error:unit -> 'a Case.t list -> 'a t

  (** Mainly useful for polymorphic variants. For example, the following produces a
      [ `Foo of int ] Case.t:

      {[
        case_raw ~name:"Foo" ~constructor:(fun x -> `Foo x) <|*> positive_int
      ]}
  *)
  val case_raw : name:string -> constructor:'a -> 'a Case.t

  (** See documentation for [variant]. *)
  val case : 'a Variantslib.Variant.t -> 'a Case.t

  (** See documentation for [variant]. *)
  val ( <|*> ) : ('a -> 'b) Case.t -> 'a t -> 'b Case.t

  (** Allows the user to choose from a list of ['a]s, which are displayed using
      the provided names in the dropdown, or enter their own choice by selecting
      "Other" in the dropdown. *)
  val dropdown_with_other
    :  other:'a t
    -> sexp_of_t:('a -> Sexp.t)
    -> (string * 'a) list
    -> 'a t

  (** This function is a wrapper around [variant] and [case_raw].
      It's useful with [ppx_enumerate]:

      {[
        module Foo = struct
          type t =
            | A
            | B
            | C
            | D
          [@@deriving enumerate, variants]
        end

        let foo : Foo.t Sexp_form.t =
          Sexp_form.Primitives.enumeration Foo.all ~to_string:Foo.Variants.to_name
      ]}

      This is displayed as a dropdown with options A, B, C, and D.

      By default, the error message lists all the possible variants, as in
      "Please select an option: Foo | Bar". If there are a lot of variants
      and you don't want this, you can use [don't_state_options_in_error].
  *)
  val enumeration
    :  ?don't_state_options_in_error:unit
    -> 'a list
    -> to_string:('a -> string)
    -> (* You can use [ppx_variants] for this. *)
    'a t

  (** For recursive data types.
      Note: [recursive Fn.id] will go into an infinite loop when you call [to_interactive]
      on it.

      Here's an example of proper usage:
      {[
        module MyList = struct
          type t =
            | Nil
            | Cons of int * t
          [@@deriving variants]
        end
        let my_list_editor : MyList.t Sexp_form.t =
          let open Sexp_form.Primitives in
          recursive (fun my_list_editor ->
            variant
              (MyList.Variants.fold
                 ~init:[]
                 ~nil: (fun acc nil   ->  case nil                                :: acc)
                 ~cons:(fun acc const -> (case cons <|*> int <|*> my_list_editor) :: acc)))
      ]}
  *)
  val recursive : ('a t -> 'a t) -> 'a t

  (** This is useful for e.g. specifying default values when the user creates a new
      element in a list. If you want a default value for the whole form, consider using
      [Init.with_default], which also shows the user a diff summarizing their
      edits.
  *)
  val defaulting_to : default:'a -> sexp_of_t:('a -> Sexp.t) -> 'a t -> 'a t

  (** These only affect formatting, not functionality. *)
  val on_new_line : 'a t -> 'a t

  val case_on_new_line : 'a Case.t -> 'a Case.t

  (** Hides the form by default, giving the user a checkbox to un-collapse the form if
      they wish. *)
  val collapse : ?editor_message_attr:Incr_dom.Vdom.Attr.t -> 'a t -> 'a t

  val unit : unit t
  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
end

(** To map from ['a t] to ['b t], we need to be able to parse default values of type ['b].

    Since we parse default values as sexps, this entails some conversion back and forth.

    If [a_to_b] throws, this function will catch it and display the error to the user.
*)
val map
  :  'a t
  -> a_to_b:('a -> 'b)
  -> b_to_a:('b -> 'a)
  -> sexp_of_a:('a -> Sexp.t)
  -> b_of_sexp:(Sexp.t -> 'b)
  -> 'b t

(** If this returns an error, the error will be displayed before the form element.

    If [a_to_b] throws, this function will catch it and display the error to the user.
*)
val map_that_can_fail
  :  'a t
  -> a_to_b:('a -> 'b Or_error.t)
  -> b_to_a:('b -> 'a)
  -> sexp_of_a:('a -> Sexp.t)
  -> b_of_sexp:(Sexp.t -> 'b)
  -> 'b t

(** This module is "Unsafe" because it relies on the user to ensure that when they map
    from 'a to 'b, 'a and 'b have the same sexp representation.

    If they don't, then it could fail at runtime when we try to parse the default value
    for the form.
*)
module Unsafe : sig
  include Applicative.S with type 'a t := 'a t

  module Let_syntax : sig
    include Applicative.S with type 'a t := 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs = Primitives
    end
  end
end

(** Validates the value in the form, displaying the provided error if it is invalid.
    [where] indicates whether the error should be displayed before or after the form
    element.
    It's recommended to use `After for errors which are right next to the area where
    the user inputs the data, and `Before for errors in complicated types which
    comprise many input fields. *)
val validate : where:[ `Before | `After ] -> 'a t -> f:('a -> unit Or_error.t) -> 'a t

(** Same as [validate], but more suited for validating properties which depend on
    external, changing state.

    Note that an [Incr] can be converted to an [Interactive] using [Interactive.of_incr].
*)
val validate_interactive
  :  where:[ `Before | `After ]
  -> 'a t
  -> 'b Incr_dom_interactive.t
  -> f:('a -> 'b -> unit Or_error.t)
  -> 'a t

(** [handle_error] displays the error, if there is one. If there is no error, it does
    nothing.

    [validate] is a wrapper around [handle_error].
*)
val handle_error : where:[ `Before | `After ] -> 'a Or_error.t t -> 'a t

(** Verifies that the provided [form] can parse [value].
    Intended for expect tests. *)
val test
  :  form:'a t
  -> value:'a
  -> sexp_of_t:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> on_failure:[ `Print | `Raise ]
  -> unit

(** For convenience -- it just calls [test] for each value. *)
val test_list
  :  form:'a t
  -> values:'a list
  -> sexp_of_t:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> on_failure:[ `Print | `Raise ]
  -> unit

(** For convenience -- it just calls [test] for each value. *)
val test_sequence
  :  form:'a t
  -> values:'a Sequence.t
  -> sexp_of_t:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> on_failure:[ `Print | `Raise ]
  -> unit
