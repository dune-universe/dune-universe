open Core_kernel
open Import
open Vdom

(** ['a Interactive.t] is a monad. Within this monad, your program can receive input from
    the user using DOM elements such as checkboxes, text fields, or buttons, and display
    output to the user using text, tables, images, or anything else that can be
    represented as a DOM node.

    The meaning of the ['a] parameter is that ['a Interactive.t] allows the user
    to provide your program a value of type ['a].

    For example:
    - A text box is a [string Interactive.t].
    - A checkbox is a [bool Interactive.t].
    - A button is a [[Pressed | Not_pressed] Interactive.t].
    - Static text is a [unit Interactive.t].

    Since [Interactive.t] is a monad, you can inspect the user's input and
    decide afterwards what the rest of the [Interactive.t] should be.

    For example, this defines a form which only allows the user to submit if
    they have entered at least 10 characters:

    {[
      let open Interactive.Let_syntax in
      let open Interactive.Primitives in
      let submit_button = button ~text:"Submit" () in
      let%bind_open user_input = text () in
      if String.length user_input < 10
      then
        let%map_open () = message "Please enter at least 10 characters." in
        None
      else
        match%map submit_button with
        | Not_pressed -> None
        | Pressed -> Some user_input
    ]}

    If you have used Incr_dom, then you are familiar with the pattern of creating
    Virtual_dom nodes with callbacks that convert user input into Incr_dom actions
    and then into Virtual_dom events using the [inject] function. For instance:

    {[ Node.input
         [ Attr.on_input (fun _ev text -> inject (Action.Submit text)) ]
         []
    ]}

    [Interactive] works in the same way. (In fact, this is how it is implemented.)
    To render an ['a Interactive.t], you must provide functions for converting values of
    ['a] into actions and actions into Virtual_dom events. These functions are used in the
    callbacks of the Virtual_dom nodes returned by the [render] function.
    Then, each time the underlying value of the ['a Interactive.t] changes as a result of
    a user action (entering text in a text field, checking a checkbox, selecting from a
    drop-down menu, etc.), this results in an event created from the updated ['a] value.

    For example, you might render the form defined above in the [view] function of your
    Incr_dom app as follows:

    {[
      let view model ~inject =
        ...
        let nodes: Node.t Incr.t =
          Interactive.render form
            ~on_input:(fun x -> Action.Submit x)
            ~inject
        in
        ...
    ]}

    We already handled invalid user inputs above, so we don't have to handle them here.

    Note: Be careful about creating a new [Interactive.t] within a bind.

    Consider this code:

    {[
      let%bind_open is_checked = checkbox () in
      if is_checked
      then text ~init:"Foo" ()
      else text ~init:"Bar" ()
    ]}

    Whenever the checkbox's value is changed, this recreates the text field.
    So if the user modifies the value of the checkbox, the text field's value is lost.
    Instead, prefer the following:

    {[
      let checked_text = text ~init:"Foo" () in
      let unchecked_text = text ~init:"Bar" () in
      let%bind_open is_checked = checkbox () in
      if is_checked
      then checked_text
      else unchecked_text
    ]}

    This code is better because if the user modifies "Foo",
    then checks the box and unchecks it again, their input will be saved.
*)
type 'a t

(* Let syntax is defined at the end of this file. *)

include Monad.S_without_syntax with type 'a t := 'a t

module Primitives : sig
  (** [id] lets you specify a value for the "id" attribute of the DOM node.
      It doesn't affect the behavior of this library (unless you accidentally
      create two nodes with the same id), but it might be useful
      if you want to refer to the node elsewhere.
  *)
  type 'a primitive = ?attrs:Attr.t list -> ?id:string -> unit -> 'a t

  val text : ?init:string -> string primitive
  val text_area : ?init:string -> string primitive

  module Button_state : sig
    type t =
      | Pressed
      | Not_pressed
  end

  val button : text:string -> Button_state.t primitive
  val disabled_button : text:string -> unit primitive

  (** [options] is a list of tuples (label, value). [label] is what will be
      displayed to the user in the dropdown. [value] is what will be produced
      by the [Interactive.t] when that option is selected by the user.

      [init] is the 0-based index of the option which is initially selected.
      It defaults to 0.

      Raises when [options] is empty or [init >= List.length options].
  *)
  val dropdown_exn : options:(string * 'a) list -> ?init:int -> 'a primitive

  (** [dropdown_with_blank_exn] is a wrapper around [dropdown_exn] which adds a blank
      option to the dropdown that produces [None]. It is otherwise the same as
      [dropdown_exn].

      If you don't provide a value for [init], the initial value for the dropdown will
      be the blank option which produces [None].

      Raises when [init >= List.length options].
  *)
  val dropdown_with_blank_exn
    :  options:(string * 'a) list
    -> ?init:int
    -> 'a option primitive

  val checkbox : ?init:bool -> bool primitive
  val nodes : Node.t list -> unit t
  val message : string -> unit t
  val line_break : unit t

  (** [create] allows you to create your own primitives.
      [init] is the initial value of the [Interactive.t].
      [inject] is how the users of your primitive tell it what to do with new values.
      In your event handler, you should call it and supply the new value of the primitive.

      You should use [value] to determine the value of your primitive.
      For example, if your primitive is a checkbox, you should do this:
      {[
        let render ~inject ~value =
          let open Incr.Let_syntax in
          let%map is_checked = value in
          let node =
            if is_checked
            then (* ... create a [Node.t] representing a checked checkbox ... *)
            else (* ... create a [Node.t] representing an unchecked checkbox ... *)
          in
          ...
      ]}

      You should not use [init] to determine the value. [value] will
      initially be the same as [init], but it might be different if the field is modified,
      then stops being rendered, then is rendered again. In this case, we want the user's
      input to be preserved. But if you use [init] to fill in the initial value, the
      user's input will be discarded in this case.

      For an example, check the implementation of [Primitives.text].
  *)
  val create
    :  init:'a
    -> render:(inject:('a -> Event.t) -> value:'a Incr.t -> Node.t list Incr.t)
    -> 'a t

  val default_text_attrs : Attr.t list
  val default_text_area_attrs : Attr.t list
  val default_button_attrs : Attr.t list
  val default_dropdown_attrs : Attr.t list
  val bootstrap_text_attrs : Attr.t list
  val bootstrap_text_area_attrs : Attr.t list
  val bootstrap_button_attrs : Attr.t list
  val bootstrap_dropdown_attrs : Attr.t list
end

(** You have to schedule an action whenever the state changes in order for the view to
    update correctly.

    If you don't want to take any action, you should define an action which
    has no effect on your model.
*)
val render
  :  'a t
  -> on_input:('a -> 'action)
  -> inject:('action -> Event.t)
  -> Node.t Incr.t

(** [map_nodes] can be used to change the presentation of the [Interactive.t].
    For example, the following takes an [Interactive.t] and produces a Node that
    has a red background when rendered:
    {[
      Interactive.map_nodes t ~f:(fun nodes ->
        Node.div [Attr.style ["background", "red"]] nodes)
    ]}
*)
val map_nodes : 'a t -> f:(Node.t list -> Node.t list) -> 'a t

(** [map_nodes_value_dependent] is like [map_nodes], but the function can also depend on
    the current value of the [Interactive.t].

    For example, in a ['a Or_error.t Interactive.t], you could use this to add a node
    which displays the error.
*)
val map_nodes_value_dependent : 'a t -> f:('a -> Node.t list -> Node.t list) -> 'a t

val wrap_in_div : ?attrs:Attr.t list -> 'a t -> 'a t
val of_incr : 'a Incr.t -> 'a t

(** [current_value] calls [Incr.stabilize].

    See also [render], which is the typical way of handling changes to the value.
*)
val current_value : 'a t -> 'a

module Let_syntax : sig
  val return : 'a -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs = Primitives
  end
end
