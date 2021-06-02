module type Shared = sig
  type json

  (** [member key json] searches the JSON object [json], which
      must be an [`Assoc] element, for [key] and returns the
      value or [`Null] if the [key] is missing *)
  val member : string -> [> `Assoc of (string * json) list ] -> json

  (** [index idx json] returns the [idx]-th JSON object in the [json] array,
      which must be an [`List] element. A negative [idx] value starts from
      the end with -1 being the last element.  A [Failure] exception is raise
      if the idx is out of bounds *)
  val index : int -> [> `List of json list ] -> json

  (** [map f json] applies the function [f] to each element of the JSON
      array [json], which must be an [`List] element, and returns a
      [`List] element *)
  val map :
    (json -> json) -> [> `List of json list ] -> [> `List of json list ]

  (** [to_assoc json] converts the JSON object [`Assoc a] to [a] *)
  val to_assoc :
    [> `Assoc of (string * json) list ] -> (string * json) list

  (** [to_bool json] converts [`Bool b] to [b] *)
  val to_bool : [> `Bool of bool ] -> bool

  (** [to_bool_option json] converts [`Bool b] to [Some b] and [`Null] to [None] *)
  val to_bool_option : [> `Bool of bool | `Null ] -> bool option

  (** [to_float json] converts [`Float f] to [f] *)
  val to_float : [> `Float of float ] -> float

  (** [to_float_option json] converts [`Float f] to [Some f] and [`Null] to [None] *)
  val to_float_option : [> `Float of float | `Null ] -> float option

  (** [to_option f json] returns [None] if [json] is [`Null] otherwise [Some (f json)].  *)
  val to_option : (([> `Null ] as 'a) -> json) -> 'a -> json option

  (** [to_list json] converts [`List l] to [l] *)
  val to_list : [> `List of json list ] -> json list

  (** [to_number json] converts [`Float f] to [f] *)
  val to_number : [> `Float of float ] -> float

  (** [to_number_option json] converts [`Float f] to [Some f] and [`Null] to [None] *)
  val to_number_option : [> `Float of float | `Null ] -> float option

  (** [to_string json] converts [`String s] to [s] *)
  val to_string : [> `String of string ] -> string

  (** [to_string_option json] converts [`String s] to [Some s] and [`Null] to [None] *)
  val to_string_option : [> `String of string | `Null ] -> string option

  (** [convert_each f json] applies the function [f] to each element of the
      JSON array [json], which must be an [`List] element, and returns a
      list of the returned values. *)
  val convert_each : (json -> json) -> [> `List of json list ] -> json list

  (** [filter_map f l] applies [f] to each element of the list [l] and returns
      a new list with the values [v] for which [f] returned [Some v].  *)
  val filter_map : ('a -> 'a option) -> 'a list -> 'a list

  (** [rev_filter_map f acc l] applies [f] to each element of the list [l] and
      prepends the values for which [f] returned [Some v] to list [acc]. [acc]
      is returned as the result and is in reverse order to the input.  This is
      a tail call optimised version of [filter_map] *)
  val rev_filter_map : ('a -> 'a option) -> 'a list -> 'a list -> 'a list

  (** [flatten l] given a list of [json] elements filters the [`List] elements
      and flattens them into a single list. This is the same as
      [filter_list |> List.flatten] *)
  val flatten : [> `List of 'a list ] list -> 'a list

  (** [rev_flatten acc l] is the tail recursive version of [flatten] with
      the result accumulated in [acc]. The result is in reverse order.  *)
  val rev_flatten : 'a list -> [> `List of 'a list ] list -> 'a list

  (** [filter_index i l] returns the [i]'th element from each [`List l1] in [l].
      Thus,
      {[
        [[`List [`Int 2; `Int 3]; `List [`Int 4; `Int 5]] |> filter_index 1]
      ]}
      returns [[`Int 3; `Int 5]]
      *)
  val filter_index : int -> [> `List of json list ] list -> json list

  (** [filter_list l] returns a list of all the values of [`List value] elements in l *)
  val filter_list : [> `List of 'a ] list -> 'a list

  (** [filter_assoc l] returns a list of all the values of [`Assoc value] elements in l *)
  val filter_assoc : [> `Assoc of 'a ] list -> 'a list 

  (** [filter_bool l] returns a list of all the values of [`Bool value] elements in l *)
  val filter_bool : [> `Bool of bool ] list -> bool list

  (** [filter_float l] returns a list of all the values of [`Float value] elements in l *)
  val filter_float : [> `Float of float ] list -> float list

  (** [filter_string l] returns a list of all the values of [`String value] elements in l *)
  val filter_string  : [> `String of string ] list -> string list

  (** [filter_member key js] given a [key] and a list of json [`Assoc]-s, [js], returns
      the list of values extracted from each of the [`Assoc]-s. Thus,
      {[
        [[`Assoc [("id", `Int 1)]; `Assoc [("id", `Int 2)]]] |> filter_member "id"]
      ]}
      returns [[`Int 1; `Int 2]] *)
  val filter_member : string -> [> `Assoc of (string * json) list ] list -> json list

  (**[filter_number l] returns a list of all the values of [`Float value] elements in l *)
  val filter_number : [> `Float of float ] list -> float list

  (** [keys assoc] returns all the keys from the [`Assoc] element *)
  val keys : [> `Assoc of (string * 'a) list ] -> string list

  (** [values assoc] returns all the values from the [`Assoc] element *)
  val values : [> `Assoc of (string * 'a) list ] -> 'a list

  (** [combine assoc1 assoc2] appends the associative lists of two [`Assoc] elements returning
      an [`Assoc] element *)
  val combine : [> `Assoc of 'a list ] -> [> `Assoc of 'a list ] -> [> `Assoc of 'a list ]

  (** [sort json] sorts the [json] tree based on field names.  Objects and lists are sorted
      recursively. Note that the function only sorts field names and not the values.
      The sort is stable *)
  val sort : ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) -> 'a
end

module type Basic = sig
  type json

  (** [to_number json] converts [`Float f] to [f] and [Int i] to [float i] *)
  val to_number : [> `Int of int | `Float of float ] -> float

  (** [to_number_option json] converts [`Float f] to [Some f], [`Int i] to [Some (float i)]
      and [`Null] to [None] *)
  val to_number_option : [> `Int of int | `Float of float | `Null ] -> float option

  (** [to_int json] converts [`Int i] to [i] *)
  val to_int : [> `Int of int ] -> int

  (** [to_int_option json] converts [`Int i] to [Some i] and [`Null] to [None] *)
  val to_int_option : [> `Int of int | `Null ] -> int option

  (** [filter_int l] returns a list of all the values of [`Int value] elements in l *)
  val filter_int : [> `Int of int ] list -> int list

  val filter_number : [> `Int of int | `Float of float ] list -> float list
end

module type Extended = sig
  type json

  (** [sort json] sorts the [json] tree based on field names.  Objects and lists are sorted
      recursively. Note that the function only sorts field names and not the values.
      The sort is stable *)
  val sort : ([> `Assoc of (string * 'a) list | `List of 'a list |
                 `Tuple of 'a list | `Variant of 'b * 'a option ] as 'a) -> 'a
end
