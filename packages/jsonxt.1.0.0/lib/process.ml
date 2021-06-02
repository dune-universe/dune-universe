let error msg _json = raise (Failure msg)

module Internal = struct
  module type S = sig
    type json
    val null : unit -> json
  end

  module type Internal_strict_intf = sig
    type json

    val member : string -> [> `Assoc of (string * json) list ] -> json
    val index : int -> [> `List of json list ] -> json
    val map : (json -> json) -> [> `List of json list ] -> [> `List of json list ]
    val to_assoc : [> `Assoc of (string * json) list ] -> (string * json) list
    val to_bool : [> `Bool of bool ] -> bool
    val to_float : [> `Float of float ] -> float
    val to_string : [> `String of string ] -> string
    val to_string_option : [> `String of string | `Null ] -> string option
    val to_option : (([> `Null ] as 'a) -> json) -> 'a -> json option
    val to_list : [> `List of json list ] -> json list
    val to_bool_option : [> `Bool of bool | `Null ] -> bool option
    val to_float_option : [> `Float of float | `Null ] -> float option
    val to_number : [> `Float of float ] -> float
    val to_number_option : [> `Float of float | `Null ] -> float option
    val convert_each : (json -> json) -> [> `List of json list ] -> json list
    val rev_filter_map : ('a -> 'a option) -> 'a list -> 'a list -> 'a list
    val filter_map : ('a -> 'a option) -> 'a list -> 'a list
    val rev_flatten : 'a list -> [> `List of 'a list ] list -> 'a list
    val flatten : [> `List of 'a list ] list -> 'a list
    val filter_index : int -> [> `List of json list ] list -> json list
    val filter_list : [> `List of 'a ] list -> 'a list
    val filter_assoc : [> `Assoc of 'a ] list -> 'a list
    val filter_bool : [> `Bool of bool ] list -> bool list
    val filter_float : [> `Float of float ] list -> float list
    val filter_string  : [> `String of string ] list -> string list
    val filter_member : string -> [> `Assoc of (string * json) list ] list -> json list
    val filter_number : [> `Float of float ] list -> float list
    val keys : [> `Assoc of (string * 'a) list ] -> string list
    val values : [> `Assoc of (string * 'a) list ] -> 'a list
    val combine : [> `Assoc of 'a list ] -> [> `Assoc of 'a list ] -> [> `Assoc of 'a list ]
    val sort : ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) -> 'a
  end

  module Shared = struct
    let rec rev_filter_map f acc l =
      match l with
      | [] -> acc
      | hd::tl ->
        match f hd with
        | None -> rev_filter_map f acc tl
        | Some v -> rev_filter_map f (v::acc) tl

    let filter_map f l = List.rev (rev_filter_map f [] l)

    let rec rev_flatten acc l =
      match l with
      | [] -> acc
      | hd::tl ->
        match hd with
        | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
        | _ -> rev_flatten acc tl

    let flatten l = List.rev (rev_flatten [] l)

  end

  module Strict(M : S) : Internal_strict_intf
    with type json = M.json
  = struct
    type json = M.json

    let assoc name obj : json = try List.assoc name obj with Not_found -> M.null ()

    let member name v : json =
      match v with
      | `Assoc obj -> assoc name obj
      | json -> error ("Expected `Assoc to find name '" ^ name ^ "' in") json

    let index i v : json =
      match v with
      | `List l ->
          let len = List.length l in
          let i' = if i < 0 then len + i else i in
          if i' < 0 || i' >= len then raise (Invalid_argument (string_of_int i ^ " out of bounds"))
          else List.nth l i'
      | json -> error "Can't index none `List type " json

    let map f v =
      match v with
      | `List l -> `List (List.map f l)
      | json -> error "Can't map over none `List type " json

    let to_assoc = function | `Assoc obj -> obj | json -> error "Expected `Assoc" json
    let to_bool = function | `Bool b -> b | json -> error "Expected `Bool" json
    let to_float = function | `Float f -> f | json -> error "Expected `Float" json

    let to_string = function
      | `String s -> s
      | json -> error "Expected `String" json

    let to_string_option = function
      | `String s -> Some s
      | `Null -> None
      | json -> error "Expected `String or `Null" json

    let to_option f v : json option =
      match v with
      | `Null -> None
      | v -> Some (f v)

    let to_list v : json list =
      match v with
      | `List l -> l
      | json -> error "Expected `List" json

    let to_float_option = function
      | `Float f -> Some f
      | `Null -> None
      | json -> error "Expected `Float or `Null" json

    let to_bool_option = function
      | `Bool b -> Some b
      | `Null -> None
      | json -> error "Expected `Bool or `Null" json

    let to_number = function
      | `Float f -> f
      | json -> error "Expected `Float" json

    let to_number_option = function
      | `Float f -> Some f
      | `Null -> None
      | json -> error "Expected `Float or `Null" json

    let convert_each f = function
      | `List l -> List.map f l
      | json -> error "Expected `List" json

    let rev_filter_map = Shared.rev_filter_map
    let filter_map = Shared.filter_map

    let rev_flatten = Shared.rev_flatten
    let flatten = Shared.flatten

    let filter_index i l =
      filter_map (function | `List l -> (try Some (List.nth l i) with _ -> None) | _ -> None) l

    let filter_list l = filter_map (function `List l -> Some l | _ -> None) l
    let filter_assoc l = filter_map (function `Assoc l -> Some l | _ -> None) l
    let filter_bool l = filter_map (function `Bool b -> Some b | _ -> None) l
    let filter_float l = filter_map (function `Float f -> Some f | _ -> None) l
    let filter_string l = filter_map (function `String s -> Some s | _ -> None) l

    let filter_member k l =
      filter_map (function `Assoc l -> (try Some (List.assoc k l) with _ -> None) | _ -> None) l

    let filter_number l =
      filter_map (
        function
        | `Float f -> Some f
        | _ -> None
      ) l

    let keys o = to_assoc o |> List.map (fun (key, _) -> key)

    let values o = to_assoc o |> List.map (fun (_, value) -> value)

    let combine first second =
      match (first, second) with
      | (`Assoc a, `Assoc b) -> `Assoc (a @ b)
      | (_, _) -> raise (Invalid_argument "Expected two objects")

    let rec sort json =
      match json with
      | `Assoc o ->
        let o = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) o) in
        `Assoc ((List.stable_sort (fun (k1, _) (k2, _) -> String.compare k1 k2)) o)
      | `List l ->
        `List (List.rev (List.rev_map sort l))
      | el ->  el
  end

  module type Internal_basic_intf = sig

    val to_number : [> `Int of int | `Float of float ] -> float
    val to_number_option : [> `Int of int | `Float of float | `Null ] -> float option
    val to_int : [> `Int of int ] -> int
    val to_int_option : [> `Int of int | `Null ] -> int option
    val filter_int : [> `Int of int ] list -> int list
    val filter_number : [> `Int of int | `Float of float ] list -> float list
  end

  module Basic(M : S) : Internal_basic_intf = struct

    let to_number = function
      | `Int i -> float i
      | `Float f -> f
      | json -> error "Expected `Int or `Float" json

    let to_number_option = function
      | `Int i -> Some (float i)
      | `Float f -> Some f
      | `Null -> None
      | json -> error "Expected `Int, `Float or `Null" json

    let to_int = function | `Int i -> i | json -> error "Expected `Int" json

    let to_int_option = function
      | `Int i -> Some i
      | `Null -> None
      | json -> error "Expected `Int or `Null" json

    let filter_int l = Shared.filter_map (function `Int i -> Some i | _ -> None) l
    let filter_number l =
      Shared.filter_map (
        function
        | `Int i -> Some (float i)
        | `Float f -> Some f
        | _ -> None
      ) l
  end

  module type Internal_extended_intf = sig
    val sort : ([> `Assoc of (string * 'a) list | `List of 'a list |
                   `Tuple of 'a list | `Variant of 'b * 'a option ] as 'a) -> 'a
  end

  module Extended(M : S) : Internal_extended_intf = struct

    let rec sort json =
      match json with
      | `Assoc o ->
        let o = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) o) in
        `Assoc ((List.stable_sort (fun (k1, _) (k2, _) -> String.compare k1 k2)) o)
      | `Tuple l
      | `List l ->
        `List (List.rev (List.rev_map sort l))
      | `Variant (k, Some v) as v1 ->
        let v' = sort v in if v' == v then v1 else `Variant (k, Some v')
      | el ->  el
  end

end

module Strict = struct
  module M = struct
    type json = Json.Strict.json
    let null () = `Null
  end
  include Internal.Strict(M)
end

module Basic = struct
  module M = struct
    type json = Json.Basic.json
    let null () = `Null
  end
  include Internal.Strict(M)
  include Internal.Basic(M)
end

module Extended = struct
  module M = struct
    type json = Json.Extended.json
    let null () = `Null
  end
  include Internal.Strict(M)
  include Internal.Basic(M)
  include Internal.Extended(M)
end

module Yojson_safe = struct
  module M = struct
    type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Intlit of string
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      | `Tuple of json list
      | `Variant of (string * json option)
      ]

    let null () = `Null
  end
  include Internal.Strict(M)
  include Internal.Basic(M)
  include Internal.Extended(M)
end
