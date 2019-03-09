
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Interface to Yojson} *)

type ('a, +'card) query = ('a, 'card) OLinq.t

let (|>) x f = f x

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option
  ]

module L = OLinq

let as_bool x = L.filter_map (function `Bool b -> Some b | _ -> None) x
let as_int x = L.filter_map (function `Int i -> Some i | _ -> None) x
let as_int_lit x = L.filter_map (function `Intlit s -> Some s | _ -> None) x
let as_float x = L.filter_map (function `Float f -> Some f | _ -> None) x
let as_null x = L.filter_map (function `Null -> Some () | _ -> None) x
let as_string x = L.filter_map (function `String s -> Some s | _ -> None) x
let as_tuple x = L.filter_map (function `Tuple x -> Some x | _ -> None) x
let as_assoc x = L.filter_map (function `Assoc x -> Some x | _ -> None) x
let as_list x = L.filter_map (function `List x -> Some x | _ -> None) x
let as_variant x = L.filter_map (function `Variant(x,y) -> Some (x,y) | _ -> None) x

let assoc k =
  L.filter_map
    (function
      | `Assoc l -> (try Some (List.assoc k l) with Not_found -> None)
      | _ -> None)

let enter_tuple x = L.flat_map_l (function `Tuple x -> x | _ -> []) x

let enter_list x = L.flat_map_l (function `List l -> l | _ -> []) x

let enter_assoc x = L.flat_map_l (function `Assoc l -> l | _ -> []) x

let enter_tuple_index x =
  L.flat_map_iter
    (function
      | `Tuple l -> (fun yield -> List.iteri (fun i x -> yield (i,x)) l)
      | _ -> (fun _ -> ()))
    x

let map_list f j = match j with
  | `List l ->
      L.of_list l
      |> L.flat_map f
      |> L.reflect_list
      |> L.map (fun l -> `List l)
  | _ -> L.empty

let map_assoc f j = match j with
  | `Assoc l ->
      L.of_list l
      |> L.flat_map (fun (key,v) -> f key v |> L.map (fun v' -> key, v'))
      |> L.reflect_list
      |> L.map (fun l -> `Assoc l)
  | _ -> L.empty

