open ExtLib

include Prelude

let () = Printexc.register_printer (function Failure s -> Some s | _ -> None)

type 'a or_var = Static of 'a | Dynamic of Tjson.var

let dynamic_default default f x =
  match x with
  | Static x -> f x
  | Dynamic _ -> default

let var_or conv x =
  match x with
  | `Var v -> Dynamic v
  | _ ->
    try Static (conv x) with Failure s -> fail "expected var or %s" s

type value = Field of string | Script of [`Painless|`Id] * string or_var

module U = struct

(** @return specified [name] from [json] dict or [`Null] when [name] is absent.
    @raise Failure if [json] is not a dict *)
let member name = function
| `Assoc l -> (try List.assoc name l with _ -> `Null)
| _ -> fail "member %S : not a dict" name

(** @return specified [name] from [json] dict
    @raise Failure if [json] is not a dict or [name] is absent *)
let assoc name json = match member name json with `Null -> fail "assoc %S : not found" name | x -> x

(** @return specified [name] from [json] dict with [conv] applied to it.
    @raise Failure if [json] is not dict, when [name] is absent, when [conv] fails *)
let get name conv json = try member name json |> conv with exn -> fail ~exn "get %S" name

(** @return specified [name] from [json] dict with [conv] applied to it.
    @raise Failure if [json] is not dict or when [conv] fails *)
let opt name conv json = try match member name json with `Null -> None | x -> Some (conv x) with exn -> fail ~exn "opt %S" name

let to_string = function `String (s:string) -> s | _ -> fail "expected string"
let to_assoc = function `Assoc a -> a | _ -> fail "expected dict"
let to_bool = function `Bool b -> b | _ -> fail "expected bool"
let to_list f = function `List l -> List.map f l | _ -> fail "expected list"

end

type mapping = { mapping : Yojson.Basic.t; name : string option; }

let to_valid_ident ~prefix s =
  assert (s <> "");
  let s = String.map (function ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c) -> c | _ -> '_') s in
  match s.[0] with '0'..'9' -> prefix ^ s | _ -> s

let to_valid_modname s =
  let s = to_valid_ident ~prefix:"M_" s in
  match s.[0] with
  | 'a'..'z' | 'A'..'Z' -> String.capitalize_ascii s
  | _ -> "M_" ^ s

module ES_name : sig
type t
val to_ocaml : t -> string
val get_path : t -> string list
val make : mapping -> string -> t
val append : t -> string -> t
val show : t -> string
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val compare : t -> t -> int

(** fold over the name itself and all of its parents *)
val fold_up : (t -> 'a -> 'a) -> t -> 'a -> 'a
end = struct

type t = (string option * string list)

let to_ocaml (modname,l) = (List.filter_map id [modname] @ l) |> List.map to_valid_modname |> String.concat "."
let get_path = snd
let make mapping s = mapping.name, String.nsplit s "."
let append (m,l) s = m, l @ String.nsplit s "."
let show (_,l) = String.concat "." l
let pp ppf t = Format.pp_print_text ppf (show t)
let fold_up f (m,l) acc =
  let rec loop acc = function
  | [] -> acc
  | _::xs as l -> loop (f (m, List.rev l) acc) xs
  in
  loop acc (List.rev l)

let equal (a:t) b = a = b
let compare = compare

end

type simple_type = [ `Int | `Int64 | `String | `Double | `Bool | `Json ]

let show_simple_type = function
| `Int -> "int"
| `Int64 -> "int64"
| `String -> "string"
| `Double -> "float"
| `Bool -> "bool"
| `Json -> "json"

let pp_simple_type ppf x = Format.pp_print_text ppf (show_simple_type x)

type multi = One | Many

type var_type = { multi : multi; ref : ES_name.t option; typ : simple_type; }
let show_var_type x = show_simple_type x.typ
let pp_var_type ppf x = pp_simple_type ppf x.typ

type required = [ `Required | `Optional ] [@@deriving show]

type input_vars = (string * (required * [ `Group of input_vars | `Simple of var_type option])) list
type var_eq = Eq_any | Eq_type of simple_type | Eq_list of simple_type | Eq_object | Eq_field of multi * string
type constraint_t = On_var of Tjson.var * var_eq | Field_num of value | Field_date of value

type source_filter = { excludes : string list option; includes : string list option }
let empty_filter = { excludes = None; includes = None }

type result_type = [
  | `List of result_type
  | `Object of result_type
  | `Dict of (string * result_type) list
  | `Ref of (ES_name.t * simple_type)
  | `Maybe of result_type
  | simple_type
  ] [@@deriving show]

type resolve_type = [
  | `Typeof of value
  | `List of resolve_type
  | `Object of resolve_type
  | `Dict of (string * resolve_type) list
  | `Ref of (ES_name.t * simple_type)
  | `Maybe of resolve_type
  | simple_type
  ]

let simple_of_es_type t =
  match t with
  | "long" -> `Int
  | "keyword" | "text" -> `String
  | "ip" -> `String
  | "date" -> `String
  | "double" | "float" -> `Double
  | "boolean" -> `Bool
  | "int64" | "murmur3" -> `Int64
  | _ -> fail "simple_of_es_type: cannot handle %S" t

let get_meta json =
  match U.member "_meta" json with
  | `Null -> `Assoc []
  | j -> j

let get_repr_opt meta =
  match U.member "repr" meta with
  | `String typ -> Some typ
  | `Null -> None
  | _ -> fail "get_repr_opt: strange esgg repr"

let typeof mapping t : simple_type =
  let rec find path schema =
    match path with
    | [] ->
      let meta = get_meta schema in
      begin match get_repr_opt meta with
      | Some repr -> repr
      | None -> U.get "type" U.to_string schema
      end
    | hd::tl ->
      let a = try U.assoc "properties" schema with _ -> U.assoc "fields" schema in
      find tl (U.member hd a)
  in
  match find (ES_name.get_path t) mapping.mapping with
  | exception _ -> fail "no such field"
  | a -> simple_of_es_type a

let typeof mapping x = try typeof mapping x with exn -> fail ~exn "typeof field %S" (ES_name.show x)
let typeof_ mapping value =
  match value with
  | Script (`Painless, Static "_score") -> `Double
  | Script _ -> `Json
  | Field f -> typeof mapping (ES_name.make mapping f)

let source_fields k j = U.(match member "_source" j with `Null -> None | a -> opt k (to_list to_string) a)

let option_to_list = function None -> [] | Some x -> [x]
