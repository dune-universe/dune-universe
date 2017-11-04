open! Core

type xml = Xml_light.Xml.xml

module type Xmlable = sig
  type t
  val xsd : xml list
  val to_xml : t -> xml list
  val of_xml : xml -> t
end

let to_string xml = Xml_light.Xml.to_string xml
let to_string_fmt xml = Xml_light.Xml.to_string_fmt xml
let to_human_string xml = Xml_light.Xml.to_human_string xml

module Parser_state = struct
  type t = Xml_light.XmlParser.t
  let make = Xml_light.XmlParser.make
end

let stateful_of_string = Xml_light.Xml.parse_string_with

let of_file file = Xml_light.Xml.parse_file file

let create_node ~tag ~body = Xml_light.Xml.Element (tag, [], body)

let create_data body = Xml_light.Xml.PCData body

let tag xml =
  match xml with
  | Xml_light.Xml.PCData _ -> None
  | Xml_light.Xml.Element (tag, _, _) -> Some tag

let attributes xml =
  match xml with
  | Xml_light.Xml.PCData _ -> []
  | Xml_light.Xml.Element (_, attrs, _) -> attrs

let children xml =
  match xml with
  | Xml_light.Xml.PCData _ -> []
  | Xml_light.Xml.Element (_, _, children) -> children

let child xml my_tag =
  List.find ~f:(fun xml -> (Some my_tag) = (tag xml)) (children xml)

let rec contents xml =
  match xml with
  | Xml_light.Xml.PCData str -> Some str
  | Xml_light.Xml.Element (_, _, [element]) -> contents element
  | Xml_light.Xml.Element _ -> None

let kind xml =
  match xml with
  | Xml_light.Xml.PCData _
  | Xml_light.Xml.Element (_, _, [Xml_light.Xml.PCData _]) -> `Leaf
  | Xml_light.Xml.Element _  -> `Internal

let xml_data string = Xml_light.Xml.PCData string

exception Unexpected_xml of (xml * string)

exception Illegal_atom of xml

let check_extra_fields xml fields =
  let fail got =
    let fields = String.concat ~sep:";" fields in
    let msg = Printf.sprintf "record expected with fields [%s] but got %S" fields got in
    raise (Unexpected_xml (xml, msg))
  in
  match xml with
  | Xml_light.Xml.PCData _ -> fail "PCData"
  | Xml_light.Xml.Element (_, _, children) ->
    let iter child =
      match child with
      | Xml_light.Xml.PCData _ -> fail "PCData"
      | Xml_light.Xml.Element (tag, _, _) ->
        if not (List.mem fields tag ~equal:String.equal) then fail tag
    in
    List.iter ~f:iter children

module Restriction = struct
  module Format = struct
    type t =
      [ `string
      | `decimal
      | `date
      | `time
      | `datetime
      | `integer ]

    exception Illegal_format

    let to_string = function
      | `time     -> "xs:time"
      | `string   -> "xs:string"
      | `decimal  -> "xs:decimal"
      | `date     -> "xs:date"
      | `datetime -> "xs:dateTime"
      | `integer  -> "xs:integer"

    let of_string = function
      | "xs:time"     -> `time
      | "xs:string"   -> `string
      | "xs:decimal"  -> `decimal
      | "xs:date"     -> `date
      | "xs:dateTime" -> `datetime
      | "xs:integer"  -> `integer
      | _             -> raise Illegal_format
  end

  type t = xml

  let simple_type ~restrictions ~format =
    let restriction =
      Xml_light.Xml.Element ("xs:restriction", ["base", Format.to_string format],
        restrictions)
    in
    [ Xml_light.Xml.Element ("xs:simpleType", [], [restriction]) ]

  let restriction kind value =
    Xml_light.Xml.Element ("xs:" ^ kind, ["value", value], [])

  let enumeration string =
    restriction "enumeration" string

  exception Illegal_restriction

  let not_negative str n =
    if n < 0 then raise Illegal_restriction
    else restriction str (string_of_int n)

  let fraction_digits n =
    not_negative "fractionDigits" n

  let length n =
    not_negative "length" n

  let max_exclusive n =
    restriction "maxExclusive" (string_of_int n)

  let min_exclusive n =
    restriction "minExclusive" (string_of_int n)

  let max_inclusive n =
    restriction "maxInclusive" (string_of_int n)

  let min_inclusive n =
    restriction "minInclusive" (string_of_int n)

  let max_length n =
    not_negative "maxLength" n

  let min_length n =
    not_negative "minLength" n

  let pattern str =
    restriction "pattern" str

  let total_digits n =
    not_negative "totalDigits" n

end

module type Atom = sig
  type t
  val of_string  : string -> t
  val to_string  : t -> string
  val xsd_format : Restriction.Format.t
  val xsd_restrictions : Restriction.t list
end

let to_xml ~to_string v =
  [Xml_light.Xml.PCData (to_string v)]

let of_xml ~of_string xml =
  match xml with
  | Xml_light.Xml.Element (_, [], []) -> of_string ""
  | Xml_light.Xml.Element (_, [], [Xml_light.Xml.PCData str])
  | Xml_light.Xml.PCData str -> of_string str
  | Xml_light.Xml.Element _ -> raise (Unexpected_xml (xml, "noname"))

module Make (Atom : Atom) : Xmlable with type t = Atom.t = struct
  type t = Atom.t
  let xsd =
    Restriction.simple_type
      ~restrictions:Atom.xsd_restrictions ~format:Atom.xsd_format
  let to_xml = to_xml ~to_string:Atom.to_string
  let of_xml = of_xml ~of_string:Atom.of_string
end

let wrap xsd =
  Xml_light.Xml.Element ("xs:schema", ["xmlns:xs","http://www.w3.org/2001/XMLSchema"],
    [xsd])

(** All the conversion functions *)
(*let atom_conversion f xml =
  match xml with
  | Xml_light.Xml.Element (_, [], [Xml_light.Xml.PCData string])
  | Xml_light.Xml.PCData string -> f string
  | Xml_light.Xml.Element _ -> raise (Illegal_atom xml)*)

let get_child name xml =
  try
    let children = Xml_light.Xml.children xml in
    match List.find ~f:(fun t -> name = Xml_light.Xml.tag t) children with
    | Some res -> res
    | None ->
      let msg =
        Printf.sprintf "Expected to find an entry %S but it was not present" name
      in
      raise (Unexpected_xml (xml, msg))
  with
  | Xml_light.Xml.Not_element xml -> raise (Unexpected_xml (xml, name))

type 'a of_xml = xml -> 'a

let of_xml_conversion of_str xml =
  match xml with
  | Xml_light.Xml.Element (_, [], []) -> of_str ""
  | Xml_light.Xml.Element (_, [], [Xml_light.Xml.PCData str])
  | Xml_light.Xml.PCData str -> of_str str
  | Xml_light.Xml.Element _ -> raise (Unexpected_xml (xml, ""))


let unit_of_xml xml =
  of_xml_conversion (fun _ -> ()) xml

let bool_of_xml xml =
  of_xml_conversion (fun str ->
    let str = String.uppercase str in
    if not (str = "TRUE" || str = "FALSE") then
      failwith
        (Printf.sprintf
          "Xml conversion: Illegal boolean %s (should be TRUE or FALSE)." str)
    else str = "TRUE") xml

let string_of_xml xml =
  of_xml_conversion (fun x -> x) xml

let char_of_xml xml =
  let f str = String.get str 0 in
  of_xml_conversion f xml

let int_of_xml xml =
  of_xml_conversion int_of_string xml

let float_of_xml xml =
  of_xml_conversion float_of_string xml

let int32_of_xml xml =
  of_xml_conversion Int32.of_string xml

let int64_of_xml xml =
  of_xml_conversion Int64.of_string xml

let nativeint_of_xml xml =
  of_xml_conversion Nativeint.of_string xml

let big_int_of_xml xml =
  of_xml_conversion Big_int.big_int_of_string xml

let nat_of_xml xml =
  of_xml_conversion Nat.nat_of_string xml

let num_of_xml xml =
  of_xml_conversion Num.num_of_string xml

let ratio_of_xml xml =
  of_xml_conversion Ratio.ratio_of_string xml

let recursive_of_xml name a__of_xml xml =
  let me = get_child name xml in
  a__of_xml me

let list_of_xml ?tag a__of_xml xml =
  match tag with
  | None -> (match xml with
    | Xml_light.Xml.Element (_, _, contents) ->
      List.map ~f:a__of_xml contents
    | Xml_light.Xml.PCData _ -> raise (Unexpected_xml (xml, "")))
  | Some tag ->
    match child xml tag with
    | None -> []
    | Some child ->
      match child with
      | Xml_light.Xml.Element (_, _, contents) ->
        List.map ~f:a__of_xml contents
      | Xml_light.Xml.PCData _ -> raise (Unexpected_xml (xml, ""))

let option_of_xml ~tag a__of_xml xml =
  match child xml tag with
  | None -> None
  | Some child -> Some (a__of_xml child)

let lazy_t_of_xml a__of_xml xml =
  Lazy.from_val (a__of_xml xml)

let ref_of_xml a__of_xml xml =
  ref (a__of_xml xml)

let array_of_xml ~tag a__of_xml xml =
  let lst = list_of_xml ~tag a__of_xml xml in
  Array.of_list lst

let conversion to_string t =
  [ Xml_light.Xml.PCData (to_string t) ]

let conversion_sexp sexp_of_t t =
  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t) in
  conversion to_string t

type 'a to_xml = 'a -> xml list

let xml_of_unit () =
  conversion (fun () -> "") ()

let xml_of_bool t =
  conversion (function true -> "TRUE" | false -> "FALSE") t

let xml_of_string t =
  conversion (fun x -> x) t

let xml_of_char t =
  let f char = String.make 1 char in
  conversion f t

let xml_of_int t =
  conversion string_of_int t

let xml_of_float t =
  conversion_sexp Sexplib.Conv.sexp_of_float t

let xml_of_int32 t =
  conversion Int32.to_string t

let xml_of_int64 t =
  conversion Int64.to_string t

let xml_of_nativeint t =
  conversion Nativeint.to_string t

let xml_of_big_int t =
  conversion Big_int.string_of_big_int t

let xml_of_nat t =
  conversion Nat.string_of_nat t

let xml_of_num t =
  conversion Num.string_of_num t

let xml_of_ratio t =
  conversion Ratio.string_of_ratio t

let xml_of_list ~tag xml_of__a t =
  [ create_node ~tag ~body:(List.map ~f:(fun t -> create_node ~tag ~body:(xml_of__a t)) t) ]

let xml_of_option ~tag xml_of__a t =
  match t with
  | None -> []
  | Some t -> [create_node ~tag ~body:(xml_of__a t)]

let xml_of_lazy_t xml_of__a t =
  xml_of__a (Lazy.force_val t)

let xml_of_ref xml_of__a t =
  xml_of__a (!t)

let xml_of_array ~tag xml_of__a t = xml_of_list ~tag xml_of__a (Array.to_list t)

let xsd_conversion base =
  [Xml_light.Xml.Element ("xs:simpleType", [],
    [Xml_light.Xml.Element ("xs:restriction", ["base", "xs:" ^ base], [])])]

type to_xsd = xml list

let xsd_of_unit =
  xsd_conversion "string"

let xsd_of_string =
  xsd_conversion "string"

let xsd_of_char =
  xsd_conversion "string"

let xsd_of_int =
  xsd_conversion "integer"

let xsd_of_float =
  xsd_conversion "decimal"

let xsd_of_int32 =
  xsd_conversion "integer"

let xsd_of_bool =
  let restriction =
    let lst = ["TRUE"; "FALSE"; "true"; "false"; "True"; "False"] in
    Xml_light.Xml.Element ("xs:restriction",
      ["base", Restriction.Format.to_string `string],
      (List.map ~f:Restriction.enumeration lst))
  in
  [Xml_light.Xml.Element ("xs:simpleType", [], [restriction])]

let xsd_of_int64 =
  xsd_conversion "integer"

let xsd_of_nativeint =
  xsd_conversion "integer"

let xsd_of_big_int =
  xsd_conversion "integer"

let complex_type contents =
  Xml_light.Xml.Element ("xs:complexType", [],
    [Xml_light.Xml.Element ("xs:sequence", [], contents)])

let decomplexify xml =
  match xml with
  | [Xml_light.Xml.Element ("xs:complexType", [],
    [Xml_light.Xml.Element ("xs:sequence", [], [contents])])] -> contents
  | _ -> failwith "Not a complex_type"

let decomplexify_opt xml =
  try Some (decomplexify xml)
  with _ -> None

let decomplexify_list xml =
  match xml with
  | [Xml_light.Xml.Element ("xs:complexType", [],
    [Xml_light.Xml.Element ("xs:sequence", [], contents)])] -> Some contents
  | _ -> None

let type_of_simple xml =
  match xml with
  | [Xml_light.Xml.Element ("xs:simpleType", [],
    [Xml_light.Xml.Element ("xs:restriction", ["base", t], children)])] ->
      (match children with
      | [] -> t
      | _ -> (* enumeration values *) if "xs:string" = t then "xs:bool" else
        failwith "Enumeration isn't representing a bool, Bad type")
  | _ -> failwith "Not a simple type"


let xsd_element ?(attr=[]) ~name contents =
  Xml_light.Xml.Element ("xs:element", ("name", name) :: attr, contents)

let xml_element ?(attr=[]) ~name contents =
  Xml_light.Xml.Element (name, attr, contents)

let xsd_of_list' ~attr field_name xsd_of__a =
  let elements =
    [complex_type [xsd_element ~attr ~name:field_name xsd_of__a]]
  in
  [complex_type [xsd_element ~attr:[ "minOccurs", "0"; "maxOccurs", "1" ] ~name:field_name elements]]

let xsd_of_list field_name xsd_of__a =
  let attr = [ "minOccurs", "0"; "maxOccurs", "unbounded" ] in
  xsd_of_list' ~attr field_name xsd_of__a

let xsd_of_array = xsd_of_list

let xsd_of_nat =
  xsd_conversion "integer"

let xsd_of_num =
  xsd_conversion "decimal"

let xsd_of_ratio =
  xsd_conversion "decimal"

let xsd_of_ref xsd_of__a =
  xsd_of__a

let xsd_of_lazy_t xsd_of__a =
  xsd_of__a

let xsd_of_option field_name xsd_of__a =
  let attr = [ "minOccurs", "0"; "maxOccurs", "1" ] in
  [complex_type [xsd_element ~attr ~name:field_name xsd_of__a]]

module type X = sig
  type t
  val add_string : t -> string -> unit
end
module Write(X:X) = struct
  module Y = struct
    include X
    let string_of_char =
      let memo = Array.init 256 ~f:(fun code -> String.make 1 (Char.of_int_exn code)) in
      (fun char -> memo.(Char.to_int char))
    let add_char t char =
      add_string t (string_of_char char)
  end
  include Xml_light.Xml.Make(Y)
end
