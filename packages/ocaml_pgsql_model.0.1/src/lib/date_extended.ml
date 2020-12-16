(*Core date includes sexp ppx extension, but not show, eq, ord (at least not explicitly), yojson, xml etc*)
module Date_extended = struct
  include Core.Date
  (*type t = Core.Core_Date*)
     
  let to_yojson t =
    let s = to_string_iso8601_basic t in
    let s = String.concat "" ["{date:";s;"}"] in 
    Yojson.Safe.from_string s;;

  let of_yojson j =
    try
      let s = Yojson.Safe.to_string j in
      let splits = String.split_on_char ':' s in
      let value_half = List.nth splits 1 in
      let rbracket_i = String.index value_half '}' in 
      let value = String.sub value_half 0 rbracket_i in
      let date = of_string_iso8601_basic ~pos:0 value in
      Ok date      
    with _err -> Error "date_extended::of_yojson() failed.";;

  let to_string t = to_string_iso8601_basic t;;
  let show t = to_string_iso8601_basic t;;

  let of_string_exn s = of_string_iso8601_basic ~pos:0 s;;
    
  let equal_date_extended t1 t2 = Core.Int.(=) (diff t1 t2) 0;;

  let compare_date_extended t1 t2 =
    match (diff t1 t2) with
    | 0 -> 0
    | x when Core.Int.(>) x 0 -> 1
    | _ -> -1

  (*Needed for eq and ord*)
  let equal = equal_date_extended
  let compare = compare_date_extended
		  
  let to_string_iso8601_basic_with_dashes t =
    (*Range of Month.to_int is 1 through 12*)
    Core.String.concat [(string_of_int (year t));"-";
			(string_of_int (Core.Month.to_int (month t)));"-";
			(string_of_int (day t))]
(* Only useful if we have a hacked version of csvfields
  let to_xml v =
    [Csvfields.Xml.parse_string
       (Core.String.concat [(to_string v)])]

  let of_xml xml =
    let sopt = Csvfields.Xml.contents xml in
    match sopt with
    | None -> raise (Failure "date_extended::of_xml() passed None as input")
    | Some s -> of_string_exn s

  let xsd_format =
    let open Csvfields.Xml.Restriction.Format in
    `string
  let xsd_restrictions = [] *)
(*[(Xml_light.Xml.parse_string "<xs:minLength value=\"8\"")]*)
(*THIS function is not exposed. Perhaps we should expose it?
[(Csvfields.Xml.Restriction.restriction "minLength" 8)]
  let xsd = []*)
end
