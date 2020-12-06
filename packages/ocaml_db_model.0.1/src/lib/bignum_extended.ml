module Bignum = Bignum
module Bignum_extended = struct
  include Bignum

  let pp = Bignum.pp_hum
  let to_string = Bignum.to_string_hum ~delimiter:',' ~decimals:9 ~strip_zero:true

  let show = Bignum.to_string_hum ~delimiter:',' ~decimals:9 ~strip_zero:true
	       
  let to_yojson t =
    let s = to_string_hum t in
    let s = String.concat "" ["{bignum:";s;"}"] in 
    Yojson.Safe.from_string s;;
    
  let of_yojson j =
    try
      let s = Yojson.Safe.to_string j in
      let splits = String.split_on_char ':' s in
      let value_half = List.nth splits 1 in
      let rbracket_i = String.index value_half '}' in 
      let value = String.sub value_half 0 rbracket_i in
      let bignum = of_string value in
      Ok bignum   
    with _err -> Error "bignum_extended::of_yojson() failed.";;

(* Only usefule with a local hacked version of csvfields

  let to_xml v =
    [Csvfields.Xml.parse_string
       (Core.String.concat [(to_string_hum v)])]

  let of_xml xml =
    let sopt = Csvfields.Xml.contents xml in
    match sopt with
    | None -> raise (Failure "bignum_extended::of_xml() passed None as input")
    | Some s -> of_string s
    
  let xsd_format =
    let open Csvfields.Xml.Restriction.Format in
    `string     
  let xsd_restrictions = []
  let xsd = [] *)    
end 
