(*Unfortunately Uint64 module does not define sexp converters, so we have to. 
  See below. Needed for ppx sexp extension.*)
module Uint8 = Uint8
open Sexplib
module Uint8_extended = struct
  module T = struct
    include Uint8
	      
    let sexp_of_t t =
      let a x = Sexp.Atom x and
	  l x = Sexp.List x in
      l [ a "uint8.t"; (Core.String.sexp_of_t (Uint8.to_string t))];;
      
    let t_of_sexp se =
      let s = Sexp.to_string se in
      let rec parse s i =
	match s.[i] with
	| '(' -> parse_list s (i+1)
	| ')' -> failwith "Unexpected closing parens"
	| _ -> parse_list s (i+1)
      and parse_list s i =
	let eoft = findend s (i+1) in
	let stype = Core.String.sub s ~pos:i ~len:8 in
	match stype with
	| "uint8.t" -> parse_uint8t s (i+9) eoft
	| _ -> failwith ("Unexpected type (expecting uint8.t; found " ^ s ^ ")")
      and findend s i =
	match s.[i] with
	| ')' -> i
	| _ -> findend s (i+1)
      and parse_uint8t s i j =
	try
	  let s = Core.String.sub s ~pos:i ~len:(j-i) in
	  Uint8.of_string s
	with _ -> failwith ("Failed to parse:" ^ s ^"; pos:" ^
			      (Core.Int.to_string i) ^ "len:" ^
				(Core.Int.to_string j))
      in
      parse s 0;;

    let sexp_of_uint8 = sexp_of_t
			  
    let uint8_of_sexp = t_of_sexp
			  
    let pp fmt t = (Format.fprintf fmt "%s") (to_string t)
    let show t = Uint8.to_string t

    let pp_uint8 = pp
    let show_uint8 = show

    let equal_uint8 t1 t2 = if Uint8.compare t1 t2 == 0 then true else false
    let compare_uint8 t1 t2 = Uint8.compare t1 t2
    let equal = equal_uint8
    let compare = compare_uint8

    let to_yojson t =
      let s = Uint8.to_string t in
      let s = Core.String.concat ["{uint8:";s;"}"] in 
      Yojson.Safe.from_string s;;
      
    let of_yojson j =
      try
	let s = Yojson.Safe.to_string j in
	let splits = String.split_on_char ':' s in
	let value_half = List.nth splits 1 in
	let rbracket_i = String.index value_half '}' in 
	let value = String.sub value_half 0 rbracket_i in
	let i = Uint8.of_string value in
	Ok i   
      with _err -> Error "uint8_extended::of_yojson() failed.";;
(* Only works with a hacked version of csvfields
    let to_xml v =
      [Csvfields.Xml.parse_string
	 (Core.String.concat [(to_string v)])]

    let of_xml xml =
      let sopt = Csvfields.Xml.contents xml in
      match sopt with
      | None -> raise (Failure "uint8_extended::of_xml() passed None as input")
      | Some s -> of_string s
			    
    let xsd_format =
      let open Csvfields.Xml.Restriction.Format in
      `string     
    let xsd_restrictions = []
    let xsd = []*)
  end 

  include T
  module T2 = Core.Comparable.Make(T)
	       
end 
