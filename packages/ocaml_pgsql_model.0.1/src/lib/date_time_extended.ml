(*Does SQL allow us to insert using a float value?
  New module for date type, represented as days since epoch? 
    As intervals of 24 hours starting from epoch + 1 second?
  New module for time type, as seconds?
READ sql standard...literals.
*)
module Date_time_extended = struct
  include Core.Time
  (*                                                0123456789ABCDEFGHIJKLM*)
  (*MUST support alernate format with this example: 20190304000000 [-5:EST] *)
  let of_string s =
    try
      of_string s
    with _ ->
	 (*try alternate format*)
	 let year_part = Core.String.slice s 0 4 in
	 let month_part = Core.String.slice s 4 6 in
	 let day_part = Core.String.slice s 6 8 in
	 let hour_part = Core.String.slice s 8 10 in
	 let minute_part = Core.String.slice s 10 12 in
	 let seconds_part = Core.String.slice s 12 14 in
	 let hours_offset_gmt = int_of_string (Core.String.slice s 16 18) in 
	 (*can parse in format: %Y-%m-%dT%H:%M:%S.%s%Z and then some...*)
	 let composed =
	   Core.String.concat [year_part;"-";month_part;"-";day_part;"T";
			  hour_part;":";minute_part;":";seconds_part] in
	 let z = Core.Time.Zone.of_utc_offset ~hours:hours_offset_gmt in 
	 (*of_string_gen ~default_zone:z ~find_zone:"new york" ~if_no_time_zone:(Use_this_one z) s*)
	 of_string_gen ~if_no_timezone:(`Use_this_one z) composed

  let show t = to_string_abs ~zone:(Core.Time.Zone.of_utc_offset ~hours:(-5)) t;;

  let to_yojson t =
    let s = show t in 
    let s = Core.String.concat ["{dt:";s;"}"] in 
    Yojson.Safe.from_string s;;

  let of_yojson j =
    try
      let s = Yojson.Safe.to_string j in
      let splits = String.split_on_char ':' s in
      let value_half = List.nth splits 1 in
      let rbracket_i = String.index value_half '}' in 
      let value = String.sub value_half 0 rbracket_i in
      let t = of_string value in
      Ok t
    with _err -> Error "date_time_extended::of_yojson() failed.";;
   
  let equal t1 t2 = not (is_earlier t1 ~than:t2) && not (is_earlier t2 ~than:t1)
  let compare t1 t2 = if is_earlier t1 ~than:t2 then 1
		      else if is_earlier t2 ~than:t1 then -1
		      else 0
(*Not useful unless have local hacked version of csvfields
  let to_xml v =
    [Csvfields.Xml.parse_string
       (Core.String.concat [(to_string v)])]

  let of_xml xml =
    let sopt = Csvfields.Xml.contents xml in
    match sopt with
    | None -> raise (Failure "date_extended::of_xml() passed None as input")
    | Some s -> of_string s

  let xsd_format =
    let open Csvfields.Xml.Restriction.Format in
    `string
  let xsd_restrictions = []
  let xsd = [] *)
end
