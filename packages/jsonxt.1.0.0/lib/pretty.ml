module type Intf = sig
  val pretty_print : Format.formatter -> 'a Json_internal.constrained -> unit
  val pretty_print_to_string : 'a Json_internal.constrained -> string
  val pretty_print_to_channel : out_channel -> 'a Json_internal.constrained -> unit
end

module Make(Compliance : Compliance.S) = struct
  let to_json_string s =
    let buf = Buffer.create 100 in
    let add_char = Buffer.add_char buf in
    let add_string = Buffer.add_string buf in
    let add_quote_string s = add_char '"'; Utils.escape ~add_char ~add_string s; add_char '"' in
    add_quote_string s;
    Buffer.contents buf

  let pp_list sep ppx out l =
    let pp_sep out () = Format.fprintf out "%s@ " sep in
    Format.pp_print_list ~pp_sep ppx out l

  let rec format (out:Format.formatter) json : unit =
    match json with
      | `Null -> Format.pp_print_string out "null"
      | `Bool b -> Format.pp_print_bool out b
      | `Int i -> Format.pp_print_string out (string_of_int i)
      | `Float f ->
        let s = Compliance.number_to_string f in
        Format.pp_print_string out s
      | `String s -> Format.pp_print_string out (to_json_string s)
      | `Intlit s
      | `Floatlit s
      | `Stringlit s -> Format.pp_print_string out s
      | `List [] -> Format.pp_print_string out "[]"
      | `List l -> Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]" (pp_list "," format) l
      | `Assoc [] -> Format.pp_print_string out "{}"
      | `Assoc l ->
        Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," format_field) l
      | `Tuple l ->
        if l = [] then Format.pp_print_string out "()"
        else Format.fprintf out "(@,%a@;<0 -2>)" (pp_list "," format) l
      | `Variant (s, None) ->
        Format.fprintf out "<%s>" (to_json_string s)
      | `Variant (s, Some json) ->
        let s = to_json_string s in
        Format.fprintf out "<@[<hv2>%s: %a@]>" s format json

  and format_field out (name, json) =
    Format.fprintf out "@[<hv2>%s: %a@]" (to_json_string name) format json

  let pretty_print out json =
    Format.fprintf out "@[<hv2>%a@]" format json

  let pretty_print_to_string json =
    Format.asprintf "%a" pretty_print json

  let pretty_print_to_channel oc json =
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@?" pretty_print json

end
