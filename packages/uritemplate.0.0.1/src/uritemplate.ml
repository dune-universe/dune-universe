module String = Stdcompat.String
module List = Stdcompat.List

type expansion_type =
  | Simple (* {var} *)
  | Reserved (* {+var} *)
  | Fragment (* {#var} *)
  | Dot (* {.var} *)
  | PathSegment (* {/var} *)
  | PathParameter (* {;var} *)
  | FormQuery (* {?var} *)
  | FormQueryContinuation (* {&var} *)

let expansion_type_of_string = function
  | "+" -> Reserved
  | "#" -> Fragment
  | "." -> Dot
  | "/" -> PathSegment
  | ";" -> PathParameter
  | "?" -> FormQuery
  | "&" -> FormQueryContinuation
  | _ -> Simple

let string_of_expansion_type = function
  | Reserved -> "+"
  | Fragment -> "#"
  | Dot -> "."
  | PathSegment -> "/"
  | PathParameter -> ";"
  | FormQuery -> "?"
  | FormQueryContinuation -> "&"
  | Simple -> ""

let separator_for_expansion_type = function
  | Simple | Reserved | Fragment -> ','
  | Dot -> '.'
  | PathSegment -> '/'
  | PathParameter -> ';'
  | FormQuery | FormQueryContinuation -> '&'


let re_for_tokens = Str.regexp "{[^{]+}\\|[^{}]+"
let re_for_is_var_expr = Str.regexp "^{.*}"
let re_for_prefix = Str.regexp "{\\([\\.#+/\\.;\\?&]?\\)\\([a-zA-Z0-9\\.%,_\\*:]+\\)}"

let is_var_expr v = Str.string_match re_for_is_var_expr v 0

(* Encoding *)
let encode_char c =
  Char.code c
  |> Printf.sprintf "%%%X"

let encode_str rex =
  Str.global_substitute
    rex
    (fun m -> String.get (Str.matched_string m) 0
              |> encode_char)

(* This is the same as a standard encodeUriComponent, but also encoding ! *)
let re_for_encode_reserved = Str.regexp "[^A-Za-z0-9-_.~*'()]"
let uri_encode_reserved = encode_str re_for_encode_reserved

let re_for_encode_full = Str.regexp "[^A-Za-z0-9;,/\\?:@&=\\+$-_\\.!~\\*'()#]"
let uri_encode_full = encode_str re_for_encode_full


let get_var expr_type var_name variables =
  match List.assoc_opt var_name variables, expr_type with
  | None, _ -> None
  | Some var, Fragment
  | Some var, Reserved -> Some (uri_encode_full var)
  | Some var, _ -> Some (uri_encode_reserved var)


let simple_expr buff _ var =
  Buffer.add_string buff var

let form_query buff var_name var =
  Buffer.add_string buff var_name; Buffer.add_char buff '='; Buffer.add_string buff var

let path_parameter buff var_name var =
  Buffer.add_string buff var_name;
  match var with
  | "" -> ()
  | var -> Buffer.add_char buff '='; Buffer.add_string buff var

let determine_expr_function buff expr_type =
  buff
  |> match expr_type with
  | FormQuery | FormQueryContinuation -> form_query
  | PathParameter -> path_parameter
  | _ -> simple_expr

let add_var_to_buff buff variables expr_type =
  let sep_str = separator_for_expansion_type expr_type in
  let f = determine_expr_function buff expr_type in
  fun var_name -> match get_var expr_type var_name variables with
    | None -> ()
    | Some var -> f var_name var; Buffer.add_char buff sep_str

let create_buffer expr_type =
  let buff = Buffer.create 10 in
  match expr_type with
  | Reserved -> buff
  | _ -> string_of_expansion_type expr_type |> Buffer.add_string buff; buff


let replace_variable ~variables str =
  match is_var_expr str with
  | false -> uri_encode_full str
  | true ->
    let _ = Str.string_match re_for_prefix str 0 in
    let expansion_type = Str.matched_group 1 str |> expansion_type_of_string in
    let vars = Str.matched_group 2 str |> String.split_on_char ',' in
    let buff = create_buffer expansion_type in
    List.iter (add_var_to_buff buff variables expansion_type) vars;
    match Buffer.length buff - 1 with
    | -1 | 0 -> ""
    | len -> Buffer.sub buff 0 len


let template_uri ~template ~variables =
  let buff = Buffer.create 10 in
  let rec aux index =
    if Str.string_match re_for_tokens template index && index < (String.length template) then
      let new_index = Str.match_end () in
      let replaced_var = Str.matched_string template |> replace_variable ~variables in
      Buffer.add_string buff replaced_var;
      aux new_index
    else
      Buffer.contents buff
  in
  aux 0
