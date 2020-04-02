open Acommon
open Atypes

let slash_sub = Astring.Sub.v "/"
let wildcard2_sub = Astring.Sub.v "**"


let remove_useless_slashes s =
  let open Astring.Sub in
  cuts ~sep:slash_sub (v s)
  |> (function 
    | [] -> []
    | [e; e'] when is_empty e && is_empty e' -> [empty; empty]
    | x :: l -> x :: List.filter (fun s -> not (is_empty s)) l )
  |> concat ~sep:slash_sub
  |> to_string

module Path = struct
  type t = string
  (* from https://tools.ietf.org/html/rfc3986#appendix-B :
     the path of a URI contains any char but '?' or '#'.
     We add '*', '[' and ']' as forbidden char *)
  let is_valid s =
    let rec is_valid_rec i =
      if i >= Astring.length s then true
      else match Astring.unsafe_get s i with
      | '?' -> false                     (* ? not allowed *)
      | '#' -> false                     (* # not allowed *)
      | '[' -> false                     (* [ not allowed *)
      | ']' -> false                     (* ] not allowed *)
      | '*' -> false                     (* * not allowed *)
      | _ -> is_valid_rec (i+1)
    in is_valid_rec 0

  let of_string_opt s =
    let s = Astring.trim s in
    if is_valid s then Some (remove_useless_slashes s) else None

  let of_string s =
    Option.get_or_else (of_string_opt s)
    (fun () -> raise (Exception (`InvalidFormat (`Msg s))))

  let to_string s = s

  let length = Astring.length

  let compare = Astring.compare

  let equal = Astring.equal

  let is_relative p = if length p = 0 then true else Astring.unsafe_get p 0 <> '/'

  let add_prefix ~prefix p = remove_useless_slashes @@ (to_string prefix)^"/"^p

  let is_prefix ~affix path = Astring.is_prefix ~affix:(to_string affix) (to_string path)

  let remove_prefix length path = Astring.after length (to_string path) |> of_string
end [@@deriving show]


module PathExpr = struct

  type t = string

  (* from https://tools.ietf.org/html/rfc3986#appendix-B except that:
      - we don't allow '[' or ']' in the path part
   *)
  let is_valid s = 
    let rec is_valid_rec i =
      if i >= String.length s then true
      else match String.unsafe_get s i with
        | '?' -> false                     (* ? not allowed *)
        | '#' -> false                     (* # not allowed *)
        | '[' -> false                     (* [ not allowed *)
        | ']' -> false                     (* ] not allowed *)
        | '/' -> is_valid_rec (i+1)
        | '*' -> (if i+2 >= String.length s then true
                  else match (String.unsafe_get s (i+1), String.unsafe_get s (i+2)) with
                  | ('*', '/') -> is_valid_rec (i+1)
                  | ('*', _) -> false      (* **a and *** not allowed *)
                  | _ -> is_valid_rec (i+1) )
        | _   -> (if i+2 >= String.length s then true
                  else match (String.unsafe_get s (i+1), String.unsafe_get s (i+2)) with
                  | ('*', '*') -> false    (* a** not allowed *)
                  | _ -> is_valid_rec (i+1) )
    in is_valid_rec 0

  let of_string_opt s =
    let s = Astring.trim s in
    if is_valid s then Some (remove_useless_slashes s) else None

  let of_string s =
    Option.get_or_else (of_string_opt s)
    (fun () -> raise (Exception (`InvalidFormat (`Msg s))))

  let to_string e = e

  let of_path p = of_string @@ Path.to_string p

  let length = Astring.length

  let compare = Astring.compare

  let equal = Astring.equal

  let is_relative e = if length e = 0 then true else Astring.unsafe_get e 0 <> '/'

  let get_prefix e : Path.t = match Astring.find (fun c -> c = '*') e with
    | Some i -> Path.of_string @@ Astring.before i e
    | None -> Path.of_string e

  let add_prefix ~prefix e =
    remove_useless_slashes @@ (Path.to_string prefix)^"/"^e

  let remove_prefix length e = Astring.after length (to_string e) |> of_string

  let is_unique e = not @@ Astring.contains '*' e

  let as_unique_path e = if is_unique e then Some (Path.of_string e) else None

  type 'a element = | Some of 'a | Wildcard | None

  let get_char s i =
    let open Astring.Sub in
    if i >= length s then None
    else
      match unsafe_get s i with
      | '*' -> Wildcard
      | c -> Some c

  let get_chunk l i = match List.nth_opt l i with 
    | Some sub -> if Astring.Sub.equal_bytes sub wildcard2_sub then Wildcard else Some sub
    | None -> None

  let intersect ?(allow_empty=true) l1 l2 get elem_intersect = 
    if not allow_empty && get l1 0 = None then get l2 0 = None
    else if not allow_empty && get l2 0 = None then get l1 0 = None
    else 
      let rec intersect_from i1 i2 = 
        match (get l1 i1, get l2 i2) with 
        | (None, None)       -> true 
        | (Wildcard, None)   -> intersect_from (i1+1) i2
        | (None, Wildcard)   -> intersect_from i1 (i2+1) 
        | (Wildcard, _)      -> if intersect_from (i1+1) i2 then true 
                                else intersect_from i1 (i2+1)
        | (_, Wildcard)      -> if intersect_from i1 (i2+1) then true 
                                else intersect_from (i1+1) i2
        | (None, _)          -> false
        | (_, None)          -> false 
        | (Some e1, Some e2) -> if elem_intersect e1 e2 then intersect_from (i1+1) (i2+1)
                                else false
      in 
      intersect_from 0 0 

  let includes sub l get elem_includes =
    let rec includes_from subi i = 
      match (get l i, get sub subi) with 
      | (None, None)         -> true 
      | (Wildcard, None)      -> includes_from subi (i+1) 
      | (Wildcard, _)         -> if includes_from subi (i+1) then true 
                                else includes_from (subi+1) i
      | (_, Wildcard)         -> false
      | (None, _)            -> false 
      | (_, None)            -> false 
      | (Some ec, Some subc) -> if elem_includes subc ec then includes_from (subi+1) (i+1)
                              else false
    in includes_from 0 0

  let chunk_expr_intersect e1 e2 = intersect e1 e2 get_char Char.equal ~allow_empty:false

  let intersect e1 e2 =
    let open Astring.Sub in
    let e1_chunks = cuts ~sep:slash_sub (v e1) in
    let e2_chunks = cuts ~sep:slash_sub (v e2) in 
    intersect e1_chunks e2_chunks get_chunk chunk_expr_intersect

  let is_matching_path = intersect

  let chunk_expr_includes sub ce = includes sub ce get_char Char.equal 

  let includes ~subexpr e = 
    let open Astring.Sub in
    let sub_chunks = cuts ~sep:slash_sub (v subexpr) in
    let e_chunks = cuts ~sep:slash_sub (v e) in 
    includes sub_chunks e_chunks get_chunk chunk_expr_includes

  let rec longest_matching_part path e =
    if length e = 0 || is_matching_path path e then e
    else
      match Astring.find ~rev:true (fun c -> c= '/') e with
      | None -> ""
      | Some i -> longest_matching_part path (Astring.before i e)

  let remaining_after_match path e : t option =
    let prefix = longest_matching_part path e in
    match length @@ longest_matching_part path e with
    | 0 -> None
    | i ->
      let remain = Astring.after i e in
      Some (if Astring.is_suffix ~affix:"**" prefix then "/**"^remain else remain)


end [@@deriving show]
