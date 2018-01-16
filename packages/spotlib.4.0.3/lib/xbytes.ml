open Base

open Bytes

(** Array *)

let get_opt s pos = try Some (get s pos) with _ -> None

(** Conversion *)

let make1 = make 1

let of_char = make1

let to_array s = Array.init (length s) & fun i -> unsafe_get s i

let to_code_array s = Array.init (length s) & fun i -> Char.code & unsafe_get s i
  
(** Transform *)
  
let chop_eols s =
  let len = length s in
  if len > 1 then
    match get s & len-1 with
    | '\n' -> 
        if len > 2 && get s (len-2) = '\r' then sub s 0 (len-2)
        else sub s 0 (len-1)
    | '\r' -> sub s 0 (len-1)
    | _ -> s
  else s

let %TEST chop_eols =
  let chop_eols' = (!>$) *< chop_eols *< (!<$) in
  chop_eols' "a" = "a"
  && chop_eols' "a\n" = "a"
  && chop_eols' "a\r\n" = "a"
  && chop_eols' "a\r" = "a"

let take len str = sub str 0 len
let prefix = take
let drop len str = sub str len (length str - len)
let drop_postfix len str = sub str 0 (length str - len)

let %TEST drop_postfix =
  drop_postfix 6 !<$"hello world" = !<$"hello"

let postfix len str = 
  let l = length str in
  sub str (l-len) len

let %TEST drop_postfix =
  postfix 5 !<$"hello world" = !<$"world"

(** Search *)

let rec index_rec s lim i c =
  if i >= lim then None
  else if unsafe_get s i = c then Some i
  else index_rec s lim (i + 1) c
  
let index_opt s c = index_rec s (length s) 0 c

let index_from_to s from to_ c =
  let l = length s in
  if from < 0 || from > to_ || to_ >= l then 
    invalid_arg "Xstring.index_from_to" 
  else
    index_rec s (to_+1) from c

(** Substring *)

let sub_from_to s from to_ = 
  if from > to_ then invalid_arg "sub_from_to";
  sub s from (to_ - from + 1)

let is_space_or_tab = function ' ' | '\t' -> true | _ -> false
let is_newline_or_return = function '\n' | '\r' -> true | _ -> false

let sub' s pos len =
  let orig_len = length s in
  let len = max (min (pos + len) orig_len - pos) 0 in
  sub s pos len

let %TEST sub'_ =
  let sub'' x a b = to_string @@ sub' (of_string x) a b in
  assert (sub'' "hello" 0 4 = "hell");
  assert (sub'' "hello" 0 5 = "hello");
  assert (sub'' "hello" 0 6 = "hello");
  assert (sub'' "hello" 0 7 = "hello");
  assert (sub'' "hello" 3 2 = "lo");
  assert (sub'' "hello" 3 3 = "lo");
  assert (sub'' "hello" 3 4 = "lo");
  assert (sub'' "hello" 5 5 = "")

(** Substring search *)

let find s pos f =
  let len = length s in
  let rec scan pos =
    if pos >= len then None
    else if f (unsafe_get s pos) then Some pos else scan (pos + 1)
  in
  scan pos

let is_prefix' ?(from=0) sub str =
  let sublen = length sub in
  try 
    if Bytes.sub str from sublen = sub then Some (drop (from + sublen) str)
    else None
  with _ -> None

let %TEST is_prefix' =
  is_prefix' !<$"hello" !<$"hello world" = Some !<$" world"

let is_prefix ?(from=0) sub str =
  let sublen = length sub in
  try 
    Bytes.sub str from sublen = sub
  with _ -> false

let %TEST is_prefix = is_prefix !<$"hello" !<$"hello world"

let is_sub ?from:(pos=0) ~needle:sub str =
  let str_len = length str in
  let sub_len = length sub in
  if pos + sub_len > str_len then false
  else 
    let rec iter pos = 
      if pos + sub_len > str_len then false
      else if is_prefix ~from:pos sub str then true
      else iter (pos+1)
    in
    iter pos

let %TEST is_sub_ =
  assert (is_sub ~needle:!<$"hello" !<$"hello world");
  assert (is_sub ~needle:!<$"hello" !<$"bye world" = false);
  assert (is_sub ~needle:!<$"shindanmaker.com" !<$"http://shindanmaker.com/341161")

let is_postfix sub str =
  let sublen = length sub in
  try postfix sublen str = sub with _ -> false
  
let %TEST is_postfix = is_postfix !<$"world" !<$"hello world"

let is_postfix' sub str =
  let sublen = length sub in
  try
    if postfix sublen str = sub then Some (drop_postfix sublen str)
    else None
  with _ -> None

let %TEST is_postfix = is_postfix' !<$"world" !<$"hello world" = Some !<$"hello "

(** Splitting *)
  
let lines s =
  let rec aux st start_pos pos = 
    match get_opt s pos with
    | None (* eos *) ->
        List.rev (
          if start_pos = pos then st
          else (sub s start_pos (pos - start_pos), empty) :: st
        )
    | Some '\n' ->
        aux ((sub s start_pos (pos - start_pos), of_string "\n") :: st) (pos+1) (pos+1)
    | Some '\r' ->
        begin match get_opt s (pos+1) with
        | Some '\n' ->
            aux ((sub s start_pos (pos - start_pos), of_string "\r\n") :: st) (pos+2) (pos+2)
        | _ ->
            aux ((sub s start_pos (pos - start_pos), of_string "\r") :: st) (pos+1) (pos+1)
        end
    | _ -> aux st start_pos (pos+1)
  in
  aux [] 0 0

let %TEST lines =
  let ss = List.map (fun (x,y) -> to_string x, to_string y) 
             & lines & of_string "hello\nworld\r\ngood\rday" in
  let res = ss = ["hello", "\n"; "world", "\r\n"; "good", "\r"; "day", ""] in
  if not res then List.iter (fun (x,y) -> Printf.eprintf "%S,%S\n" x y) ss;
  res

let %TEST lines =
  lines (of_string "\na\nb\rc\r\nd\n\re\n\nf\ng") = 
    List.map (fun (x,y) -> of_string x, of_string y)
      [ ""  , "\n";
        "a" , "\n";
        "b" , "\r";
        "c" , "\r\n";
        "d" , "\n";
        ""  , "\r";
        "e" , "\n";
        ""  , "\n";
        "f" , "\n";
        "g" , "" ] 

(* split a string according to char_sep predicate *)
let split char_sep str =
  let len = length str in
  if len = 0 then [] else
    let rec skip_sep cur =
      if cur >= len then cur
      else if char_sep (get str cur) then skip_sep (succ cur)
      else cur  in
    let rec split beg cur =
      if cur >= len then 
	if beg = cur then []
	else [sub str beg (len - beg)]
      else if char_sep (get str cur)
	   then 
	     let nextw = skip_sep cur in
	      (sub str beg (cur - beg))
		::(split nextw nextw)
	   else split beg (succ cur) in
    let wstart = skip_sep 0 in
    split wstart wstart

let %TEST split =
  split (function ' ' -> true | _ -> false) !<$" hello  world " = [!<$"hello"; !<$"world"]

let split_at len str = sub str 0 len, sub str len (length str - len)

let %TEST split_at =
  split_at 3 !<$"hello world" = (!<$"hel", !<$"lo world")

let split1 ?(from=0) f str =
  match find str from f with
  | None -> None
  | Some pos -> Some (sub str 0 pos, sub str (pos+1) (length str - pos - 1))

let %TEST split1 =
  split1 ((=) ' ') (of_string "hello world bye") = Some ((of_string "hello"), (of_string "world bye"))

let words = split (function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false)

module Set = Xset.Make(struct type t = bytes let compare (x:t) y = compare x y end)

let index_bytes_from str pos sub =
  let sub_len = length sub in
  if sub_len = 0 then pos 
  else 
    let limit = length str - sub_len in
    let rec iter i = 
      if i > limit then raise Not_found
      else if is_sub str ~from:i ~needle:sub then i
      else iter (i+1)
    in
    iter pos

let scani_left f acc ?from ?to_ s = 
  let from = Option.default from (fun () -> 0) in
  let to_ = Option.default to_ (fun () -> length s - 1) in
  let rec fold acc pos = 
    if pos > to_ then acc
    else 
      match f pos acc & unsafe_get s pos with
      | `Continue acc -> fold acc & pos + 1
      | `Stop acc -> acc
  in
  fold acc from
    
let foldi_left f acc s = scani_left f acc s

let random len = 
  let s = create len in
  for i = 0 to len - 1 do
    unsafe_set s i (Char.chr (Random.int 256))
  done;
  s

let random_hum len =
  let s = create len in
  let range = Char.code '~' - Char.code ' ' + 1 in
  let shift = Char.code ' ' in
  for i = 0 to len - 1 do
    unsafe_set s i (Char.chr (Random.int range + shift))
  done;
  s

let replace_chars from to_ s =
  let s' = copy s in
  iteri (fun p -> function
    | c when c = from -> unsafe_set s' p to_
    | _ -> ()) s';
  s'

module Pervasives = struct
  let chop_eols = chop_eols
end
