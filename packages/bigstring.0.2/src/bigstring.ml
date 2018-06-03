
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 Interface to 1-dimension Bigarrays of bytes (char)} *)

type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module B = Bigarray.Array1

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create size = B.create Bigarray.char Bigarray.c_layout size

let empty = create 0

let make sz c =
  let buf = create sz in
  B.fill buf c;
  buf

let init size f =
  let a = create size in
  for i = 0 to size-1 do
    B.unsafe_set a i (f i)
  done;
  a

let fill = B.fill

let fill_slice s c i len = fill (B.sub s i len) c

let get = B.get

let unsafe_get = B.unsafe_get

let set = B.set

let unsafe_set = B.unsafe_set

let size = B.dim
let length = B.dim

let sub = B.sub

let blit a i b j len =
  let a' = sub a i len in
  let b' = sub b j len in
  B.blit a' b'

let copy a =
  let b = create (size a) in
  B.blit a b;
  b

(*$T
  copy (of_string "abcd") |> to_string = "abcd"
  *)

let fold f acc a =
  let rec aux f acc a i len =
    if i = len then acc
    else
      let acc = f acc (get a i) in
      aux f acc a (i+1) len
  in
  aux f acc a 0 (size a)

let foldi f acc a =
  let rec aux f acc a i len =
    if i = len then acc
    else
      let acc = f acc i (get a i) in
      aux f acc a (i+1) len
  in
  aux f acc a 0 (size a)

let iter f a =
  let n = size a in
  for i = 0 to n-1 do
    f (B.unsafe_get a i)
  done

let iteri f a =
  let n = size a in
  for i = 0 to n-1 do
    f i (B.unsafe_get a i)
  done

let rec equal_rec a b i len =
  i = len
  ||
  ( get a i = get b i && equal_rec a b (i+1) len)

let equal a b =
  size a = size b
  &&
  equal_rec a b 0 (size a)

(*$Q
  Q.(pair printable_string printable_string) (fun (s1, s2) -> \
    let a1 = of_string s1 and a2 = of_string s2 in \
    equal a1 a2 = (s1 = s2))
*)

let rec compare_rec a b i len_a len_b =
  if i=len_a && i=len_b then 0
  else if i=len_a then -1
  else if i=len_b then 1
  else
    match Char.compare (get a i) (get b i) with
    | 0 -> compare_rec a b (i+1) len_a len_b
    | n -> n

let compare a b =
  compare_rec a b 0 (size a) (size b)

(*$T
  compare (of_string "abc") (of_string "abd") < 0
  compare (of_string "abc") (of_string "abcd") < 0
  compare (of_string "abcd") (of_string "abc") > 0
  compare (of_string "abc") (of_string "b") < 0
*)

(*$Q
  Q.(pair string string) (fun (s1, s2) -> \
    let a1 = of_string s1 and a2 = of_string s2 in \
    (compare a1 a2 <= 0) = (String.compare s1 s2 <= 0))
*)

(** {2 Conversions} *)

let to_bytes a =
  Bytes.init (size a) (fun i -> B.unsafe_get a i)

let of_bytes b =
  init (Bytes.length b) (fun i -> Bytes.get b i)

let of_bytes_slice b i len =
  if i < 0 || i+len > Bytes.length b then invalid_arg "Bigstring.of_bytes";
  init len (fun j -> Bytes.get b (i+j))

let sub_bytes a i len =
  if i < 0 || i+len > size a then invalid_arg "Bigstring.sub_bytes";
  Bytes.init len (fun j -> B.get a (i+j))

let blit_to_bytes a i b j len =
  if i < 0 || j < 0 || i+len > size a || j+len > Bytes.length b
    then invalid_arg "Bigstring.blit_to_bytes";
  for x=0 to len-1 do
    Bytes.set b (j+x) (B.get a (i+x))
  done

let blit_of_bytes a i b j len =
  if i < 0 || j < 0 || i+len > Bytes.length a || j+len > size b
    then invalid_arg "Bigstring.blit_of_bytes";
  for x=0 to len-1 do
    B.set b (j+x) (Bytes.get a (i+x))
  done

(* naive replacement for {!String.init}, which is only available after 4.02 *)
let str_init_ n f =
  let bytes = Bytes.init n f in
  Bytes.unsafe_to_string bytes

let to_string a =
  str_init_ (size a) (fun i -> B.unsafe_get a i)

let of_string s =
  init (String.length s) (fun i -> String.get s i)

let of_string_slice s i len =
  if i < 0 || i+len > String.length s then invalid_arg "Bigstring.of_string_slice";
  init len (fun j -> String.get s (i+j))

let of_buffer b =
  let len = Buffer.length b in
  init len (Buffer.nth b)

let of_gen g =
  (* read [g] into some buffer *)
  let rec aux_ b g = match g() with
    | None -> ()
    | Some c -> Buffer.add_char b c; aux_ b g
  in
  let b = Buffer.create 64 in
  aux_ b g;
  of_buffer b

let sub_string a i len =
  if i < 0 || i+len > size a then invalid_arg "Bigstring.sub_string";
  str_init_ len (fun j -> B.get a (i+j))

(*$T
  of_string_slice "abcde" 1 3 |> to_string = "bcd"
*)

let blit_of_string a i b j len =
  if i < 0 || j < 0 || i+len > String.length a || j+len > size b
    then invalid_arg "Bigstring.blit_of_string";
  for x=0 to len-1 do
    B.set b (j+x) (String.get a (i+x))
  done

let blit_of_buffer buf i s j len =
  if i < 0 || j < 0 || i+len > Buffer.length buf || j+len > size s
    then invalid_arg "Bigstring.blit_of_buffer";
  for x=0 to len-1 do
    B.set s (j+x) (Buffer.nth buf (i+x))
  done

let to_seq a k = iter k a

let to_gen a =
  let i = ref 0 in
  let n = size a in
  fun () ->
    if !i = n then None
    else (
      let x = get a !i in
      incr i;
      Some x
    )

(*$T
  of_string "abcd" |> to_gen |> of_gen |> to_string = "abcd"
*)

let to_seq_slice a i len =
  to_seq (sub a i len)

let to_gen_slice a i len =
  to_gen (sub a i len)

let to_buffer s buf = iter (Buffer.add_char buf) s

let print out s =
  Format.pp_print_char out '"';
  iter
    (function
      | '\n' -> Format.pp_print_string out "\\n"
      | '\t' -> Format.pp_print_string out "\\t"
      | '\\' -> Format.pp_print_string out "\\\\"
      | '\000' -> Format.pp_print_string out "\\000"
      | c -> Format.pp_print_char out c)
    s;
  Format.pp_print_char out '"'

(*$= & ~printer:(fun s->s)
  (Format.asprintf "%a" print (create 3)) "\"\\000\\000\\000\""
  (Format.asprintf "%a" print (init 3 (fun i->Char.chr (i+65)))) "\"ABC\""
*)

(** {2 Utils} *)

let concat sep l =
  let len_sep = String.length sep in
  (* compute length of result *)
  let len =
    List.fold_left
      (fun n s ->
        let n = if n>0 then n+len_sep else n in (* add length of separator *)
        n + length s)
      0 l
  in
  (* allocate result *)
  let res = create len in
  let i = ref 0 in
  let j = ref 0 in
  List.iter
    (fun s ->
      if !j > 0 then (
        blit_of_string sep 0 res !i len_sep;
        i := !i + len_sep
      );
      incr j;
      blit s 0 res !i (length s);
      i := !i + length s)
    l;
  assert (!i = len);
  res

(*$T
  concat ";" [of_string "ab"; of_string "cd"; of_string "ef"] |> to_string = "ab;cd;ef"
  concat "yolo" [] |> to_string = ""
  concat "" [of_string "a"; of_string "bc"; of_string ""; of_string "d"] |> to_string = "abcd"
*)

let map ~f s = init (length s) (fun i -> f (unsafe_get s i))

let mapi ~f s = init (length s) (fun i -> f i (unsafe_get s i))

let lowercase s = map ~f:Char.lowercase_ascii s

let uppercase s = map ~f:Char.uppercase_ascii s

let index_pred ~f s =
  let rec aux f s i =
    if i=size s then raise Not_found
    else if f (unsafe_get s i) then i
    else aux f s (i+1)
  in
  aux f s 0

let rindex_pred ~f s =
  let rec aux f s i =
    if i= ~-1 then raise Not_found
    else if f (unsafe_get s i) then i
    else aux f s (i-1)
  in
  aux f s (size s-1)

let index s ~c = index_pred s ~f:(fun c' -> c=c')

(*$T
  index (of_string "abcdabcd") ~c:'a' = 0
  index (of_string "abcdabcd") ~c:'c' = 2
  try ignore (index (of_string "abcdabcd") ~c:'e'); false with Not_found -> true
*)

let rindex s ~c = rindex_pred s ~f:(fun c' -> c=c')

(*$T
  rindex (of_string "abcdabcd") ~c:'a' = 4
  rindex (of_string "abcdabcd") ~c:'c' = 6
  try ignore (rindex (of_string "abcdabcd") ~c:'e'); false with Not_found -> true
*)

let contains s ~c =
  try ignore (index s ~c); true
  with Not_found -> false

(*$T
  of_string "abcd" |> contains ~c:'a'
  of_string "abcd" |> contains ~c:'c'
  not (of_string "abcd" |> contains ~c:'e')
*)

let is_not_white_ = function ' ' | '\t' | '\r' | '\n' | '\012' -> false | _ -> true

let trim s =
  try
    let i = index_pred s ~f:is_not_white_ in
    let j = rindex_pred s ~f:is_not_white_ in
    assert (i<j);
    let len = j+1-i in
    sub s i len
  with Not_found -> empty

(*$T
  trim empty |> to_string = ""
  of_string "    \t\n\r\012   " |> trim |> to_string = ""
  of_string "  hello world  \n" |> trim |> to_string = "hello world"
  of_string "hello world  \n" |> trim |> to_string = "hello world"
  of_string "  hello world" |> trim |> to_string = "hello world"
*)

let for_all ~f s =
  try ignore (index_pred s ~f:(fun c -> not (f c))); false
  with Not_found -> true

let exists ~f s =
  try ignore (index_pred s ~f); true
  with Not_found -> false

let split_gen ~by s =
  let stop = ref false in
  let cur_sub = ref s in (* suffix slice of [s] *)
  (* line generator *)
  let g() =
    if !stop then None
    else
      try
        let i = index ~c:by !cur_sub in
        let slice = B.sub !cur_sub 0 i in
        cur_sub := B.sub !cur_sub (i+1) (size !cur_sub - i-1);
        Some slice
      with Not_found ->
        stop := true;
        if size !cur_sub > 0 then Some !cur_sub else None
  in
  g

let split ~by s =
  let rec gen_to_list acc g = match g() with
    | None -> List.rev acc
    | Some x -> gen_to_list (x::acc) g
  in
  gen_to_list [] (split_gen ~by s)

let lines_gen s = split_gen ~by:'\n' s
let lines s = split ~by:'\n' s

(*$T
  empty |> lines = []
  of_string "ab\ncde\nfg\nh" |> lines |> List.map to_string = ["ab"; "cde"; "fg"; "h"]
*)
