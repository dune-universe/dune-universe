open Core

open Bigstring
type t = t_frozen [@@deriving sexp, bin_io, compare, hash]

let to_bigstring t = t
let of_bigstring t = t

let to_string_monoid t = String_monoid.of_bigstring t
let of_string_monoid t = String_monoid.to_bigstring t

let to_string t = to_string t
let of_string s = of_string s

let empty = Bigstring.create 0;;

let length t  = Bigstring.length t

let sub ?pos ?len t =
  let pos, len =
    match pos, len with
    | None, None         -> 0, length t
    | None, Some len     -> 0, len
    | Some pos, None     -> pos, ((length t) - pos)
    | Some pos, Some len -> pos, len
  in
  Bigstring.sub_shared ~pos ~len t
;;

let to_lexbuf t =
  let offset = ref 0 in
  let len = length t in
  Lexing.from_function
    (fun dst n ->
       let read = min n (len - !offset) in
       Bigstring.To_string.blit
         ~src:t
         ~src_pos:!offset
         ~len:read
         ~dst
         ~dst_pos:0
       ;
       offset := !offset + read;
       read)
;;

let foldi t ~init ~f =
  let len = length t in
  let rec loop init pos =
    if pos >= len then
      init
    else
      loop (f pos init t.{pos}) (pos + 1)
  in
  loop init 0
;;

(* Copied from String.split_lines. *)
let split_lines =
  let back_up_at_newline ~t ~pos ~eol =
    pos := !pos - (if !pos > 0 && get t (!pos - 1) = '\r' then 2 else 1);
    eol := !pos + 1;
  in
  fun t ->
    let n = length t in
    if n = 0
    then []
    else
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if get t !pos = '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if get t !pos <> '\n'
        then decr pos
        else
          (* Becuase [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol
      done;
      sub t ~pos:0 ~len:!eol :: !ac
;;

let%test_unit _ =
  List.iter ~f:(fun s ->
    let actual = split_lines (of_string s) |> List.map ~f:to_string in
    let expect = String.split_lines s in
    if actual <> expect
    then failwiths "split_lines bug" (s, `actual actual , `expect expect)
           [%sexp_of:
             string *
             [ `actual of string list ] *
             [ `expect of string list ]])
    [ "";
      "\n";
      "a";
      "a\n";
      "a\nb";
      "a\nb\n";
      "a\n\n";
      "a\n\nb";
      "a\r\n\nb";
      "\ra\r\n\nb";
      "\ra\r\n\nb\r";
      "\ra\r\n\nb\r\n";
    ]
;;

let of_bigbuffer_volatile buffer =
  (* If this isn't done, the buffer might contain extra uninitialized characters *)
  Bigstring.sub_shared
    ~pos:0
    ~len:(Bigbuffer.length buffer)
    (Bigbuffer.volatile_contents buffer)
;;

let substr_index ?(pos=0) t ~pattern =
  if length pattern = 0
  then Some pos
  else (
    let c = Bigstring.get pattern 0 in
    let last_index = Bigstring.length t - Bigstring.length pattern in
    let rec loop pos =
      if pos > last_index
      then None
      else (
        match Bigstring.find c t ~pos ~len:(last_index - pos + 1) with
        | None -> None
        | Some pos ->
          assert (pos <= last_index);
          let found_ = Bigstring.sub_shared t ~pos ~len:(Bigstring.length pattern) in
          if Bigstring.equal pattern found_
          then Some pos
          else loop (pos + 1))
    in
    loop pos)
;;
