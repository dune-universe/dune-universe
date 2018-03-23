module Stable = struct
  open !Core.Core_stable
  module V1 = struct
    type t = Core.Bigstring.t_frozen [@@deriving sexp, bin_io, compare, hash]
  end
end

open Core
open Bigstring

type t = Stable.V1.t [@@deriving sexp_of, compare, hash]

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
       Bigstring.To_bytes.blit
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
let iter_lines_rev t ~f =
  let back_up_at_newline ~t ~pos ~eol =
    pos := !pos - (if !pos > 0 && get t (!pos - 1) = '\r' then 2 else 1);
    eol := !pos + 1;
  in
  let n = length t in
  if n = 0
  then ()
  else begin
    (* Invariant: [-1 <= pos < eol]. *)
    let pos = ref (n - 1) in
    let eol = ref n in
    (* We treat the end of the string specially, because if the string ends with a
       newline, we don't want an extra empty string at the end of the output. *)
    if get t !pos = '\n' then back_up_at_newline ~t ~pos ~eol;
    while !pos >= 0 do
      if get t !pos <> '\n'
      then decr pos
      else
        (* Becuase [pos < eol], we know that [start <= eol]. *)
        let start = !pos + 1 in
        f (sub t ~pos:start ~len:(!eol - start));
        back_up_at_newline ~t ~pos ~eol
    done;
    f (sub t ~pos:0 ~len:!eol)
  end
;;

let split_lines t =
  let acc = ref [] in
  iter_lines_rev t ~f:(fun line -> acc := line :: !acc);
  !acc
;;

let lines_seq ?include_empty_last_line t =
  let open Sequence.Generator in
  let rec traverse ~sol ~pos =
    let prev_char_is_cr = pos <> 0 && get t (pos -1) = '\r' in
    if pos = length t
    then begin
      (* Safe because [length t > 0] *)
      if Option.is_some include_empty_last_line || not (get t (pos - 1) = '\n')
      then begin
        let len = pos - sol in
        yield (sub t ~pos:sol ~len)
        >>= fun () ->
        return ()
      end else return ()
    end
    else begin
      if get t pos <> '\n'
      then traverse ~sol ~pos:(pos + 1)
      else begin
        let len = pos - sol - (if prev_char_is_cr then 1 else 0) in
        yield (sub t ~pos:sol ~len)
        >>= fun () ->
        let pos' = pos + 1 in
        traverse ~sol:pos' ~pos:pos'
      end
    end
  in
  if length t = 0
  then Sequence.empty
  else Sequence.Generator.run (traverse ~sol:0 ~pos:0)
;;

let iter_lines t ~f = Sequence.iter (lines_seq t) ~f

let%expect_test "split_lines and iter_lines" =
  let split_lines t =
    split_lines (of_string t)
    |> List.map ~f:to_string
  in
  let split_lines_via_iter_lines t =
    let acc = ref [] in
    iter_lines (of_string t) ~f:(fun line -> acc := line :: !acc);
    List.rev_map !acc ~f:to_string
  in
  let impls =
    [ "Bigstring.iter_lines_rev", split_lines
    ; "Bigstring.iter_lines",     split_lines_via_iter_lines
    ; "String.split_lines",       String.split_lines
    ]
  in
  List.iter ~f:(fun s ->
    let results = List.map impls ~f:(fun (desc, f) -> desc, f s) in
    let all_equal =
      List.dedup_and_sort results ~compare:(fun (_, r1) (_, r2) ->
        [%compare: string list] r1 r2)
      |> List.length
      |> Int.equal 1
    in
    if not all_equal
    then
      raise_s
        [%message "Mismatching implementations"
                    ~input:(s : string)
                    ~_:(results : (string * string list) list)])
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
