module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type t = string [@@deriving sexp, bin_io, compare]
  end
end

open Core

type t = string [@@deriving sexp_of, compare, hash]
type boundary = t

let of_string = Fn.id
let to_string = Fn.id

module Open = struct
  let to_string_monoid t = String_monoid.concat_string [ "\n"; "--"; t; "\n" ]
end

module Close = struct
  let to_string_monoid t = String_monoid.concat_string [ "\n"; "--"; t; "--" ]
end

module Open_first = struct
  let to_string_monoid t = String_monoid.concat_string [ "--"; t; "\n" ]
end

let split t bstr =
  let lf = Bigstring_shared.of_string "\n" in
  let crlf = Bigstring_shared.of_string "\r\n" in
  let dashdash = Bigstring_shared.of_string "--" in
  let t = Bigstring_shared.of_string ("--" ^ t) in
  let match_after ~pos bstr ~pattern =
    let len = Bigstring_shared.length pattern in
    Option.some_if
      (pos >= 0
       && pos + len <= Bigstring_shared.length bstr
       && [%compare.equal: Bigstring_shared.t]
            pattern
            (Bigstring_shared.sub bstr ~pos ~len))
      (pos + len)
  in
  let match_before ~pos:end_ bstr ~pattern =
    let start = end_ - Bigstring_shared.length pattern in
    match_after ~pos:start bstr ~pattern
    |> Option.map ~f:(fun end_' ->
      assert (end_ = end_');
      start)
  in
  let match_crlf direction ~pos bstr =
    if pos = 0 || pos = Bigstring_shared.length bstr
    then Some pos
    else (
      let match_ =
        match direction with
        | `After -> fun pattern -> match_after ~pos bstr ~pattern
        | `Before -> fun pattern -> match_before ~pos bstr ~pattern
      in
      Option.first_some (match_ crlf) (match_ lf))
  in
  let rec find_boundary pos =
    match Bigstring_shared.substr_index bstr ~pos ~pattern:t with
    | None ->
      (* No more occurrences of [BOUNDARY] so definitely at EOF *)
      `Eof
    | Some pos ->
      let no_prologue = pos = 0 in
      (* Ensure we are at the start of a line (after [CR]LF, or beginning of bigstring) *)
      (match match_crlf `Before ~pos bstr with
       | None ->
         find_boundary (pos + 1) (* skip a character to avoid getting stuck in a loop *)
       | Some begin_ ->
         let pos = pos + Bigstring_shared.length t in
         let is_terminal, pos =
           match match_after ~pos bstr ~pattern:dashdash with
           | Some pos -> true, pos
           | None -> false, pos
         in
         (* Ensure we are at the end of a line (before [CR]LF, or end of bigstring) *)
         (match match_crlf `After ~pos bstr with
          | None -> find_boundary pos
          | Some end_ ->
            if is_terminal
            then `Close_boundary (begin_, pos)
            else if no_prologue
            then `Open_boundary_first end_
            else `Open_boundary (begin_, end_)))
  in
  let rec loop pos acc has_prologue =
    let sub ?stop () =
      let stop = Option.value stop ~default:(Bigstring_shared.length bstr) in
      let len = stop - pos in
      if len <= 0 then Bigstring_shared.empty else Bigstring_shared.sub ~pos ~len bstr
    in
    match find_boundary pos with
    | `Open_boundary_first pos -> loop pos acc false
    | `Open_boundary (stop, pos) ->
      let chunk = sub ~stop () in
      loop pos (chunk :: acc) has_prologue
    | `Close_boundary (stop, pos) ->
      let chunk = sub ~stop () in
      let epilogue =
        if pos < Bigstring_shared.length bstr
        then Some (Bigstring_shared.sub ~pos bstr)
        else None
      in
      chunk :: acc, epilogue, has_prologue
    | `Eof ->
      let chunk = sub () in
      chunk :: acc, None, has_prologue
  in
  (* RFC 2046: A multipart body may have a prologue and an epilogue *)
  let parts, epilogue, has_prologue = loop 0 [] true in
  match List.rev parts with
  | [] -> Some bstr, [], epilogue
  | prologue :: parts when has_prologue -> Some prologue, parts, epilogue
  | parts -> None, parts, epilogue
;;

module Generator = struct
  type nonrec t = t Sequence.t

  let sexp_of_t t =
    [%sexp ((Sequence.take t 5 |> Sequence.to_list) @ [ "..." ] : string list)]
  ;;

  (* This boundary pattern ensures that the boundary should not appear in
     - Headers
     - Quoted-printable text
     - Base64 encoded content.

     The only posibility is that it might appear in plaintext, but
     that would be incredibly rare.

     We avoid conflicts by generating IDs with different numbers as needed.
  *)
  let default =
    Sequence.unfold ~init:0 ~f:(fun num ->
      let str = sprintf "--==::BOUNDARY::%06d::==--" num in
      Some (str, num + 1))
  ;;

  let%expect_test _ =
    Sequence.take default 5 |> Sequence.iter ~f:print_endline;
    [%expect
      {|
       --==::BOUNDARY::000000::==--
       --==::BOUNDARY::000001::==--
       --==::BOUNDARY::000002::==--
       --==::BOUNDARY::000003::==--
       --==::BOUNDARY::000004::==--
      |}]
  ;;

  (* Increment the last numeric component to avoid number conflicts. *)
  let from_existing_boundary str = Sequence.append (Sequence.singleton str) default

  let%expect_test _ =
    Sequence.take (from_existing_boundary "BOUNDARY") 5 |> Sequence.iter ~f:print_endline;
    [%expect
      {|
      BOUNDARY
      --==::BOUNDARY::000000::==--
      --==::BOUNDARY::000001::==--
      --==::BOUNDARY::000002::==--
      --==::BOUNDARY::000003::==--
      |}]
  ;;

  let find_nonflicting t parts =
    Sequence.find_exn t ~f:(fun t ->
      List.for_all parts ~f:(fun part ->
        (* This will incorrectly report a conflict if [BOUNDARY] occurs in the text or is
           a prefix/suffix of any existing boundary. This is very unlikely to happen in
           practice, and would just result in an alternative boundary being used. *)
        not (String_monoid.is_substring part ~substring:t)))
  ;;

  let%expect_test _ =
    find_nonflicting
      (from_existing_boundary "BOUNDARY")
      [ String_monoid.of_string "foobar" ]
    |> print_endline;
    [%expect {| BOUNDARY |}]
  ;;

  let%expect_test _ =
    find_nonflicting
      (from_existing_boundary "BOUNDARY")
      [ String_monoid.of_string "--BOUNDARY--" ]
    |> print_endline;
    [%expect {| --==::BOUNDARY::000000::==-- |}]
  ;;

  let%expect_test _ =
    find_nonflicting
      (from_existing_boundary "BOUNDARY")
      [ String_monoid.of_string "...BOUNDARY...--==::BOUNDARY::000000::==--..." ]
    |> print_endline;
    [%expect {| --==::BOUNDARY::000001::==-- |}]
  ;;
end

let generate_non_conflicting_boundary ?prologue ~parts ?epilogue t =
  Generator.find_nonflicting
    t
    ((Option.to_list prologue |> List.map ~f:Bigstring_shared.to_string_monoid)
     @ parts
     @ (Option.to_list epilogue |> List.map ~f:Bigstring_shared.to_string_monoid))
;;

let join_without_checking_for_conflicts ?prologue ~parts ?epilogue t =
  if List.is_empty parts
  then (
    match prologue, epilogue with
    | Some prologue, Some epilogue ->
      String_monoid.plus
        (Bigstring_shared.to_string_monoid prologue)
        (Bigstring_shared.to_string_monoid epilogue)
    | Some content, None | None, Some content ->
      Bigstring_shared.to_string_monoid content
    | None, None -> String_monoid.of_string "\n")
  else (
    (* Different types of boundaries that may appear in a message *)
    let boundary_open_first = t |> Open_first.to_string_monoid in
    let boundary_open = t |> Open.to_string_monoid in
    let boundary_close = t |> Close.to_string_monoid in
    let first_boundary =
      if Option.is_some prologue then boundary_open else boundary_open_first
    in
    let prologue =
      Option.value_map
        prologue
        ~f:Bigstring_shared.to_string_monoid
        ~default:String_monoid.empty
    in
    let inner_boundary = boundary_open in
    let last_boundary = boundary_close in
    let epilogue =
      Option.value_map
        epilogue
        ~f:Bigstring_shared.to_string_monoid
        ~default:String_monoid.empty
    in
    String_monoid.concat
      [ prologue
      ; first_boundary
      ; String_monoid.concat ~sep:inner_boundary parts
      ; last_boundary
      ; epilogue
      ])
;;
