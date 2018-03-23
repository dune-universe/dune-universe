module Stable = struct
  open Core.Core_stable
  module V1 = struct
    type t = string [@@deriving sexp, bin_io, compare]
  end
end

open Core


type t = string [@@deriving sexp_of, compare, hash]
let create = Fn.id

module Generator = struct
  (* This prefix guarantees that the boundary will not appear in
     - Headers
     - Quoted-printable text
     - Base64 encoded content.

     The only posibility is that it might appear in plaintext, but
     that would be incredibly rare when using a good random number
     generator.
  *)
  let generate_raw ?(validate=(Fn.const true)) () =
    let rec generate () =
      let boundary = sprintf !"--_::%{Uuid}::_--" (Uuid.create ()) in
      if validate boundary then
        boundary
      else
        generate ()
    in
    if am_running_inline_test
    then "TEST-BOUNDARY"
    else generate ()
  ;;

  let generate ?text ?suggest () =
    ignore text;
    match suggest with
    | Some suggestion -> suggestion
    | None -> create (generate_raw ())
  ;;

end

let generate = Generator.generate

module Open = struct

  let to_string_monoid t =
    String_monoid.concat_string [ "\n"; "--"; t; "\n" ]
  ;;

end

module Close = struct

  let to_string_monoid t =
    String_monoid.concat_string ["\n"; "--"; t; "--"]
  ;;
end

module Open_first = struct

  let to_string_monoid t =
    String_monoid.concat_string ["--"; t; "\n"]
  ;;
end

let of_string = Fn.id
let to_string = Fn.id

let split t bstr =
  let lf       = Bigstring_shared.of_string "\n"       in
  let crlf     = Bigstring_shared.of_string "\r\n"     in
  let dashdash = Bigstring_shared.of_string "--"       in
  let t        = Bigstring_shared.of_string ("--" ^ t) in
  let match_after ~pos bstr ~pattern =
    let len = Bigstring_shared.length pattern in
    Option.some_if
      ((pos >= 0)
       && (pos + len <= Bigstring_shared.length bstr)
       && [%compare.equal: Bigstring_shared.t] pattern (Bigstring_shared.sub bstr ~pos ~len))
      (pos + len)
  in
  let match_before ~pos:end_ bstr ~pattern =
    let start = end_ - Bigstring_shared.length pattern in
    match_after ~pos:start bstr ~pattern
    |> Option.map ~f:(fun end_' -> assert (end_ = end_'); start)
  in
  let match_crlf direction ~pos bstr =
    if (pos = 0 || pos = Bigstring_shared.length bstr)
    then Some pos
    else
      let match_ =
        match direction with
        | `After  -> fun pattern -> match_after ~pos bstr ~pattern
        | `Before -> fun pattern -> match_before ~pos bstr ~pattern
      in
      Option.first_some (match_ crlf) (match_ lf)
  in
  let rec find_boundary pos =
    match Bigstring_shared.substr_index bstr ~pos ~pattern:t with
    | None ->
      (* No more occurrences of [BOUNDARY] so definitely at EOF *)
      `Eof
    | Some pos ->
      let no_prologue = pos = 0 in
      (* Ensure we are at the start of a line (after [CR]LF, or beginning of bigstring) *)
      match match_crlf `Before ~pos bstr with
      | None ->
        find_boundary (pos + 1) (* skip a character to avoid getting stuck in a loop *)
      | Some begin_ ->
        let pos = pos + Bigstring_shared.length t in
        let is_terminal, pos =
          match match_after ~pos bstr ~pattern:dashdash with
          | Some pos -> true,  pos
          | None     -> false, pos
        in
        (* Ensure we are at the end of a line (before [CR]LF, or end of bigstring) *)
        match match_crlf `After ~pos bstr with
        | None -> find_boundary pos
        | Some end_ ->
          if is_terminal
          then `Close_boundary (begin_,pos)
          else (
            if no_prologue
            then `Open_boundary_first end_
            else `Open_boundary (begin_,end_))
  in
  let rec loop pos acc has_prologue =
    let sub ?stop () =
      let stop = Option.value stop ~default:(Bigstring_shared.length bstr) in
      let len = stop - pos in
      if len <= 0
      then Bigstring_shared.empty
      else Bigstring_shared.sub ~pos ~len bstr
    in
    match find_boundary pos with
    | `Open_boundary_first pos ->
      loop pos acc false
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
      (chunk :: acc, epilogue, has_prologue)
    | `Eof ->
      let chunk = sub () in
      (chunk :: acc, None, has_prologue)
  in
  (* RFC 2046: A multipart body may have a prologue and an epilogue *)
  let parts, epilogue, has_prologue = (loop 0 [] true) in
  match List.rev parts with
  | [] -> (Some bstr, [], epilogue)
  | (prologue :: parts) when has_prologue ->
    (Some prologue, parts, epilogue)
  | parts ->
    (None, parts, epilogue)
;;

let join t (prologue, parts, epilogue) =
  if List.is_empty parts
  then (
    match prologue, epilogue with
    | Some prologue, Some epilogue ->
      String_monoid.plus
        (Bigstring_shared.to_string_monoid prologue)
        (Bigstring_shared.to_string_monoid epilogue)
    | Some content, None | None, Some content ->
      Bigstring_shared.to_string_monoid content
    | None, None ->
      String_monoid.of_string "\n")
  else (
    (* Different types of boundaries that may appear in a message *)
    let boundary_open_first = t |> Open_first.to_string_monoid in
    let boundary_open       = t |> Open.to_string_monoid in
    let boundary_close      = t |> Close.to_string_monoid in

    let first_boundary =
      if Option.is_some prologue then boundary_open else boundary_open_first
    in
    let prologue =
      Option.value_map prologue
        ~f:Bigstring_shared.to_string_monoid
        ~default:String_monoid.empty
    in
    let inner_boundary = boundary_open in
    let last_boundary = boundary_close in
    let epilogue =
      Option.value_map epilogue
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
