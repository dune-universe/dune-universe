(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.3.0
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open R.Infix

type fields = (string * Yaml.value) list

type body = string

type t = fields * body

let of_string t =
  let module S = String.Sub in
  let s = S.v t in
  let sep = S.v "---\n" in
  let win = S.v "---\r\n" in
  match (S.cut ~sep s, S.cut ~sep:win s) with
  | None, None -> Ok ([], t)
  | Some (pre, post), _ | _, Some (pre, post) ->
      if S.length pre = 0 then
        match (S.cut ~sep post, S.cut ~sep:win post) with
        | None, None ->
            print_endline "none?";
            Ok ([], t)
        | Some (yaml, body), _ | _, Some (yaml, body) -> (
            match Yaml.of_string (S.to_string yaml) with
            | Ok (`O fields) -> Ok (fields, S.to_string body)
            | Ok v ->
                Error
                  (`Msg
                    (Fmt.strf "Unexpected Yaml: %s"
                       (Sexplib.Sexp.to_string_hum (Yaml_sexp.sexp_of_value v))))
            | Error (`Msg msg) -> Error (`Msg msg))
      else Ok ([], t)

exception Parse_failure of string

let result_to_exn = function
  | Ok r -> r
  | Error (`Msg m) -> raise (Parse_failure m)

let find key f = List.assoc_opt key f

let keys f = List.map (fun (k, _) -> k) f

let of_string_exn t = of_string t |> result_to_exn

let fields = fst

let fields_to_yaml fs = `O fs

let body = snd

let pp_body ppf b = Fmt.(pf ppf "%s" b)

let pp_fields ppf fields =
  let open Fmt in
  let pp_colon = unit ":" in
  let pp_field =
    pair ~sep:pp_colon string (fun fmt v ->
        Fmt.pf fmt "%s" (Ezjsonm.value_to_string v))
  in
  let pp_fields = list ~sep:Format.pp_force_newline pp_field in
  pf ppf "%a" pp_fields fields

let pp ppf t =
  let open Fmt in
  pf ppf "\n---\n";
  pf ppf "%a" (pair ~sep:(unit "\n---\n") pp_fields pp_body) t

let parse_date_exn ?(and_time = true) s =
  let ymd, hms, tz =
    match and_time with
    | true -> (
        match String.cuts ~sep:" " s with
        | [ ymd ] -> (Some ymd, None, None)
        | [ ymd; hms ] -> (Some ymd, Some hms, None)
        | ymd :: hms :: tz :: _ -> (Some ymd, Some hms, Some tz)
        | [] -> (None, None, None))
    | false -> (Some s, None, None)
  in
  let dfail s = raise (Parse_failure s) in
  match ymd with
  | None -> dfail "No valid year/month/date found"
  | Some ymd -> (
      let to_int l s =
        try int_of_string s
        with _ -> dfail (l ^ " component is not a valid integer")
      in
      let date =
        match String.cuts ~sep:"-" ymd with
        | [ y; m; d ] -> (to_int "year" y, to_int "month" m, to_int "date" d)
        | [ y; m ] -> dfail "No date component found"
        | [ y ] -> dfail "No month or date component found"
        | [] -> dfail "Empty date component"
        | _ -> dfail "Date component must be in form YYYY-MM-DD"
      in
      let time =
        match hms with
        | None -> (0, 0, 0)
        | Some hms -> (
            match String.cuts ~sep:":" hms with
            | [ h; m; s ] ->
                (to_int "hour" h, to_int "minute" m, to_int "seconds" s)
            | [ h; m ] -> (to_int "hour" h, to_int "minute" m, 0)
            | [ h ] -> (to_int "hour" h, 0, 0)
            | [] -> dfail "Empty time component"
            | _ -> dfail "Time component must be in form HH:MM:SS")
      in
      let tz =
        (* can be [+-]HH[:]MM *)
        match tz with
        | None -> 0
        | Some tz ->
            String.filter
              (function '0' .. '9' | '+' | '-' -> true | _ -> false)
              tz
            |> fun tz ->
            let off, hh, mn =
              match String.length tz with
              | 5 ->
                  let hh =
                    String.with_range ~first:1 ~len:2 tz
                    |> to_int "timezone hour"
                  in
                  let mn =
                    String.with_range ~first:3 ~len:2 tz
                    |> to_int "timezone min"
                  in
                  let off =
                    match String.get_head tz with
                    | '+' -> 1
                    | '-' -> -1
                    | _ -> dfail "Invalid timezone direction"
                  in
                  (off, hh, mn)
              | 4 ->
                  let hh =
                    String.with_range ~first:0 ~len:2 tz
                    |> to_int "timezone hour"
                  in
                  let mn =
                    String.with_range ~first:2 ~len:2 tz
                    |> to_int "timezone min"
                  in
                  let off = 1 in
                  (off, hh, mn)
              | _ -> dfail ("Unable to parse timezone " ^ tz)
            in
            ((hh * 3600) + (mn * 1800)) * off
      in
      Ptime.of_date_time (date, (time, tz)) |> function
      | None -> dfail "Invalid date/time"
      | Some p -> p)

let parse_date ?(and_time = true) s =
  try R.ok (parse_date_exn ~and_time s) with Parse_failure m -> R.error_msg m

let parse_filename s =
  let open R.Infix in
  let dashc = String.concat ~sep:"-" in
  match String.cuts ~sep:"-" s with
  | y :: m :: d :: title -> (
      dashc title |> function
      | "" -> R.error_msg "Empty title not allowed"
      | title ->
          Fpath.v title |> Fpath.split_ext |> fun (title, ext) ->
          String.drop ~max:1 ext |> fun ext ->
          Fpath.to_string title |> fun title ->
          parse_date ~and_time:false (dashc [ y; m; d ]) >>| fun time ->
          (time, title, ext))
  | _ -> R.error_msg "Unable to find a date component in filename"

let parse_filename_exn s = parse_filename s |> result_to_exn

let slug_of_string s =
  String.map
    (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '-')
    s
  |> String.Ascii.lowercase

let title ?fname f =
  let open R.Infix in
  match find "title" f with
  | Some (`String t) -> Ok t
  | _ -> (
      match fname with
      | None ->
          R.error_msg "Unable to find a title key or parse the filename for it"
      | Some fname -> parse_filename fname >>| fun (_, title, _) -> title)

let date ?fname f =
  let open R.Infix in
  match (find "date" f, fname) with
  | Some (`String d), _ -> parse_date ~and_time:true d
  | _, Some fname -> parse_filename fname >>| fun (date, _, _) -> date
  | _, None ->
      R.error_msg "Unable to find a date key or parse the filename for it"

let slug ?fname f =
  let open R.Infix in
  match find "slug" f with
  | Some (`String s) -> R.ok (slug_of_string s)
  | _ -> (
      (* query filename instead *)
      match fname with
      | Some fname ->
          parse_filename fname >>| fun (_, title, _) -> slug_of_string title
      | None -> (
          match title ?fname f with
          | Ok t -> R.ok (slug_of_string t)
          | Error _ ->
              R.error_msg
                "Unable to find a slug key or parse the filename for it"))

let date_exn ?fname f = date ?fname f |> result_to_exn

let title_exn ?fname f = title ?fname f |> result_to_exn

let slug_exn ?fname f = title ?fname f |> result_to_exn

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
