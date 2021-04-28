(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.2.0
  ---------------------------------------------------------------------------*)

open Astring

type highlight = { lang : string option; body : String.sub; linenos : bool }

let mk_highlight ?lang ?(body = String.Sub.empty) ?(linenos = false) () =
  { lang; body; linenos }

let pp_highlight ppf { lang; body; linenos } =
  let open Fmt in
  pf ppf "lang: %a linenos: %a@,body:@,%a" (option string) lang String.Sub.pp
    body bool linenos

let extract_tag ?(start = 0) ~start_tag ~stop_tag s =
  match String.find_sub ~start ~sub:start_tag s with
  | None -> None
  | Some start_idx -> (
      let start_data_idx = String.length start_tag + start_idx in
      match String.find_sub ~start:start_data_idx ~sub:stop_tag s with
      | None -> None
      | Some end_data_idx -> (
          let end_idx = String.length stop_tag + end_data_idx in
          String.with_index_range ~first:start_data_idx ~last:(end_data_idx - 1)
            s
          |> fun tag ->
          String.trim tag |> function
          | "" -> None
          | tag_data -> Some (start_idx, tag_data, end_idx)))

let extract_tags ?(start = 0) ~start_tag ~stop_tag s =
  let rec find_one acc start =
    if start >= String.length s then List.rev acc
    else
      match extract_tag ~start ~start_tag ~stop_tag s with
      | None -> List.rev acc
      | Some (start_idx, tag_data, end_idx) ->
          let acc = (start_idx, tag_data, end_idx) :: acc in
          find_one acc end_idx
  in
  find_one [] start

let extract_liquid_tag ?(start = 0) s =
  extract_tag ~start ~start_tag:"{%" ~stop_tag:"%}" s

let extract_liquid_tags ?(start = 0) s =
  extract_tags ~start ~start_tag:"{%" ~stop_tag:"%}" s

let map_tag ~sub (start_idx, _, end_idx) body =
  String.Sub.with_index_range ~last:(start_idx - 1) body |> fun hd ->
  String.Sub.with_index_range ~first:end_idx body |> fun tl ->
  String.Sub.concat [ hd; sub; tl ]

let map_tags ~start_tag ~stop_tag ~f body =
  extract_tags ~start_tag ~stop_tag (String.Sub.to_string body)
  |> List.fold_left
       (fun (acc, curpos) (start_idx, tag, end_idx) ->
         match f tag with
         | None ->
             String.Sub.with_index_range ~first:curpos ~last:(end_idx - 1) body
             |> fun hd -> (hd :: acc, end_idx)
         | Some sub ->
             String.Sub.v sub |> fun sub ->
             String.Sub.with_index_range ~first:curpos ~last:(start_idx - 1)
               body
             |> fun hd ->
             let acc = sub :: hd :: acc in
             (acc, end_idx))
       ([], 0)
  |> fun (acc, lastpos) ->
  let chunks = String.Sub.with_index_range ~first:lastpos body :: acc in
  String.Sub.concat (List.rev chunks)

let map_tag_bodies ~start_tag ~stop_tag ~f_start ~f_stop ~f_map body =
  extract_tags ~start_tag ~stop_tag (String.Sub.to_string body)
  |> List.fold_left
       (fun (acc, found_start, body_start_idx, args, curpos)
            (start_idx, tag, end_idx) ->
         match found_start with
         | false -> (
             match f_start tag with
             | None ->
                 String.Sub.with_index_range ~first:curpos ~last:(end_idx - 1)
                   body
                 |> fun hd -> (hd :: acc, false, 0, args, end_idx)
             | args ->
                 String.Sub.with_index_range ~first:curpos ~last:(start_idx - 1)
                   body
                 |> fun hd ->
                 let acc = hd :: acc in
                 (acc, true, end_idx, args, end_idx))
         | true -> (
             match f_stop tag with
             | false ->
                 String.Sub.with_index_range ~first:curpos ~last:(end_idx - 1)
                   body
                 |> fun hd -> (hd :: acc, true, body_start_idx, args, end_idx)
             | true ->
                 String.Sub.with_index_range ~first:body_start_idx
                   ~last:(start_idx - 1) body
                 |> fun tag_body ->
                 f_map args tag_body |> fun tag_body ->
                 let acc = tag_body :: acc in
                 (acc, false, 0, None, end_idx)))
       ([], false, 0, None, 0)
  |> fun (acc, found_start, _, _, curpos) ->
  if found_start then raise (Failure "dangling start tag and no end tag found");
  let chunks = String.Sub.with_index_range ~first:curpos body :: acc in
  String.Sub.concat (List.rev chunks)

let map_liquid_tags ~f body = map_tags ~start_tag:"{%" ~stop_tag:"%}" ~f body

let map_liquid_tag_bodies ~f_start ~f_stop ~f_map body =
  map_tag_bodies ~start_tag:"{%" ~stop_tag:"%}" ~f_start ~f_stop ~f_map body

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
