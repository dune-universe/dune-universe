(*
 * Copyright (c) 2016 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Astring

(* Split string into whitespace-separated substrings,
   taking into account quoting *)

let parse s =
  let skip_white s = String.Sub.drop
      ~max:Sys.max_string_length
      ~sat:Char.Ascii.is_white s in

  let split s =
    let rec inner in_quoted s so_far acc =
      let is_data = function
        | '\\' -> false
        | '"' -> false
        | c when Char.Ascii.is_white c -> in_quoted
        | _ -> true in

      let data,rem = String.Sub.span
          ~sat:is_data
          ~max:Sys.max_string_length s in

      match String.Sub.head rem with
      | Some c when Char.Ascii.is_white c ->
        let so_far = List.rev (data :: so_far) in
        inner in_quoted (skip_white rem) [] ((String.Sub.concat so_far)::acc)
      | Some '"' ->
        let so_far = data :: so_far in
        inner (not in_quoted) (String.Sub.tail rem) so_far acc
      | Some '\\' ->
        let rem = String.Sub.tail rem in
        begin match String.Sub.head rem with
          | Some c ->
            let so_far' = String.(sub (of_char c)) :: data :: so_far in
            inner in_quoted (String.Sub.tail rem) so_far' acc
          | None ->
            Error "Invalid escaping at end of string"
        end
      | Some c ->
        let e = Printf.sprintf "Something went wrong in the argv parser: Matched '%c'" c in
        Error e
      | None ->
        let so_far = List.rev (data :: so_far) in
        Ok (List.map (String.Sub.to_string) (List.rev ((String.Sub.concat so_far) :: acc)))
    in
    inner false s [] []
  in
  match split (String.sub s |> skip_white) with
  | Error s -> Error s
  | Ok s -> Ok (List.filter (fun s -> String.length s > 0) s)
