(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* EXAMPLE: BEAM reader *)
open! Base
open Obeam

let () =
  let beam_filename =
    match Sys.argv with
    | [|_; n|] -> n
    | _ -> failwith (Printf.sprintf "format: %s <beam_filename>" Sys.argv.(0))
  in

  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match Beam.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Beam.cl_abst = opt_abst;
       Beam.cl_dbgi = opt_dbgi;
     } = layout in
     let debug_info_buf =
       match opt_abst with
       | Some abst ->
          abst.Beam.abst_buf
       | None ->
          begin
            match opt_dbgi with
            | Some dbgi ->
               dbgi.Beam.dbgi_buf
            | None ->
               failwith "abst and dbgi chunk is not found"
          end
     in
     let _ =
       match External_term_format.parse debug_info_buf with
       | Ok (expr, _rest) ->
          let etf_view = expr |> External_term_format.sexp_of_t |> Sexp.to_string_hum in
          Stdio.printf "%s\n" etf_view;
          begin match expr |> Abstract_format.of_etf with
          | Ok abs_form ->
             let abs_form_view = abs_form |> Abstract_format.sexp_of_t |> Sexp.to_string_hum in
             Stdio.printf "%s\n" abs_form_view
          | Error _ ->
             ()
          end
       | Error (msg, rest) ->
          Stdio.printf "Failed to parse etf: %s\n" msg;
          Bitstring.hexdump_bitstring Stdio.stdout rest
     in
     ()
  | Error (msg, rest) ->
     Stdio.printf "Failed to parse chunk: %s\n" msg;
     Bitstring.hexdump_bitstring Stdio.stdout rest
