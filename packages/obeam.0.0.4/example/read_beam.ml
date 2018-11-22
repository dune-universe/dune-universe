(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* EXAMPLE: BEAM reader *)
open Obeam

let () =
  let beam_filename =
    match Sys.argv with
    | [|_; n|] -> n
    | _ -> failwith (Printf.sprintf "format: %s <beam_filename>" Sys.argv.(0))
  in

  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match Chunk.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Chunk.cl_abst = opt_abst;
       Chunk.cl_dbgi = opt_dbgi;
     } = layout in
     let debug_info_buf =
       match opt_abst with
       | Some abst ->
          abst.Chunk.abst_buf
       | None ->
          begin
            match opt_dbgi with
            | Some dbgi ->
               dbgi.Chunk.dbgi_buf
            | None ->
               failwith "abst and dbgi chunk is not found"
          end
     in
     let _ =
       match External_term_format.parse debug_info_buf with
       | Ok (expr, _) ->
          Printf.printf "%s\n" (expr |> External_term_format.show);
          Printf.printf "%s\n" (expr |> Abstract_format.of_etf |> Abstract_format.show);
       | Error (msg, rest) ->
          Printf.printf "Failed to parse etf: %s\n" msg;
          Bitstring.hexdump_bitstring stdout rest
     in
     ()
  | Error (msg, rest) ->
     Printf.printf "Failed to parse chunk: %s\n" msg;
     Bitstring.hexdump_bitstring stdout rest
