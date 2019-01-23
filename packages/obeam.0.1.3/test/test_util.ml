(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* checker utils *)
let load_ast_from_beam beam_filename =
  let open Obeam in
  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match Beam.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Beam.cl_abst = opt_abst;
       Beam.cl_dbgi = opt_dbgi;
     } = layout in
     let parsed_result =
       match opt_abst with
       | Some abst ->
          External_term_format.parse abst.Beam.abst_buf
       | None ->
          begin
            match opt_dbgi with
            | Some dbgi ->
               External_term_format.parse dbgi.Beam.dbgi_buf
            | None ->
               failwith "abst and dbgi chunk is not found"
          end
     in
       (match parsed_result with
        | Ok (etf, _rest) ->
           Abstract_format.of_etf etf
        | Error (msg, _rest) ->
           failwith (Printf.sprintf "Failed: %s" msg))
  | Error (msg, _rest) ->
     failwith (Printf.sprintf "Failed : %s\n" msg)

let print_ast beam_filename =
  let open Obeam in
  load_ast_from_beam beam_filename
  |> [%sexp_of: (Abstract_format.t, Abstract_format.err_t) Result.t]
  |> Expect_test_helpers_kernel.print_s
