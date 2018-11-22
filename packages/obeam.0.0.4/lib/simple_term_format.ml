(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(*
  External term format is too detailed. Thus, This module provides simplified format.
 *)

module Z = Aux.Z

type t =
  | Integer of int
  | Atom of string
  | Tuple of int * t list
  | Map of int * (t * t) list
  | String of string
  | Binary of string
  | BigInt of Z.t
  | List of t list
  | Float of float
[@@deriving show]

(* from External term format *)
let rec of_etf etf =
  let module ETF = External_term_format in
  match etf with
  | ETF.SmallInteger v ->
     Integer v
  | ETF.Integer v ->
     Integer (Int32.to_int v)
  | ETF.Float v ->
     Float (Scanf.sscanf v "%f" (fun f -> f))
  | ETF.Atom name ->
     Atom name
  | ETF.SmallTuple (n, xs) ->
     Tuple (n, xs |> List.map of_etf)
  | ETF.Map (a, ps) ->
     Map (Int32.to_int a, ps |> List.map (fun (k, v) -> (of_etf k, of_etf v)))
  | ETF.Nil ->
     List []
  | ETF.String v ->
     String v
  | ETF.Binary bin ->
     Binary (Bitstring.string_of_bitstring bin)
  | ETF.SmallBig z ->
     BigInt z
  | ETF.LargeBig z ->
     BigInt z
  | ETF.List (xs, ETF.Nil) ->
     List (xs |> List.map of_etf)
  | ETF.List (xs, tail) ->
     (* TODO: fix *)
     failwith "unsupported"
  | ETF.NewFloat v ->
     Float v
  | ETF.AtomUtf8 name
  | ETF.SmallAtomUtf8 name ->
     Atom name
