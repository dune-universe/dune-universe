(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** QuantizeLinear, DequantizeLinear, DynamicQuantizeLinear *)

open Owl_symbolic_types

(** One input *)

module QuantizeLinear = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "QuantizeLinear"

  let create ?name x y_scale y_zero_point =
    let input = [| x; y_scale; y_zero_point |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module DeQuantizeLinear = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "DeQuantizeLinear"

  let create ?name x x_scale x_zero_point =
    let input = [| x; x_scale; x_zero_point |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module DynamicQuantizeLinear = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "DynamicQuantizeLinearr"

  let create ?output ?name x =
    let input = [| x |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    { name; input; output; attrs; out_shape = [| None; None; None |] }
end
