(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** RoiAlign, NonMaxSuppression *)

open Owl_symbolic_types

module RoiAlign = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable mode : [ `avg | `max ]
    ; mutable output_height : int
    ; mutable output_width : int
    ; mutable sampling_ratio : int
    ; mutable spatial_scale : float
    }

  let op_type = "RoiAlign"

  let create
      ?(mode = `avg)
      ?(height = 1)
      ?(width = 1)
      ?(ratio = 0)
      ?(scale = 1.)
      ?name
      x
      rois
      indices
    =
    let attrs = [||] in
    let input = [| x; rois; indices |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name
    ; input
    ; attrs
    ; out_shape = [| None |]
    ; mode
    ; output_height = height
    ; output_width = width
    ; sampling_ratio = ratio
    ; spatial_scale = scale
    }
end

module NonMaxSuppression = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable center_point_box : int
    }

  let op_type = "NonMaxSuppression"

  let create
      ?name
      ?(center_point_box = 0)
      boxes
      scores
      max_output_boxes_per_class
      iou_threshold
      score_threshold
    =
    let input =
      [| boxes; scores; max_output_boxes_per_class; iou_threshold; score_threshold |]
    in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; center_point_box }
end
