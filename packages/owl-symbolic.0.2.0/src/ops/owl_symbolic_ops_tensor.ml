(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Reshape, Concat, Split, Identity, Pad, Cast, Squeeze, Tile 
  * Shape, Size, Transpose, Slice, SpaceToDepth, IsNaN, NonZero, Where
  * ScatterElements，ScatterND, GatherElements, GatherND, IsInf, UnSqueeze, 
  * DepthToSpace, Compress, ReverseSequence, Unique, Resize, OneHot
  * Gather(deprecated), Scatter(deprecated), Upsample(deprecated),
  *)

open Owl_symbolic_types

module Reshape = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable shape : int array
    ; mutable out_shape : int array option array
    }

  let op_type = "Reshape"

  let create ?name shp data_name shp_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; shp_name |] in
    { name; input; attrs; shape = shp; out_shape = [| Some shp |] }
end

module Identity = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable idx : int (* the index of its parent's corresonding output *)
    }

  let op_type = "Identity"

  let create ?name ?(idx = 0) x =
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input = [| x |]; attrs = [||]; out_shape = [| None |]; idx }
end

module Split = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    ; mutable split : int array
    }

  let op_type = "Split"

  let create ?output ?name ?(axis = 0) x split =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    let out_shape = Array.(make (length split) None) in
    { name; input; output; attrs; out_shape; axis; split }
end

module Concat = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    }

  let op_type = "Concat"

  let create ?name ?(axis = 0) xs =
    let attrs = [||] in
    let input = xs in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axis }
end

(* Note: The pads value are made part of t, but we also make a ``pads'' node  
 * when building graph, so as to meet the specification of ONNX. 
 *)

module Pad = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable mode : string
    ; mutable p : int array
    }

  let op_type = "Pad"

  let create ?name ?(mode = "constant") ?value data pads pdata =
    if mode <> "constant" && mode <> "reflect" && mode <> "edge"
    then failwith "Pad mode should be constant, reflect, or edge.";
    let attrs = [||] in
    let input =
      match value with
      | Some v -> [| data; pads; v |]
      | None   -> [| data; pads |]
    in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; mode; p = pdata }
end

module Cast = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable target : number_type
    }

  let op_type = "Cast"

  let create ?name x target =
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input = [| x |]; attrs = [||]; out_shape = [| None |]; target }
end

module Squeeze = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array option
    }

  let op_type = "Squeeze"

  let create ?name ?axes data_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name |] in
    { name; input; attrs; out_shape = [| None |]; axes }
end

module UnSqueeze = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    }

  let op_type = "UnSqueeze"

  let create ?name axes data_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name |] in
    { name; input; attrs; out_shape = [| None |]; axes }
end

module Tile = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable repeats : int array
    }

  let op_type = "Tile"

  let create ?name x_name repeats_name repeats =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name; repeats_name |] in
    { name; input; attrs; out_shape = [| None |]; repeats }
end

module Shape = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Shape"

  let create ?name x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module Size = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Size"

  let create ?name x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module Transpose = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable perm : int array option
    }

  let op_type = "Transpose"

  let create ?name ?perm x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |]; perm }
end

module Slice = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array option
    ; mutable starts : int array
    ; mutable ends : int array
    ; mutable steps : int array option
    }

  let op_type = "Slice"

  let create ?name ?axes ?steps starts ends x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |]; axes; starts; ends; steps }
end

module SpaceToDepth = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable blocksize : int
    }

  let op_type = "SpaceToDepth"

  let create ?name x_name blocksize =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |]; blocksize }
end

module DepthToSpace = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable blocksize : int
    ; mutable mode : string
    }

  let op_type = "DepthToSpace"

  let create ?name ?(mode = "DCR") blocksize x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    if mode <> "CRD" && mode <> "DCR" then failwith "DepthToSpace: illegal mode.";
    { name; input; attrs; out_shape = [| None |]; blocksize; mode }
end

module IsNaN = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "IsNaN"

  let create ?name x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module IsInf = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable detect_neg : bool
    ; mutable detect_pos : bool
    }

  let op_type = "IsInf"

  let create ?name ?(neg = true) ?(pos = true) x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |]; detect_neg = neg; detect_pos = pos }
end

module NonZero = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "NonZero"

  let create ?name x_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module Where = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Where"

  let create ?name cond_name x_name y_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| cond_name; x_name; y_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module ScatterElements = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    }

  let op_type = "ScatterElements"

  let create ?name ?(axis = 0) data_name indices_name updates_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; indices_name; updates_name |] in
    { name; input; attrs; out_shape = [| None |]; axis }
end

module ScatterND = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "ScatterND"

  let create ?name data_name indices_name updates_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; indices_name; updates_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module GatherElements = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    }

  let op_type = "GatherElements"

  let create ?name ?(axis = 0) data_name indices_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; indices_name |] in
    { name; input; attrs; out_shape = [| None |]; axis }
end

module GatherND = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "GatherND"

  let create ?name data_name indices_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; indices_name |] in
    { name; input; attrs; out_shape = [| None |] }
end

module Compress = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int option
    }

  let op_type = "Compress"

  let create ?name ?axis data_name indices_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; indices_name |] in
    { name; input; attrs; out_shape = [| None |]; axis }
end

module ReverseSeq = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable batch_axis : int
    ; mutable time_axis : int
    }

  let op_type = "ReverseSequence"

  let create ?name ?(batch_axis = 1) ?(time_axis = 0) data_name indices_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name; indices_name |] in
    if not ((batch_axis, time_axis) = (1, 0) || (batch_axis, time_axis) = (0, 1))
    then failwith "reverseSequence: illegal batch or time axis";
    { name; input; attrs; out_shape = [| None |]; batch_axis; time_axis }
end

module Unique = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int option
    ; mutable sorted : bool
    }

  let op_type = "Unique"

  let create ?name ~output ?axis ?(sorted = true) data_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data_name |] in
    { name; input; output; attrs; out_shape = [| None; None; None; None |]; axis; sorted }
end

module Resize = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable coordinate_transformation_mode : string
    ; mutable cubic_coeff_a : float
    ; mutable exclude_outside : int
    ; mutable extrapolation_value : float
    ; mutable mode : string
    ; mutable nearest_mode : string
    ; mutable scales : float array option
    ; mutable sizes : int array option
    }

  let op_type = "Resize"

  let create
      ?name
      ?(coordinate_mode = "half_pixel")
      ?(cubic_coeff_a = -0.75)
      ?(exclude_outside = 0)
      ?(extrapolation_value = 0.)
      ?(mode = "nearest")
      ?(nearest_mode = "round_prefer_floor")
      ?scales
      ?(scales_name = "")
      ?sizes_name
      ?sizes
      x_name
      roi_name
    =
    assert (
      Array.mem
        coordinate_mode
        [| "half_pixel"
         ; "pytorch_half_pixel"
         ; "align_corners"
         ; "asymmetric"
         ; "tf_half_pixel_for_nn"
         ; "tf_crop_and_resize"
        |]);
    assert (Array.mem mode [| "nearest"; "linear"; "cubic" |]);
    assert (
      Array.mem
        nearest_mode
        [| "round_prefer_floor"; "round_prefer_ceil"; "floor"; "ceil" |]);
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input =
      match sizes_name with
      | Some s -> [| x_name; roi_name; scales_name; s |]
      | None   -> [| x_name; roi_name; scales_name |]
    in
    { name
    ; input
    ; attrs
    ; out_shape = [| None |]
    ; coordinate_transformation_mode = coordinate_mode
    ; cubic_coeff_a
    ; exclude_outside
    ; extrapolation_value
    ; mode
    ; nearest_mode
    ; scales
    ; sizes
    }
end

module OneHot = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    ; mutable depth : int
    }

  let op_type = "Identity"

  let create ?name ?(axis = -1) depth indices_n depth_n value_n =
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| indices_n; depth_n; value_n |] in
    { name; input; attrs = [||]; out_shape = [| None |]; axis; depth }
end
