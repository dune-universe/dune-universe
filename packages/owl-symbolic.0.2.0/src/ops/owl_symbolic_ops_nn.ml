(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Implemented: Conv, MaxPool, BatchNormalization, Dropout,
  * GlobalAveragePool, GlobalMaxPool, AveragePool, Flatten, 
  * InstanceNormalization *)

(** ConvTranspose, 
MaxUnpool, LpPool, MaxRoiPool, QLinearConv, ConvInteger, GlobalLpPool, 
LpNormalization, Shrink, LRN, TfIdfVectorizer, StringNormalizer, MeanVarianceNormalization
*)

open Owl_symbolic_types

module Conv = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable auto_pad : string
          (* one of NOTSET (default), SAME_UPPER, SAME_LOWER and VALID *)
    ; mutable dilations : int array
    ; mutable pads : int array option
    ; mutable strides : int array
    ; mutable group : int
    ; mutable dim : int
    }

  let op_type = "Conv"

  let create
      ?name
      ?(dim = 2)
      ?strides
      ?(padding = VALID)
      ?dilations
      ?bias_name
      input_name
      kernel_name
    =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let dilations =
      match dilations with
      | Some d ->
        assert (Array.length d = dim);
        d
      | None   -> Array.make dim 1
    in
    let strides =
      match strides with
      | Some s ->
        assert (Array.length s = dim);
        s
      | None   -> Array.make dim 1
    in
    let auto_pad, pads =
      match padding with
      | SAME_UPPER -> "SAME_UPPER", None
      | SAME_LOWER -> "SAME_LOWRE", None
      | VALID      -> "VALID", None
      | PAD p      ->
        assert (Array.length p = dim);
        "NOTSET", Some p
    in
    let input =
      match bias_name with
      | Some b -> [| input_name; kernel_name; b |]
      | None   -> [| input_name; kernel_name |]
    in
    { name
    ; input
    ; attrs
    ; out_shape = [| None |]
    ; auto_pad
    ; dilations
    ; pads
    ; strides
    ; group = 1
    ; dim
    }
end

module ConvTranspose = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable auto_pad : string
    ; mutable dilations : int array
    ; mutable pads : int array option
    ; mutable strides : int array
    ; mutable group : int
    ; mutable dim : int
    }

  let op_type = "ConvTranspose"

  let create
      ?name
      ?(dim = 2)
      ?strides
      ?(padding = VALID)
      ?dilations
      ?bias_name
      input_name
      kernel_name
    =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let dilations =
      match dilations with
      | Some d ->
        assert (Array.length d = dim);
        d
      | None   -> Array.make dim 1
    in
    let strides =
      match strides with
      | Some s ->
        assert (Array.length s = dim);
        s
      | None   -> Array.make dim 1
    in
    let auto_pad, pads =
      match padding with
      | SAME_UPPER -> "SAME_UPPER", None
      | SAME_LOWER -> "SAME_LOWRE", None
      | VALID      -> "VALID", None
      | PAD p      ->
        assert (Array.length p = dim);
        "NOTSET", Some p
    in
    let input =
      match bias_name with
      | Some b -> [| input_name; kernel_name; b |]
      | None   -> [| input_name; kernel_name |]
    in
    { name
    ; input
    ; attrs
    ; out_shape = [| None |]
    ; auto_pad
    ; dilations
    ; pads
    ; strides
    ; group = 1
    ; dim
    }
end

module MaxPool = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable auto_pad : string
    ; mutable ceil_mode : int
    ; mutable dilations : int array
    ; mutable kernel_shp : int array
    ; mutable pads : int array option
    ; mutable storage_order : int
    ; mutable strides : int array
    }

  let op_type = "MaxPool"

  let create ?output ?(padding = VALID) ?strides ?dilations ?name input_name kernel_shp =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| input_name |] in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    let dim = Array.length kernel_shp in
    let dilations =
      match dilations with
      | Some d ->
        assert (Array.length d = dim);
        d
      | None   -> Array.make dim 1
    in
    let strides =
      match strides with
      | Some s ->
        assert (Array.length s = dim);
        s
      | None   -> Array.make dim 1
    in
    let auto_pad, pads =
      match padding with
      | SAME_UPPER -> "SAME_UPPER", None
      | SAME_LOWER -> "SAME_LOWRE", None
      | VALID      -> "VALID", None
      | PAD p      ->
        assert (Array.length p = dim);
        "NOTSET", Some p
    in
    { name
    ; input
    ; output
    ; attrs
    ; out_shape = [| None; None |]
    ; auto_pad
    ; pads
    ; ceil_mode = 0 (* TODO: should we use floor or ceil? *)
    ; dilations
    ; kernel_shp
    ; storage_order = 0 (* We stick with Row-major *)
    ; strides
    }
end

module AveragePool = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable auto_pad : string
    ; mutable ceil_mode : bool
    ; mutable count_include_pad : bool
    ; mutable dilations : int array
    ; mutable kernel_shp : int array
    ; mutable pads : int array option
    ; mutable strides : int array
    }

  let op_type = "AveragePool"

  let create
      ?(padding = VALID)
      ?strides
      ?dilations
      ?name
      ?(ceil_mode = false)
      ?(count_include_pad = false)
      input_name
      kernel_shp
    =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| input_name |] in
    let dim = Array.length kernel_shp in
    let dilations =
      match dilations with
      | Some d ->
        assert (Array.length d = dim);
        d
      | None   -> Array.make dim 1
    in
    let strides =
      match strides with
      | Some s ->
        assert (Array.length s = dim);
        s
      | None   -> Array.make dim 1
    in
    let auto_pad, pads =
      match padding with
      | SAME_UPPER -> "SAME_UPPER", None
      | SAME_LOWER -> "SAME_LOWRE", None
      | VALID      -> "VALID", None
      | PAD p      ->
        assert (Array.length p = dim);
        "NOTSET", Some p
    in
    { name
    ; input
    ; attrs
    ; out_shape = [| None; None |]
    ; auto_pad
    ; pads
    ; ceil_mode
    ; count_include_pad
    ; dilations
    ; kernel_shp
    ; strides
    }
end

module BatchNormalization = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable epsilon : float
    ; mutable momentum : float
    }

  let op_type = "BatchNormalization"

  let create ?output ?name ?eps ?momentum x_n scale_n b_n mean_n var_n =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x_n; scale_n; b_n; mean_n; var_n |] in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    let epsilon =
      match eps with
      | Some e -> e
      | None   -> 1e-5
    in
    let momentum =
      match momentum with
      | Some m -> m
      | None   -> 0.9
    in
    let out_shape = [| None; None; None; None; None |] in
    { name; input; output; attrs; out_shape; epsilon; momentum }
end

module Dropout = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable ratio : float
    }

  let op_type = "Dropout"

  let create ?output ?name ?(ratio = 0.5) x =
    assert (ratio >= 0. && ratio <= 1.);
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x |] in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    { name; input; output; attrs; out_shape = [| None; None |]; ratio }
end

module GlobalMaxPool = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "GlobalMaxPool"

  let create ?name x =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x |] in
    { name; input; attrs; out_shape = [| None |] }
end

module GlobalAveragePool = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "GlobalAveragePool"

  let create ?name x =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x |] in
    { name; input; attrs; out_shape = [| None |] }
end

module Flatten = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    }

  let op_type = "Flatten"

  let create ?name ?(axis = 1) x =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x |] in
    { name; input; attrs; out_shape = [| None |]; axis }
end

module InstanceNorm = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable eps : float
    }

  let op_type = "InstanceNormalization"

  let create ?name ?(eps = 1e-5) data scale b =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| data; scale; b |] in
    { name; input; attrs; out_shape = [| None |]; eps }
end
