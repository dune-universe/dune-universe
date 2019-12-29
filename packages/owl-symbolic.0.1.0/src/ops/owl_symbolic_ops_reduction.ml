(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Implemented: ReduceMax, ReduceSum,  ReduceMin, ReduceSumSquare,
  * ReduceMean, ReduceProd, ReduceLogSum, ReduceLogSumExp, ReduceL1, ReduceL2
  *)

open Owl_symbolic_types

module ReduceSum = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool (* NOTE: ONNX requires an int parameter *)
    }

  let op_type = "ReduceSum"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceMax = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceMax"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceMin = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceMin"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceMean = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceMean"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceSumSquare = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceSumSquare"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceProd = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceProd"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceLogSum = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceLogSum"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceLogSumExp = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceLogSumExp"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceL1 = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceL1"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end

module ReduceL2 = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceL2"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axes; keepdims }
end
