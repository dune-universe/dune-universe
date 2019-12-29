(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(* Implemented: Sin, Cos, Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh, Asinh, 
Acosh, Atanh, Add, Sub, Mul, Div, Neg, Abs, Floor, Ceil, Sqrt, Relu, Exp, Log,
Pow, Round, Gemm, MatMul, Max, Min, Sum, Mean, Mod, Sigmoid, Softmax, Clip, Sign
LeakyRelu, Elu, Softsign, Softplus *)

(*  HardSigmoid, LogSoftmax, ThreasholdedRelu, Selu, PRelu, *)
(* Cumsum, Det, Reciprocal, 
Hardmax, Expand, QLinearMatMul, MatMulInteger,
*)

open Owl_symbolic_types

(** One input *)

module Sin = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sin"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Cos = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Cos"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Tan = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Tan"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Asin = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Asin"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Acos = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Acos"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Atan = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Atan"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Sinh = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sinh"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Cosh = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Cosh"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Tanh = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Tanh"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Asinh = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Asinh"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Acosh = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Acosh"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Atanh = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Atanh"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Sqrt = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sqrt"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Exp = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Exp"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Log = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Log"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Sigmoid = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sigmoid"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Abs = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Abs"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Neg = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Neg"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Sign = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sign"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Floor = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Floor"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Ceil = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Ceil"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Round = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Round"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Clip = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Clip"

  let create ?name x min max =
    let input = [| x; min; max |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Relu = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Relu"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Elu = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable alpha : float
    }

  let op_type = "Elu"

  let create ?name ?(alpha = 1.) x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; alpha }
end

module LeakyRelu = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable alpha : float
    }

  let op_type = "LeakyRelu"

  let create ?name ?(alpha = 0.01) x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; alpha }
end

module Softmax = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable axis : int
    }

  let op_type = "Softmax"

  let create ?name ?(axis = 1) x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; axis }
end

module Softsign = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Softsign"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Softplus = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Softplus"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Erf = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Erf"

  let create ?name x_name =
    let input = [| x_name |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

(** Two inputs *)

module Add = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
          (* ; mutable pos_scalar : int --> differentiate add (0), scalaradd(-1), and addscalar(1) *)
    }

  let op_type = "Add"

  let create ?name x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Sub = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sub"

  let create ?name x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Mul = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Mul"

  let create ?name x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Div = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Div"

  let create ?name x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Pow = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Pow"

  let create ?name x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Mod = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable fmod : int
    }

  let op_type = "Mod"

  let create ?name ?(fmod = 0) x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; fmod }
end

module Gemm = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable alpha : float
    ; mutable beta : float
    ; mutable transA : bool
    ; mutable transB : bool
    }

  let op_type = "Gemm"

  let create ?name ?(alpha = 1.) ?(beta = 1.) ?(transA = false) ?(transB = false) ?c a b =
    let attrs = [||] in
    let input =
      match c with
      | Some c -> [| a; b; c |]
      | _      -> [| a; b |]
    in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; alpha; beta; transA; transB }
end

(* Matrix product that behaves like numpy.matmul:
 * https://docs.scipy.org/doc/numpy-1.13.0/reference/generated/numpy.matmul.html *)
module MatMul = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "MatMul"

  let create ?name x y =
    let input = [| x; y |] in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Max = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Max"

  let create ?name xs =
    let input = xs in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Min = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Min"

  let create ?name xs =
    let input = xs in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Sum = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Sum"

  let create ?name xs =
    let input = xs in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module Mean = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Mean"

  let create ?name xs =
    let input = xs in
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

(* p and q are not specified by user; but rather later calculated in canonical part *)
(* TODO: remove this op *)
module Rational = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable p : int
    ; mutable q : int
    }

  let op_type = "Rational"

  let create name input attrs =
    { name; input; attrs; out_shape = [| None |]; p = 0; q = 0 }
end
