(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type number_type =
  | SNT_Noop
  | SNT_Float
  | SNT_Double
  | SNT_Complex32
  | SNT_Complex64
  | SNT_Bool
  | SNT_String
  | SNT_Int8
  | SNT_Int16
  | SNT_Int32
  | SNT_Int64
  | SNT_Uint8
  | SNT_Uint16
  | SNT_Uint32
  | SNT_Uint64
  | SNT_Float16
  | SNT_SEQ of number_type

val number_type_to_string : number_type -> string

type tensor =
  { mutable dtype : number_type
  ; mutable shape : int array
  ; mutable str_val : string array option
  ; mutable flt_val : float array option
  ; mutable int_val : int array option
  ; mutable raw_val : bytes option
  }

type pad =
  | SAME_UPPER
  | SAME_LOWER
  | VALID
  | PAD of int array

val make_tensor
  :  ?dtype:number_type
  -> ?flt_val:float array
  -> ?int_val:int array
  -> ?str_val:string array
  -> ?raw_val:bytes
  -> int array
  -> tensor

val get_tensor_dtype : tensor -> number_type

type activation =
  | Relu
  | Tanh
  | Sigmoid
  | Affine of float * float
  | LeakyRelu of float
  | ThresholdedRelu of float
  | ScaledTanh of float * float
  | HardSigmoid of float * float
  | Elu of float
  | Softsign
  | Softplus
  | Softmax of int

val activation_to_string : activation -> string

type nn_init =
  | Uniform of float * float
  | Gaussian of float * float
  | Standard
  | Tanh

type attrvalue =
  | ATTR_Nil
  | ATTR_Int of int
  | ATTR_Bool of bool
  | ATTR_Type of number_type
  | ATTR_Float of float
  | ATTR_Shape of int array
  | ATTR_String of string
  | ATTR_Tensor of tensor
  | ATTR_Array of attrvalue array
  | ATTR_NameArray of
      { name : string
      ; attr : (string * attrvalue) array
      }

val get_attrvalue_int : attrvalue -> int

val get_attrvalue_float : attrvalue -> float

val get_attrvalue_type : attrvalue -> number_type

val get_attrvalue_shape : attrvalue -> int array

exception TYPE_CHECK of string

exception INVALID_NAME of string
