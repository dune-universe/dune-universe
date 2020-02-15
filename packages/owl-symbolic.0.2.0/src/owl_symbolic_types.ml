(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(* Or elt_type ? *)
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
  | SNT_SEQ       of number_type

let rec number_type_to_string = function
  | SNT_Noop      -> "SNT_Noop"
  | SNT_Float     -> "SNT_Float"
  | SNT_Uint8     -> "SNT_Uint8"
  | SNT_Int8      -> "SNT_Int8"
  | SNT_Uint16    -> "SNT_Uint16"
  | SNT_Int16     -> "SNT_Int16"
  | SNT_Int32     -> "SNT_Int32"
  | SNT_Int64     -> "SNT_Int64"
  | SNT_String    -> "SNT_String"
  | SNT_Bool      -> "SNT_Bool"
  | SNT_Float16   -> "SNT_Float16"
  | SNT_Double    -> "SNT_Double"
  | SNT_Uint32    -> "SNT_Uint32"
  | SNT_Uint64    -> "SNT_Uint64"
  | SNT_Complex32 -> "SNT_Complex32"
  | SNT_Complex64 -> "SNT_Complex64"
  | SNT_SEQ x     -> number_type_to_string x ^ " Sequence"


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
  | PAD        of int array

(* One and only one of the value arguments should be used *)
let make_tensor ?dtype ?flt_val ?int_val ?str_val ?raw_val shape =
  let counter = ref 0 in
  if flt_val <> None then counter := !counter + 1;
  if int_val <> None then counter := !counter + 1;
  if str_val <> None then counter := !counter + 1;
  if raw_val <> None then counter := !counter + 1;
  if !counter <> 1
  then
    raise (Invalid_argument "make_tensor: one and only one type of value should be used.");
  if flt_val <> None
  then (
    let tp =
      match dtype with
      | Some x ->
        assert (
          Array.mem
            x
            [| SNT_Float; SNT_Float16; SNT_Double; SNT_Complex32; SNT_Complex64 |]);
        x
      | None   -> SNT_Float
    in
    { dtype = tp; shape; flt_val; int_val = None; str_val = None; raw_val = None })
  else if int_val <> None
  then (
    let tp =
      match dtype with
      | Some x ->
        assert (
          Array.mem
            x
            [| SNT_Uint8
             ; SNT_Uint16
             ; SNT_Uint32
             ; SNT_Uint64
             ; SNT_Int8
             ; SNT_Int16
             ; SNT_Int32
             ; SNT_Int64
            |]);
        x
      | None   -> SNT_Int32
    in
    { dtype = tp; shape; flt_val = None; int_val; str_val = None; raw_val = None })
  else if str_val <> None
  then
    { dtype = SNT_String; shape; flt_val = None; int_val = None; str_val; raw_val = None }
  else if raw_val <> None
  then
    { dtype = SNT_String; shape; flt_val = None; int_val = None; str_val = None; raw_val }
  else raise (Invalid_argument "make_tensor: unsupported data type")


let get_tensor_dtype (t : tensor) = t.dtype

(** Activation functions for LSTM *)

type activation =
  | Relu
  | Tanh
  | Sigmoid
  | Affine          of float * float
  | LeakyRelu       of float
  | ThresholdedRelu of float
  | ScaledTanh      of float * float
  | HardSigmoid     of float * float
  | Elu             of float
  | Softsign
  | Softplus
  | Softmax         of int

let activation_to_string = function
  | Relu               -> "Relu"
  | Tanh               -> "Tanh"
  | Sigmoid            -> "Sigmoid"
  | Affine (_, _)      -> "Affine"
  | LeakyRelu _        -> "LeakyRelu"
  | ThresholdedRelu _  -> "ThresholdedRelu"
  | ScaledTanh (_, _)  -> "ScaledTanh"
  | HardSigmoid (_, _) -> "HardSigmoid"
  | Elu _              -> "Elu"
  | Softsign           -> "Softsign"
  | Softplus           -> "Softplus"
  | Softmax _          -> "Softmax"


type nn_init =
  | Uniform  of float * float
  | Gaussian of float * float
  | Standard
  | Tanh

(** Currently useless types: attrvalue *)

type attrvalue =
  | ATTR_Nil
  | ATTR_Int       of int
  | ATTR_Bool      of bool
  | ATTR_Type      of number_type
  | ATTR_Float     of float
  | ATTR_Shape     of int array
  | ATTR_String    of string
  | ATTR_Tensor    of tensor
  | ATTR_Array     of attrvalue array
  | ATTR_NameArray of
      { name : string
      ; attr : (string * attrvalue) array
      }

let get_attrvalue_int v =
  match v with
  | ATTR_Int i -> i
  | _          -> failwith "get_attrvalue_int: incorrect attr type"


let get_attrvalue_float v =
  match v with
  | ATTR_Float f -> f
  | _            -> failwith "get_attrvalue_float: incorrect attr type"


let get_attrvalue_type v =
  match v with
  | ATTR_Type typ -> typ
  | _             -> failwith "get_attrvalue_type: incorrect attr type"


let get_attrvalue_shape v =
  match v with
  | ATTR_Shape s -> s
  | _            -> failwith "get_attrvalue_shape: incorrect attr type"


(* Exception definition *)
exception TYPE_CHECK of string

exception INVALID_NAME of string
