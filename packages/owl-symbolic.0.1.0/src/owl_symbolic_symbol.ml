(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_ops_reduction
open Owl_symbolic_ops_generator
open Owl_symbolic_ops_logical
open Owl_symbolic_ops_math
open Owl_symbolic_ops_nn
open Owl_symbolic_ops_rnn
open Owl_symbolic_ops_tensor
open Owl_symbolic_ops_sequence
open Owl_symbolic_ops_object_detection

type t =
  | NOOP
  (* Input *)
  | Int of Int.t
  | Complex of Complex.t
  | Float of Float.t
  | Tensor of Tensor.t
  | Variable of Variable.t
  | RandomUniform of RandomUniform.t
  | RandomNormal of RandomNormal.t
  | Zero of Zero.t
  | One of One.t
  | NegOne of NegOne.t
  | Pi of Pi.t
  (* Math *)
  | Sin of Sin.t
  | Cos of Cos.t
  | Tan of Tan.t
  | Asin of Asin.t
  | Acos of Acos.t
  | Atan of Atan.t
  | Sinh of Sinh.t
  | Cosh of Cosh.t
  | Tanh of Tanh.t
  | Asinh of Asinh.t
  | Acosh of Acosh.t
  | Atanh of Atanh.t
  | Sqrt of Sqrt.t
  | Exp of Exp.t
  | Log of Log.t
  | Erf of Erf.t
  | Sigmoid of Sigmoid.t
  | Relu of Relu.t
  | Elu of Elu.t
  | LeakyRelu of LeakyRelu.t
  | Softmax of Softmax.t
  | Softsign of Softsign.t
  | Softplus of Softplus.t
  | Abs of Abs.t
  | Neg of Neg.t
  | Sign of Sign.t
  | Floor of Floor.t
  | Ceil of Ceil.t
  | Round of Round.t
  | Clip of Clip.t
  | Rational of Rational.t
  | Add of Add.t
  | Sub of Sub.t
  | Mul of Mul.t
  | Div of Div.t
  | Pow of Pow.t
  | Mod of Mod.t
  | MatMul of MatMul.t
  | Gemm of Gemm.t
  | Max of Max.t
  | Min of Min.t
  | Sum of Sum.t
  | Mean of Mean.t
  (* Logical ops *)
  | And of And.t
  | Or of Or.t
  | Not of Not.t
  | Xor of Xor.t
  | Greater of Greater.t
  | Less of Less.t
  | Equal of Equal.t
  | BitShift of BitShift.t
  | EqualTo of EqualTo.t
  (* Reduction *)
  | ReduceSum of ReduceSum.t
  | ReduceMax of ReduceMax.t
  | ReduceMin of ReduceMin.t
  | ReduceMean of ReduceMean.t
  | ReduceSumSquare of ReduceSumSquare.t
  | ReduceProd of ReduceProd.t
  | ReduceLogSum of ReduceLogSum.t
  | ReduceLogSumExp of ReduceLogSumExp.t
  | ReduceL1 of ReduceL1.t
  | ReduceL2 of ReduceL2.t
  (* Tensor *)
  | Reshape of Reshape.t
  | Identity of Identity.t
  | Split of Split.t
  | Concat of Concat.t
  | Pad of Pad.t
  | Cast of Cast.t
  | Squeeze of Squeeze.t
  | Tile of Tile.t
  | Shape of Shape.t
  | Size of Size.t
  | Transpose of Transpose.t
  | Slice of Slice.t
  | SpaceToDepth of SpaceToDepth.t
  | IsNaN of IsNaN.t
  | NonZero of NonZero.t
  | Where of Where.t
  | ScatterElements of ScatterElements.t
  | ScatterND of ScatterND.t
  | GatherElements of GatherElements.t
  | GatherND of GatherND.t
  (* NN *)
  | Conv of Conv.t
  | ConvTranspose of ConvTranspose.t
  | MaxPool of MaxPool.t
  | AveragePool of AveragePool.t
  | BatchNormalization of BatchNormalization.t
  | InstanceNorm of InstanceNorm.t
  | Dropout of Dropout.t
  | GlobalMaxPool of GlobalMaxPool.t
  | GlobalAveragePool of GlobalAveragePool.t
  | Flatten of Flatten.t
  (* RNN *)
  | LSTM of LSTM.t
  (* Object Detection *)
  | RoiAlign of RoiAlign.t
  (* Sequence *)
  | SequenceEmpty of SequenceEmpty.t

let name = function
  | Int x                -> Int.(x.name)
  | Float x              -> Float.(x.name)
  | Complex x            -> Complex.(x.name)
  | Tensor x             -> Tensor.(x.name)
  | Variable x           -> Variable.(x.name)
  | RandomUniform x      -> RandomUniform.(x.name)
  | RandomNormal x       -> RandomNormal.(x.name)
  | Zero x               -> Zero.(x.name)
  | One x                -> One.(x.name)
  | NegOne x             -> NegOne.(x.name)
  | Pi x                 -> Pi.(x.name)
  | Sin x                -> Sin.(x.name)
  | Cos x                -> Cos.(x.name)
  | Tan x                -> Tan.(x.name)
  | Asin x               -> Asin.(x.name)
  | Acos x               -> Acos.(x.name)
  | Atan x               -> Atan.(x.name)
  | Sinh x               -> Sinh.(x.name)
  | Cosh x               -> Cosh.(x.name)
  | Tanh x               -> Tanh.(x.name)
  | Asinh x              -> Asinh.(x.name)
  | Acosh x              -> Acosh.(x.name)
  | Atanh x              -> Atanh.(x.name)
  | Sqrt x               -> Sqrt.(x.name)
  | Exp x                -> Exp.(x.name)
  | Log x                -> Log.(x.name)
  | Erf x                -> Erf.(x.name)
  | Sigmoid x            -> Sigmoid.(x.name)
  | Relu x               -> Relu.(x.name)
  | Elu x                -> Elu.(x.name)
  | LeakyRelu x          -> LeakyRelu.(x.name)
  | Softmax x            -> Softmax.(x.name)
  | Softsign x           -> Softsign.(x.name)
  | Softplus x           -> Softplus.(x.name)
  | Abs x                -> Abs.(x.name)
  | Floor x              -> Floor.(x.name)
  | Ceil x               -> Ceil.(x.name)
  | Round x              -> Round.(x.name)
  | Clip x               -> Clip.(x.name)
  | Neg x                -> Neg.(x.name)
  | Sign x               -> Sign.(x.name)
  | Rational x           -> Rational.(x.name)
  | Add x                -> Add.(x.name)
  | Sub x                -> Sub.(x.name)
  | Mul x                -> Mul.(x.name)
  | Div x                -> Div.(x.name)
  | Pow x                -> Pow.(x.name)
  | Mod x                -> Mod.(x.name)
  | MatMul x             -> MatMul.(x.name)
  | Gemm x               -> Gemm.(x.name)
  | Max x                -> Max.(x.name)
  | Min x                -> Min.(x.name)
  | Sum x                -> Sum.(x.name)
  | Mean x               -> Mean.(x.name)
  | And x                -> And.(x.name)
  | Or x                 -> Or.(x.name)
  | Not x                -> Not.(x.name)
  | Xor x                -> Xor.(x.name)
  | Greater x            -> Greater.(x.name)
  | Less x               -> Less.(x.name)
  | Equal x              -> Equal.(x.name)
  | BitShift x           -> BitShift.(x.name)
  | EqualTo x            -> EqualTo.(x.name)
  | ReduceSum x          -> ReduceSum.(x.name)
  | ReduceMax x          -> ReduceMax.(x.name)
  | ReduceMin x          -> ReduceMin.(x.name)
  | ReduceMean x         -> ReduceMean.(x.name)
  | ReduceSumSquare x    -> ReduceSumSquare.(x.name)
  | ReduceProd x         -> ReduceProd.(x.name)
  | Reshape x            -> Reshape.(x.name)
  | Identity x           -> Identity.(x.name)
  | Split x              -> Split.(x.name)
  | Concat x             -> Concat.(x.name)
  | Pad x                -> Pad.(x.name)
  | Cast x               -> Cast.(x.name)
  | Squeeze x            -> Squeeze.(x.name)
  | Tile x               -> Tile.(x.name)
  | Shape x              -> Shape.(x.name)
  | Size x               -> Size.(x.name)
  | Transpose x          -> Transpose.(x.name)
  | Slice x              -> Slice.(x.name)
  | SpaceToDepth x       -> SpaceToDepth.(x.name)
  | IsNaN x              -> IsNaN.(x.name)
  | NonZero x            -> NonZero.(x.name)
  | Where x              -> Where.(x.name)
  | ScatterElements x    -> ScatterElements.(x.name)
  | ScatterND x          -> ScatterND.(x.name)
  | GatherElements x     -> GatherElements.(x.name)
  | GatherND x           -> GatherND.(x.name)
  | Conv x               -> Conv.(x.name)
  | ConvTranspose x      -> ConvTranspose.(x.name)
  | MaxPool x            -> MaxPool.(x.name)
  | AveragePool x        -> AveragePool.(x.name)
  | BatchNormalization x -> BatchNormalization.(x.name)
  | InstanceNorm x       -> InstanceNorm.(x.name)
  | Dropout x            -> Dropout.(x.name)
  | GlobalMaxPool x      -> GlobalMaxPool.(x.name)
  | GlobalAveragePool x  -> GlobalAveragePool.(x.name)
  | Flatten x            -> Flatten.(x.name)
  | LSTM x               -> LSTM.(x.name)
  | RoiAlign x           -> RoiAlign.(x.name)
  | SequenceEmpty x      -> SequenceEmpty.(x.name)
  | _                    -> failwith "owl_symbolic_symbol.name"


let op_type = function
  | Int _                -> Int.op_type
  | Float _              -> Float.op_type
  | Complex _            -> Complex.op_type
  | Tensor _             -> Tensor.op_type
  | Variable _           -> Variable.op_type
  | RandomUniform _      -> RandomUniform.op_type
  | RandomNormal _       -> RandomNormal.op_type
  | Zero _               -> Zero.op_type
  | One _                -> One.op_type
  | NegOne _             -> NegOne.op_type
  | Pi _                 -> Pi.op_type
  | Sin _                -> Sin.op_type
  | Cos _                -> Cos.op_type
  | Tan _                -> Tan.op_type
  | Asin _               -> Asin.op_type
  | Acos _               -> Acos.op_type
  | Atan _               -> Atan.op_type
  | Sinh _               -> Sinh.op_type
  | Cosh _               -> Cosh.op_type
  | Tanh _               -> Tanh.op_type
  | Asinh _              -> Asinh.op_type
  | Acosh _              -> Acosh.op_type
  | Atanh _              -> Atanh.op_type
  | Sqrt _               -> Sqrt.op_type
  | Exp _                -> Exp.op_type
  | Log _                -> Log.op_type
  | Sigmoid _            -> Sigmoid.op_type
  | Rational _           -> Rational.op_type
  | Neg _                -> Neg.op_type
  | Sign _               -> Sign.op_type
  | Abs _                -> Abs.op_type
  | Floor _              -> Floor.op_type
  | Ceil _               -> Ceil.op_type
  | Round _              -> Round.op_type
  | Clip _               -> Clip.op_type
  | Relu _               -> Relu.op_type
  | Elu _                -> Elu.op_type
  | LeakyRelu _          -> LeakyRelu.op_type
  | Softmax _            -> Softmax.op_type
  | Softsign _           -> Softsign.op_type
  | Softplus _           -> Softplus.op_type
  | Add _                -> Add.op_type
  | Sub _                -> Sub.op_type
  | Mul _                -> Mul.op_type
  | Div _                -> Div.op_type
  | Pow _                -> Pow.op_type
  | Mod _                -> Mod.op_type
  | MatMul _             -> MatMul.op_type
  | Gemm _               -> Gemm.op_type
  | Max _                -> Max.op_type
  | Min _                -> Min.op_type
  | Sum _                -> Sum.op_type
  | Mean _               -> Mean.op_type
  | And _                -> And.op_type
  | Or _                 -> Or.op_type
  | Not _                -> Not.op_type
  | Xor _                -> Xor.op_type
  | Greater _            -> Greater.op_type
  | Less _               -> Less.op_type
  | Equal _              -> Equal.op_type
  | BitShift _           -> BitShift.op_type
  | EqualTo _            -> EqualTo.op_type
  | ReduceSum _          -> ReduceSum.op_type
  | ReduceMax _          -> ReduceMax.op_type
  | ReduceMin _          -> ReduceMin.op_type
  | ReduceMean _         -> ReduceMean.op_type
  | ReduceSumSquare _    -> ReduceSumSquare.op_type
  | ReduceProd _         -> ReduceProd.op_type
  | Reshape _            -> Reshape.op_type
  | Identity _           -> Identity.op_type
  | Split _              -> Split.op_type
  | Concat _             -> Concat.op_type
  | Pad _                -> Pad.op_type
  | Cast _               -> Cast.op_type
  | Squeeze _            -> Squeeze.op_type
  | Tile _               -> Tile.op_type
  | Shape _              -> Shape.op_type
  | Size _               -> Size.op_type
  | Transpose _          -> Transpose.op_type
  | Slice _              -> Slice.op_type
  | SpaceToDepth _       -> SpaceToDepth.op_type
  | IsNaN _              -> IsNaN.op_type
  | NonZero _            -> NonZero.op_type
  | Where _              -> Where.op_type
  | ScatterElements _    -> ScatterElements.op_type
  | ScatterND _          -> ScatterND.op_type
  | GatherElements _     -> GatherElements.op_type
  | GatherND _           -> GatherND.op_type
  | Conv _               -> Conv.op_type
  | ConvTranspose _      -> ConvTranspose.op_type
  | MaxPool _            -> MaxPool.op_type
  | AveragePool _        -> AveragePool.op_type
  | BatchNormalization _ -> BatchNormalization.op_type
  | InstanceNorm _       -> InstanceNorm.op_type
  | Dropout _            -> Dropout.op_type
  | GlobalMaxPool _      -> GlobalMaxPool.op_type
  | GlobalAveragePool _  -> GlobalAveragePool.op_type
  | Flatten _            -> Flatten.op_type
  | LSTM _               -> LSTM.op_type
  | RoiAlign _           -> RoiAlign.op_type
  | SequenceEmpty _      -> SequenceEmpty.op_type
  | _                    -> failwith "owl_symbolic_symbol.op_type"


let input = function
  | Int _                -> [||]
  | Float _              -> [||]
  | Complex _            -> [||]
  | Tensor _             -> [||]
  | Variable _           -> [||]
  | RandomUniform _      -> [||]
  | RandomNormal _       -> [||]
  | Zero _               -> [||]
  | One _                -> [||]
  | NegOne _             -> [||]
  | Pi _                 -> [||]
  | Sin x                -> Sin.(x.input)
  | Cos x                -> Cos.(x.input)
  | Tan x                -> Tan.(x.input)
  | Asin x               -> Asin.(x.input)
  | Acos x               -> Acos.(x.input)
  | Atan x               -> Atan.(x.input)
  | Sinh x               -> Sinh.(x.input)
  | Cosh x               -> Cosh.(x.input)
  | Tanh x               -> Tanh.(x.input)
  | Asinh x              -> Asinh.(x.input)
  | Acosh x              -> Acosh.(x.input)
  | Atanh x              -> Atanh.(x.input)
  | Sqrt x               -> Sqrt.(x.input)
  | Exp x                -> Exp.(x.input)
  | Log x                -> Log.(x.input)
  | Erf x                -> Erf.(x.input)
  | Sigmoid x            -> Sigmoid.(x.input)
  | Neg x                -> Neg.(x.input)
  | Sign x               -> Sign.(x.input)
  | Abs x                -> Abs.(x.input)
  | Floor x              -> Floor.(x.input)
  | Ceil x               -> Ceil.(x.input)
  | Round x              -> Round.(x.input)
  | Clip x               -> Clip.(x.input)
  | Relu x               -> Relu.(x.input)
  | Elu x                -> Elu.(x.input)
  | LeakyRelu x          -> LeakyRelu.(x.input)
  | Softmax x            -> Softmax.(x.input)
  | Softsign x           -> Softsign.(x.input)
  | Softplus x           -> Softplus.(x.input)
  | Rational x           -> Rational.(x.input)
  | Add x                -> Add.(x.input)
  | Sub x                -> Sub.(x.input)
  | Mul x                -> Mul.(x.input)
  | Div x                -> Div.(x.input)
  | Pow x                -> Pow.(x.input)
  | Mod x                -> Mod.(x.input)
  | MatMul x             -> MatMul.(x.input)
  | Gemm x               -> Gemm.(x.input)
  | Max x                -> Max.(x.input)
  | Min x                -> Min.(x.input)
  | Sum x                -> Sum.(x.input)
  | Mean x               -> Mean.(x.input)
  | And x                -> And.(x.input)
  | Or x                 -> Or.(x.input)
  | Not x                -> Not.(x.input)
  | Xor x                -> Xor.(x.input)
  | Greater x            -> Greater.(x.input)
  | Less x               -> Less.(x.input)
  | Equal x              -> Equal.(x.input)
  | BitShift x           -> BitShift.(x.input)
  | EqualTo x            -> EqualTo.(x.input)
  | ReduceSum x          -> ReduceSum.(x.input)
  | ReduceMax x          -> ReduceMax.(x.input)
  | ReduceMin x          -> ReduceMin.(x.input)
  | ReduceMean x         -> ReduceMean.(x.input)
  | ReduceSumSquare x    -> ReduceSumSquare.(x.input)
  | ReduceProd x         -> ReduceProd.(x.input)
  | Reshape x            -> Reshape.(x.input)
  | Identity x           -> Identity.(x.input)
  | Split x              -> Split.(x.input)
  | Concat x             -> Concat.(x.input)
  | Pad x                -> Pad.(x.input)
  | Cast x               -> Cast.(x.input)
  | Squeeze x            -> Squeeze.(x.input)
  | Tile x               -> Tile.(x.input)
  | Shape x              -> Shape.(x.input)
  | Size x               -> Size.(x.input)
  | Transpose x          -> Transpose.(x.input)
  | Slice x              -> Slice.(x.input)
  | SpaceToDepth x       -> SpaceToDepth.(x.input)
  | IsNaN x              -> IsNaN.(x.input)
  | NonZero x            -> NonZero.(x.input)
  | Where x              -> Where.(x.input)
  | ScatterElements x    -> ScatterElements.(x.input)
  | ScatterND x          -> ScatterND.(x.input)
  | GatherElements x     -> GatherElements.(x.input)
  | GatherND x           -> GatherND.(x.input)
  | Conv x               -> Conv.(x.input)
  | ConvTranspose x      -> ConvTranspose.(x.input)
  | MaxPool x            -> MaxPool.(x.input)
  | AveragePool x        -> AveragePool.(x.input)
  | BatchNormalization x -> BatchNormalization.(x.input)
  | InstanceNorm x       -> InstanceNorm.(x.input)
  | Dropout x            -> Dropout.(x.input)
  | GlobalMaxPool x      -> GlobalMaxPool.(x.input)
  | GlobalAveragePool x  -> GlobalAveragePool.(x.input)
  | Flatten x            -> Flatten.(x.input)
  | LSTM x               -> LSTM.(x.input)
  | RoiAlign x           -> RoiAlign.(x.input)
  | SequenceEmpty _      -> [||]
  | _                    -> failwith "owl_symbolic_symbol.input"


let set_input sym inputs =
  match sym with
  | Sin x                -> x.input <- inputs
  | Cos x                -> x.input <- inputs
  | Tan x                -> x.input <- inputs
  | Asin x               -> x.input <- inputs
  | Acos x               -> x.input <- inputs
  | Atan x               -> x.input <- inputs
  | Sinh x               -> x.input <- inputs
  | Cosh x               -> x.input <- inputs
  | Tanh x               -> x.input <- inputs
  | Asinh x              -> x.input <- inputs
  | Acosh x              -> x.input <- inputs
  | Atanh x              -> x.input <- inputs
  | Sqrt x               -> x.input <- inputs
  | Exp x                -> x.input <- inputs
  | Log x                -> x.input <- inputs
  | Erf x                -> x.input <- inputs
  | Neg x                -> x.input <- inputs
  | Sign x               -> x.input <- inputs
  | Abs x                -> x.input <- inputs
  | Floor x              -> x.input <- inputs
  | Ceil x               -> x.input <- inputs
  | Round x              -> x.input <- inputs
  | Clip x               -> x.input <- inputs
  | Relu x               -> x.input <- inputs
  | Elu x                -> x.input <- inputs
  | LeakyRelu x          -> x.input <- inputs
  | Softmax x            -> x.input <- inputs
  | Softsign x           -> x.input <- inputs
  | Softplus x           -> x.input <- inputs
  | Rational x           -> x.input <- inputs
  | Add x                -> x.input <- inputs
  | Sub x                -> x.input <- inputs
  | Mul x                -> x.input <- inputs
  | Div x                -> x.input <- inputs
  | Pow x                -> x.input <- inputs
  | Mod x                -> x.input <- inputs
  | Sigmoid x            -> x.input <- inputs
  | MatMul x             -> x.input <- inputs
  | Gemm x               -> x.input <- inputs
  | Max x                -> x.input <- inputs
  | Min x                -> x.input <- inputs
  | Sum x                -> x.input <- inputs
  | Mean x               -> x.input <- inputs
  | And x                -> x.input <- inputs
  | Or x                 -> x.input <- inputs
  | Not x                -> x.input <- inputs
  | Xor x                -> x.input <- inputs
  | Greater x            -> x.input <- inputs
  | Less x               -> x.input <- inputs
  | Equal x              -> x.input <- inputs
  | BitShift x           -> x.input <- inputs
  | EqualTo x            -> x.input <- inputs
  | ReduceSum x          -> x.input <- inputs
  | ReduceMax x          -> x.input <- inputs
  | ReduceMin x          -> x.input <- inputs
  | ReduceMean x         -> x.input <- inputs
  | ReduceSumSquare x    -> x.input <- inputs
  | ReduceProd x         -> x.input <- inputs
  | Reshape x            -> x.input <- inputs
  | Identity x           -> x.input <- inputs
  | Split x              -> x.input <- inputs
  | Concat x             -> x.input <- inputs
  | Pad x                -> x.input <- inputs
  | Cast x               -> x.input <- inputs
  | Squeeze x            -> x.input <- inputs
  | Tile x               -> x.input <- inputs
  | Shape x              -> x.input <- inputs
  | Size x               -> x.input <- inputs
  | Transpose x          -> x.input <- inputs
  | Slice x              -> x.input <- inputs
  | SpaceToDepth x       -> x.input <- inputs
  | IsNaN x              -> x.input <- inputs
  | NonZero x            -> x.input <- inputs
  | Where x              -> x.input <- inputs
  | ScatterElements x    -> x.input <- inputs
  | ScatterND x          -> x.input <- inputs
  | GatherElements x     -> x.input <- inputs
  | GatherND x           -> x.input <- inputs
  | Conv x               -> x.input <- inputs
  | ConvTranspose x      -> x.input <- inputs
  | MaxPool x            -> x.input <- inputs
  | AveragePool x        -> x.input <- inputs
  | BatchNormalization x -> x.input <- inputs
  | InstanceNorm x       -> x.input <- inputs
  | Dropout x            -> x.input <- inputs
  | GlobalMaxPool x      -> x.input <- inputs
  | GlobalAveragePool x  -> x.input <- inputs
  | Flatten x            -> x.input <- inputs
  | LSTM x               -> x.input <- inputs
  | RoiAlign x           -> x.input <- inputs
  | _                    -> failwith "owl_symbolic_symbol.set_input"


let out_shape = function
  | Int x                -> Int.(x.out_shape)
  | Float x              -> Float.(x.out_shape)
  | Complex x            -> Complex.(x.out_shape)
  | Tensor x             -> Tensor.(x.out_shape)
  | Variable x           -> Variable.(x.out_shape)
  | RandomUniform x      -> RandomUniform.(x.out_shape)
  | RandomNormal x       -> RandomNormal.(x.out_shape)
  | Zero x               -> Zero.(x.out_shape)
  | One x                -> One.(x.out_shape)
  | NegOne x             -> NegOne.(x.out_shape)
  | Pi x                 -> Pi.(x.out_shape)
  | Sin x                -> Sin.(x.out_shape)
  | Cos x                -> Cos.(x.out_shape)
  | Tan x                -> Tan.(x.out_shape)
  | Asin x               -> Asin.(x.out_shape)
  | Acos x               -> Acos.(x.out_shape)
  | Atan x               -> Atan.(x.out_shape)
  | Sinh x               -> Sinh.(x.out_shape)
  | Cosh x               -> Cosh.(x.out_shape)
  | Tanh x               -> Tanh.(x.out_shape)
  | Asinh x              -> Asinh.(x.out_shape)
  | Acosh x              -> Acosh.(x.out_shape)
  | Atanh x              -> Atanh.(x.out_shape)
  | Sqrt x               -> Sqrt.(x.out_shape)
  | Exp x                -> Exp.(x.out_shape)
  | Log x                -> Log.(x.out_shape)
  | Erf x                -> Erf.(x.out_shape)
  | Sigmoid x            -> Sigmoid.(x.out_shape)
  | Neg x                -> Neg.(x.out_shape)
  | Sign x               -> Sign.(x.out_shape)
  | Abs x                -> Abs.(x.out_shape)
  | Floor x              -> Floor.(x.out_shape)
  | Ceil x               -> Ceil.(x.out_shape)
  | Round x              -> Round.(x.out_shape)
  | Clip x               -> Clip.(x.out_shape)
  | Relu x               -> Relu.(x.out_shape)
  | Elu x                -> Elu.(x.out_shape)
  | LeakyRelu x          -> LeakyRelu.(x.out_shape)
  | Softmax x            -> Softmax.(x.out_shape)
  | Softsign x           -> Softsign.(x.out_shape)
  | Softplus x           -> Softplus.(x.out_shape)
  | Rational x           -> Rational.(x.out_shape)
  | Add x                -> Add.(x.out_shape)
  | Sub x                -> Sub.(x.out_shape)
  | Mul x                -> Mul.(x.out_shape)
  | Div x                -> Div.(x.out_shape)
  | Pow x                -> Pow.(x.out_shape)
  | Mod x                -> Mod.(x.out_shape)
  | MatMul x             -> MatMul.(x.out_shape)
  | Gemm x               -> Gemm.(x.out_shape)
  | Max x                -> Max.(x.out_shape)
  | Min x                -> Min.(x.out_shape)
  | Sum x                -> Sum.(x.out_shape)
  | Mean x               -> Mean.(x.out_shape)
  | Or x                 -> Or.(x.out_shape)
  | Not x                -> Not.(x.out_shape)
  | Xor x                -> Xor.(x.out_shape)
  | Greater x            -> Greater.(x.out_shape)
  | Less x               -> Less.(x.out_shape)
  | And x                -> And.(x.out_shape)
  | Equal x              -> Equal.(x.out_shape)
  | BitShift x           -> BitShift.(x.out_shape)
  | EqualTo x            -> EqualTo.(x.out_shape)
  | ReduceSum x          -> ReduceSum.(x.out_shape)
  | ReduceMax x          -> ReduceMax.(x.out_shape)
  | ReduceMin x          -> ReduceMin.(x.out_shape)
  | ReduceMean x         -> ReduceMean.(x.out_shape)
  | ReduceSumSquare x    -> ReduceSumSquare.(x.out_shape)
  | ReduceProd x         -> ReduceProd.(x.out_shape)
  | Reshape x            -> Reshape.(x.out_shape)
  | Identity x           -> Identity.(x.out_shape)
  | Split x              -> Split.(x.out_shape)
  | Concat x             -> Concat.(x.out_shape)
  | Pad x                -> Pad.(x.out_shape)
  | Cast x               -> Cast.(x.out_shape)
  | Squeeze x            -> Squeeze.(x.out_shape)
  | Tile x               -> Tile.(x.out_shape)
  | Shape x              -> Shape.(x.out_shape)
  | Size x               -> Size.(x.out_shape)
  | Transpose x          -> Transpose.(x.out_shape)
  | Slice x              -> Slice.(x.out_shape)
  | SpaceToDepth x       -> SpaceToDepth.(x.out_shape)
  | IsNaN x              -> IsNaN.(x.out_shape)
  | NonZero x            -> NonZero.(x.out_shape)
  | Where x              -> Where.(x.out_shape)
  | ScatterElements x    -> ScatterElements.(x.out_shape)
  | ScatterND x          -> ScatterND.(x.out_shape)
  | GatherElements x     -> GatherElements.(x.out_shape)
  | GatherND x           -> GatherND.(x.out_shape)
  | Conv x               -> Conv.(x.out_shape)
  | ConvTranspose x      -> ConvTranspose.(x.out_shape)
  | MaxPool x            -> MaxPool.(x.out_shape)
  | AveragePool x        -> AveragePool.(x.out_shape)
  | BatchNormalization x -> BatchNormalization.(x.out_shape)
  | InstanceNorm x       -> InstanceNorm.(x.out_shape)
  | Dropout x            -> Dropout.(x.out_shape)
  | GlobalMaxPool x      -> GlobalMaxPool.(x.out_shape)
  | GlobalAveragePool x  -> GlobalAveragePool.(x.out_shape)
  | Flatten x            -> Flatten.(x.out_shape)
  | LSTM x               -> LSTM.(x.out_shape)
  | RoiAlign x           -> RoiAlign.(x.out_shape)
  | SequenceEmpty x      -> SequenceEmpty.(x.out_shape)
  | _                    -> failwith "out_shape: unsupported op."


let set_out_shape sym shapes =
  match sym with
  | Tensor x             -> x.out_shape <- shapes
  | Variable x           -> x.out_shape <- shapes
  | RandomUniform x      -> x.out_shape <- shapes
  | RandomNormal x       -> x.out_shape <- shapes
  | Sin x                -> x.out_shape <- shapes
  | Cos x                -> x.out_shape <- shapes
  | Tan x                -> x.out_shape <- shapes
  | Asin x               -> x.out_shape <- shapes
  | Acos x               -> x.out_shape <- shapes
  | Atan x               -> x.out_shape <- shapes
  | Sinh x               -> x.out_shape <- shapes
  | Cosh x               -> x.out_shape <- shapes
  | Tanh x               -> x.out_shape <- shapes
  | Asinh x              -> x.out_shape <- shapes
  | Acosh x              -> x.out_shape <- shapes
  | Atanh x              -> x.out_shape <- shapes
  | Sqrt x               -> x.out_shape <- shapes
  | Exp x                -> x.out_shape <- shapes
  | Log x                -> x.out_shape <- shapes
  | Erf x                -> x.out_shape <- shapes
  | Sigmoid x            -> x.out_shape <- shapes
  | Neg x                -> x.out_shape <- shapes
  | Sign x               -> x.out_shape <- shapes
  | Abs x                -> x.out_shape <- shapes
  | Floor x              -> x.out_shape <- shapes
  | Ceil x               -> x.out_shape <- shapes
  | Round x              -> x.out_shape <- shapes
  | Clip x               -> x.out_shape <- shapes
  | Relu x               -> x.out_shape <- shapes
  | Elu x                -> x.out_shape <- shapes
  | LeakyRelu x          -> x.out_shape <- shapes
  | Softmax x            -> x.out_shape <- shapes
  | Softsign x           -> x.out_shape <- shapes
  | Softplus x           -> x.out_shape <- shapes
  | Rational x           -> x.out_shape <- shapes
  | Add x                -> x.out_shape <- shapes
  | Sub x                -> x.out_shape <- shapes
  | Mul x                -> x.out_shape <- shapes
  | Div x                -> x.out_shape <- shapes
  | Pow x                -> x.out_shape <- shapes
  | Mod x                -> x.out_shape <- shapes
  | MatMul x             -> x.out_shape <- shapes
  | Gemm x               -> x.out_shape <- shapes
  | Max x                -> x.out_shape <- shapes
  | Min x                -> x.out_shape <- shapes
  | Sum x                -> x.out_shape <- shapes
  | Mean x               -> x.out_shape <- shapes
  | And x                -> x.out_shape <- shapes
  | Or x                 -> x.out_shape <- shapes
  | Not x                -> x.out_shape <- shapes
  | Xor x                -> x.out_shape <- shapes
  | Greater x            -> x.out_shape <- shapes
  | Less x               -> x.out_shape <- shapes
  | Equal x              -> x.out_shape <- shapes
  | BitShift x           -> x.out_shape <- shapes
  | EqualTo x            -> x.out_shape <- shapes
  | ReduceSum x          -> x.out_shape <- shapes
  | ReduceMax x          -> x.out_shape <- shapes
  | ReduceMin x          -> x.out_shape <- shapes
  | ReduceMean x         -> x.out_shape <- shapes
  | ReduceSumSquare x    -> x.out_shape <- shapes
  | ReduceProd x         -> x.out_shape <- shapes
  | Reshape x            -> x.out_shape <- shapes
  | Identity x           -> x.out_shape <- shapes
  | Split x              -> x.out_shape <- shapes
  | Concat x             -> x.out_shape <- shapes
  | Pad x                -> x.out_shape <- shapes
  | Cast x               -> x.out_shape <- shapes
  | Squeeze x            -> x.out_shape <- shapes
  | Tile x               -> x.out_shape <- shapes
  | Shape x              -> x.out_shape <- shapes
  | Size x               -> x.out_shape <- shapes
  | Transpose x          -> x.out_shape <- shapes
  | Slice x              -> x.out_shape <- shapes
  | SpaceToDepth x       -> x.out_shape <- shapes
  | IsNaN x              -> x.out_shape <- shapes
  | NonZero x            -> x.out_shape <- shapes
  | Where x              -> x.out_shape <- shapes
  | ScatterElements x    -> x.out_shape <- shapes
  | ScatterND x          -> x.out_shape <- shapes
  | GatherElements x     -> x.out_shape <- shapes
  | GatherND x           -> x.out_shape <- shapes
  | Conv x               -> x.out_shape <- shapes
  | ConvTranspose x      -> x.out_shape <- shapes
  | MaxPool x            -> x.out_shape <- shapes
  | AveragePool x        -> x.out_shape <- shapes
  | BatchNormalization x -> x.out_shape <- shapes
  | InstanceNorm x       -> x.out_shape <- shapes
  | Dropout x            -> x.out_shape <- shapes
  | GlobalMaxPool x      -> x.out_shape <- shapes
  | GlobalAveragePool x  -> x.out_shape <- shapes
  | Flatten x            -> x.out_shape <- shapes
  | LSTM x               -> x.out_shape <- shapes
  | RoiAlign x           -> x.out_shape <- shapes
  | SequenceEmpty x      -> x.out_shape <- shapes
  | _                    -> failwith "set_out_shape: unsupported op."


(** operaations that only apply to certain symbol *)

let attrs = function
  | Int x                -> Int.(x.attrs)
  | Float x              -> Float.(x.attrs)
  | Complex x            -> Complex.(x.attrs)
  | Tensor x             -> Tensor.(x.attrs)
  | Variable x           -> Variable.(x.attrs)
  | RandomUniform x      -> RandomUniform.(x.attrs)
  | RandomNormal x       -> RandomNormal.(x.attrs)
  | Zero x               -> Zero.(x.attrs)
  | One x                -> One.(x.attrs)
  | NegOne x             -> NegOne.(x.attrs)
  | Pi x                 -> Pi.(x.attrs)
  | Sin x                -> Sin.(x.attrs)
  | Cos x                -> Cos.(x.attrs)
  | Sqrt x               -> Sqrt.(x.attrs)
  | Exp x                -> Exp.(x.attrs)
  | Log x                -> Log.(x.attrs)
  | Erf x                -> Erf.(x.attrs)
  | Sigmoid x            -> Sigmoid.(x.attrs)
  | Rational x           -> Rational.(x.attrs)
  | Neg x                -> Neg.(x.attrs)
  | Abs x                -> Abs.(x.attrs)
  | Floor x              -> Floor.(x.attrs)
  | Ceil x               -> Ceil.(x.attrs)
  | Round x              -> Round.(x.attrs)
  | Clip x               -> Clip.(x.attrs)
  | Sign x               -> Sign.(x.attrs)
  | Relu x               -> Relu.(x.attrs)
  | Elu x                -> Elu.(x.attrs)
  | LeakyRelu x          -> LeakyRelu.(x.attrs)
  | Softmax x            -> Softmax.(x.attrs)
  | Softsign x           -> Softsign.(x.attrs)
  | Softplus x           -> Softplus.(x.attrs)
  | Add x                -> Add.(x.attrs)
  | Sub x                -> Sub.(x.attrs)
  | Mul x                -> Mul.(x.attrs)
  | Div x                -> Div.(x.attrs)
  | Pow x                -> Pow.(x.attrs)
  | Mod x                -> Mod.(x.attrs)
  | MatMul x             -> MatMul.(x.attrs)
  | Gemm x               -> Gemm.(x.attrs)
  | Max x                -> Max.(x.attrs)
  | Min x                -> Min.(x.attrs)
  | Sum x                -> Sum.(x.attrs)
  | Mean x               -> Mean.(x.attrs)
  | And x                -> And.(x.attrs)
  | Or x                 -> Or.(x.attrs)
  | Not x                -> Not.(x.attrs)
  | Xor x                -> Xor.(x.attrs)
  | Greater x            -> Greater.(x.attrs)
  | Less x               -> Less.(x.attrs)
  | Equal x              -> Equal.(x.attrs)
  | BitShift x           -> BitShift.(x.attrs)
  | EqualTo x            -> EqualTo.(x.attrs)
  | ReduceSum x          -> ReduceSum.(x.attrs)
  | ReduceMax x          -> ReduceMax.(x.attrs)
  | ReduceMin x          -> ReduceMin.(x.attrs)
  | ReduceMean x         -> ReduceMean.(x.attrs)
  | ReduceSumSquare x    -> ReduceSumSquare.(x.attrs)
  | ReduceProd x         -> ReduceProd.(x.attrs)
  | Reshape x            -> Reshape.(x.attrs)
  | Identity x           -> Identity.(x.attrs)
  | Split x              -> Split.(x.attrs)
  | Concat x             -> Concat.(x.attrs)
  | Pad x                -> Pad.(x.attrs)
  | Cast x               -> Cast.(x.attrs)
  | Squeeze x            -> Squeeze.(x.attrs)
  | Tile x               -> Tile.(x.attrs)
  | Shape x              -> Shape.(x.attrs)
  | Size x               -> Size.(x.attrs)
  | Transpose x          -> Transpose.(x.attrs)
  | Slice x              -> Slice.(x.attrs)
  | SpaceToDepth x       -> SpaceToDepth.(x.attrs)
  | IsNaN x              -> IsNaN.(x.attrs)
  | NonZero x            -> NonZero.(x.attrs)
  | Where x              -> Where.(x.attrs)
  | ScatterElements x    -> ScatterElements.(x.attrs)
  | ScatterND x          -> ScatterND.(x.attrs)
  | GatherElements x     -> GatherElements.(x.attrs)
  | GatherND x           -> GatherND.(x.attrs)
  | Conv x               -> Conv.(x.attrs)
  | ConvTranspose x      -> ConvTranspose.(x.attrs)
  | MaxPool x            -> MaxPool.(x.attrs)
  | AveragePool x        -> AveragePool.(x.attrs)
  | BatchNormalization x -> BatchNormalization.(x.attrs)
  | InstanceNorm x       -> InstanceNorm.(x.attrs)
  | Dropout x            -> Dropout.(x.attrs)
  | GlobalMaxPool x      -> GlobalMaxPool.(x.attrs)
  | GlobalAveragePool x  -> GlobalAveragePool.(x.attrs)
  | Flatten x            -> Flatten.(x.attrs)
  | LSTM x               -> LSTM.(x.attrs)
  | RoiAlign x           -> RoiAlign.(x.attrs)
  | SequenceEmpty x      -> SequenceEmpty.(x.attrs)
  | _                    -> [||]


let set_attrs sym a =
  match sym with
  | Int x                -> x.attrs <- a
  | Float x              -> x.attrs <- a
  | Complex x            -> x.attrs <- a
  | Tensor x             -> x.attrs <- a
  | Variable x           -> x.attrs <- a
  | RandomUniform x      -> x.attrs <- a
  | RandomNormal x       -> x.attrs <- a
  | Zero x               -> x.attrs <- a
  | One x                -> x.attrs <- a
  | NegOne x             -> x.attrs <- a
  | Pi x                 -> x.attrs <- a
  | Sin x                -> x.attrs <- a
  | Cos x                -> x.attrs <- a
  | Sqrt x               -> x.attrs <- a
  | Exp x                -> x.attrs <- a
  | Log x                -> x.attrs <- a
  | Erf x                -> x.attrs <- a
  | Sigmoid x            -> x.attrs <- a
  | Rational x           -> x.attrs <- a
  | Neg x                -> x.attrs <- a
  | Abs x                -> x.attrs <- a
  | Sign x               -> x.attrs <- a
  | Floor x              -> x.attrs <- a
  | Ceil x               -> x.attrs <- a
  | Round x              -> x.attrs <- a
  | Clip x               -> x.attrs <- a
  | Relu x               -> x.attrs <- a
  | Elu x                -> x.attrs <- a
  | LeakyRelu x          -> x.attrs <- a
  | Softmax x            -> x.attrs <- a
  | Softsign x           -> x.attrs <- a
  | Softplus x           -> x.attrs <- a
  | Add x                -> x.attrs <- a
  | Sub x                -> x.attrs <- a
  | Mul x                -> x.attrs <- a
  | Div x                -> x.attrs <- a
  | Pow x                -> x.attrs <- a
  | Mod x                -> x.attrs <- a
  | MatMul x             -> x.attrs <- a
  | Gemm x               -> x.attrs <- a
  | Max x                -> x.attrs <- a
  | Min x                -> x.attrs <- a
  | Sum x                -> x.attrs <- a
  | Mean x               -> x.attrs <- a
  | And x                -> x.attrs <- a
  | Or x                 -> x.attrs <- a
  | Not x                -> x.attrs <- a
  | Xor x                -> x.attrs <- a
  | Greater x            -> x.attrs <- a
  | Less x               -> x.attrs <- a
  | Equal x              -> x.attrs <- a
  | BitShift x           -> x.attrs <- a
  | EqualTo x            -> x.attrs <- a
  | ReduceSum x          -> x.attrs <- a
  | ReduceMax x          -> x.attrs <- a
  | ReduceMin x          -> x.attrs <- a
  | ReduceMean x         -> x.attrs <- a
  | ReduceSumSquare x    -> x.attrs <- a
  | ReduceProd x         -> x.attrs <- a
  | Reshape x            -> x.attrs <- a
  | Identity x           -> x.attrs <- a
  | Split x              -> x.attrs <- a
  | Concat x             -> x.attrs <- a
  | Pad x                -> x.attrs <- a
  | Cast x               -> x.attrs <- a
  | Squeeze x            -> x.attrs <- a
  | Tile x               -> x.attrs <- a
  | Shape x              -> x.attrs <- a
  | Size x               -> x.attrs <- a
  | Transpose x          -> x.attrs <- a
  | Slice x              -> x.attrs <- a
  | SpaceToDepth x       -> x.attrs <- a
  | IsNaN x              -> x.attrs <- a
  | NonZero x            -> x.attrs <- a
  | Where x              -> x.attrs <- a
  | ScatterElements x    -> x.attrs <- a
  | ScatterND x          -> x.attrs <- a
  | GatherElements x     -> x.attrs <- a
  | GatherND x           -> x.attrs <- a
  | Conv x               -> x.attrs <- a
  | ConvTranspose x      -> x.attrs <- a
  | MaxPool x            -> x.attrs <- a
  | AveragePool x        -> x.attrs <- a
  | BatchNormalization x -> x.attrs <- a
  | InstanceNorm x       -> x.attrs <- a
  | Dropout x            -> x.attrs <- a
  | GlobalMaxPool x      -> x.attrs <- a
  | GlobalAveragePool x  -> x.attrs <- a
  | Flatten x            -> x.attrs <- a
  | LSTM x               -> x.attrs <- a
  | RoiAlign x           -> x.attrs <- a
  | SequenceEmpty x      -> x.attrs <- a
  | _                    -> ()


let output sym =
  match sym with
  | Split x              -> Split.(x.output)
  | BatchNormalization x -> BatchNormalization.(x.output)
  | MaxPool x            -> MaxPool.(x.output)
  | Dropout x            -> Dropout.(x.output)
  | LSTM x               -> LSTM.(x.output)
  | _                    -> [| name sym |]


let dtype = function
  | Float x         -> x.dtype
  | Int x           -> x.dtype
  | Complex _       -> SNT_Complex32
  | Pi x            -> Pi.(x.dtype)
  | Tensor x        ->
    let (t : tensor) = Tensor.(x.value) in
    t.dtype
  | Variable x      -> Variable.(x.dtype)
  | RandomUniform x -> RandomUniform.(x.dtype)
  | RandomNormal x  -> RandomNormal.(x.dtype)
  | _               -> failwith "owl_symboic_symobl.dtype: not var or constant op"


let shape = function
  | Tensor x        ->
    let (t : tensor) = Tensor.(x.value) in
    t.shape
  | Variable x      -> Variable.(x.shape)
  | RandomUniform x -> RandomUniform.(x.shape)
  | RandomNormal x  -> RandomNormal.(x.shape)
  | _               -> [||]


let axes = function
  | ReduceSum x -> x.axes
  | _           -> failwith "axes: unsupported op."


let float_value = function
  | Float x -> Float.(x.value)
  | _       -> failwith "owl_symbolic_symbol.float_value"


let int_value = function
  | Int x -> Int.(x.value)
  | _     -> failwith "owl_symbolic_symbol.int_value"


let complex_value = function
  | Complex x -> Complex.(x.real), Complex.(x.img)
  | _         -> failwith "owl_symbolic_symbol.int_value"


let tensor_value = function
  | Tensor x -> Tensor.(x.value)
  | _        -> failwith "owl_symbolic_symbol.tensor_value"


let initializer_ = function
  | Variable x -> Variable.(x.init)
  | _          -> failwith "owl_symbolic_symbol.initializer_"


(* TODO: check if the value is indeed changed *)
let update_tensor_dtype op new_typ =
  match op with
  | Tensor x ->
    let t = x.value in
    t.dtype <- new_typ
  | _        -> failwith "owl_symbolic_symbol.update_tensor_dtype"


let compare sx sy =
  let order =
    [| "Zero"
     ; "One"
     ; "NegOne"
     ; "Integer"
     ; "Rational"
     ; "Float"
     ; "Pi"
     ; "Variable"
     ; "Pow"
     ; "Mul"
     ; "Add"
     ; "Sqrt"
     ; "Exp"
     ; "Log"
     ; "Sin"
     ; "Cos"
    |]
  in
  let a = op_type sx in
  let b = op_type sy in
  if Array.mem a order && Array.mem b order
  then (
    let ai = ref 0 in
    let bi = ref 0 in
    Array.iteri
      (fun i x ->
        if x = a then ai := i;
        if x = b then bi := i)
      order;
    !ai - !bi)
  else String.compare a b
