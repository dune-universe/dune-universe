(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t =
  | NOOP
  | Int                   of Owl_symbolic_ops_generator.Int.t
  | Complex               of Owl_symbolic_ops_generator.Complex.t
  | Float                 of Owl_symbolic_ops_generator.Float.t
  | Tensor                of Owl_symbolic_ops_generator.Tensor.t
  | Variable              of Owl_symbolic_ops_generator.Variable.t
  | RandomUniform         of Owl_symbolic_ops_generator.RandomUniform.t
  | RandomNormal          of Owl_symbolic_ops_generator.RandomNormal.t
  | Zero                  of Owl_symbolic_ops_generator.Zero.t
  | One                   of Owl_symbolic_ops_generator.One.t
  | NegOne                of Owl_symbolic_ops_generator.NegOne.t
  | Pi                    of Owl_symbolic_ops_generator.Pi.t
  | EyeLike               of Owl_symbolic_ops_generator.EyeLike.t
  | RandomUniformLike     of Owl_symbolic_ops_generator.RandomUniformLike.t
  | RandomNormalLike      of Owl_symbolic_ops_generator.RandomNormalLike.t
  | Multinomial           of Owl_symbolic_ops_generator.Multinomial.t
  | ConstantOfShape       of Owl_symbolic_ops_generator.ConstantOfShape.t
  | Range                 of Owl_symbolic_ops_generator.Range.t
  | Sin                   of Owl_symbolic_ops_math.Sin.t
  | Cos                   of Owl_symbolic_ops_math.Cos.t
  | Tan                   of Owl_symbolic_ops_math.Tan.t
  | Asin                  of Owl_symbolic_ops_math.Asin.t
  | Acos                  of Owl_symbolic_ops_math.Acos.t
  | Atan                  of Owl_symbolic_ops_math.Atan.t
  | Sinh                  of Owl_symbolic_ops_math.Sinh.t
  | Cosh                  of Owl_symbolic_ops_math.Cosh.t
  | Tanh                  of Owl_symbolic_ops_math.Tanh.t
  | Asinh                 of Owl_symbolic_ops_math.Asinh.t
  | Acosh                 of Owl_symbolic_ops_math.Acosh.t
  | Atanh                 of Owl_symbolic_ops_math.Atanh.t
  | Sqrt                  of Owl_symbolic_ops_math.Sqrt.t
  | Exp                   of Owl_symbolic_ops_math.Exp.t
  | Log                   of Owl_symbolic_ops_math.Log.t
  | Erf                   of Owl_symbolic_ops_math.Erf.t
  | Sigmoid               of Owl_symbolic_ops_math.Sigmoid.t
  | HardSigmoid           of Owl_symbolic_ops_math.HardSigmoid.t
  | Relu                  of Owl_symbolic_ops_math.Relu.t
  | ThresholdedRelu       of Owl_symbolic_ops_math.ThresholdedRelu.t
  | PRelu                 of Owl_symbolic_ops_math.PRelu.t
  | Selu                  of Owl_symbolic_ops_math.Selu.t
  | Elu                   of Owl_symbolic_ops_math.Elu.t
  | LeakyRelu             of Owl_symbolic_ops_math.LeakyRelu.t
  | Softmax               of Owl_symbolic_ops_math.Softmax.t
  | LogSoftmax            of Owl_symbolic_ops_math.LogSoftmax.t
  | Softsign              of Owl_symbolic_ops_math.Softsign.t
  | Softplus              of Owl_symbolic_ops_math.Softplus.t
  | Abs                   of Owl_symbolic_ops_math.Abs.t
  | Neg                   of Owl_symbolic_ops_math.Neg.t
  | Sign                  of Owl_symbolic_ops_math.Sign.t
  | Floor                 of Owl_symbolic_ops_math.Floor.t
  | Ceil                  of Owl_symbolic_ops_math.Ceil.t
  | Round                 of Owl_symbolic_ops_math.Round.t
  | Clip                  of Owl_symbolic_ops_math.Clip.t
  | Reciprocal            of Owl_symbolic_ops_math.Reciprocal.t
  | Rational              of Owl_symbolic_ops_math.Rational.t
  | Add                   of Owl_symbolic_ops_math.Add.t
  | Sub                   of Owl_symbolic_ops_math.Sub.t
  | Mul                   of Owl_symbolic_ops_math.Mul.t
  | Div                   of Owl_symbolic_ops_math.Div.t
  | Pow                   of Owl_symbolic_ops_math.Pow.t
  | Mod                   of Owl_symbolic_ops_math.Mod.t
  | MatMul                of Owl_symbolic_ops_math.MatMul.t
  | MatMulInteger         of Owl_symbolic_ops_math.MatMulInteger.t
  | QLinearMatMul         of Owl_symbolic_ops_math.QLinearMatMul.t
  | Gemm                  of Owl_symbolic_ops_math.Gemm.t
  | Max                   of Owl_symbolic_ops_math.Max.t
  | Min                   of Owl_symbolic_ops_math.Min.t
  | Sum                   of Owl_symbolic_ops_math.Sum.t
  | Mean                  of Owl_symbolic_ops_math.Mean.t
  | CumSum                of Owl_symbolic_ops_math.CumSum.t
  | Hardmax               of Owl_symbolic_ops_math.Hardmax.t
  | Det                   of Owl_symbolic_ops_math.Det.t
  | Expand                of Owl_symbolic_ops_math.Expand.t
  | And                   of Owl_symbolic_ops_logical.And.t
  | Or                    of Owl_symbolic_ops_logical.Or.t
  | Not                   of Owl_symbolic_ops_logical.Not.t
  | Xor                   of Owl_symbolic_ops_logical.Xor.t
  | Greater               of Owl_symbolic_ops_logical.Greater.t
  | Less                  of Owl_symbolic_ops_logical.Less.t
  | Equal                 of Owl_symbolic_ops_logical.Equal.t
  | BitShift              of Owl_symbolic_ops_logical.BitShift.t
  | EqualTo               of Owl_symbolic_ops_logical.EqualTo.t
  | ReduceSum             of Owl_symbolic_ops_reduction.ReduceSum.t
  | ReduceMax             of Owl_symbolic_ops_reduction.ReduceMax.t
  | ReduceMin             of Owl_symbolic_ops_reduction.ReduceMin.t
  | ReduceMean            of Owl_symbolic_ops_reduction.ReduceMean.t
  | ReduceSumSquare       of Owl_symbolic_ops_reduction.ReduceSumSquare.t
  | ReduceProd            of Owl_symbolic_ops_reduction.ReduceProd.t
  | ReduceLogSum          of Owl_symbolic_ops_reduction.ReduceLogSum.t
  | ReduceLogSumExp       of Owl_symbolic_ops_reduction.ReduceLogSumExp.t
  | ReduceL1              of Owl_symbolic_ops_reduction.ReduceL1.t
  | ReduceL2              of Owl_symbolic_ops_reduction.ReduceL2.t
  | Reshape               of Owl_symbolic_ops_tensor.Reshape.t
  | Identity              of Owl_symbolic_ops_tensor.Identity.t
  | Split                 of Owl_symbolic_ops_tensor.Split.t
  | Concat                of Owl_symbolic_ops_tensor.Concat.t
  | Pad                   of Owl_symbolic_ops_tensor.Pad.t
  | Cast                  of Owl_symbolic_ops_tensor.Cast.t
  | Squeeze               of Owl_symbolic_ops_tensor.Squeeze.t
  | UnSqueeze             of Owl_symbolic_ops_tensor.UnSqueeze.t
  | Tile                  of Owl_symbolic_ops_tensor.Tile.t
  | Shape                 of Owl_symbolic_ops_tensor.Shape.t
  | Size                  of Owl_symbolic_ops_tensor.Size.t
  | Transpose             of Owl_symbolic_ops_tensor.Transpose.t
  | Slice                 of Owl_symbolic_ops_tensor.Slice.t
  | SpaceToDepth          of Owl_symbolic_ops_tensor.SpaceToDepth.t
  | DepthToSpace          of Owl_symbolic_ops_tensor.DepthToSpace.t
  | IsNaN                 of Owl_symbolic_ops_tensor.IsNaN.t
  | IsInf                 of Owl_symbolic_ops_tensor.IsInf.t
  | NonZero               of Owl_symbolic_ops_tensor.NonZero.t
  | Where                 of Owl_symbolic_ops_tensor.Where.t
  | ScatterElements       of Owl_symbolic_ops_tensor.ScatterElements.t
  | ScatterND             of Owl_symbolic_ops_tensor.ScatterND.t
  | GatherElements        of Owl_symbolic_ops_tensor.GatherElements.t
  | GatherND              of Owl_symbolic_ops_tensor.GatherND.t
  | Compress              of Owl_symbolic_ops_tensor.Compress.t
  | ReverseSeq            of Owl_symbolic_ops_tensor.ReverseSeq.t
  | Unique                of Owl_symbolic_ops_tensor.Unique.t
  | Resize                of Owl_symbolic_ops_tensor.Resize.t
  | OneHot                of Owl_symbolic_ops_tensor.OneHot.t
  | Conv                  of Owl_symbolic_ops_nn.Conv.t
  | ConvTranspose         of Owl_symbolic_ops_nn.ConvTranspose.t
  | MaxPool               of Owl_symbolic_ops_nn.MaxPool.t
  | AveragePool           of Owl_symbolic_ops_nn.AveragePool.t
  | BatchNormalization    of Owl_symbolic_ops_nn.BatchNormalization.t
  | InstanceNorm          of Owl_symbolic_ops_nn.InstanceNorm.t
  | Dropout               of Owl_symbolic_ops_nn.Dropout.t
  | GlobalMaxPool         of Owl_symbolic_ops_nn.GlobalMaxPool.t
  | GlobalAveragePool     of Owl_symbolic_ops_nn.GlobalAveragePool.t
  | Flatten               of Owl_symbolic_ops_nn.Flatten.t
  | LSTM                  of Owl_symbolic_ops_rnn.LSTM.t
  | RNN                   of Owl_symbolic_ops_rnn.RNN.t
  | GRU                   of Owl_symbolic_ops_rnn.GRU.t
  | RoiAlign              of Owl_symbolic_ops_object_detection.RoiAlign.t
  | NonMaxSuppression     of Owl_symbolic_ops_object_detection.NonMaxSuppression.t
  | QuantizeLinear        of Owl_symbolic_ops_quantization.QuantizeLinear.t
  | DeQuantizeLinear      of Owl_symbolic_ops_quantization.DeQuantizeLinear.t
  | DynamicQuantizeLinear of Owl_symbolic_ops_quantization.DynamicQuantizeLinear.t
  | SequenceEmpty         of Owl_symbolic_ops_sequence.SequenceEmpty.t
  | SequenceAt            of Owl_symbolic_ops_sequence.SequenceAt.t
  | SequenceInsert        of Owl_symbolic_ops_sequence.SequenceInsert.t
  | SequenceLength        of Owl_symbolic_ops_sequence.SequenceLength.t
  | SequenceConstruct     of Owl_symbolic_ops_sequence.SequenceConstruct.t
  | SequenceErase         of Owl_symbolic_ops_sequence.SequenceErase.t
  | SplitToSequence       of Owl_symbolic_ops_sequence.SplitToSequence.t
  | ConcatFromSequence    of Owl_symbolic_ops_sequence.ConcatFromSequence.t

val name : t -> string

val op_type : t -> string

val input : t -> string array

val set_input : t -> string array -> unit

val out_shape : t -> int array option array

val set_out_shape : t -> int array option array -> unit

val attrs : t -> (string * Owl_symbolic_types.attrvalue) array

val set_attrs : t -> (string * Owl_symbolic_types.attrvalue) array -> unit

val output : t -> string array

val dtype : t -> Owl_symbolic_types.number_type

val set_dtype : t -> Owl_symbolic_types.number_type -> unit

val shape : t -> int array

val axes : t -> int array

val float_value : t -> float

val int_value : t -> int

val complex_value : t -> float * float

val tensor_value : t -> Owl_symbolic_types.tensor

val initializer_ : t -> Owl_symbolic_types.tensor option

val update_tensor_dtype : t -> Owl_symbolic_types.number_type -> unit

val compare : t -> t -> int
