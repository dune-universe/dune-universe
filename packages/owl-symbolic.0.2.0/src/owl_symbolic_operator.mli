(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph

val noop : symbol

val int : ?name:string -> ?dtype:Owl_symbolic_types.number_type -> int -> symbol

val float : ?name:string -> ?dtype:Owl_symbolic_types.number_type -> float -> symbol

val complex : ?name:string -> float -> float -> symbol

val pi : ?name:string -> unit -> symbol

val tensor : ?name:string -> Owl_symbolic_types.tensor -> symbol

val zeros : ?name:string -> ?dtype:Owl_symbolic_types.number_type -> int array -> symbol

val ones : ?name:string -> ?dtype:Owl_symbolic_types.number_type -> int array -> symbol

val variable
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?shape:int array
  -> ?init:Owl_symbolic_types.tensor
  -> string
  -> symbol

val random_uniform
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?seed:float option
  -> ?low:float
  -> ?high:float
  -> ?name:string
  -> int array
  -> symbol

val random_normal
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?seed:float option
  -> ?mean:float
  -> ?stddev:float
  -> ?name:string
  -> int array
  -> symbol

val eyelike : ?dtype:Owl_symbolic_types.number_type -> ?k:int -> symbol -> symbol

val random_uniform_like
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?seed:float
  -> ?low:float
  -> ?high:float
  -> ?name:string
  -> symbol
  -> symbol

val random_normal_like
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?seed:float
  -> ?mean:float
  -> ?stddev:float
  -> ?name:string
  -> symbol
  -> symbol

val multinomial
  :  ?name:string
  -> ?dtype:Owl_symbolic_types.number_type
  -> ?seed:float
  -> ?sample_size:int
  -> symbol
  -> symbol

val content_of_shape
  :  ?name:string
  -> ?value:Owl_symbolic_types.tensor
  -> symbol
  -> symbol

val range : ?name:string -> symbol -> symbol -> symbol -> symbol

val sin : ?name:string -> symbol -> symbol

val cos : ?name:string -> symbol -> symbol

val tan : ?name:string -> symbol -> symbol

val asin : ?name:string -> symbol -> symbol

val acos : ?name:string -> symbol -> symbol

val atan : ?name:string -> symbol -> symbol

val sinh : ?name:string -> symbol -> symbol

val cosh : ?name:string -> symbol -> symbol

val tanh : ?name:string -> symbol -> symbol

val asinh : ?name:string -> symbol -> symbol

val acosh : ?name:string -> symbol -> symbol

val atanh : ?name:string -> symbol -> symbol

val sqrt : ?name:string -> symbol -> symbol

val exp : ?name:string -> symbol -> symbol

val log : ?name:string -> symbol -> symbol

val erf : ?name:string -> symbol -> symbol

val sigmoid : ?name:string -> symbol -> symbol

val hard_sigmoid : ?name:string -> ?alpha:float -> ?beta:float -> symbol -> symbol

val abs : ?name:string -> symbol -> symbol

val neg : ?name:string -> symbol -> symbol

val sign : ?name:string -> symbol -> symbol

val floor : ?name:string -> symbol -> symbol

val ceil : ?name:string -> symbol -> symbol

val round : ?name:string -> symbol -> symbol

val reciprocal : ?name:string -> symbol -> symbol

val clip : ?name:string -> min:float -> max:float -> symbol -> symbol

val relu : ?name:string -> symbol -> symbol

val prelu : ?name:string -> symbol -> symbol -> symbol

val thresholded_relu : ?name:string -> ?alpha:float -> symbol -> symbol

val selu : ?name:string -> ?alpha:float -> ?gamma:float -> symbol -> symbol

val elu : ?name:string -> ?alpha:float -> symbol -> symbol

val leaky_relu : ?name:string -> ?alpha:float -> symbol -> symbol

val softmax : ?name:string -> ?axis:int -> symbol -> symbol

val logsoftmax : ?name:string -> ?axis:int -> symbol -> symbol

val softsign : ?name:string -> symbol -> symbol

val softplus : ?name:string -> symbol -> symbol

val add : ?name:string -> symbol -> symbol -> symbol

val sub : ?name:string -> symbol -> symbol -> symbol

val mul : ?name:string -> symbol -> symbol -> symbol

val div : ?name:string -> symbol -> symbol -> symbol

val pow : ?name:string -> symbol -> symbol -> symbol

val modular : ?name:string -> ?fmod:int -> symbol -> symbol -> symbol

val matmul : ?name:string -> symbol -> symbol -> symbol

val matmul_int
  :  ?name:string
  -> ?a_zero_point:symbol
  -> ?b_zero_point:symbol
  -> symbol
  -> symbol
  -> symbol

val qlinear_matmul
  :  ?name:string
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol

val gemm
  :  ?name:string
  -> ?alpha:float
  -> ?beta:float
  -> ?transA:bool
  -> ?transB:bool
  -> ?c:symbol
  -> symbol
  -> symbol
  -> symbol

val max : ?name:string -> symbol array -> symbol

val min : ?name:string -> symbol array -> symbol

val sum : ?name:string -> symbol array -> symbol

val mean : ?name:string -> symbol array -> symbol

val cumsum
  :  ?name:string
  -> ?axis:int
  -> ?exclusive:bool
  -> ?reverse:bool
  -> symbol
  -> symbol

val hardmax : ?name:string -> ?axis:int -> symbol array -> symbol

val det : ?name:string -> symbol -> symbol

val expand : ?name:string -> int array -> symbol -> symbol

val logic_and : ?name:string -> symbol -> symbol -> symbol

val logic_or : ?name:string -> symbol -> symbol -> symbol

val logic_not : ?name:string -> symbol -> symbol -> symbol

val logic_xor : ?name:string -> symbol -> symbol -> symbol

val greater : ?name:string -> symbol -> symbol -> symbol

val less : ?name:string -> symbol -> symbol -> symbol

val equal : ?name:string -> symbol -> symbol -> symbol

val bitshift : ?name:string -> ?rightshift:bool -> symbol -> symbol -> symbol

val equal_to : ?name:string -> symbol -> symbol -> symbol

val reduce_sum : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_max : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_min : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_mean : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_sum_square : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_prod : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_logsum : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_logsumexp : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_l1 : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reduce_l2 : ?keepdims:bool -> ?name:string -> symbol -> int array -> symbol

val reshape : ?name:string -> int array -> symbol -> symbol

val split : ?name:string -> ?axis:int -> symbol -> int array -> symbol array

val concat : ?name:string -> ?axis:int -> symbol array -> symbol

val cast : ?name:string -> symbol -> Owl_symbolic_types.number_type -> symbol

val pad : ?name:string -> ?mode:string -> ?v:symbol -> symbol -> int array -> symbol

val squeeze : ?name:string -> ?axes:int array -> symbol -> symbol

val unsqueeze : ?name:string -> int array -> symbol -> symbol

val tile : ?name:string -> symbol -> int array -> symbol

val shape : ?name:string -> symbol -> symbol

val size : ?name:string -> symbol -> symbol

val transpose : ?name:string -> ?perm:int array -> symbol -> symbol

val slice
  :  ?name:string
  -> ?axes:int array
  -> ?steps:int array
  -> int array
  -> int array
  -> symbol
  -> symbol

val space_to_depth : ?name:string -> int -> symbol -> symbol

val depth_to_space : ?name:string -> ?mode:string -> int -> symbol -> symbol

val is_nan : ?name:string -> symbol -> symbol

val is_inf : ?name:string -> symbol -> symbol

val non_zero : ?name:string -> symbol -> symbol

val where : ?name:string -> symbol -> symbol -> symbol -> symbol

val scatter_elem : ?name:string -> ?axis:int -> symbol -> symbol -> symbol -> symbol

val scatter_nd : ?name:string -> symbol -> symbol -> symbol -> symbol

val gather_elem : ?name:string -> ?axis:int -> symbol -> symbol -> symbol

val gather_nd : ?name:string -> symbol -> symbol -> symbol

val compress : ?name:string -> ?axis:int -> symbol -> symbol -> symbol

val reverse_seq
  :  ?name:string
  -> ?batch_axis:int
  -> ?time_axis:int
  -> symbol
  -> symbol
  -> symbol

val unique
  :  ?name:string
  -> ?axis:int
  -> ?sorted:bool
  -> symbol
  -> symbol * symbol * symbol * symbol

val resize
  :  ?name:string
  -> ?coordinate_mode:string
  -> ?cubic_coeff_a:float
  -> ?exclude_outside:int
  -> ?extrapolation_value:float
  -> ?mode:string
  -> ?nearest_mode:string
  -> ?scales:float array
  -> ?sizes:int array
  -> symbol
  -> symbol
  -> symbol

val onehot : ?name:string -> ?axis:int -> int -> symbol -> symbol -> symbol

val conv
  :  ?name:string
  -> ?dim:int
  -> ?padding:Owl_symbolic_types.pad
  -> ?strides:int array
  -> ?dilations:int array
  -> ?bias:symbol
  -> symbol
  -> symbol
  -> symbol

val conv_transpose
  :  ?name:string
  -> ?dim:int
  -> ?padding:Owl_symbolic_types.pad
  -> ?strides:int array
  -> ?dilations:int array
  -> ?bias:symbol
  -> symbol
  -> symbol
  -> symbol

val maxpool
  :  ?name:string
  -> ?strides:int array
  -> ?dilations:int array
  -> ?padding:Owl_symbolic_types.pad
  -> symbol
  -> int array
  -> symbol * symbol

val avgpool
  :  ?name:string
  -> ?strides:int array
  -> ?dilations:int array
  -> ?padding:Owl_symbolic_types.pad
  -> ?ceil_mode:bool
  -> ?count_include_pad:bool
  -> symbol
  -> int array
  -> symbol

val global_max_pool : ?name:string -> symbol -> symbol

val global_avg_pool : ?name:string -> symbol -> symbol

val batch_norm
  :  ?name:string
  -> ?eps:float
  -> ?momentum:float
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol
  -> symbol * symbol * symbol * symbol * symbol

val instance_norm : ?name:string -> ?eps:float -> symbol -> symbol -> symbol -> symbol

val flatten : ?name:string -> ?axis:int -> symbol -> symbol

val dropout : ?name:string -> ?ratio:float -> symbol -> symbol * symbol

val lstm
  :  ?name:string
  -> ?alpha:float array
  -> ?beta:float array
  -> ?clip:float
  -> ?activations:Owl_symbolic_types.activation array
  -> ?direction:string
  -> ?input_forget:int
  -> int
  -> symbol
  -> symbol
  -> symbol
  -> symbol * symbol * symbol

val rnn
  :  ?name:string
  -> ?alpha:float array
  -> ?beta:float array
  -> ?clip:float
  -> ?activations:Owl_symbolic_types.activation array
  -> ?direction:string
  -> ?b:symbol
  -> ?sequence_lens:symbol
  -> ?initial_h:symbol
  -> int
  -> symbol
  -> symbol
  -> symbol
  -> symbol * symbol

val gru
  :  ?name:string
  -> ?alpha:float array
  -> ?beta:float array
  -> ?clip:float
  -> ?activations:Owl_symbolic_types.activation array
  -> ?direction:string
  -> ?linear_before_reset:int
  -> ?b:symbol
  -> ?sequence_lens:symbol
  -> ?initial_h:symbol
  -> int
  -> symbol
  -> symbol
  -> symbol
  -> symbol * symbol

val roi_align
  :  ?name:string
  -> ?mode:[ `avg | `max ]
  -> ?height:int
  -> ?width:int
  -> ?ratio:int
  -> ?scale:float
  -> symbol
  -> symbol
  -> symbol
  -> symbol

val non_max_suppression
  :  ?name:string
  -> ?center_point_box:int
  -> ?max_output_boxes_per_class:int
  -> ?iou_threshold:float
  -> ?score_threshold:float
  -> symbol
  -> symbol
  -> symbol

val quantize_linear
  :  ?name:string
  -> ?y_zero_point:int
  -> y_scale:float
  -> symbol
  -> symbol

val dequantize_linear
  :  ?name:string
  -> ?x_zero_point:int
  -> x_scale:float
  -> symbol
  -> symbol

val dynamic_quantize_linear : ?name:string -> symbol -> symbol * symbol * symbol

val seq_empty : ?name:string -> ?dtype:Owl_symbolic_types.number_type -> unit -> symbol

val seq_at : ?name:string -> int -> symbol -> symbol

val seq_insert : ?name:string -> int -> symbol -> symbol -> symbol

val seq_erase : ?name:string -> int -> symbol -> symbol

val seq_length : ?name:string -> symbol -> symbol

val seq_construct : ?name:string -> symbol array -> symbol

val split_to_seq
  :  ?name:string
  -> ?axis:int
  -> ?keepdims:bool
  -> ?split_scalar:int
  -> ?split_array:int array
  -> symbol
  -> symbol

val concat_from_seq : ?name:string -> ?new_axis:bool -> int -> symbol -> symbol
