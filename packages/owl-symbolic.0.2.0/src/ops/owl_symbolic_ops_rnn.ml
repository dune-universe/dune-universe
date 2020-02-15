(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** LSTM, RNN, GRU *)

open Owl_symbolic_types

module LSTM = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable activation_alpha : float array option
    ; mutable activation_beta : float array option
    ; mutable activations : activation array
    ; mutable clip : float option
    ; mutable direction : string
    ; mutable hidden_size : int
    ; mutable input_forget : int
    }

  let op_type = "LSTM"

  (* TODO: the optional inputs are omitted for now: sequence_lens, 
   * initial_h, initial_c, P. 
   * Also process the alpha and beta according to different activations.
   *)

  let create
      ?name
      ?output
      ?alpha
      ?beta
      ?clip
      ?activations
      ?(direction = "forward")
      ?(input_forget = 0)
      hidden_size
      ?b
      x
      w
      r
    =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input =
      match b with
      | Some b -> [| x; w; r; b |]
      | None   -> [| x; w; r |]
    in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    let activations =
      match activations with
      | Some a -> a
      | None   -> [| Sigmoid; Tanh; Tanh |]
    in
    assert (Array.mem direction [| "forward"; "reverse"; "bidirectional" |]);
    (* TODO: what's the implication of three optional outputs? *)
    let out_shape = [| None; None; None |] in
    { name
    ; input
    ; output
    ; attrs
    ; out_shape
    ; activation_alpha = alpha
    ; activation_beta = beta
    ; activations
    ; clip
    ; direction
    ; hidden_size
    ; input_forget
    }
end

module RNN = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable activation_alpha : float array option
    ; mutable activation_beta : float array option
    ; mutable activations : activation array
    ; mutable clip : float option
    ; mutable direction : string
    ; mutable hidden_size : int
    }

  let op_type = "RNN"

  let create
      ?name
      ?output
      ?alpha
      ?beta
      ?clip
      ?activations
      ?(direction = "forward")
      hidden_size
      x
      w
      r
      b
      sequence_len
      initial_h
    =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x; w; r; b; sequence_len; initial_h |] in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    let activations =
      match activations with
      | Some a -> a
      | None   -> [| Sigmoid; Tanh; Tanh |]
    in
    assert (Array.mem direction [| "forward"; "reverse"; "bidirectional" |]);
    let out_shape = [| None; None |] in
    { name
    ; input
    ; output
    ; attrs
    ; out_shape
    ; activation_alpha = alpha
    ; activation_beta = beta
    ; activations
    ; clip
    ; direction
    ; hidden_size
    }
end

module GRU = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable output : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable activation_alpha : float array option
    ; mutable activation_beta : float array option
    ; mutable activations : activation array
    ; mutable clip : float option
    ; mutable direction : string
    ; mutable hidden_size : int
    ; mutable linear_before_reset : int
    }

  let op_type = "GRU"

  let create
      ?name
      ?output
      ?alpha
      ?beta
      ?clip
      ?activations
      ?(direction = "forward")
      ?(linear_before_reset = 0)
      hidden_size
      x
      w
      r
      b
      sequence_len
      initial_h
    =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| x; w; r; b; sequence_len; initial_h |] in
    let output =
      match output with
      | Some o -> o
      | None   -> [| name |]
    in
    let activations =
      match activations with
      | Some a -> a
      | None   -> [| Sigmoid; Tanh; Tanh |]
    in
    assert (Array.mem direction [| "forward"; "reverse"; "bidirectional" |]);
    let out_shape = [| None; None |] in
    { name
    ; input
    ; output
    ; attrs
    ; out_shape
    ; activation_alpha = alpha
    ; activation_beta = beta
    ; activations
    ; clip
    ; direction
    ; hidden_size
    ; linear_before_reset
    }
end
