(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph
open Owl_symbolic_types

val init : nn_init -> int array -> symbol

val activation : ?name:string -> activation -> symbol -> symbol

val input : ?name:string -> ?dtype:number_type -> int array -> symbol

val max_pool2d
  :  ?name:string
  -> ?padding:pad
  -> int array
  -> int array
  -> symbol
  -> symbol

val avg_pool2d
  :  ?name:string
  -> ?padding:pad
  -> int array
  -> int array
  -> symbol
  -> symbol

val global_max_pool2d : ?name:string -> symbol -> symbol

val global_avg_pool2d : ?name:string -> symbol -> symbol

val dropout : ?name:string -> float -> symbol -> symbol

val lambda : (symbol -> symbol) -> symbol -> symbol

val fully_connected : ?name:string -> ?init_typ:nn_init -> int -> symbol -> symbol

val conv2d
  :  ?name:string
  -> ?padding:pad
  -> ?init_typ:nn_init
  -> int array
  -> int array
  -> symbol
  -> symbol

val transpose_conv2d
  :  ?name:string
  -> ?padding:pad
  -> ?init_typ:nn_init
  -> int array
  -> int array
  -> symbol
  -> symbol

val linear : ?name:string -> ?init_typ:nn_init -> int -> symbol -> symbol

val normalisation
  :  ?name:string
  -> ?_axis:'a
  -> ?eps:float
  -> ?momentum:float
  -> symbol
  -> symbol

val zero_padding2d : ?name:string -> int array -> symbol -> symbol

val concat : ?name:string -> ?axis:int -> symbol array -> symbol

val add : ?name:string -> symbol -> symbol -> symbol

val flt : ?name:string -> ?dtype:number_type -> float -> symbol

val tanh : ?name:string -> symbol -> symbol

val get_network : ?name:string -> symbol -> t
