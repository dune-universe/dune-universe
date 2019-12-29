(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Implemented: SequenceEmpty *)

(** SequenceConstruct, SequenceInsert, SequenceAt, 
SequenceErease, SequenceLength, SplitToSequence, ConcatFromSequence *)

open Owl_symbolic_types

module SequenceEmpty = struct
  type t =
    { mutable name : string
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable dtype : number_type
    }

  let op_type = "SequenceEmpty"

  let create ?name ?(dtype = SNT_Float) () =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; attrs; out_shape = [| None |]; dtype }
end
