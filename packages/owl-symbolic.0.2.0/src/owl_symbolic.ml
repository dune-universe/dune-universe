(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*
 * Owl's implementation of ONNX standards. This implementation connects Owl's
 * internal computation graph, i.e. cgraph, with other accelerator frameworks
 * using ONNX protobuf. By so doing, we can offload computations defined by
 * Owl to various accelerators.
 *
 * Refer to
 *     1. https://onnx.ai/
 *     2. https://github.com/onnx/onnx/
 *)

module Type = Owl_symbolic_types
module Specs = Owl_symbolic_specs
module Op = Owl_symbolic_operator
module Symbol = Owl_symbolic_symbol
module SymGraph = Owl_symbolic_graph
module Infix = Owl_symbolic_infix
module ONNX_Engine = Owl_symbolic_engine_onnx
module LaTeX_Engine = Owl_symbolic_engine_latex
