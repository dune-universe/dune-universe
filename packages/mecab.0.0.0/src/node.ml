(* MeCab --- A MeCab binding for OCaml

   Copyright (c) 2017 Akinori ABE

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Parsing results *)

open Sexplib.Std

type stat =
  | NOR (** Normal node defined in the dictionary. *)
  | UNK (** Unknown node not defined in the dictionary. *)
  | BOS (** Virtual node representing a beginning of the sentence. *)
  | EOS (** Virtual node representing a end of the sentence. *)
  | EON (** Virtual node representing a end of the N-best enumeration. *)
[@@deriving sexp]

type t =
  {
    surface : string; (** surface string. *)
    feature : string; (** feature string. *)
    id : int; (** unique node id *)
    rc_attr : int; (** right attribute id *)
    lc_attr : int; (** left attribute id *)
    posid : int; (** unique part of speech id. This value is defined in [pos.def] file. *)
    char_type : int; (** character type *)
    stat : stat; (** status of this model. *)
    isbest : bool; (** [true] if this node is best node. *)
    alpha : float; (** forward accumulative log summation.
                    * This value is only available when [MECAB_MARGINAL_PROB] is passed. *)
    beta : float; (** backward accumulative log summation.
                   * This value is only available when [MECAB_MARGINAL_PROB] is passed. *)
    prob : float; (** marginal probability.
                   * This value is only available when [MECAB_MARGINAL_PROB] is passed. *)
    wcost : int; (** word cost. *)
    cost : int; (** best accumulative cost from bos node to this node. *)
  }
[@@deriving sexp]
