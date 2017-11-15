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

(** The core module for part-of-speech and morphological analysis. *)

open Sexplib.Std

type t

type lattice_level =
  | ONE_BEST (** Suggest the single best solution *)
  | NBEST (** Suggest N-best solution *)
  | PROB  (** Separating words with probabilities *)
[@@deriving sexp]

(** {2 Construction} *)

external create : string array -> t
  = "ml_mecab_create"

external create2 : string -> t
  = "ml_mecab_create2"

(** {2 Properties} *)

external get_partial : t -> bool
  = "ml_mecab_get_partial"

external set_partial : t -> bool -> unit
  = "ml_mecab_set_partial"

external get_theta : t -> float
  = "ml_mecab_get_theta"

external set_theta : t -> float -> unit
  = "ml_mecab_set_theta"

external get_lattice_level : t -> lattice_level
  = "ml_mecab_get_lattice_level"

external set_lattice_level : t -> lattice_level -> unit
  = "ml_mecab_set_lattice_level"

external get_all_morphs : t -> bool
  = "ml_mecab_get_all_morphs"

external set_all_morphs : t -> bool -> unit
  = "ml_mecab_set_all_morphs"

(** {2 Dictionary information} *)

type dictionary_type =
  | SYS_DIC (** This is a system dictionary. *)
  | USR_DIC (** This is a user dictionary. *)
  | UKN_DIC (** This is a unknown word dictionary. *)
[@@deriving sexp]

type dictionary_info =
  {
    filename : string; (** filename of dictionary.
                           On Windows, filename is stored in UTF-8 encoding. *)
    charset : string; (** character set of the dictionary.
                          e.g., ["SHIFT-JIS"], ["UTF-8"]. *)
    size : int; (** How many words are registered in this dictionary. *)
    dic_type : dictionary_type; (** dictionary type *)
    lsize : int; (** left attributes size *)
    rsize : int; (** right attributes size *)
    version : int; (** version of this dictionary *)
  }
[@@deriving sexp]

external dictionary_info : t -> dictionary_info list
  = "ml_mecab_dictionary_info_stub"

(** {2 Parse sentense} *)

external sparse_tostr : t -> ?pos:int -> ?len:int -> string -> string
  = "ml_mecab_sparse_tostr"

external sparse_tonode : t -> ?pos:int -> ?len:int -> string -> Node.t list
  = "ml_mecab_sparse_tonode"

external nbest_sparse_tostr
  : t -> n:int -> ?pos:int -> ?len:int -> string -> string
  = "ml_mecab_nbest_sparse_tostr"

external nbest_init : t -> ?pos:int -> ?len:int -> string -> unit
  = "ml_mecab_nbest_init"

external nbest_next_tostr : t -> string
  = "ml_mecab_nbest_next_tostr"

external nbest_next_tonode : t -> Node.t list
  = "ml_mecab_nbest_next_tonode"
