(* ppx_bigarray --- A PPX extension for providing big array literals in OCaml

   Copyright (C) 2015 Akinori ABE
   This software is distributed under MIT License
   See LICENSE.txt for details. *)

type ('a, 'b) t =
  | Leaf of 'a * 'b
  | Node of 'a * ('a, 'b) t list

(** [size mat] returns a list of dimensions of matrix [mat]. *)
val size : ('a, 'b) t -> int list

(** [check_rect size mat] checks matrix [mat] is rectangular, or not.
    @return a list of pairs of an expected size and an ll-formed node. *)
val check_rect : int list -> ('a, 'b) t -> (int * ('a, 'b) t) list

(** [string_of_size size] returns a string representation of [size]. *)
val string_of_size : int list -> string

(** [pad size mat x y] adds [Leaf (x, y)] to each nodes in [mat] and forms
    [mat] into a rectangle. *)
val pad : int list -> ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

(** {2 Conversion from OCaml expressions} *)

val of_expression :
  ?level:int -> Parsetree.expression -> (Location.t, Parsetree.expression) t

(** {2 Conversion into OCaml expressions} *)

type bigarray_type =
  | Array1
  | Array2
  | Array3
  | Genarray

type bigarray_kind =
  | Float32
  | Float64
  | Int8_signed
  | Int8_unsigned
  | Int16_signed
  | Int16_unsigned
  | Int32
  | Int64
  | Int
  | Nativeint
  | Complex32
  | Complex64
  | Char
  | Dynamic of Parsetree.expression

type bigarray_layout =
  | C_layout
  | Fortran_layout
  | Dynamic_layout of Parsetree.expression

val to_expression :
  ?loc:Location.t -> ?attrs:Parsetree.attribute list ->
  bigarray_type -> bigarray_kind -> bigarray_layout ->
  int list -> (Location.t, Parsetree.expression) t -> Parsetree.expression
