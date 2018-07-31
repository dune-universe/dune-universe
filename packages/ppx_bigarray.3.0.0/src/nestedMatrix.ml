(* ppx_bigarray --- A PPX extension for providing big array literals in OCaml

   Copyright (C) 2015 Akinori ABE
   This software is distributed under MIT License
   See LICENSE.txt for details. *)

open Compat

module List =
struct
  include List

  let init n f =
    let rec aux acc i = if i < 0 then acc else aux (f i :: acc) (i - 1) in
    aux [] (n - 1)

  let make n x = init n (fun _ -> x)

  let take n l =
    let rec aux acc n l = match n, l with
      | 0, _ -> List.rev acc
      | _, hd :: tl -> aux (hd :: acc) (n - 1) tl
      | _ -> failwith "List.take"
    in
    aux [] n l
end

type ('a, 'b) t =
  | Leaf of 'a * 'b
  | Node of 'a * ('a, 'b) t list

let merge_sizes xs ys =
  let rec aux acc xs ys = match xs, ys with
    | [], [] -> List.rev acc
    | x :: xs', y :: ys' -> aux (max x y :: acc) xs' ys'
    | [], x :: xs' | x :: xs', [] -> aux (x :: acc) [] xs'
  in
  aux [] xs ys

let rec size = function
  | Leaf _ -> []
  | Node (_, children) ->
    children
    |> List.map size
    |> List.fold_left merge_sizes []
    |> (fun xs -> List.length children :: xs)

let check_rect size root =
  let rec aux size errors node = match size, node with
    | [], Leaf _ -> errors
    | n :: _, Leaf _ -> (n, node) :: errors
    | [], Node _ -> (-1, node) :: errors
    | m :: size', Node (_, children) ->
      let errors' = List.fold_left (aux size') errors children in
      let n = List.length children in
      if m = n then errors' else (m, node) :: errors'
  in
  aux size [] root

let string_of_size size =
  size
  |> List.map string_of_int
  |> String.concat "x"

let make_padding x y =
  let rec aux = function
    | [] -> Leaf (x, y)
    | n :: size -> Node (x, List.init n (fun _ -> aux size))
  in
  aux

let pad size root x y =
  let rec aux size node = match size, node with
    | m :: size', Node (z, children) ->
      let children' = List.map (aux size') children in
      let n = List.length children in
      if m = n then Node (z, children')
      else if m < n then Node (z, List.take m children')
      else Node (z, children' @ List.make (m - n) (make_padding x y size'))
    | _ -> node
  in
  aux size root

(** {2 Conversion from OCaml expressions} *)

open Asttypes
open Parsetree
open Longident

(** [get_elements expr] obtains a list of elements of a big array literal from
    expression [expr]. If [expr] is NOT a list, an array or a tuple, [None] is
    returned. *)
let get_elements =
  let rec from_list acc = function
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ } ->
      Some (List.rev acc)
    | { pexp_desc = Pexp_construct
            ({ txt = Lident "::"; _ },
             Some ({ pexp_desc = Pexp_tuple ([hd; tl]); _ })); _ } ->
      from_list (hd :: acc) tl
    | _ -> None
  in
  function
  | { pexp_desc = Pexp_array exprs; _ } -> Some exprs
  | expr -> from_list [] expr
(*
let rec of_expression expr = match get_elements expr with
  | None -> Leaf (expr.pexp_loc, expr)
  | Some exprs -> Node (expr.pexp_loc, List.map of_expression exprs)
                  *)
let of_expression ?(level = max_int) expr =
  let rec aux level expr =
    if level = 0 then Leaf (expr.pexp_loc, expr)
    else match get_elements expr with
      | None -> Leaf (expr.pexp_loc, expr)
      | Some exprs -> Node (expr.pexp_loc, List.map (aux (level - 1)) exprs)
  in
  aux level expr

(** {2 Conversion into OCaml expressions} *)

open Ast_helper

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

module Exp =
struct
  include ExtAst.Exp

  (** Create expressions related to big arrays *)
  module Ba =
  struct
    let kind ?loc ?attrs =
      let id = ident ?loc ?attrs in
      function
      | Float32 -> id "Bigarray.float32"
      | Float64 -> id "Bigarray.float64"
      | Int8_signed -> id "Bigarray.int8_signed"
      | Int8_unsigned -> id "Bigarray.int8_unsigned"
      | Int16_signed -> id "Bigarray.int16_signed"
      | Int16_unsigned -> id "Bigarray.int16_unsigned"
      | Int32 -> id "Bigarray.int32"
      | Int64 -> id "Bigarray.int64"
      | Int -> id "Bigarray.int"
      | Nativeint -> id "Bigarray.nativeint"
      | Complex32 -> id "Bigarray.complex32"
      | Complex64 -> id "Bigarray.complex64"
      | Char -> id "Bigarray.char"
      | Dynamic expr -> expr

    let layout ?loc ?attrs =
      let id = ident ?loc ?attrs in
      function
      | C_layout -> id "Bigarray.c_layout"
      | Fortran_layout -> id "Bigarray.fortran_layout"
      | Dynamic_layout expr -> expr

    let array1_create ?loc ?attrs kind_ layout_ dim =
      let kind_ = kind ?loc kind_ in
      let layout_ = layout ?loc layout_ in
      let dim = int ?loc dim in
      let f = ident ?loc "Bigarray.Array1.create" in
      apply ?loc ?attrs f [(Asttypes.Nolabel, kind_); (Asttypes.Nolabel, layout_);
                           (Asttypes.Nolabel, dim)]

    let array1_set ?loc ?attrs ba index rhs =
      let index = int ?loc index in
      let f = ident ?loc "Bigarray.Array1.unsafe_set" in
      apply ?loc ?attrs f [(Asttypes.Nolabel, ba); (Asttypes.Nolabel, index);
                           (Asttypes.Nolabel, rhs)]

    let array1_set_all ?loc ?attrs ~ret ba vals =
      vals
      |> List.rev
      |> List.map (fun (i, rhs) -> array1_set ~loc:rhs.pexp_loc ba i rhs)
      |> List.fold_left (fun acc expr -> sequence ?loc ?attrs expr acc) ret

    let genarray_of_array1 ?loc ?attrs ba =
      let f = ident ?loc "Bigarray.genarray_of_array1" in
      apply ?loc ?attrs f [(Asttypes.Nolabel, ba)]

    let reshape_1 ?loc ?attrs ba size =
      let n = match size with
        | [] -> 0
        | [n] -> n
        | _ -> Error.exnf ?loc
                 "Error: @[This literal expects 1-dimensional big array@\n\
                  but the size is %s@." (string_of_size size) () in
      let f = ident ?loc "Bigarray.reshape_1" in
      apply ?loc ?attrs f [(Asttypes.Nolabel, ba); (Asttypes.Nolabel, int ?loc n)]

    let reshape_2 ?loc ?attrs ba size =
      let (m, n) = match size with
        | [] -> (0, 0)
        | [m] -> (m, 0)
        | [m; n] -> (m, n)
        | _ -> Error.exnf ?loc
                 "Error: @[This literal expects 2-dimensional big array@\n\
                  but the size is %s@." (string_of_size size) () in
      let f = ident ?loc "Bigarray.reshape_2" in
      apply ?loc ?attrs f [(Asttypes.Nolabel, ba); (Asttypes.Nolabel, int ?loc m);
                           (Asttypes.Nolabel, int ?loc n)]

    let reshape_3 ?loc ?attrs ba size =
      let (m, n, k) = match size with
        | [] -> (0, 0, 0)
        | [m] -> (m, 0, 0)
        | [m; n] -> (m, n, 0)
        | [m; n; k] -> (m, n, k)
        | _ -> Error.exnf ?loc
                 "Error: @[This literal expects 3-dimensional big array@\n\
                  but the size is %s@." (string_of_size size) () in
      let f = ident ?loc "Bigarray.reshape_3" in
      apply ?loc ?attrs f [(Asttypes.Nolabel, ba); (Asttypes.Nolabel, int ?loc m);
                           (Asttypes.Nolabel, int ?loc n);
                           (Asttypes.Nolabel, int ?loc k)]

    let reshape ?loc ?attrs ba size =
      let f = ident ?loc "Bigarray.reshape" in
      let dims = array ?loc (List.map (int ?loc ?attrs:None) size) in
      apply ?loc ?attrs f [(Asttypes.Nolabel, ba); (Asttypes.Nolabel, dims)]
  end
end

let calc_c_index size indices =
  List.fold_left2 (fun acc n i -> n * acc + i) 0 size indices

let calc_fortran_index size indices =
  List.fold_right2 (fun n i acc -> n * acc + i) size indices 0 + 1

let rec mk_setter ?loc ~ret layout var size serialized_mat = match layout with
  | C_layout ->
    serialized_mat
    |> List.map (fun (indices, expr) -> (calc_c_index size indices, expr))
    |> Exp.Ba.array1_set_all ?loc ~ret var
  | Fortran_layout ->
    serialized_mat
    |> List.map (fun (indices, expr) -> (calc_fortran_index size indices, expr))
    |> Exp.Ba.array1_set_all ?loc ~ret var
  | Dynamic_layout expr ->
    let e1 = Exp.apply ?loc (Exp.ident ?loc "Ppx_bigarray_runtime.is_c_layout")
        [(Asttypes.Nolabel, expr)] in
    let e2 = mk_setter ?loc ~ret C_layout var size serialized_mat in
    let e3 = mk_setter ?loc ~ret Fortran_layout var size serialized_mat in
    Exp.ifthenelse ?loc e1 e2 (Some e3)

let serialize root =
  let rec aux indices node = match node with
    | Leaf (_, expr) -> [(List.rev indices, expr)]
    | Node (_, children) ->
      List.mapi (fun i -> aux (i :: indices)) children
      |> List.flatten
  in
  aux [] root

let to_expression ?(loc = !default_loc) ?attrs ba_type kind layout size mat =
  let name = "$ppx_ba" in
  let var = Exp.ident ~loc name in
  let dim = List.fold_left ( * ) 1 size in
  let arr1 = Exp.Ba.array1_create ~loc kind layout dim in
  mat
  |> serialize
  |> mk_setter ~loc ~ret:var layout var size
  |> Exp.letval ~loc ?attrs:None name arr1
  |> Exp.Ba.genarray_of_array1 ~loc ?attrs:None
  |> (fun genarr -> match ba_type with
      | Array1 -> Exp.Ba.reshape_1 ~loc ?attrs genarr size
      | Array2 -> Exp.Ba.reshape_2 ~loc ?attrs genarr size
      | Array3 -> Exp.Ba.reshape_3 ~loc ?attrs genarr size
      | Genarray -> Exp.Ba.reshape ~loc ?attrs genarr size)
