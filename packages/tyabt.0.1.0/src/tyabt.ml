(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

include Intf

let counter = ref 0

module Make(Sort : Sort)(Operator : Operator) = struct
  module Sort = Sort
  module Operator = Operator
  module Variable = struct
    type 'sort t = {
      id : int;
      name : string;
      sort : 'sort Sort.t;
    }

    type 'sort sort = 'sort Sort.t

    let fresh sort name =
      let id = !counter in
      counter := id + 1;
      { id; name; sort }

    let sort var = var.sort

    let name var = var.name

    let equal : type s1 s2. s1 t -> s2 t -> (s1, s2) eq option =
      fun v1 v2 -> match Sort.equal v1.sort v2.sort with
        | Left Refl when v1.id = v2.id -> Some Refl
        | _ -> None
  end

  type 'valence t =
    | Bound : int * 'sort Sort.t -> 'sort va t
    | Free : 'sort Variable.t -> 'sort va t
    | Abstr : string * 'sort Sort.t * 'valence t -> ('sort -> 'valence) t
    | Oper
      : ('arity, 'sort) Operator.t * ('arity, 'sort) operands -> 'sort va t

  and ('arity, 'sort) operands =
    | [] : ('sort ar, 'sort) operands
    | (::) : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands

  type 'valence view =
    | Abs : 'sort Variable.t * 'valence t -> ('sort -> 'valence) view
    | Op
      : ('arity, 'sort) Operator.t * ('arity, 'sort) operands -> 'sort va view
    | Var : 'sort Variable.t -> 'sort va view

  type poly = { f : 'v. 'v t -> 'v t } [@@ocaml.unboxed]

  let rec map_operands
    : type a s. poly -> (a, s) operands -> (a, s) operands =
    fun poly operands -> match operands with
      | [] -> []
      | x :: xs -> poly.f x :: map_operands poly xs

  let rec bind : type s v. s Variable.t -> int -> v t -> v t =
    fun v i t -> match t with
      | Free v' ->
        begin match Variable.equal v v' with
          | Some Refl -> Bound(i, Variable.sort v)
          | None -> t
        end
      | Bound(b, sort) -> if b < i then t else Bound(b + 1, sort)
      | Abstr(name, sort, body) -> Abstr(name, sort, bind v (i + 1) body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> bind v i x } ands)

  let abs v body = Abstr(Variable.name v, v.Variable.sort, bind v 0 body)

  let op ator ands = Oper(ator, ands)

  let var v = Free v

  let into : type v. v view -> v t = function
    | Abs(v, body) -> abs v body
    | Op(ator, ands) -> op ator ands
    | Var v -> var v

  let rec unbind : type s v. s Variable.t -> int -> v t -> v t =
    fun v i t -> match t with
      | Free _ -> t
      | Bound(b, sort) ->
        if b = i then match Sort.equal v.sort sort with
          | Left Refl -> Free v
          | Right _ -> failwith "unbind: Sort mismatch!"
        else if b < i then t
        else Bound(b - 1, sort)
      | Abstr(name, sort, body) -> Abstr(name, sort, unbind v (i + 1) body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> unbind v i x } ands)

  let out : type v. v t -> v view = function
    | Free v -> Var v
    | Bound _ -> failwith "out: Unbound variable!"
    | Abstr(name, sort, body) ->
      let v = Variable.fresh sort name in
      Abs(v, unbind v 0 body)
    | Oper(ator, ands) -> Op(ator, ands)

  let rec subst
    : type s1 s2.
      s1 Sort.t -> (s1 Variable.t -> s1 va t option) -> s2 t -> s2 t =
    fun sort sub abt -> match abt with
      | Free var ->
        begin match Sort.equal sort var.sort with
          | Left Refl ->
            begin match sub var with
              | Some abt -> abt
              | None -> abt
            end
          | Right _ -> abt
        end
      | Bound _ -> abt
      | Abstr(name, sort', body) -> Abstr(name, sort', subst sort sub body)
      | Oper(ator, ands) ->
        Oper(ator, map_operands { f = fun x -> subst sort sub x } ands)

  let rec aequiv : type v. v t -> v t -> bool = fun t1 t2 ->
    match t1, t2 with
    | Free var1, Free var2 ->
      begin match Variable.equal var1 var2 with
        | Some Refl -> true
        | None -> false
      end
    | Bound(var1, _), Bound(var2, _) -> var1 = var2
    | Abstr(_, _, body1), Abstr(_, _, body2) -> aequiv body1 body2
    | Oper(ator1, ands1), Oper(ator2, ands2) ->
      begin match Operator.equal ator1 ator2 with
        | Some Refl -> aequiv_operands ands1 ands2
        | None -> false
      end
    | _, _ -> false

  and aequiv_operands
    : type a s. (a, s) operands -> (a, s) operands -> bool =
    fun ands1 ands2 -> match ands1, ands2 with
      | [], [] -> true
      | x :: xs, y :: ys -> aequiv x y && aequiv_operands xs ys

  let pp_print_var ppf var =
    Format.fprintf ppf "%s/%d" (Variable.name var) var.id

  let rec pp_print : type s. Format.formatter -> s t -> unit =
    fun ppf t -> match out t with
      | Var var -> pp_print_var ppf var
      | Abs(var, body) ->
        Format.fprintf
          ppf
          "%a.%a"
          pp_print_var var
          pp_print body
      | Op(ator, []) -> Format.fprintf ppf "%a()" Operator.pp_print ator
      | Op(ator, abt :: ands) ->
        Format.fprintf
          ppf
          "%a(@[<hv>%a%a)@]"
          Operator.pp_print ator
          pp_print abt
          pp_print_operands ands

  and pp_print_operands
    : type a s. Format.formatter -> (a, s) operands -> unit =
    fun ppf operands -> match operands with
      | [] -> ()
      | abt :: next ->
        Format.fprintf
          ppf
          ";@,%a%a"
          pp_print abt
          pp_print_operands next
end
