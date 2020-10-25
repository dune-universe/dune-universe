(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

module type Arith = sig
  type t

  val neg : t -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val to_string : t -> string
end


module Make (T : Arith) = struct
  type scalar = T.t

  type monomial = { coef: T.t; degree: int }
  type elt = monomial list
  type t = elt

  let make_monomial c d =
    { coef = c; degree = d }

  let create () = []
  let of_monomial m = [m]

  let make (l : elt) : t = l

  let get (p : t) : elt = p

  let to_string p =
    String.concat
      " + "
      (List.map
         (fun m ->
           (T.to_string m.coef) ^
             if m.degree = 0 then
               ""
             else
               ("x^{" ^ (string_of_int m.degree) ^ "}")
         )
         p
      )

  let const f =
    make_monomial f 0

  let scale p a =
    List.map (fun m -> { m with coef = T.mul a m.coef }) p

  let neg p =
    List.map (fun m -> { coef = T.neg m.coef; degree = m.degree }) p

  let add p1 p2 =
    let rec add r p1 p2 =
      match p1, p2 with
      | [], [] -> List.rev r
      | _, [] -> List.rev (List.rev_append p1 r)
      | [], _ -> List.rev (List.rev_append p2 r)
      | h1::t1, h2::t2 ->
         if h1.degree = h2.degree then
           let mono = { coef = T.add h1.coef h2.coef; degree = h1.degree } in
           add (mono::r) t1 t2
         else if h1.degree < h2.degree then
           add (h1::r) t1 p2
         else (* h2.degree < h1.degree *)
           add (h2::r) p1 t2
    in
    add [] p1 p2

  let translate p a =
    add p [{coef = a; degree = 0}]

  let sub p1 p2 =
    add p1 (neg p2)

  let mul p1 p2 =
    let mul_mono m1 m2 =
      { coef = T.mul m1.coef m2.coef; degree = m1.degree + m2.degree }
    in
    let add_scale r p m =
      add r (List.map (fun x -> mul_mono m x) p)
    in
    List.fold_left (fun r m -> add_scale r p2 m) [] p1

  let div p1 p2 =
    raise (Failure "Polynomial.div not implemented")

  let ( ~+ ) p = p
  let ( ~- ) = neg

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end
