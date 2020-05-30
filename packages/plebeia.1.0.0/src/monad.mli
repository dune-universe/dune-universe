(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
module type S1 = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      
  val map : ('a -> 'b) -> 'a t -> 'b t

  val (>>|) : 'a t -> ('a -> 'b) -> 'b t (** same as map *)

  val (>>) : unit t -> 'b t -> 'b t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val mapM_ : ('a -> unit t) -> 'a list -> unit t
  val fold_leftM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  val parseM : ('a -> 'b list -> ('a * 'b list) t) -> 'a -> 'b list -> 'a t

  module Op : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val return : 'a -> 'a t
  end
end

module Make1(A : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end) : S1 with type 'a t = 'a A.t

module type S2 = sig
  type ('a, 'z) t
  val return : 'a -> ('a, 'z) t
  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
  val (>>=) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

  val map : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t
  val (>>|) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t (** same as map *)

  val mapM : ('a -> ('b, 'z) t) -> 'a list -> ('b list, 'z) t
  val mapM_ : ('a -> (unit, 'z) t) -> 'a list -> (unit, 'z) t
  val fold_leftM : ('a -> 'b -> ('a, 'z) t) -> 'a -> 'b list -> ('a, 'z) t
  val parseM : ('a -> 'b list -> ('a * 'b list, 'error) t) -> 'a -> 'b list -> ('a, 'error) t

  module Op : sig
    val (>>=) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
    val (>>|) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t
    val return : 'a -> ('a, 'z) t
  end
end

module Make2(A : sig
  type ('a, 'z) t
  val return : 'a -> ('a, 'z) t
  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
end) : S2 with type ('a, 'z) t = ('a, 'z) A.t
