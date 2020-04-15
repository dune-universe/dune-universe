(*
 * concurrent.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew.
 *)

module type S = sig
  module Thread : sig
    type 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t

    val both : 'a t -> 'b t -> ('a * 'b) t
    val join : unit t list -> unit t

    val pick : 'a t list -> 'a t
    val choose : 'a t list -> 'a t

    val async : (unit -> unit t)-> unit
    val cancel : 'a t-> unit

    val sleep : float -> unit t

    val run : 'a t -> 'a
  end

  module MsgBox : sig
    type 'a t
    val create : unit -> 'a t
    val put : 'a t -> 'a -> unit Thread.t
    val get : 'a t -> 'a Thread.t
  end
end

