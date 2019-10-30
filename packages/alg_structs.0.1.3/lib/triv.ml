(** A solitary type *)

open Util

(** [Triv] specifies a module with a single type [t].

    It is used in composing other structures. *)
module type S = sig
  type t
end

(** [make (Proxy : a proxy)] is a [(module T : S with t = a)]. *)
let make (type a) (Proxy : a proxy) =
  (module struct type t = a end : S with type t = a)
