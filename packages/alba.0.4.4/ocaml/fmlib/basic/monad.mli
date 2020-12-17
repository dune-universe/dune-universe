open Module_types


(** Minimal signature for a monad. *)
module type SIG_MIN =
  sig
    type _ t
    val return: 'a -> 'a t
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  end


(** Minimal signature for a monad with [map]. *)
module type SIG_WITH_MAP =
  sig
    include SIG_MIN
    val map: ('a -> 'b) -> 'a t -> 'b t
  end


(** Signature for the result monad. *)
module type RESULT =
  sig
    include MONAD
    type error
    val throw: error -> 'a t
    val catch: 'a t -> (error -> 'a t) -> 'a t
    val continue: 'a t -> ('a -> 'z) -> (error -> 'z) -> 'z
  end



module Of_sig_min (M:SIG_MIN): MONAD with type 'a t = 'a M.t

module Of_sig_with_map (M:SIG_WITH_MAP): MONAD with type 'a t = 'a M.t


(** Identity monad *)
module Identity:
sig
    include MONAD
    val eval: 'a t -> 'a
end


(** Result monad *)
module Result (Error:ANY): RESULT with type error = Error.t and
                                       type 'a t = ('a,Error.t) result
