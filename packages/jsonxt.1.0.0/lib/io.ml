module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
end

