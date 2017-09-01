module type Monad = sig
  type +'a t
  type ('a, 'b) result = Ok of 'a | Error of 'b
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val ( >>? ) : 'a t -> (('a, exn) result -> 'b t) -> 'b t
end

