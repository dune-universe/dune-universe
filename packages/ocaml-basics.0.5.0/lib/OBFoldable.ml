module type S1 = sig
  type 'a t
  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module Make1 (Monad: OBMonad.S1): S1
  with type 'a t := 'a Monad.t
= struct
  let fold f init monad =
    let accu = ref init in
    let _ = Monad.bind monad (fun x ->
      accu := f !accu x;
      Monad.return ()) in
    !accu
end

module type S2 = sig
  type ('a, 'b) t
  val fold: ('a -> 'b -> 'a) -> 'a -> ('b, _) t -> 'a
end

module Make2 (Monad: OBMonad.S2): S2
  with type ('a, 'b) t := ('a, 'b) Monad.t
= struct
  let fold f init monad =
    let accu = ref init in
    let _ = Monad.bind monad (fun x ->
      accu := f !accu x;
      Monad.return ()) in
    !accu
end
