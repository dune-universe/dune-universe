module type S1 = sig
  type 'a t
  val traverse: ('a -> 'b t) -> 'a list -> 'b list t
  val sequence: 'a t list -> 'a list t
end

module type S2 = sig
  type ('a, 'b) t
  val traverse: ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
  val sequence: ('a, 'b) t list -> ('a list, 'b) t
end

let rec traverse bind return f accu l =
  match l with
  | [] -> accu |> List.rev |> return
  | head :: tail ->
    bind (f head) (fun x ->
      traverse bind return f (x :: accu) tail)

let traverse bind return f l = traverse bind return f [] l
let sequence bind return l = traverse bind return (fun x -> x) l

module Make1(Monad: OBMonad.S1): S1
  with type 'a t := 'a Monad.t
= struct
  let traverse f l = traverse Monad.bind Monad.return f l
  let sequence l = sequence Monad.bind Monad.return l
end

module Make2(Monad: OBMonad.S2): S2
  with type ('a, 'b) t := ('a, 'b) Monad.t
= struct
  let traverse f l = traverse Monad.bind Monad.return f l
  let sequence l = sequence Monad.bind Monad.return l
end
