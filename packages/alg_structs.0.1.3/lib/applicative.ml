module type Seed = sig
  include Functor.S
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type S = sig
  include Seed
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> )  : 'a t -> 'b t -> 'b t
  val ( <* )  : 'a t -> 'b t -> 'a t

  val both : 'a t -> 'b t -> ('a * 'b) t

  module Let_bind : sig
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    val (and+) : 'a t -> 'b t -> ('a * 'b) t
  end
end

module Make (B : Seed) : S with type 'a t = 'a B.t  = struct
  include B
  let map2 a b ~f = apply (B.map ~f a) b

  let ( <*> ) = apply
  let ( *> ) a b = map2 ~f:Fun.(flip const) a b
  let ( <* ) a b = map2 ~f:Fun.const a b

  let both a b = map2 ~f:(fun x y -> (x, y)) a b

  module Let_bind = struct
    let (let+) x f = map ~f x
    let (and+) = both
  end
end

module Law (A : S) = struct
  open A

  let (%) f g x = f (g x)

  let identity x = return Fun.id <*> x = x
  let composition u v w = (return (%) <*> u <*> v <*> w) = (u <*> (v <*> w))
  let homomorphism f x = return f <*> return x = return (f x)
  let interchange u y = (u <*> return y) = (return (fun f -> f y) <*> u)
end

(* Implementations *)

module Option = struct
  include Make (struct
      include Functor.Option
      let return x = Some x
      let apply f x = match f with
        | Some f -> map ~f x
        | None -> None
    end)
end

module List = struct
  include Make (struct
      include Functor.List
      let return x = [x]
      let apply fs xs =
        fs
        |> List.map (fun f -> List.map f xs)
        |> List.flatten
    end)
end

module Array = struct
  include Make (struct
      include Functor.Array
      let return x = [|x|]
      let apply fs xs =
        fs
        |> Array.map (fun f -> Array.map f xs)
        |> Array.to_list
        |> Array.concat
    end)
end (* Array *)
