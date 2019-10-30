(* Seed *)

module type Seed = sig

  (** The principle type.

      The type constructor [t] maps some [a] to a type [a t]. This
  *)

  type 'a t

  (** [map ~f] maps the function [f : 'a -> 'b] to a function ['f T : 'a T ->
      'b T] *)

  val map : f:('a -> 'b) -> ('a t -> 'b t)
end

module type UnlabeledSeed = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end


(* Interface *)

module type S = sig
  include Seed

  (** Mapped version of [@@] and infix of {!val:map} *)
  val ( <@> ) : ('a -> 'b) -> 'a t -> 'b t

  (** Mapped version of [|>] (which is flipped {!val:(<&>)}) *)
  val ( |>> ) :  'a t -> ('a -> 'b) ->'b t
end


(* Laws *)

module Law (F : S) = struct
  let (%) f g x = f (g x)
  let identity x = F.map ~f:Fun.id x = Fun.id x
  let composition f g x = F.map ~f:(f % g) x = (F.map ~f % F.map ~f:g) x
end


(* Constructors *)

module Make (Seed : Seed) : S with type 'a t = 'a Seed.t = struct
  include Seed
  let (<@>) f x = Seed.map ~f x
  let (|>>) x f = x |> Seed.map ~f
end

module MakeUnlabeled (Seed : UnlabeledSeed) : S with type 'a t = 'a Seed.t = struct
  include Make (struct
      include Seed
      let map ~f = map f
    end)
end


(* Implementations  *)

module Fun = struct
  module Reader = struct
    module Make (T : Triv.S) : S with type 'a t = (T.t -> 'a) = struct
      include Make (struct
          type 'a t = T.t -> 'a
          let map ~f g = fun x -> f (g x)
        end)
    end
  end
end

module Option = MakeUnlabeled (Option)
module List = MakeUnlabeled (List)
module Array = MakeUnlabeled (Array)
