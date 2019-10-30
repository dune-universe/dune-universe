module type Seed = sig
  type 'a t
  val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
end

module type S = sig
  include Seed
  val fold : (module Monoid.S with type t = 'a) -> 'a t -> 'a
  val fold_map : m:(module Monoid.S with type t = 'm) -> f:('a -> 'm) -> 'a t -> 'm
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t ->  'b
  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val any : f:('a -> bool) -> 'a t -> bool
  val all : f:('a -> bool) -> 'a t -> bool
  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
  val max : compare:('a -> 'a -> int) -> 'a t -> 'a option
  val min : compare:('a -> 'a -> int) -> 'a t -> 'a option
end

module Law (F : S) = struct
  let fold_right (type a) f (init : a) t =
    let m = Monoid.Endo.make (Proxy : a Util.proxy) in
    F.fold_right ~f ~init t = (F.fold_map ~m ~f t) init

  let fold_left (type a) f (init : a) t =
    let m = Monoid.Endo.make (Proxy : a Util.proxy) |> Monoid.Dual.dualize in
    F.fold_right ~f ~init t = (F.fold_map ~m ~f:(Fun.flip f) t) init

  let fold (type a) (module M : Monoid.S with type t = a) (t : a F.t) =
    F.fold (module M) t = F.fold_map ~m:(module M) ~f:Fun.id t

  let length t =
    let m = (module Monoid.Int.Sum : Monoid.S with type t = int) in
    F.length t = F.fold_map ~m ~f:(Fun.const 1) t
end

let max_of_compare compare a b = match compare a b with
  | i when i < 0 -> b
  | i when i > 0 -> a
  | _ -> a

let min_of_compare compare a b = match compare a b with
  | i when i < 0 -> a
  | i when i > 0 -> b
  | _ -> a

module Make (Seed : Seed) : S with type 'a t = 'a Seed.t = struct
  include Seed

  let fold_map (type a) ~m:(module M : Monoid.S with type t = a) ~f t =
    fold_right ~f:(fun x y -> M.op (f x) y) ~init:M.unit t

  let fold (type a) (module M : Monoid.S with type t = a) t =
    fold_map ~m:(module M) ~f:Fun.id t

  let fold_left ~f ~init xs =
    let f' x k z = k (f z x) in
    fold_right xs ~f:f' ~init:Fun.id init

  let to_list t = fold_right ~f:List.cons ~init:[] t
  let is_empty t = fold_right ~f:(fun _ _ -> false) ~init:true t
  let length t = fold_left ~f:(fun c _ -> c + 1) ~init:0 t
  let any ~f t = fold_map ~m:(module Monoid.Bool.Or) ~f t
  let all ~f t = fold_map ~m:(module Monoid.Bool.And) ~f t
  let mem t x ~equal = any ~f:(fun y -> equal x y) t

  let max (type a) ~compare t =
    let max' = max_of_compare compare in
    let module Max_semi = (val Semigroup.make max' : Semigroup.S with type t = a) in
    let module Opt_max_monoid = Monoid.Option.Make (Max_semi)
    in
    fold_map ~m:(module Opt_max_monoid) ~f:Option.some t

  let min (type a) ~compare t =
    let min' = min_of_compare compare in
    let module Min_semi = (val Semigroup.make min' : Semigroup.S with type t = a) in
    let module Opt_max_monoid = Monoid.Option.Make (Min_semi)
    in
    fold_map ~m:(module Opt_max_monoid) ~f:Option.some t
    (* (fold_map ~f:(_max compare) t) *)

end

module Option : S with type 'a t = 'a Option.t = struct
  module Seed = struct
    include Option

    let fold_right ~f t ~init = match t with
      | None -> init
      | Some x -> f x init
  end

  include Make (Seed)
end

module List : S with type 'a t = 'a List.t = struct
  include Make (ListLabels)
end

module Array : S with type 'a t = 'a Array.t = struct
  include Make (ArrayLabels)
end
