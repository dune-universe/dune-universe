module type S_base = sig
  type +'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  include S_base
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
  val (>>) : 'a t -> 'b t Lazy.t -> 'b t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val seq : 'a t list -> 'a list t
end

module Make(M : S_base) : S with type 'a t = 'a M.t = struct
  include M
  let (>>=) = M.bind
  let (=<<) f m = M.bind m f
  let (>>) m1 m2 = M.bind m1 (fun _ -> Lazy.force m2)
  let fmap f m = M.bind m (fun x -> x |> f |> M.return)
  let (>|=) m f = fmap f m
  let seq ms = List.fold_right (fun m acc -> acc >>= fun l -> m >>= fun x -> return (x::l)) ms (return [])
end

module type ErrorType = sig
  type t
  val of_exn : exn -> t
end
