module H = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal

  let hash = Rand.hash_int
end)

type t = unit H.t

let create size : t = H.create size

let add (t : t) (x : int) : unit = H.replace t x ()

let mem (t : t) (x : int) : bool = H.mem t x

let remove (t : t) x = H.remove t x

let cardinal (t : t) = H.length t

let iter (f : int -> unit) (t : t) : unit = H.iter (fun x () -> f x) t

let reset (t : t) = H.reset t

let choose_opt t =
  match H.to_seq_keys t () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

let choose t = Option.get (choose_opt t)
