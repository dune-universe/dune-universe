type ('a, 'b, 'c, 'd) t = {
  table : ('b,'d) Hashtbl.t ;
  zero : 'd ;
  proj : 'a -> 'b ;
  add : 'c -> 'd -> 'd
}

let create ?(n = 251) ~bin:proj ~zero ~add () = {
  table = Hashtbl.create n ;
  zero ; proj ; add
}

let add t x y =
  let bin = t.proj x in
  let accu = Option.value (Hashtbl.find_opt t.table bin) ~default:t.zero in
  Hashtbl.replace t.table bin (t.add y accu)

let seq t = Hashtbl.to_seq t.table

let find t x =
  match Hashtbl.find t.table x with
  | x -> x
  | exception Not_found -> t.zero

let of_seq create xs =
  let c = create () in
  Seq.iter (fun (x, y) -> add c x y) xs ;
  c

let transform1 ~bin ~zero ~add:f xs =
  let c = create ~bin ~zero ~add:f () in
  Seq.iter (fun x -> add c x x) xs ;
  seq c

let transform ~bin ~zero ~add xs =
  of_seq (create ~bin ~zero ~add) xs
  |> seq

module Counter = struct
  type nonrec 'a t = ('a, 'a, int, int) t

  let create ?n () =
    create ?n ~zero:0 ~bin:(fun x -> x) ~add:( + ) ()

  let of_seq xs = of_seq create xs

  let tick accu x = add accu x 1
end

let counts e =
  transform1
    ~bin:(fun x -> x)
    ~zero:0
    ~add:(fun _ m -> m + 1)
    e

module Relation = struct
  type nonrec ('a, 'b) t = ('a, 'a, 'b, 'b list) t

  let create ?n () =
    create ?n ~zero:[] ~bin:(fun x -> x) ~add:(fun x xs -> x :: xs) ()

  let of_seq xs = of_seq create xs
end

let relation xs =
  transform ~zero:[] ~bin:(fun x -> x) ~add:(fun x xs -> x :: xs) xs
