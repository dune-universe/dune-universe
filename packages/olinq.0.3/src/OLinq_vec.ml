
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Resizable array} *)

type 'a iter = ('a -> unit) -> unit

type 'a t = {
  mutable size : int;
  mutable vec : 'a array;
}

let is_empty v = v.size = 0

let return x = { size=1; vec= [| x |]; }

let create () = { size = 0; vec = [| |]; }

let init n f = { size=n; vec=Array.init n f; }

(* assuming the underlying array isn't empty, resize it *)
let resize_ v newcapacity =
  assert (newcapacity >= v.size);
  assert (Array.length v.vec > 0);
  let new_vec = Array.make newcapacity v.vec.(0) in
  Array.blit v.vec 0 new_vec 0 v.size;
  v.vec <- new_vec;
  ()

(* grow the array, using [x] as a filler if required *)
let grow_ v x =
  if Array.length v.vec=0
  then v.vec <- Array.make 128 x
  else (
    let n = Array.length v.vec in
    let size = min (2 * n) Sys.max_array_length in
    if size = n then failwith "vec: can't grow any further";
    resize_ v size
  )

let push_unsafe_ v x =
  Array.unsafe_set v.vec v.size x;
  v.size <- v.size + 1

let push v x =
  if v.size = Array.length v.vec
  then grow_ v x;
  push_unsafe_ v x

let length v = v.size

let get v i =
  if i < 0 || i >= v.size then invalid_arg "vec.get";
  Array.unsafe_get v.vec i

let set v i x =
  if i < 0 || i >= v.size then invalid_arg "vec.set";
  Array.unsafe_set v.vec i x

let iteri ~f v =
  for i = 0 to v.size -1 do
    f i (Array.unsafe_get v.vec i)
  done

let append_iter a seq = seq (fun x -> push a x)
let append_seq a seq = Seq.iter (fun x -> push a x) seq

let map f v =
  if Array.length v.vec = 0
  then create ()
  else (
    let vec = Array.init v.size (fun i -> f (Array.unsafe_get v.vec i)) in
    { size=v.size; vec; }
  )

let to_iter v k =
  for i = 0 to v.size -1 do
    k (Array.unsafe_get v.vec i)
  done

let fold f acc v =
  let rec fold acc i =
    if i = v.size then acc
    else
      let x = Array.unsafe_get v.vec i in
      fold (f acc x) (i+1)
  in fold acc 0

let flat_map_iter f v =
  let v' = create () in
  to_iter v
    (fun x ->
      let iter = f x in
      append_iter v' iter);
  v'

let of_list l =
  let v = create() in
  List.iter (push v) l;
  v

let of_array a = { vec=a; size=Array.length a }

let of_iter iter =
  let v = create() in
  append_iter v iter;
  v

let to_list v = List.rev (fold (fun l x -> x::l) [] v)

let to_array v = Array.sub v.vec 0 v.size
