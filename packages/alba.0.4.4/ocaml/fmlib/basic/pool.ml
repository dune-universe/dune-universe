type next_pointer = int option
type 'a elem =
  | Elem of 'a
  | Free of next_pointer

type 'a t = {vector: 'a elem Vector.t;
             mutable first_free: next_pointer}

let make_empty (): 'a t =
  {vector = Vector.make_empty ();
   first_free = None}

let capacity (p:'a t): int =
  Vector.count p.vector

let has (p:'a t) (i:int): bool =
  assert (i < capacity p);
  match Vector.elem p.vector i with
  | Elem _ -> true
  | Free _ -> false

let elem (p:'a t) (i:int): 'a =
  assert (i < capacity p);
  match Vector.elem p.vector i with
  | Elem a -> a
  | Free _ ->
     assert false (* Illegal call! *)


let find (p:'a t) (i:int): int =
  assert (i <= capacity p);
  let rec fnd i =
    if i = capacity p || has p i then
      i
    else
      fnd (i+1)
  in
  fnd i




let iter (f: 'a -> unit) (p: 'a t): unit =
    let rec iter i =
        if i = capacity p then
            ()
        else (
            f (elem p i);
            iter (find p (i + 1))
        )
    in
    iter (find p 0)




let occupy (p:'a t) (a:'a): int =
  match p.first_free with
  | None ->
     let i = Vector.count p.vector in
     Vector.push_rear p.vector (Elem a);
     i
  | Some i ->
     match Vector.elem p.vector i with
     | Free free ->
        p.first_free <- free;
        Vector.put p.vector i (Elem a);
        i
     | Elem _ ->
        assert false (* cannot happen *)

let release (p:'a t) (i:int): unit =
  assert (i < capacity p);
  assert (has p i);
  Vector.put p.vector i (Free p.first_free);
  p.first_free <- Some i
