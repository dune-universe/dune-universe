open Js

type nonrec 'a t = < > t

let obj = Unsafe.global##._Object

let create () : 'a t = new%js obj

let add (t : 'a t) (k : string) (v : 'a) = Unsafe.set t (string k) v

let add_list (t : 'a t) (l : (string * 'a) list) =
  List.iter (fun (k, v) -> add t k v) l

let add_listf (t : 'b t) (f : ('a -> 'b)) (l : (string * 'a) list) =
  List.iter (fun (k, v) -> add t k (f v)) l

let make (l : (string * 'a) list) : 'a t =
  let t = create () in
  add_list t l;
  t

let makef (f : ('a -> 'b)) (l : (string * 'a) list) : 'b t =
  let t = create () in
  add_listf t f l;
  t

let remove (t : 'a t) (k : string) = Unsafe.delete t (string k)

let find (t : 'a t) (k : string) : 'a option =
  Optdef.to_option (Unsafe.get t (string k))

let keys (t : 'a t) : string list =
  to_listf to_string @@ obj##keys t

let items (t : 'a t) : (string * 'a) list =
  to_listf (fun k -> to_string k, Unsafe.get t k) @@ obj##keys t

let itemsf (f : ('a -> 'b)) (t : 'a t) : (string * 'b) list =
  to_listf (fun k -> to_string k, f @@ Unsafe.get t k) @@ obj##keys t

let length (t : 'a t) = (obj##keys t)##.length

let merge (l : 'a t list) : 'a t =
  let t = create () in
  List.iter (fun x -> List.iter (fun (k, v) -> add t k v) (items x)) l;
  t
