open Js_of_ocaml
open Js

type nonrec 'a t = 'a t
type nonrec 'a optdef = 'a optdef
type nonrec 'a opt = 'a opt
type nonrec 'a js_array = 'a js_array
type nonrec 'a readonly_prop = 'a readonly_prop
type nonrec 'a meth = 'a meth
type nonrec 'a constr = 'a constr
type nonrec 'a callback = 'a callback
type nonrec ('a, 'b) meth_callback = ('a, 'b) meth_callback
type nonrec js_string = js_string
type top = Unsafe.top
type any = Unsafe.any
type any_item = string * Unsafe.any

let optdef f : ('a option -> 'b optdef) = function
  | None -> undefined
  | Some x -> def (f x)
let to_optdef f : ('a optdef -> 'b option) = fun x -> match Optdef.to_option x with
  | None -> None
  | Some x -> Some (f x)

let def_list f = function
  | [] -> undefined
  | l -> def (f l)

let of_list l : 'a js_array t = array @@ Array.of_list l
let of_listf f l : 'a js_array t = of_list @@ List.map f l

let to_list (a : 'a js_array t) = Array.to_list @@ to_array a
let to_listf f (a : 'a js_array t) = List.map f (to_list a)

let manip_list f a = of_list @@ f @@ to_list a

module Table = struct
  type nonrec 'a t = < > t
  let obj = Unsafe.global##._Object
  let create () : 'a t = new%js obj
  let add (t : 'a t) (k : string) (v : 'a) = Unsafe.set t (string k) v
  let add_list (t : 'a t) (l : (string * 'a) list) =
    List.iter (fun (k, v) -> add t k v) l
  let add_listf (t : 'a t) f (l : (string * 'a) list) =
    List.iter (fun (k, v) -> add t k (f v)) l
  let make (l : 'a list) : 'a t =
    let t = create () in
    List.iter (fun (k, v) -> add t k v) l;
    t
  let makef f (l : 'a list) : 'a t =
    let t = create () in
    List.iter (fun (k, v) -> add t k (f v)) l;
    t
  let remove (t : 'a t) (k : string) = Unsafe.delete t (string k)
  let find (t : 'a t) (k : string) : 'a option =
    Optdef.to_option (Unsafe.get t (string k))
  let keys (t : 'a t) : string list =
    to_listf to_string @@ obj##keys t
  let items (t : 'a t) : (string * 'a) list =
    to_listf (fun k -> to_string k, Unsafe.get t k) @@ obj##keys t
  let itemsf f (t : 'a t) : (string * 'a) list =
    to_listf (fun k -> to_string k, f @@ Unsafe.get t k) @@ obj##keys t
  let length (t : 'a t) = (obj##keys t)##.length
  let merge (l : 'a t list) : 'a t =
    let t = create () in
    List.iter (fun x -> List.iter (fun (k, v) -> add t k v) (items x)) l;
    t
end

type 'a table = 'a Table.t

type 'a table_cons = T of 'a table | L of (string * 'a) list

let to_table : 'a table_cons -> 'a table = function
  | T t -> t
  | L l -> Table.make l

let to_tablef f : 'a table_cons -> 'a table = function
  | T t -> t
  | L l -> Table.makef f l

let to_table_def : 'a table_cons -> 'a table optdef = function
  | T t -> if Table.length t = 0 then undefined else def t
  | L l -> def_list Table.make l

let to_tablef_def f : 'a table_cons -> 'a table optdef = function
  | T t -> if Table.length t = 0 then undefined else def t
  | L l -> def_list (Table.makef f) l

let to_any = Unsafe.inject
let coerce = Unsafe.coerce

let to_any_table f = function
  | L l -> L (List.map (fun (k, v) -> k, f v) l)
  | T t -> T (Table.(make (itemsf f t)))

class type unit_promise = object
  method then_ : (unit -> unit) -> unit_promise t meth
  method catch : ('error -> unit) -> unit_promise t meth
end

module Async = struct

  let js (x : unit_promise t) = x
  let cb (x : unit_promise t) f = ignore @@ x##then_ f
  let lwt (x : unit_promise t) =
    let waiter, notifier = Lwt.wait () in
    ignore @@ x##then_ (fun x -> Lwt.wakeup notifier x);
    waiter

end
