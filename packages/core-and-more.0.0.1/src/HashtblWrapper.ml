open Util
open Core

module type ImperativeDict =
sig
  type key
  type 'a t

  val create : int -> 'a t
  val empty : unit -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val add: key -> 'a -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold2 : (key -> 'a -> key -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val copy: 'a t -> 'a t
  val find_or_add : key -> (unit -> 'a) -> 'a t -> 'a
  (*val add : elt -> t -> unit
  val remove : elt -> t -> unit
  val size : t -> int
  val is_empty : t -> bool
  val contains : elt -> t -> bool
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
  val as_list : t -> elt list
  val iter : (elt -> unit) -> t -> unit
  val union : t -> t -> t
  val pp : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    val update : (elt option -> unit) -> elt -> t -> unit*)
end

module Make(D : Data) =
struct
  module D = struct
    type t = D.t
    [@@deriving ord, show, hash]

    let sexp_of_t _ = failwith "cant call"
  end

  type key = D.t
  type 'a t = (key,'a) Hashtbl.t

  let create size =
    Hashtbl.create ~size (module D)

  let empty _ =
    Hashtbl.create (module D)

  let add
      (k:key)
      (v:'a)
      (s:'a t)
    : unit =
    Hashtbl.set ~key:k ~data:v s

  let find_or_add
      (k:key)
      (default:unit -> 'a)
      (s:'a t)
    : 'a =
    Hashtbl.find_or_add ~default s k

  let update
      (k:key)
      (update:'a option -> 'a)
      (d:'a t)
    : unit =
    Hashtbl.update
      ~f:update
      d
      k

  let size
      (s:'a t)
    : int =
    Hashtbl.length s

  let is_empty
      (s:'a t)
    : bool =
    Hashtbl.is_empty s

  let contains_key
      (key:D.t)
      (s:'a t)
    : bool =
    Hashtbl.mem s key

  let find_opt
      (key:D.t)
      (d:'a t)
    : 'a option =
    Hashtbl.find d key

  let fold
      (type a)
      (type b)
      (f:key -> a -> b -> b)
      (s:a t)
      (init:b)
    : b =
    Hashtbl.fold
      ~f:(fun ~key ~data acc -> f key data acc)
      ~init
      s

  let fold2 f a b x =
    let fold2' ka va x = fold (f ka va) b x in
    fold fold2' a x

  let iter f s =
    fold (fun k v () -> f k v) s ()

  let as_kvp_list d =
    fold
      (fun k v l -> (k,v)::l)
      d
      []

  let pp
      (type a)
      (k_pp:Format.formatter -> key -> unit)
      (v_pp:Format.formatter -> a -> unit)
      (f:Format.formatter)
      (d:a t)
    : unit =
    let rec pp_kvp f kvp =
      begin match kvp with
        | [] -> ()
        | [(k,v)] ->
          Format.fprintf
            f
            "(%a -> %a)"
            k_pp
            k
            v_pp
            v
        | (k,v)::l ->
          Format.fprintf
            f
            "(%a -> %a);%a"
            k_pp
            k
            v_pp
            v
            pp_kvp
            l

      end
    in
    let kvp = as_kvp_list d in
    Format.fprintf
      f
      "[";
    pp_kvp f kvp;
    Format.fprintf
      f
      "]"

  (*let exists f s =
    Hash_set.exists
      ~f
      s

  let as_list s =
    Hash_set.to_list
      s

  let union s1 s2 =
    Hash_set.union s1 s2

  let pp k_pp f s =
    Format.fprintf
      f
      "[";
    iter
      (fun k -> k_pp f k)
      s;
    Format.fprintf
      f
      "]"*)

  let copy
      (s:'a t)
    : 'a t =
    Hashtbl.copy s
end
