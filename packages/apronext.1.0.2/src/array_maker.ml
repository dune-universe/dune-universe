(* This file provides a generic way to build high-level module on *)
(* array like structures                                          *)

module type ArrayLike = sig

  type elem
  type t

  val array_get : t -> int -> elem
  val array_set :  t -> int -> elem -> unit
  val array_length : t -> int
  val make : elem -> int -> t
  val empty : t

end

module Make (A:ArrayLike) = struct

  let array_fold f a arr =
    let size = A.array_length arr in
    let rec aux accu idx =
      if idx >= size then accu
      else aux (f accu (A.array_get arr idx)) (idx+1)
    in aux a 0

  let array_iter f arr =
    array_fold (fun _ a -> f a) () arr

  let array_iteri f arr =
    let i = ref 0 in
    array_iter (f !i) arr

  let array_for_all pred arr =
    try
      array_iter (fun a -> if pred a |> not then raise Exit) arr;
      true
    with Exit -> false

  let array_exists pred arr =
    try
      array_iter (fun a -> if pred a then raise Exit) arr;
      true
    with Exit -> false

  let array_to_list arr =
    let rec aux accu idx =
      if idx < 0 then accu
      else aux ((A.array_get arr idx)::accu) (idx-1)
    in aux [] (A.array_length arr - 1)

  let array_of_list = function
    | [] -> A.empty
    | h::_ as l ->
       let size = List.length l in
       let ear = A.make h size in
       List.iteri (A.array_set ear) l;
       ear
end

module LinconsExt = Make (struct
  open Apron

  include Lincons1

  type elem = t
  type t = earray

  let make elem = array_make elem.env

  let empty = array_make (Environment.make [||] [||]) 0
end)

module TconsExt = Make (struct
  open Apron

  include Tcons1

  type elem = t
  type t = earray

  let make elem = array_make elem.env

  let empty = array_make (Environment.make [||] [||]) 0
end)

module GeneratorExt = Make (struct
  open Apron

  include Generator1

  type elem = t
  type t = earray

  let make elem = array_make elem.env

  let empty = array_make (Environment.make [||] [||]) 0
end)
