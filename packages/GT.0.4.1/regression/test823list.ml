(* type 'a list = [] | (::) of 'a * 'a list
 * [@@deriving gt ~options:{ show; html; fmt; gmap; eval; stateful; foldl; foldr; compare; eq }] *)


(* type 'a option = None | Some of 'a
 * [@@deriving gt ~options:{ show; html; fmt; gmap; eval; stateful; foldl; foldr; compare; eq }] *)

(* type ('a,'b) tuple2 = T2 of 'a * 'b
 * [@@deriving gt ~options:{ show; html; fmt; gmap; eval; stateful; foldl; foldr; compare; eq }] *)


module L : sig
  type 'a list = [ `Nil  | `Cons of ('a * 'a list) ]
  [@@deriving gt ~options:{ (* show; *) gmap; }]
end = struct
  type 'a list = [ `Nil  | `Cons of ('a * 'a list) ]
  [@@deriving gt ~options:{ (* show; *) gmap; }]
end
