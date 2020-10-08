(* The same as test 086 but in PPX syntax *)

module T : sig
  type t2 = GT.int * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t3 = GT.int * 'a * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t4 = GT.bytes [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t1 = 'a [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type bindings = (GT.string * GT.int) GT.list [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a u1 = 'a GT.option [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]
  type 'a u2 = 'a GT.Lazy.t [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a u3 = {aa: GT.int; bb:GT.string} [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]

  type 'a r1 = 'a GT.ref [@@deriving gt ~options:{fmt; html}]

  type ('a,'b) arr1 = ('a * 'b) GT.array [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]
end = struct

  type t2 = GT.int * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t3 = GT.int * 'a * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t4 = GT.bytes [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t1 = 'a [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type bindings = (GT.string * GT.int) GT.list [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a u1 = 'a GT.option [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]
  type 'a u2 = 'a GT.Lazy.t [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a u3 = {aa: GT.int; bb:GT.string} [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]

  type 'a r1 = 'a GT.ref [@@deriving gt ~options:{fmt; html}]
  let () = ()

  type ('a,'b) arr1 = ('a * 'b) GT.array [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]

end
