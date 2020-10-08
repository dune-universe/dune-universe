type ('a, 'b) p = GT.int * (GT.string * 'a)
  [@@deriving gt ~options:{show; gmap; compare; foldl }]

type ('a, 'b, 'c) triple = 'a * 'b * 'c
  [@@deriving gt ~options:{show; gmap; compare; foldl }]

type ('a, 'b, 'c) quattre = 'a * 'b * 'c * 'c
  [@@deriving gt ~options:{show; gmap; compare; foldl }]

include (struct
type t = GT.int * GT.string
  [@@deriving gt ~options:{show; gmap; compare; eq; eval; foldl; html; stateful }]
end : sig
type t = GT.int * GT.string
  [@@deriving gt ~options:{show; gmap; compare; eq; eval; foldl; html; stateful }]
end)
