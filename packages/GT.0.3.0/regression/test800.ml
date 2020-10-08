
module PV: sig
  type a = A of b | C of GT.int    | E of a
  and  b = B of a | D of GT.string | F of b
  [@@deriving gt ~options:{show;gmap}]
end = struct
  type a = A of b | C of GT.int    | E of a
  and  b = B of a | D of GT.string | F of b
  [@@deriving gt ~options:{show;gmap}]
end

(*
TODO:
type 'a t = [> `Abs of GT.string * 'a ] as 'a
  [@@deriving gt ~options:{show;gmap}]
   *)
