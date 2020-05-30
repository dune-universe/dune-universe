type hash
val zerohash : hash
    
type segment
val zerosegment : segment

type hash_segment = hash * segment
    
type node =
  | Leaf of hash
  | Bud of node option
  | Internal of node * node
  | Extender of segment * node

val merge : hash_segment * hash_segment -> hash

let rec h = function
  | Leaf hash -> hash, zerosegment
  | Bud None -> zerohash, zerosegment
  | Bud (Some n) -> merge (h n, zerohash), zerosegment
  | Internal (n1, n2) -> merge (h n1, h n2), zerosegment
  | Extender (segment, n) -> h n, segment

let rec invariant_node = function
  | Leaf hash -> hash <> zerohash
  | Extender (segment, Extender _) -> false
  | Extender (segment, n) -> 
      segment <> zerosegment
      && invariant_node n
  | Internal (n1, n2) -> invariant_node n1 && invariant_node n2

(* invariant for merge
   
   forall hash_segment1, hash_segment2, hash_segment3, hash_segment4,
   if (hash_segment1, hash_segment2) <> (hash_segment3, hash_segment4),
   merge (hash_segment1, hash_segment2) <> merge (hash_segment3, hash_segment4)
*)

(* Theorem
   
   forall n1, n2, if n1 <> n2, h(n1) <> h(n2)
*)

