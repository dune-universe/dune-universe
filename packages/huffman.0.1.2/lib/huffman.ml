(** Type for Huffman trees *)
type 'a htree =
  | Node of float * 'a htree * 'a htree
  | Leaf of float * 'a

type 'a t = 'a htree

(**
   Compare two huffman trees
   @param ht1 a tree
   @param ht2 a tree
*)
let compare (ht1:'a htree) (ht2: 'a htree) =
  match (ht1, ht2) with
  | Node (f1, _, _), Node (f2, _, _)
  | Node (f1, _, _), Leaf (f2, _)
  | Leaf (f1, _), Node (f2, _, _)
  | Leaf (f1, _), Leaf (f2, _) ->
    if f1 > f2 then 1 else -1

(**
   Combine twho huffman trees
   @param ht1 a tree
   @param ht2 a tree
*)
let combine (ht1:'a htree) (ht2:'a htree) =
  match (ht1, ht2) with
  | Node (f1, _, _), Node (f2, _, _)
  | Node (f1, _, _), Leaf (f2, _)
  | Leaf (f1, _), Node (f2, _, _)
  | Leaf (f1, _), Leaf (f2, _) ->
    Node (f1 +. f2, ht1, ht2)

(**
   Sort a list of huffman trees
   @param htl the list
*)
let sort htl =
  List.sort compare htl

(**
   Create a Huffman tree based on the description of an emitter
   @param l Symbol -> Frequency association
*)
let huffman l =
  let rec step pop =
    match sort pop with
    | [] -> failwith "Trying to step over empty list"
    | [ht] -> ht
    | ht1::ht2::r ->
      step ((combine ht1 ht2)::r)
  in
  step (List.map (fun (a,f) -> Leaf (f, a)) l)

(**
   Outputs a tree to a dot file
   @param f   The output file
   @param ht  The Huffman tree
*)
let dump_as_dot f ht =
  let rec dump ht i oc =
    match ht with
    | Node (f, ht1, ht2) ->
      let i1 = dump ht1 i oc in
      let i2 = dump ht2 (i1+1) oc in
      let i3 = i2 + 1 in
      Printf.fprintf stdout "i3 %d %d %d %d\n" i i1 i2 i3;
      Printf.fprintf oc "%d -> %d [label=0]\n" i3 i1;
      Printf.fprintf oc "%d -> %d [label=1]\n" i3 i2;
      Printf.fprintf oc "%d [label=\"%f\"]\n" i3 f;
      i3
    | Leaf (f, c) ->
      Printf.fprintf oc "%d [label=\"%c : %.3f\"]\n" i c f;
      i
  in
  let oc = open_out f in
  Printf.fprintf oc "Digraph {\n";
  ignore(dump ht 0 oc);
  Printf.fprintf oc "}\n";
  close_out oc