open GT

@type tree = Node of int * tree list


class ['a, 'b] tree_gmap_t f fself =
  object
    inherit ['a, 'b, _] @tree
    method c_Node x _ n l = Node (f () n, List.map (fself ()) l)
  end

class ['a, 'b, 'extra] tree_fold_t f fself =
  object
    inherit ['a, 'extra, 'b] @tree
    method c_Node acc _ n l = List.fold_left fself (f acc n) l
  end

let num_of_nodes t =
  GT.transform(tree) (new tree_fold_t (fun n _ -> n+1)) 0 t

let increment t =
  GT.transform(tree) (new tree_gmap_t (fun () n -> n+1)) () t

let toString t =
  Buffer.contents @@
  GT.transform(tree)
    (new tree_fold_t
      (fun buf n ->
         Buffer.add_string buf (string_of_int n);
         buf
      ))
    (Buffer.create 1024)
    t

let _ =
  let t = Node (1, [Node (2, [Node (4, [])]); Node (2, [])]) in
  Printf.printf "Tree: %s\n" (toString t);
  Printf.printf "Number of nodes: %d\n" (num_of_nodes t);
  Printf.printf "Incremented: %s\n" (toString (increment t))


let () = print_endline "wtf"
