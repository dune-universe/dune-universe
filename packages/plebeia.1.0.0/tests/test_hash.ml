open Plebeia.Internal
open Test_utils

let f ctxt n = 
  let _, nh = Node_hash.compute ctxt n in
  Format.eprintf "hash: %s@."
    (Node_hash.to_hex_string nh);
  Format.eprintf "%a@." Node.pp n

open Node

let () =
  test_with_context 1000 @@ fun ctxt ->
  let l = new_leaf @@ Value.of_string "hello world" in 
  f ctxt l;
  let i = new_internal (new_bud None) (new_bud None) in
  f ctxt i;
  let b = new_bud (Some i) in
  f ctxt b;
  let e = new_extender Segment.(of_sides [Right]) (new_bud None) in
  f ctxt e
  
    
  

