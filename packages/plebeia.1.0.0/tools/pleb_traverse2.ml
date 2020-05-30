open Plebeia.Internal
open Utils

let tbl = Hashtbl.create 1001

let g (cached, news as st) _i (k, c) =
  let context = Cursor.context c in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in
  match k with
  | `New | `Old -> st
  | `Cached -> 
      begin match Cursor.view c with
        | _, Node.Leaf (v, _, _) when Value.length v <= hashcons_max_leaf_size ->
            begin match Hashtbl.find_opt tbl v with
              | None -> assert false
              | Some (saved, hit) -> Hashtbl.replace tbl v (saved, hit+1)
            end
        | _ -> assert false
      end;
      (cached + 1, news)
  | `NewSmallLeaf -> 
      begin match Cursor.view c with
        | _, Node.Leaf (v, _, _) when Value.length v <= hashcons_max_leaf_size ->
            begin match Hashtbl.find_opt tbl v with
              | None -> Hashtbl.replace tbl v (1, 0)
              | Some (saved, hit) -> 
                  (* cache miss *)
                  Hashtbl.replace tbl v (saved+1, hit)
            end
        | _ -> assert false
      end;
      (cached, news + 1)

let h (cached, news as st) ent =
  Format.eprintf "index=%d cached=%d news=%d@." (Index.to_int ent.Roots.index) cached news;
  st

let () =
  let path1 = Sys.argv.(1) in
  let vc1 = Vc.open_ ~mode:Storage.Reader path1 in
  let context = Vc.context vc1 in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in

  let (cached, news) = Plebeia.Internal.Traverse.f vc1 g h (0, 0) in
  Format.eprintf "cached=%d news=%d@." cached news;
  
  let size = Hashtbl.length tbl in
  Format.eprintf "diffrent small values: %d@." size;

  let a = Array.init (hashcons_max_leaf_size + 1) (fun _ -> 0,0,0) in
  Hashtbl.iter (fun v (saved, hit) ->
      let size = Value.length v in
      let (n, s_saved, s_hit) = Array.unsafe_get a size in
      Array.unsafe_set a size (n + 1, s_saved + saved, s_hit + hit)) tbl;
  Format.eprintf "size, values, saved, hit@.";
  for size = 0 to hashcons_max_leaf_size do
    let (n, s_saved, s_hit) = Array.unsafe_get a size in
    Format.eprintf "%d, %d, %d, %d@."
      size n s_saved s_hit
  done

