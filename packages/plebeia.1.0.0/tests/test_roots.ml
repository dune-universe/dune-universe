open Plebeia.Internal
open Roots
open Test_utils
open Stdint

module RS = Random.State
              
let random_hash st =
  Hash.of_string @@ random_string st 28
  
let random_index st =
  Uint32.of_int64 @@ RS.int64 st @@ Int64.(of_uint32 Uint32.max_int - 256L + 1L)

let st = Random.State.make_self_init ()

let () = 
  test_with_context 100000 @@ fun c ->

  (* Create context and roots files *)
  let storage_context = c.Context.storage in
  let storage_roots = Storage.create (Storage.filename storage_context ^ ".roots") in

  (* Create the roots *)
  let t = create ~storage_context ~storage_roots in

  let rec loop acc = function
    | 0 -> ()
    | n ->
        (* insert a leaf then commit *)
        let v = Value.of_string @@ string_of_int n in
        let node = Node.new_leaf v in
        let _node, i, h = Node_storage.commit_node c (Bud_cache.empty ()) node in
        (* half of them are genesis *)
        let parent = match acc with
          | [] -> None
          | _ -> 
              if RS.int st 2 = 0 then None
              else Some (Rand.one_of st (Array.of_list acc))
        in
        Roots.add t ?parent (Roots.RootHash.of_plebeia_hash h) i ~meta:(String.make 20 '\000');
        loop (i::acc) (n-1)
  in
  loop [] 1000;

  prerr_endline "Added 1000 roots";

  (* reload *)
  let t' = create ~storage_context ~storage_roots in
  assert (List.sort compare (List.map (fun e -> { e with prev= None }) 
                             @@ List.of_seq @@ Roots.to_seq t)
          = List.sort compare (List.map (fun e -> { e with prev= None })
                               @@ List.of_seq @@ Roots.to_seq t'));

  prerr_endline "Rebuild test with empty roots file...";

  (* create an empty roots *)
  let storage_roots' = Storage.create (Storage.filename storage_context ^ ".roots'") in
  let t'' = (* hope storage_roots' are filled by recovered roots from storage_context *)
    create 
      ~storage_context
      ~storage_roots:storage_roots'
  in
  assert (List.sort compare (List.of_seq @@ Roots.to_seq t)
          = List.sort compare (List.of_seq @@ Roots.to_seq t''));
  
