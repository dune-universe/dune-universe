(*
   Print the stats of leaves of the last commit.
   
   count_leaves ~/.tezos-node/plebeia.context
*)

open Plebeia.Internal
open Utils.Open

let (//) = Filename.concat

let () =
  let path = Sys.argv.(1) in
  let vc = Vc.open_ ~mode:Storage.Reader path in
  let context = Vc.context vc in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in
  let roots = Vc.roots vc in
  
  let new_roots = 
    Roots.fold (fun e acc ->
        match Roots.children roots e with
        | [] -> e::acc
        | _ -> acc) roots []
  in

  let leaves = Array.init hashcons_max_leaf_size (fun _ -> Hashtbl.create 0) in

  match new_roots with
  | [] -> assert false
  | { hash= h ; _ } :: _ ->
      let c = from_Some @@ Vc.checkout vc h in
      let ls = Cursor.fold ~init:0 c (fun ls c ->
          let _, v = Cursor.view c in
          let ls = match v with
            | Leaf (v, _, _) ->
                let len = Value.length v in
                if  len = 0 || len > hashcons_max_leaf_size then ()
                else begin
                  let n = match Hashtbl.find_opt leaves.(len-1) v with
                    | None -> 1
                    | Some n -> n + 1
                  in
                  Hashtbl.replace leaves.(len-1) v n
                end;
                let ls = ls + 1 in
                if ls mod 100000 = 0 then Format.eprintf "checked %d leaves@." ls;
                ls
            | _ -> ls
          in
          `Continue, ls)
      in
      Format.eprintf "checked %d leaves!@." ls;
      
      Array.iteri (fun i tbl -> 
          let size = i + 1 in
          let count = Hashtbl.fold (fun _ n acc -> n + acc) tbl 0 in
          let distinct = Hashtbl.length tbl in
          Format.eprintf "%d, %d, %d@." size count distinct;
          ()) leaves;
      
      let buds = Hashtbl.create 0 in

      let uniq, copied = Cursor.fold ~init:(0,0) c @@ fun (uniq, copied as st) c ->
        let _, v = Cursor.view c in
        match v with
        | Bud _ ->
            let index = from_Some @@ Node.index_of_view v in
            if Hashtbl.mem buds index then `Up, (uniq, copied+1)
            else begin
              Hashtbl.add buds index ();
              `Continue, (uniq+1, copied)
            end
        | _ -> `Continue, st
      in
      Format.eprintf "%d uniq buds,  %d copied@." uniq copied
      
