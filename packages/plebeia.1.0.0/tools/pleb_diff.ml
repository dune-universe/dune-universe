(* input 2 trees 
   
   output diff of buds and leaves, checked their correctness 
   by applying them to the original.
   
   Checked diffs and applied them succcessfully in 63.9 mins for 22G data
   in Jun's machine kurama.
*)

open Plebeia.Internal
open Utils.Open
open Node

let () =
  let path = Sys.argv.(1) in
  let vc = Vc.open_ ~mode:Storage.Reader path in
  let roots = Vc.roots vc in
  let context = Vc.context vc in

  let nroots = Roots.length roots in
  let processed = ref 0 in
  let t0 = Unix.gettimeofday () in

  let rec loop = function
    | [] -> ()
    | root::jobs ->
        incr processed;

        let n1 = match root.Roots.parent with
          | None -> View (_Bud (None, Not_Indexed, Not_Hashed))
          | Some i ->
              let v = Node_storage.load_node context i Not_Extender in
              View v
        in
        let Cursor (_, n2, _) = from_Some @@ Vc.checkout vc root.Roots.hash in

        let diffs = Ediff.diff context n1 n2 in
        Format.eprintf "checking index %d@." (Index.to_int root.Roots.index);
        Format.eprintf "%s: %d diffs@." (Roots.RootHash.to_hex_string root.Roots.hash) (List.length diffs);
        Ediff.check diffs context n1 n2;

        let buds,leaves = List.fold_left (fun (buds,leaves) -> function
            | Ediff.CopyBud _ -> buds+1,leaves
            | CopyLeaf _ -> buds,leaves+1
            | _ -> buds,leaves) (0,0) diffs 
        in
        Format.eprintf "diff, %d, %d, %d@." (Index.to_int root.Roots.index) buds leaves;

        let t = Unix.gettimeofday () in
        Format.eprintf "ETC: %.1f mins (Passed %.1f mins)@." 
          ((t -. t0) /. float !processed *. float (nroots - !processed) /. 60.0)
          ((t -. t0) /. 60.0);

        loop @@ (Roots.children roots root) @ jobs
  in
  loop @@ Roots.genesis roots
