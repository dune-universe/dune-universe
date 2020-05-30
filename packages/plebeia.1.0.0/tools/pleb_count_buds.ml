(*
  
   OBSOLETE: this root hash traversal is random therefore super slow.
   Do not use this tool.  Use the technique like Pleb_copy.

   Counts the buds of ALL the roots.  It does not visit the buds already seen.
   Warning: it takes super long time for a big plebeia context.

   $ dune exec ./pleb_count_buds.exe plebeia.context

*)
open Plebeia.Internal
open Utils.Open

let (//) = Filename.concat

module IS = Set.Make(struct 
    type t = Index.t 
    let compare = compare 
  end) 

let () =
  let path = Sys.argv.(1) in
  let ctxt = Vc.open_ ~mode:Storage.Reader path in
  let roots = Vc.roots ctxt in

  let nhashes = Roots.length roots in

  (* Cursor.traversal can be too slow *)
  let t1 = Unix.gettimeofday () in
  let _ = Roots.fold (fun { Roots.index=_; hash; _} ->
      fun (seen, nseen, pointed, ncopied, nhashes_done) -> 
        Format.eprintf "Checkout %s %d/%d@." (Roots.RootHash.to_hex_string hash) nhashes_done nhashes;
        let c = from_Some @@ Vc.checkout ctxt hash in
        let (seen, nseen, pointed, ncopied) = 
          Cursor.fold ~init:(seen, nseen, pointed, ncopied) c 
            (fun (seen, nseen, pointed, ncopied) c ->
                   let _, v = Cursor.view c in
                   match v with
                   | Bud (_, Not_Indexed, _) -> assert false
                   | Bud (nopt, Indexed i, _) ->
                       if IS.mem i seen then
                         `Up, (seen, nseen, pointed, ncopied)
                       else begin
                         let seen = IS.add i seen in
                         let nseen = nseen + 1 in
                         if nseen mod 1000 = 0 then begin 
                           Format.eprintf "%d bud seen@." nseen;
                         end;
                         begin match nopt with
                           | None -> 
                               `Continue, (seen, nseen, pointed, ncopied)
                           | Some n ->
                               match Node.index n with
                               | None -> assert false
                               | Some j ->
                                   if IS.mem j pointed then begin
                                     let ncopied = ncopied + 1 in
                                     if ncopied mod 100 = 0 then Format.eprintf "%d copies seen@." ncopied;
                                     `Up, (seen, nseen, pointed, ncopied)
                                   end else
                                     `Continue, (seen, nseen, IS.add j pointed, ncopied)
                         end
                       end
                   | _ -> 
                       `Continue, (seen, nseen, pointed, ncopied))
            in

            let nhashes_done = nhashes_done + 1 in

            let t2 = Unix.gettimeofday () in
            Format.eprintf "%.2f sec / 10000 bud@." ((t2 -. t1) /. float (nseen / 10000));
            Format.eprintf "%d bud / commit@." (nseen / nhashes_done);

            (seen, nseen, pointed, ncopied, nhashes_done)) 
                roots (IS.empty, 0, IS.empty, 0, 0)
            in
            ()
