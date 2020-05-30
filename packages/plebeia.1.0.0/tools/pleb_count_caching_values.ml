open Plebeia.Internal
open Utils.Open

let incr_count tbl value =
  match Hashtbl.find_opt tbl value with
  | None ->
     Hashtbl.add tbl value 1
  | Some count ->
     Hashtbl.replace tbl value (count + 1)

let count_new_nodes vc rh num_of_shared_values num_of_replesentative_values =
  let entry = from_Some @@ Roots.find (Vc.roots vc) rh in
  let parent_i = match entry.parent with Some i -> i | None -> Index.of_int 0 in

  let c = from_Some @@ Vc.checkout vc rh in

  let is_new_value index = (Index.to_int index >= Index.to_int parent_i) in

  Cursor.fold ~init:() c (fun () c ->
      let _, v = Cursor.view c in
      let i = from_Some @@ Node.index_of_view v in
      match v with
      | Leaf (value, _, _) when Value.length value <= 36 (* XXX hard coded *) ->
         begin if is_new_value i then
           (* not cached *)
           incr_count num_of_replesentative_values value
         else
           (* cached *)
           incr_count num_of_shared_values value
         end;
         `Continue, ()
      | _ ->
         if is_new_value i then
           `Continue, ()
         else
           `Up, ())

let map_zip tbl1 tbl2 =
  let res = Hashtbl.create (Hashtbl.length tbl1) in
  Hashtbl.iter (fun k v ->
      let v1 = Some v in
      let v2 = Hashtbl.find_opt tbl2 k in
      Hashtbl.add res k (v1, v2)) tbl1;
  Hashtbl.iter (fun k v ->
      if Hashtbl.mem tbl1 k = false then
        let v1 = None in
        let v2 = Some v in
        Hashtbl.add res k (v1, v2)) tbl2;
  res

let dump_result cached reps =
  Format.eprintf "=== cached: (%d)\n" (Hashtbl.length cached);
  Format.eprintf "=== reps:   (%d)\n" (Hashtbl.length reps);
  Format.printf "Value, Value Size, Cached, Not cached, Caching performance[byte]\n";
  let rows =
    map_zip reps cached
    |> Hashtbl.to_seq
    |> Seq.filter (fun (_, (_, cached)) -> cached <> None)
    |> Seq.map (fun (v, (reps,cached)) -> (v, from_Some reps, from_Some cached))
    |> Seq.map (fun (value, reps, cached) ->
           let cells = if Value.length value <= 32 then 2 else 3 in
           if Value.length value > 36 then
             failwith (Printf.sprintf "BIG VALUE: %s: %d" (Value.to_hex_string value) (Value.length value));
           let performance = 32 * cached * (cells - 1) in
           (value, Value.length value, cached, reps, performance))
    |> List.of_seq
    |> List.sort (fun x y ->
           let size (_, len, cached, _, _) = len * cached in
           compare (size y) (size x))
  in
  rows
  |> List.iter (fun (value, len, cached, reps, performance) ->
         Format.printf "'%s', %d, %d, %d, %d\n" (Value.to_hex_string value)
           len cached reps performance
       );
  let (total_len, total_cached, total_reps, total_performance) =
    List.fold_left (fun
          (len_sum, cached_sum, reps_sum, perf_sum)
          (_value, len, cached, reps, performance) ->
        (len+len_sum, cached+cached_sum, reps+reps_sum, performance+perf_sum)) (0, 0, 0, 0) rows
  in
  Format.printf "TOTAL, %d, %d, %d, %d\n" total_len total_cached total_reps total_performance

let () =
  let path1 = Sys.argv.(1) in
  let vc1 = Vc.open_ ~mode:Storage.Reader path1 in
  let roots = Vc.roots vc1 in

  let cells = Index.to_int @@ Storage.get_current_length (Vc.context vc1).Context.storage in

  let t1 = Unix.gettimeofday () in

  let num_of_shared_values = Hashtbl.create 0 in
  let num_of_replesentative_values = Hashtbl.create 0 in

  let f e () =
    let { Roots.hash = h ; _ } = from_Some @@ Roots.find_by_index roots e.Roots.index in
    let () = count_new_nodes vc1 h num_of_shared_values num_of_replesentative_values in
    let nchildren = List.length @@ Roots.children roots e in
    if nchildren > 0 then begin
      Format.eprintf "Counting %d@." (Index.to_int e.Roots.index);
    end
  in

  Roots.fold_breadth_first f roots ();
  let t2 = Unix.gettimeofday () in
  dump_result num_of_shared_values num_of_replesentative_values;
  Format.eprintf "Counted %d cells in %f secs@." cells (t2 -. t1)
