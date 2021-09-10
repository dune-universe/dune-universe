open Remu_ts.Infer
open Remu_ts.Comm
module TC : TState = (val crate_tc empty_tctx : TState)

let _ = let open TC in
  let i32 = new_type "int32" in
  let a = new_tvar() in
  let b = new_tvar() in
  let is_ok = unify (Tuple [a; a]) (Tuple [i32; b]) in
  assert (is_ok && prune a = i32 && prune b = i32);
  let rho = new_tvar() in
  let record1 = Record(record ["f1", i32] (RowPoly rho)) in
  let record2 = Record(record ["f2", i32; "f1", a] RowMono) in
  let is_ok = unify record1 record2 in
  assert is_ok;
  let rho = prune rho in
  print_endline @@ show rho;
  assert begin
  match rho with
  | Record rowt -> begin
    match extract_row rowt with
      | fields, None ->
        Map.mem "f2" fields && Map.cardinal fields = 1
      | _ -> false
    end
  | _ -> false
  end