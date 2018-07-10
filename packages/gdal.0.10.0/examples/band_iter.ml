open Gdal

let () =
    if Array.length Sys.argv <> 2 then (
      Printf.eprintf "Usage: %s [source file]\n" Sys.argv.(0);
      exit 1
    );
  let file = Sys.argv.(1) in
  Lib.init_dynamic ();
  Lib.register_all ();
  let ds = Data_set.of_source_exn file in
  let band = Data_set.get_band ds 1 Band.Data.Int16 in
  let minv = ref max_int in
  let maxv = ref min_int in
  let minv3x = ref max_int in
  let maxv3x = ref min_int in
  (* Iterate over a single band *)
  Band.iter_read band (
    fun _col _row v ->
      minv := min !minv v;
      maxv := max !maxv v;
  );
  (* Iterate over three bands, or at least one band three times *)
  Band.itera_read [|band; band|] band (
    fun _col _row src dst ->
      let sum = Array.fold_left ( + ) dst src in
      minv3x := min !minv3x sum;
      maxv3x := max !maxv3x sum;
  );
  Data_set.close ds;
  Printf.printf "min: %d\n" !minv;
  Printf.printf "max: %d\n" !maxv;
  Printf.printf "3 * min: %d\n" !minv3x;
  Printf.printf "3 * max: %d\n" !maxv3x;
  ()
