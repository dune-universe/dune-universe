open Inferno

let postincrement r =
  let v = !r in
  r := v + 1;
  v

(* This function prepares [k] random integers of amplitude [n]. *)
let prepare_random n k =
  let integers = Array.init k (fun _ -> Random.int n) in
  let c = ref 0 in
  fun () ->
    integers.(postincrement c)

let time msg f =
  let start = Unix.gettimeofday() in
  let v = f() in
  let finish = Unix.gettimeofday() in
  Printf.fprintf stderr "%s: %.2f seconds.\n%!" msg (finish -. start);
  v

let testUF n k =
  let random : unit -> int =
    time "Preparation of the random numbers" (fun () -> prepare_random n (3 * k))
  in
  let point : int UnionFind.point array =
    time "Initialization" (fun () -> Array.init n UnionFind.fresh)
  in
  time "UnionFind" (fun () ->
    for _op = 1 to k do
      if random() mod 2 = 0 then begin
        (* Union. *)
        let i = random()
        and j = random() in
        if not (UnionFind.equivalent point.(i) point.(j)) then
          UnionFind.union (+) point.(i) point.(j)
      end
      else begin
        (* Find. *)
        let i = random() in
        let _ = UnionFind.find point.(i) in
        ()
      end
    done
  )

let testTUF n k =
  let random : unit -> int =
    time "Preparation of the random numbers" (fun () -> prepare_random n (3 * k))
  in
  let point : int TUnionFind.point array =
    time "Initialization" (fun () -> Array.init n TUnionFind.fresh)
  in
  time "TUnionFind" (fun () ->
    TRef.tentatively (fun t ->
    for _op = 1 to k do
      if random() mod 2 = 0 then begin
        (* Union. *)
        let i = random()
        and j = random() in
        if not (TUnionFind.equivalent point.(i) point.(j)) then
          TUnionFind.union t (+) point.(i) point.(j)
      end
      else begin
        (* Find. *)
        let i = random() in
        let _ = TUnionFind.find point.(i) in
        ()
      end
    done
    )
  )

let () =
  Random.self_init();
  Printf.printf "Running the UnionFind benchmarks...\n%!";
  testUF  1000000 10000000;
  testTUF 1000000 10000000
