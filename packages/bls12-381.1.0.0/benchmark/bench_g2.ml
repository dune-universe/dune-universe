open Core
open Core_bench

let t1 =
  let open Bls12_381 in
  let s = G2.Scalar.random () in
  let p = G2.random () in
  Bench.Test.create ~name:"Multiplication on G2" (fun () -> ignore (G2.mul p s))

let t2 =
  let open Bls12_381 in
  let p = G2.random () in
  Bench.Test.create ~name:"Double on G2" (fun () -> ignore (G2.double p))

let t3 =
  let open Bls12_381 in
  let p1 = G2.random () in
  let p2 = G2.random () in
  Bench.Test.create ~name:"Addition on G2" (fun () -> ignore (G2.add p1 p2))

let t4 =
  let open Bls12_381 in
  let p = G2.random () in
  Bench.Test.create ~name:"Negate on G2" (fun () -> ignore (G2.negate p))

let t5 =
  let open Bls12_381 in
  let p = G2.random () in
  let p_bytes = G2.to_bytes p in
  Bench.Test.create ~name:"of_bytes_exn on G2" (fun () ->
      ignore (G2.of_bytes_exn p_bytes))

let t6 =
  let open Bls12_381 in
  let p = G2.random () in
  Bench.Test.create ~name:"to_bytes on G2" (fun () -> ignore (G2.to_bytes p))

let t7 =
  let open Bls12_381 in
  let p = G2.random () in
  let p_bytes = G2.to_compressed_bytes p in
  Bench.Test.create ~name:"of_compressed_bytes_exn on G2" (fun () ->
      ignore (G2.of_compressed_bytes_exn p_bytes))

let t8 =
  let open Bls12_381 in
  let p = G2.random () in
  Bench.Test.create ~name:"to_compressed_bytes on G2" (fun () ->
      ignore (G2.to_compressed_bytes p))

let t9 =
  let open Bls12_381 in
  let message_length = Random.int 512 in
  let dst_length = Random.int 48 in
  let message =
    Bytes.init message_length ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let dst = Bytes.init dst_length ~f:(fun _i -> char_of_int (Random.int 48)) in
  Bench.Test.create ~name:"hash_to_curve on G2" (fun () ->
      ignore (G2.hash_to_curve message dst))

let command = Bench.make_command [t1; t2; t3; t4; t5; t6; t7; t8; t9]

let () = Core.Command.run command
