open Core_bench

module MakeBenchBlst = struct
  module F = Bls12_381.Fr

  let test_addition =
    let e1 = F.random () in
    let e2 = F.random () in
    Bench.Test.create ~name:"Addition Fr" (fun () -> ignore (F.add e1 e2))

  let test_multiplication =
    let e1 = F.random () in
    let e2 = F.random () in
    Bench.Test.create ~name:"Multiplication Fr" (fun () -> ignore (F.mul e1 e2))

  let test_sub =
    let e1 = F.random () in
    let e2 = F.random () in
    Bench.Test.create ~name:"Substraction Fr" (fun () -> ignore (F.sub e1 e2))

  let test_negate =
    let e1 = F.random () in
    Bench.Test.create ~name:"Opposite Fr" (fun () -> ignore (F.negate e1))

  let test_square =
    let e1 = F.random () in
    Bench.Test.create ~name:"Square Fr" (fun () -> ignore (F.square e1))

  let test_double =
    let e1 = F.random () in
    Bench.Test.create ~name:"Double Fr" (fun () -> ignore (F.double e1))

  let test_inverse =
    let e1 = F.random () in
    Bench.Test.create ~name:"Inverse Fr" (fun () -> ignore (F.inverse_exn e1))

  let test_pow =
    let e1 = F.random () in
    let n = F.(to_z (random ())) in
    Bench.Test.create ~name:"Pow Fr" (fun () -> ignore (F.pow e1 n))

  let test_of_bytes_exn =
    let e = F.random () in
    let e_bytes = F.to_bytes e in
    Bench.Test.create ~name:"of_bytes_exn Fr" (fun () ->
        ignore (F.of_bytes_exn e_bytes))

  let test_to_bytes =
    let e = F.random () in
    Bench.Test.create ~name:"to_bytes Fr" (fun () -> ignore (F.to_bytes e))

  let get_benches () =
    [ test_addition;
      test_multiplication;
      test_negate;
      test_sub;
      test_square;
      test_inverse;
      test_pow;
      test_double;
      test_of_bytes_exn;
      test_to_bytes ]
end

let () =
  let commands = List.concat [MakeBenchBlst.get_benches ()] in
  Core.Command.run (Core_bench.Bench.make_command commands)
