open Core
open Core_bench
open Bls12_381

let t1 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  Bench.Test.create
    ~name:"Sign on random message of random size - Basic scheme"
    (fun () -> ignore @@ Signature.Basic.sign sk msg)

let t2 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  Bench.Test.create
    ~name:"Sign on random message of random size - Aug scheme"
    (fun () -> ignore @@ Signature.Aug.sign sk msg)

let t3 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  Bench.Test.create
    ~name:"Sign on random message of random size - Pop scheme"
    (fun () -> ignore @@ Signature.Pop.sign sk msg)

let t4 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let pk = Signature.derive_pk sk in
  let signature = Signature.Basic.sign sk msg in
  Bench.Test.create
    ~name:"Verify correct signature with correct pk - Basic Scheme"
    (fun () -> ignore @@ Signature.Basic.verify pk msg signature)

let t5 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let pk = Signature.derive_pk sk in
  let signature = Signature.Aug.sign sk msg in
  Bench.Test.create
    ~name:"Verify correct signature with correct pk - Aug Scheme"
    (fun () -> ignore @@ Signature.Aug.verify pk msg signature)

let t6 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let pk = Signature.derive_pk sk in
  let signature = Signature.Pop.sign sk msg in
  Bench.Test.create
    ~name:"Verify correct signature with correct pk - Pop Scheme"
    (fun () -> ignore @@ Signature.Pop.verify pk msg signature)

let t7 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let ikm' = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let sk' = Signature.generate_sk ikm' in
  let pk' = Signature.derive_pk sk' in
  let signature = Signature.Basic.sign sk msg in
  Bench.Test.create
    ~name:"Verify correct signature with incorrect pk - Basic Scheme"
    (fun () -> ignore @@ Signature.Basic.verify pk' msg signature)

let t8 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let ikm' = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let sk' = Signature.generate_sk ikm' in
  let pk' = Signature.derive_pk sk' in
  let signature = Signature.Aug.sign sk msg in
  Bench.Test.create
    ~name:"Verify correct signature with incorrect pk - Aug Scheme"
    (fun () -> ignore @@ Signature.Aug.verify pk' msg signature)

let t9 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let ikm' = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let sk' = Signature.generate_sk ikm' in
  let pk' = Signature.derive_pk sk' in
  let signature = Signature.Pop.sign sk msg in
  Bench.Test.create
    ~name:"Verify correct signature with incorrect pk - Pop Scheme"
    (fun () -> ignore @@ Signature.Pop.verify pk' msg signature)

let t10 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let msg' =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let pk = Signature.derive_pk sk in
  let signature = Signature.Basic.sign sk msg in
  Bench.Test.create
    ~name:"Verify incorrect message with correct pk - Basic Scheme"
    (fun () -> ignore @@ Signature.Basic.verify pk msg' signature)

let t11 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let msg' =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let pk = Signature.derive_pk sk in
  let signature = Signature.Aug.sign sk msg in
  Bench.Test.create
    ~name:"Verify incorrect message with correct pk - Aug Scheme"
    (fun () -> ignore @@ Signature.Aug.verify pk msg' signature)

let t12 =
  let ikm = Bytes.init 32 ~f:(fun _i -> char_of_int (Random.int 256)) in
  let msg =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let msg' =
    Bytes.init (1 + Random.int 512) ~f:(fun _i -> char_of_int (Random.int 256))
  in
  let sk = Signature.generate_sk ikm in
  let pk = Signature.derive_pk sk in
  let signature = Signature.Pop.sign sk msg in
  Bench.Test.create
    ~name:"Verify incorrect message with correct pk - Pop Scheme"
    (fun () -> ignore @@ Signature.Pop.verify pk msg' signature)

let () =
  Core.Command.run
    (Bench.make_command [t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t11; t12])
