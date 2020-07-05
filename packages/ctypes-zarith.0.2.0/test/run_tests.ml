open Ctypes_zarith

module Add (X : sig
  val z_add :
    Ctypes_zarith.MPZ.t Ctypes.abstract Ctypes.ptr -> Z.t -> Z.t -> unit

  val q_add :
    Ctypes_zarith.MPQ.t Ctypes.abstract Ctypes.ptr -> Q.t -> Q.t -> unit
end) =
struct
  open X

  let z_add a b =
    let r = MPZ.make () in
    z_add r a b;
    MPZ.to_z r

  let q_add a b =
    let r = MPQ.make () in
    q_add r a b;
    MPQ.to_q r

  let check x = Alcotest.(check bool) "" true x

  let test_add () =
    let one = Z.of_int 1 in
    let two = Z.of_int 2 in
    let three = Z.of_int 3 in
    let six = Z.of_int 6 in
    let ( / ) n d = Q.make n d in
    check (three = z_add one two);
    check (one / two = q_add (one / three) (one / six));
    let fz n =
      let r = Z.(n + n) in
      check (r = z_add n n)
    in
    let n = Z.of_int max_int in
    fz n;
    let n = Z.(n + n) in
    fz n;
    let n = Z.(n * n) in
    fz n;
    let n = Z.pow n 256 in
    fz n;
    let fq n =
      let r = Q.(n + n) in
      check (r = q_add n n)
    in
    let n = six / Z.of_int max_int in
    fq n;
    let n = Q.(n + n) in
    fq n;
    let n = Q.(n * n) in
    fq n;
    ()
end

module Add_foreign = Add (Test_foreign)
module Add_stubgen = Add (Test_stubgen)
module Add_ppx = Add (Test_ppx)

let () =
  Alcotest.run "ctypes-zarith"
    [
      ("add (foreign)", [ ("add (foreign)", `Quick, Add_foreign.test_add) ]);
      ("add (stubgen)", [ ("add (stubgen)", `Quick, Add_stubgen.test_add) ]);
      ("add (ppx)", [ ("add (ppx)", `Quick, Add_ppx.test_add) ]);
      ("gc", [ ("gc", `Slow, Gc.compact) ]);
    ]
