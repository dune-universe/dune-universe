open Prom

let rec randseq n vmax () =
  let open Seq in
  Cons
    (Random.float vmax, match n with 0 -> empty | n -> randseq (pred n) vmax)

let basic n =
  let t = KLL.create () in
  Seq.iter (fun v -> KLL.update t v) (randseq n 10000.);
  Format.eprintf "%a" (KLL.pp_cdf Fmt.float) (KLL.cdf t)

let kll =
  [
    ("basic10", `Quick, fun () -> basic 10);
    ("basic1_000", `Quick, fun () -> basic 1_000);
    ("basic100_000", `Quick, fun () -> basic 100_000);
  ]

let () = Alcotest.run "prometheus" [ ("kll", kll) ]
