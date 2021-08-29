(* Generate finite approximations of various dists *)

let rng_state = Random.State.make [| 0x1337; 0x533D |]

let spec = Binning.regular ~origin:0.0 ~width:0.05 ~truncate:(Some (-2., 2.))

let empirical_gaussian =
  Emp.of_generative ~nsamples:1000 (Gen.gaussian ~mean:0.0 ~std:1.0) rng_state

let empirical_exp =
  Emp.of_generative ~nsamples:1000 (Gen.exponential ~rate:1.) rng_state

let binned_gaussian =
  Binning.compute spec
  @@ Fin.Float.counts_of_empirical
       (module Basic_impl.Free_module.Float_valued.Float)
       empirical_gaussian

let binned_exp =
  Binning.compute spec
  @@ Fin.Float.counts_of_empirical
       (module Basic_impl.Free_module.Float_valued.Float)
       empirical_exp

let empirical_exp' =
  Emp.of_generative ~nsamples:1000 (Gen.exponential ~rate:2.) rng_state

let binned_exp' =
  Binning.compute spec
  @@ Fin.Float.counts_of_empirical
       (module Basic_impl.Free_module.Float_valued.Float)
       empirical_exp

(* Basic algebraic properties *)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"KL: diagonal" ~count:1 QCheck.unit
  @@ fun () -> Fin.Float.Dist.kl binned_gaussian binned_gaussian =. 0.0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"KL: nontrivial" ~count:1 QCheck.unit
  @@ fun () -> not (Fin.Float.Dist.kl binned_gaussian binned_exp =. 0.0)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Lp: diagonal" QCheck.pos_float
  @@ fun p ->
  let p = 1. +. p in
  Fin.Float.Dist.lp ~p binned_gaussian binned_gaussian =. 0.0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Linf: diagonal" ~count:1 QCheck.unit
  @@ fun () -> Fin.Float.Dist.linf binned_gaussian binned_gaussian =. 0.0

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Lp: nontrivial" ~count:1 QCheck.pos_float
  @@ fun p ->
  let p = 1. +. p in
  not (Fin.Float.Dist.lp ~p binned_gaussian binned_exp =. 0.0)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Linf: nontrivial" ~count:1 QCheck.unit
  @@ fun () -> not (Fin.Float.Dist.linf binned_gaussian binned_exp =. 0.0)

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Lp: triangular" QCheck.pos_float
  @@ fun p ->
  let p = 1. +. p in
  Fin.Float.Dist.lp ~p binned_gaussian binned_exp
  +. Fin.Float.Dist.lp ~p binned_exp binned_exp'
  <=. Fin.Float.Dist.lp ~p binned_gaussian binned_exp'

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make ~name:"Linf: triangular" ~count:1 QCheck.unit
  @@ fun () ->
  Fin.Float.Dist.linf binned_gaussian binned_exp
  +. Fin.Float.Dist.linf binned_exp binned_exp'
  <=. Fin.Float.Dist.linf binned_gaussian binned_exp'

(* Convergence test *)

let truth = [("a", 0.5); ("b", 0.001); ("c", 1. -. (0.5 +. 0.001))]

let truth_fin_prb =
  Fin.Float.probability
    (module Basic_impl.Free_module.Float_valued.String)
    truth

let categorical = Gen.categorical truth

let convergent_sequence dist =
  ListLabels.map
    [1_000; 10_000; 100_000; 500_000; 1_000_000]
    ~f:(fun nsamples ->
      let emp = Emp.of_generative ~nsamples categorical rng_state in
      let normalized =
        Fin.Float.normalize
        @@ Fin.Float.counts_of_empirical
             (module Basic_impl.Free_module.Float_valued.String)
             emp
      in
      dist (Fin.as_measure normalized) (Fin.as_measure truth_fin_prb))

let make_conv_test distname f =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:(Printf.sprintf "%s: convergence" distname)
       ~count:1
       QCheck.unit
  @@ fun () ->
  let dists = f () in
  let fst = List.hd dists in
  let lst = List.hd (List.rev dists) in
  lst /. fst <=. 0.3

let () = make_conv_test "kl" (fun () -> convergent_sequence Fin.Float.Dist.kl)

let () =
  make_conv_test "l1" (fun () -> convergent_sequence (Fin.Float.Dist.lp ~p:1.))

let () =
  make_conv_test "l2" (fun () -> convergent_sequence (Fin.Float.Dist.lp ~p:2.))

let () =
  make_conv_test "linf" (fun () -> convergent_sequence Fin.Float.Dist.linf)
