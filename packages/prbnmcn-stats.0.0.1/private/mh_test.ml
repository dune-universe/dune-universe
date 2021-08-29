open Stats

(* Testing MH *)

let r = Random.State.make [| 0x1337; 0x533D |]

(* Construct a finite quotient of a metric space *)
module Partition (M : Basic_intf.Metric) = struct
  type t = { cells : M.t array }

  type cell = { partition : t; pt : int }

  module Cell_ordered = struct
    type t = cell

    let compare c1 c2 = Int.compare c1.pt c2.pt

    let equal c1 c2 = compare c1 c2 = 0

    let hash = Hashtbl.hash

    let pp fmtr (cell : t) =
      match cell with
      | { partition; pt } ->
          let (low, high) =
            if pt = Array.length partition.cells - 1 then
              (partition.cells.(pt - 2), partition.cells.(pt - 1))
            else (partition.cells.(pt), partition.cells.(pt + 1))
          in
          Format.fprintf fmtr "[%a, %a]" M.pp low M.pp high
  end

  module Cell_ordered_map = Map.Make (Cell_ordered)
  module Cell_ordered_vec =
    Basic_impl.Free_module.Make (Cell_ordered) (Basic_impl.Reals.Float)
      (Cell_ordered_map)

  let make (points : M.t array) : t = { cells = points }

  let quotient (partition : t) (p : M.t) : cell =
    let rec loop (i : int) nearest dist =
      if i >= Array.length partition.cells then nearest
      else
        let dist' = M.dist p partition.cells.(i) in
        if Basic_impl.Reals.Float.(dist' < dist) then loop (i + 1) i dist'
        else loop (i + 1) nearest dist
    in
    let index = loop 1 0 (M.dist p partition.cells.(0)) in
    let result = { partition; pt = index } in
    result

  let quotient_kernel partition =
    Fin.Float.kernel
      (module Cell_ordered_vec)
      (fun x -> [(quotient partition x, 1.0)])

  let averaged_density (partition : t) (den : (M.t, float) Stats_intf.fin_mes) :
      (cell, float) Stats_intf.fin_mes =
    Fin.Float.pushforward den (quotient_kernel partition)
end

module MH_test () = struct
  module Reals : Basic_intf.Metric with type t = float = struct
    include Basic_impl.Reals.Float

    let dist (x : t) (y : t) = abs_float (y -. x)
  end

  module Part = Partition (Reals)

  let float_points =
    let pts = Array.init 2000 (fun _ -> -10. +. Random.float 20.0) in
    let npts = Array.map (fun x -> ~-.x) pts in
    let line = Array.concat [npts; pts] in
    Array.sort Stdlib.Float.compare line ;
    line

  let partition = Part.make float_points

  module Test : Mh.MH_parameters with type t = Part.Cell_ordered.t = struct
    include Part.Cell_ordered

    let alpha = 3.0

    (* construct discretized transition matrix *)

    let rng_state = Random.State.make [| 0x1337; 0x533D |]

    let jump x =
      let open Gen.Infix in
      let* delta = Gen.float alpha in
      let* choice = Gen.bool in
      if choice then Gen.return (Part.quotient partition (x +. delta))
      else Gen.return (Part.quotient partition (x -. delta))

    let table =
      let dim = Array.length partition.cells in
      Array.init dim (fun i ->
          Format.printf "\r precomputing table %d/%d%!" (i + 1) dim ;
          let representative = partition.cells.(i) in
          let gen = jump representative in
          let (`Empirical emp) =
            Emp.to_raw_data @@ Emp.of_generative ~nsamples:1000 gen rng_state
          in
          let dist =
            Fin.Float.counts_of_empirical (module Part.Cell_ordered_vec) emp
            |> Fin.Float.normalize
          in
          let (`Probability weights) = Fin.Float.raw_data_probability dist in
          let alias = Gen.categorical weights in
          (dist, alias))

    let proposal (x : t) = snd table.(x.pt)

    let proposal_log_density (x : t) (y : t) =
      let p = fst table.(x.pt) in
      Log_space.of_float (Fin.Float.eval_prb p y)

    (* The target unnormalized density *)
    let weight (c : Part.cell) =
      let open Part in
      let x = partition.cells.(c.pt) in
      (sin x ** 2.0)
      *. ((sin @@ (2.0 *. x)) ** 2.0)
      *. Pdfs.gaussian ~mean:0.0 ~std:1.0 x

    let log_weight c = Log_space.of_float (weight c)
  end

  module Sampler = Mh.Make (Test)

  let _ =
    let mcmc =
      Sampler.mcmc
        ~verbosity:`Trace
        ~initial:(Part.quotient partition 0.0)
        ~burn_in:3000
        r
    in
    let empirical = Emp.of_generative ~nsamples:8000 mcmc r in
    let (`Empirical data) = Emp.to_raw_data empirical in
    let float_data =
      Array.map (fun { Part.pt; _ } -> partition.cells.(pt)) data
    in
    let open Plot in
    let plot  =
      Plot.plot2 ~xaxis:"x" ~yaxis:"freq" ~title:(Some "mcmc result") @@
      [ Plot.Histogram.hist
          ~points:(Seq.map r1 (Array.to_seq float_data))
          ~opts:{ color = None; bins = None; binwidth = Some 0.1 } ()
      ]
    in
  Plot.run_plot ~target:(Plot.png ~png_file:"mcmc.png" ()) ~plot:plot ()
end

module Test = MH_test ()
