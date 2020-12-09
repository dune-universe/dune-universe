module BLSFr = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module BenchmarkBLSFr = Ff_bench.MakeBench (BLSFr)

let () =
  let commands = List.concat [BenchmarkBLSFr.get_benches "Fr"] in
  Core.Command.run (Core_bench.Bench.make_command commands)
