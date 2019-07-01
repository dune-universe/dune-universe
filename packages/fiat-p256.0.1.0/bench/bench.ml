let crypto_random_bytes n =
  let ic = Pervasives.open_in_bin "/dev/urandom" in
  let s = Pervasives.really_input_string ic n in
  close_in ic; s

let of_some = function
  | None ->
      assert false
  | Some x ->
      x

let random_private_key () =
  crypto_random_bytes 32
  |> Hex.of_string
  |> Fiat_p256.scalar_of_hex
  |> of_some

let bench_dh () =
  let scalar = random_private_key () in
  let point = Fiat_p256.public @@ random_private_key () in
  let run () : Cstruct.t = Fiat_p256.dh ~scalar ~point in
  Benchmark.throughputN 1 [("P-256", run, ())]

let () =
  let open Benchmark.Tree in
  register @@ "Fiat" @>>> ["dh" @> lazy (bench_dh ())]

let () = Benchmark.Tree.run_global ()
