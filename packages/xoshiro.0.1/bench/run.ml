module type BASIC = MakeRandom.Sig.Basic
module type FULL = MakeRandom.Sig.Full

type gen = B of (module BASIC) | F of (module FULL)

let gens : (string * gen) list = [
  "stdlib",                  F (module Random);
  "splitmix64 (pure)",       F (module Splitmix64_pure);
  "xoshiro256++ (pure)",     F (module Xoshiro256plusplus_pure);
  "xoshiro256++ (bindings)", F (module Xoshiro256plusplus_bindings);
]

type basic_gen_to_test = BGTT : ((module BASIC) -> unit -> 'a) -> basic_gen_to_test
type  full_gen_to_test = FGTT : ((module FULL) -> unit -> 'a) ->  full_gen_to_test

let tests : (string * basic_gen_to_test * full_gen_to_test) list = [
  "bits",
  BGTT (fun (module G : BASIC) () -> G.bits ()),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.bits state);

  "int",
  BGTT (fun (module G : BASIC) () -> G.int (1 lsl 30 - 1)),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.int state (1 lsl 30 - 1));

  "int32",
  BGTT (fun (module G : BASIC) () -> G.int32 Int32.max_int),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.int32 state Int32.max_int);

  "int64",
  BGTT (fun (module G : BASIC) () -> G.int64 Int64.max_int),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.int64 state Int64.max_int);

  "nativeint",
  BGTT (fun (module G : BASIC) () -> G.nativeint Nativeint.max_int),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.nativeint state Nativeint.max_int);

  "float",
  BGTT (fun (module G : BASIC) () -> G.float 1.),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.float state 1.);

  "bool",
  BGTT (fun (module G : BASIC) () -> G.bool ()),
  FGTT (fun (module G : FULL) -> let state = G.get_state () in fun () -> G.State.bool state);
]

let print_header test_name =
  let free = 70 - (12 + String.length test_name) in
  let right = free / 2 in
  let left = free - right in
  Format.printf "\n## %s [ %s ] %s ##@."
    (String.make left '=') test_name (String.make right '=')

let run_test basic_gen_to_test full_gen_to_test =
  Core.Command.run @@ Core_bench.Bench.make_command (
    List.concat_map
      (fun (name, gen) ->
         match gen with
         | B (module G : BASIC) ->
           [ Core_bench.Bench.Test.create ~name (basic_gen_to_test (module G : BASIC)) ]
         | F (module G : FULL) ->
           [ Core_bench.Bench.Test.create ~name (basic_gen_to_test (module G : BASIC)) ;
             Core_bench.Bench.Test.create ~name:(name ^ " [state]") (full_gen_to_test (module G : FULL)) ])
      gens
  )

let () =
  List.iter
    (fun (test_name, BGTT basic_gen_to_test, FGTT full_gen_to_test) ->
       print_header test_name;
       run_test basic_gen_to_test full_gen_to_test)
    tests
