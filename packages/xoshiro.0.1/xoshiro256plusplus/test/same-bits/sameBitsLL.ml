module Pure = Xoshiro256plusplus_pure.LowLevel
module Bindings = Xoshiro256plusplus_bindings.LowLevel

let state = [| 1L; 2L; 3L; 4L |]

let p_state = Pure.of_int64_array state
let b_state = Bindings.of_int64_array state

let test_case =
  LibSameBits.make_test_case
    ~name:"next"
    ~pp:(fun fmt -> Format.fprintf fmt "0x%Lx")
    "pure" (fun () -> Pure.next p_state)
    "bindings" (fun () -> Bindings.next b_state)

let () = LibSameBits.print_header ()

let () =
  Format.printf "basic test:@.";
  LibSameBits.run_test_case test_case;
  Format.printf "@."

let () =
  Format.printf "after jump:@.";
  Pure.jump p_state;
  Bindings.jump b_state;
  LibSameBits.run_test_case test_case;
  Format.printf "@."

let () =
  Format.printf "after long jump:@.";
  Pure.long_jump p_state;
  Bindings.long_jump b_state;
  LibSameBits.run_test_case test_case;
  Format.printf "@."
