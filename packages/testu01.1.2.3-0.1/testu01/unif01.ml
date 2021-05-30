type gen

external create_extern_gen_bits : string -> (unit -> int) -> gen = "caml_unif01_CreateExternGenBits"

let create_extern_gen_bits name bits =
  (* Add a test that the [bits] function does not provide more than 30, in which
     case they would be lost. Doing the test every time does not have any real
     impact on the performances. *)
  create_extern_gen_bits name (fun () ->
      let b = bits () in
      if b lsr 30 <> 0 then
        invalid_arg "more than 30 bits received";
      b)

external create_extern_gen_int32 : string -> (unit -> int32) -> gen = "caml_unif01_CreateExternGenInt32"

external create_extern_gen_01 : string -> (unit -> float) -> gen = "caml_unif01_CreateExternGen01"

let create_extern_gen_01 name bits =
  (* Add a test that the [bits] function provides a float in [0, 1). The
     contrary could provoke segmentation faults in certain tests. Doing the test
     every time does not have any real impact on the performances. *)
  create_extern_gen_01 name (fun () ->
      let b = bits () in
      if b < 0. || b >= 1. then
        invalid_arg "float outside of [0, 1) received";
      b)
