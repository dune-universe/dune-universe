open Core
open Bigint

let%bench_fun "random" =
  let state = Random.State.make [| 1 ; 2 ; 3 |] in
  let range = shift_left one 10_000 in
  fun () -> random ~state range

let%bench_module "vs. Big_int" =
  (module struct
    let elt_self i = pow (of_int_exn 1_000_000_000) (of_int_exn (Int.succ i))
    let elt_other i = Big_int.power_int_positive_int 1_000_000_000 (Int.succ i)

    let count = 4

    let array_self = Array.init count ~f:elt_self
    let array_other = Array.init count ~f:elt_other

    let%bench "plus_self" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (array_self.(i) + array_self.(j) : t);
        done;
      done

    let%bench "plus_other" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (Big_int.add_big_int array_other.(i) array_other.(j) : Big_int.big_int);
        done;
      done

    let%bench "mult_self" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (array_self.(i) * array_self.(j) : t);
        done;
      done

    let%bench "mult_other" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (Big_int.mult_big_int array_other.(i) array_other.(j) : Big_int.big_int);
        done;
      done

  end)

let%bench_module "binio" =
  (module struct
    let small_number = Bigint.of_string "12"
    let medium_number = Bigint.of_int32 Int32.max_value
    let large_number = Bigint.of_string "1234567890123456789012345678901234567890"

    let [@inline always] run_poke b bin_writer_t =
      Iobuf.Poke.bin_prot bin_writer_t b ~pos:0 small_number;
      Iobuf.Poke.bin_prot bin_writer_t b ~pos:0 medium_number;
      Iobuf.Poke.bin_prot bin_writer_t b ~pos:0 large_number;
      ()

    let%bench_fun "poke V1" =
      let b = Iobuf.create ~len:100 in
      fun () ->
        run_poke b Bigint.Stable.V1.bin_writer_t

    let%bench_fun "poke V2" =
      let b = Iobuf.create ~len:100 in
      fun () ->
        run_poke b Bigint.Stable.V2.bin_writer_t

    let create_peek bin_writer_t =
      let b_small =
        Iobuf.of_string (Bin_prot.Writer.to_string bin_writer_t small_number)
      in
      let b_medium =
        Iobuf.of_string (Bin_prot.Writer.to_string bin_writer_t medium_number)
      in
      let b_large =
        Iobuf.of_string (Bin_prot.Writer.to_string bin_writer_t large_number)
      in
      b_small, b_medium, b_large

    let [@inline always] run_peek b_small b_medium b_large bin_reader_t =
      let (_ : Bigint.t) =
        Sys.opaque_identity (Iobuf.Peek.bin_prot bin_reader_t b_small ~pos:0)
      in
      let (_ : Bigint.t) =
        Sys.opaque_identity (Iobuf.Peek.bin_prot bin_reader_t b_medium ~pos:0)
      in
      let (_ : Bigint.t) =
        Sys.opaque_identity (Iobuf.Peek.bin_prot bin_reader_t b_large ~pos:0)
      in
      ()

    let%bench_fun "peek V1" =
      let (b_small, b_medium, b_large) = create_peek Bigint.Stable.V1.bin_writer_t in
      fun () ->
        run_peek b_small b_medium b_large Bigint.Stable.V1.bin_reader_t

    let%bench_fun "peek V2" =
      let (b_small, b_medium, b_large) = create_peek Bigint.Stable.V2.bin_writer_t in
      fun () ->
        run_peek b_small b_medium b_large Bigint.Stable.V2.bin_reader_t
  end)
