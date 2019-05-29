
let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  Random.self_init ();
  let n = 10_000_000 in
  let a1 = Array.init n (fun _i ->
      Random.float 1.0
    ) in
  let plain_fn = Filename.temp_file "lz4_chans_test_" ".bin" in
  Log.info "plain_fn: %s" plain_fn;
  (* plain output-rate *)
  let start = ref (Unix.gettimeofday ()) in
  with_out_file plain_fn (fun out ->
      Array.iter (fun x ->
          Marshal.(to_channel out x [No_sharing])
        ) a1
    );
  let stop = ref (Unix.gettimeofday ()) in
  let plain_output_rate = (float n) /. (!stop -. !start) in
  Log.info "plain output: %.2f float/s" plain_output_rate;
  (* plain input-rate *)
  let i = ref 0 in
  let a2 = Array.create_float n in
  start := Unix.gettimeofday ();
  with_in_file plain_fn (fun input ->
      try
        while true do
          Array.unsafe_set a2 !i (Marshal.from_channel input: float);
          incr i
        done
      with End_of_file -> ()
    );
  stop := Unix.gettimeofday ();
  let plain_input_rate = (float n) /. (!stop -. !start) in
  Log.info "plain input: %.2f float/s" plain_input_rate;
  assert(a1 = a2);
  (* lz4 output-rate *)
  let lz4_fn = Filename.temp_file "lz4_chans_test_" ".bin.lz4" in
  Log.info "lz4_fn: %s" lz4_fn;
  start := Unix.gettimeofday ();
  Lz4_chans.with_out_file lz4_fn (fun out ->
      Array.iter (fun x ->
          Marshal.(to_channel out x [No_sharing])
        ) a1
    );
  stop := Unix.gettimeofday ();
  let lz4_output_rate = (float n) /. (!stop -. !start) in
  Log.info "lz4 output: %.2f float/s; eficiency: %.2f"
    lz4_output_rate (lz4_output_rate /. plain_output_rate);
  (* lz4 input-rate *)
  let i = ref 0 in
  let a3 = Array.create_float n in
  start := Unix.gettimeofday ();
  Lz4_chans.with_in_file lz4_fn (fun input ->
      try
        while true do
          Array.unsafe_set a3 !i (Marshal.from_channel input: float);
          incr i
        done
      with End_of_file -> ()
    );
  stop := Unix.gettimeofday ();
  let lz4_input_rate = (float n) /. (!stop -. !start) in
  Log.info "lz4 input: %.2f float/s; efficiency: %.2f"
    lz4_input_rate (lz4_input_rate /. plain_input_rate);
  assert(a1 = a3)

let () = main ()
