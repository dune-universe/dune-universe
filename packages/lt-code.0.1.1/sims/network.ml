open Core

let encode_all_upfront = false

let rounds = 100

let data_block_count = 100

let setups =
  [
    (* make_setup ~systematic:false ~encode_all_upfront ~data_block_count *)
    (* ~max_redundancy:2.00 ~data_block_size:1300 ~data_loss_rate:0.50 ~rounds (); *)
    (* make_setup ~systematic:true ~systematic_scaling_factor:1.0 ~encode_all_upfront ~data_block_count *)
    (* ~max_redundancy:2.00 ~data_block_size:1300 ~data_loss_rate:0.50 ~rounds (); *)
    (* make_setup ~systematic:false ~encode_all_upfront ~data_block_count *)
    (* ~max_redundancy:2.00 ~data_block_size:1300 ~data_loss_rate:0.40 ~rounds (); *)
    (* make_setup ~systematic:true ~systematic_scaling_factor:1.0 ~encode_all_upfront ~data_block_count *)
    (* ~max_redundancy:2.00 ~data_block_size:1300 ~data_loss_rate:0.40 ~rounds (); *)
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.30 ~rounds ();
    make_setup ~systematic:true ~systematic_scaling_factor:2.0
      ~encode_all_upfront ~data_block_count ~max_redundancy:1.00
      ~data_block_size:1300 ~data_loss_rate:0.30 ~rounds ();
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.20 ~rounds ();
    make_setup ~systematic:true ~systematic_scaling_factor:2.0
      ~encode_all_upfront ~data_block_count ~max_redundancy:1.00
      ~data_block_size:1300 ~data_loss_rate:0.20 ~rounds ();
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.10 ~rounds ();
    make_setup ~systematic:true ~systematic_scaling_factor:10.0
      ~encode_all_upfront ~data_block_count ~max_redundancy:1.00
      ~data_block_size:1300 ~data_loss_rate:0.10 ~rounds ();
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.05 ~rounds ();
    make_setup ~systematic:true ~systematic_scaling_factor:20.0
      ~encode_all_upfront ~data_block_count ~max_redundancy:1.00
      ~data_block_size:1300 ~data_loss_rate:0.05 ~rounds ();
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.01 ~rounds ();
    make_setup ~systematic:true ~systematic_scaling_factor:20.0
      ~encode_all_upfront ~data_block_count ~max_redundancy:1.00
      ~data_block_size:1300 ~data_loss_rate:0.01 ~rounds ();
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.005 ~rounds
      ();
    make_setup ~systematic:true ~systematic_scaling_factor:20.0
      ~encode_all_upfront ~data_block_count ~max_redundancy:1.00
      ~data_block_size:1300 ~data_loss_rate:0.005 ~rounds ();
  ]

let () =
  Random.self_init ();
  List.iter run_and_print setups
