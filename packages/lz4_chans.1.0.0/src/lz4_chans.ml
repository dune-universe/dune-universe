
(* open/close binary channels, with LZ4-compression happening in the background,
   using a separate process and a named FIFO/pipe *)

module Ht = Hashtbl

(* register of created FIFO filenames *)
let out_chan_to_fn = Ht.create 11

let run_command ?(debug = false) (cmd: string): unit =
  if debug then Log.info "run_command: %s" cmd;
  match Unix.system cmd with
  | Unix.WSIGNALED _ -> (Log.fatal "run_command: signaled: %s" cmd; exit 1)
  | Unix.WSTOPPED _ -> (Log.fatal "run_command: stopped: %s" cmd; exit 1)
  | Unix.WEXITED i when i <> 0 ->
    (Log.fatal "run_command: exit %d: %s" i cmd; exit 1)
  | Unix.WEXITED _ (* i = 0 then *) -> ()

let lz4_open_out_bin fn =
  let fifo_fn = fn ^ ".out.fifo" in
  Unix.mkfifo fifo_fn 0o600;
  (* launch background compression process reading from the FIFO *)
  run_command (Printf.sprintf "lz4 -f -z -1 -q %s %s &" fifo_fn fn);
  let out_chan = Pervasives.open_out_bin fifo_fn in
  assert(not (Ht.mem out_chan_to_fn out_chan));
  Ht.add out_chan_to_fn out_chan fifo_fn;
  out_chan

let lz4_close_out out_chan =
  let fifo_fn = Ht.find out_chan_to_fn out_chan in
  Ht.remove out_chan_to_fn out_chan;
  Pervasives.close_out out_chan;
  Sys.remove fifo_fn

let lz4_with_out_file fn f =
  let output = lz4_open_out_bin fn in
  let res = f output in
  lz4_close_out output;
  res

let in_chan_to_fn = Ht.create 11

(* WARNING: this works but is way too slow.
 * Perhaps, making the FIFO size bigger (via a fcntl call) would work faster *)
let lz4_open_in_bin fn =
  let fifo_fn = fn ^ ".in.fifo" in
  Unix.mkfifo fifo_fn 0o600;
  (* launch decompression process *)
  run_command (Printf.sprintf "lz4 -d -q %s > %s &" fn fifo_fn);
  let in_chan = Pervasives.open_in_bin fifo_fn in
  assert(not (Ht.mem in_chan_to_fn in_chan));
  Ht.add in_chan_to_fn in_chan fifo_fn;
  in_chan

let lz4_close_in in_chan =
  let fifo_fn = Ht.find in_chan_to_fn in_chan in
  Ht.remove in_chan_to_fn in_chan;
  Pervasives.close_in in_chan;
  Sys.remove fifo_fn

let lz4_with_in_file fn f =
  let input = lz4_open_in_bin fn in
  let res = f input in
  lz4_close_in input;
  res

(* aliases, for conveniency *)
let open_out_bin = lz4_open_out_bin
let close_out = lz4_close_out
let with_out_file = lz4_with_out_file
let open_in_bin = lz4_open_in_bin
let close_in = lz4_close_in
let with_in_file = lz4_with_in_file
