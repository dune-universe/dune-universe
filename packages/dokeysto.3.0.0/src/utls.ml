type filename = string

let with_in_file (fn: filename) (f: in_channel -> 'a): 'a =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

(* marshal x to file *)
let save fn x =
  with_out_file fn (fun out ->
      Marshal.to_channel out x [Marshal.No_sharing]
    )

(* unmarshal x from file *)
let restore fn =
  with_in_file fn (fun input ->
      Marshal.from_channel input
    )
