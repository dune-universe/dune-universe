open Mlpost

let emit, dump =
  let base_name = Filename.chop_extension Sys.argv.(0) in
  let emit =
    let cnt = ref 0 in
    fun f ->
      incr cnt;
      Metapost.emit (Printf.sprintf "%s-%0.4d" base_name !cnt) f
  in
  let dump =
    let base_name = Filename.chop_extension Sys.argv.(0) in
    fun () -> Metapost.dump base_name
  in
  (emit, dump)
