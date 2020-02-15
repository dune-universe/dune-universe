(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

let _global_name_counter = ref 0

let generate_suffix () =
  _global_name_counter := !_global_name_counter + 1;
  !_global_name_counter
