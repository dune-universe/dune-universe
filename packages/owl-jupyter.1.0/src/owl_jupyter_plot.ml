(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2018 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

include Owl_plplot.Plot


let output h =
  let new_output = Filename.temp_file "plot_" ".png" in
  (* set the old_output to new_output if it is not set *)
  let old_output =
    if get_output h <> "" then get_output h
    else new_output
  in
  (* plot then reset the output *)
  set_output h new_output;
  output h;
  set_output h old_output;
  (* render in the notebook *)
  Jupyter_notebook.display_file ~base64:true "image/png" new_output
  |> ignore
