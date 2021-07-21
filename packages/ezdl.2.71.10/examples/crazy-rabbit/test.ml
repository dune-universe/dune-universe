open Event;;
open Ldbg;;
open Ldbg_utils;;


add_rp "sut:ocaml:rabbit.cmxs:";;
add_rp "env:lutin:rabbit.lut:-main:rabbit:-L:libm.so:-loc";;

stl 100;;
set_sim2chro false;;
set_gnuplot false;;

let rec n e i = 
  if i = 0 then (print e; e) 
  else 
    let e = next e in 
      if !show_trace then print e; 
      n e (i-1);;
