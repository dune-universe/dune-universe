open Interval_intel

(* Standard Griewank function without rotation matrix, very easy to maximize *)
let dim = 7
let highbound = 600.0
let lowbound  = -. 400.0
let precisionx = 0.01
let precisionfx = 0.0001

let start_inter =
  Array.init dim (function _i -> {low=lowbound;high=highbound})

let griewank_x v =
  let s1 = ref 0.0 and s2 = ref 1.0 in
  for i= 0 to (Array.length v)-1 do
    s1:= !s1+. v.(i)*. v.(i);
    s2:= !s2*. (cos (v.(i)/. (sqrt (float (i+1)))))
  done;
  let sum = !s1/. 4000.0 -. !s2 +.1. in
  1./.(1.+. sum)

let griewank_X vec =
  let s1 = ref I.zero and s2 = ref I.one in
  for i = 0 to Array.length vec - 1 do
    s1 := I.(!s1 + vec.(i)**2);
    s2 := I.(!s2 * cos(vec.(i) / (sqrt (of_int U.(i+1)))))
  done;
  let sum = I.((!s1 /. 4000. - !s2) +. 1.) in
  I.(inv (sum +. 1.))

let _ =
  let (int,fint,p,pv)=
    B_and_b.branch_and_bound griewank_x griewank_X start_inter
      precisionx precisionfx in
  I.Arr.pr stdout int; print_string " = ";
  I.pr stdout fint; print_newline ();
  Array.iter (function x -> Printf.printf "%f " x) p;
  Printf.printf " = %f\n%!" pv
