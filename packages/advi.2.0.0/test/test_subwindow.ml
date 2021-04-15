let message s = print_string s; print_newline (); flush stdout;;

open Graphics;;

(* Useless to sleep: synchronize gives an active sleep!

let arm_timer t =
  Unix.setitimer Unix.ITIMER_REAL {Unix.it_interval = 0.0; Unix.it_value = t}
;;

let sleep t = let _ = arm_timer t in ();;
*)

let wait () =
 print_string "Press return\n"; flush stdout;
 let _ = input_line stdin in ();;

open_graph "";;

fill_rect 0 0 400 500;;

open GraphicsX11;;
open GraphicsY11;;

let s1 = open_subwindow ~x:0 ~y:0 ~width:20 ~height:30;;

wait ();;

let s2 = open_subwindow ~x:100 ~y:10 ~width:30 ~height:60;;

wait ();;

set_color green; fill_rect 90 0 80 10;;

set_color red; fill_rect 100 10 130 60;;

set_color blue;;

wait ();;

let s3 = open_subwindow ~x:100 ~y:10 ~width:130 ~height:60;;

wait ();;

moveto 350 100;;

lineto 0 0;;

wait ();;

move_subwindow s3 100 100;;

wait ();;

move_subwindow s2 200 200;;

wait ();;

for i = 0 to 200 do
 move_subwindow s3 (100 + i) (100 + i);
 Graphics.synchronize ();
 (* sleep 1.0;*)
done;;

move_subwindow s3 100 10;;

wait ();;

move_subwindow s1 20 30;;

wait ();;

let s4 = open_subwindow ~x:0 ~y:0 ~width:20 ~height:30;;

wait ();;

move_subwindow s1 0 0;;

wait ();;

exit 0;;
