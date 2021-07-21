
(**************************************************************************)
(* Utilities *)

let draw x y =
(*   if Sys.argv.(3) = "dot" then *)
(*     Graphics.plot (truncate x) (truncate y) *)
(*   else *)
    Graphics.lineto (truncate x) (truncate y)

open Data
let get_float var sol =  
  match (try List.assoc var sol with Not_found -> assert false) 
  with (F f) -> f | _ -> 
    Printf.printf "*** error when parsing float\n";
    flush stdout;
    assert false
 
let get_bool var sol =  
  match (try List.assoc var sol with Not_found -> assert false) 
  with (B b) -> b | _ -> 
    Printf.printf "*** error when parsing bool\n";
    flush stdout;
    assert false


(**************************************************************************)
let _ = 
  Graphics.open_graph " ";  Graphics.clear_graph ();  
  Graphics.set_window_title "The Crazy Rabbit travel - test";  
  Graphics.moveto 100 100
(*   ignore (Graphics.read_key ()) *)
;;

(**************************************************************************)

let init  _ = ()
let kill  _ = ()
let reset  _ = ()
let ss _i = ()
let rs _i = ()
type var_type = string

let (inputs :(string * Data.t) list) = [
  "x", Data.Real;  
  "y", Data.Real;  
  "p1x", Data.Real;  
  "p1y", Data.Real;  
  "p2x", Data.Real;  
  "p2y", Data.Real;  
  "p3x", Data.Real;  
  "p3y", Data.Real;  
  "p4x", Data.Real;  
  "p4y", Data.Real;
  "freeze", Data.Bool
]

let mems_i = []

let mems_o = [
  "x_min", Data.F 0.0;  "x_max", Data.F 100.0;  
  "y_min", Data.F 0.0;  "y_max", Data.F 100.0
]
  
let (outputs :(string * Data.t) list) = 
  [
    "x_min", Data.Real;  "x_max", Data.Real;  
    "y_min", Data.Real;  "y_max", Data.Real 
  ]

let cross_product(ux,uy,vx,vy)  = (ux*.vy-.uy*.vx);;


let image = ref (Graphics.get_image 0 0 (Graphics.size_x ()) (Graphics.size_y ()))


let cross_product(ux,uy,vx,vy) = (ux*.vy-.uy*.vx)
let is_inside(px,py,p1x,p1y,p2x,p2y,p3x,p3y,p4x,p4y) = 
  let p1p_x = px-.p1x in
  let p1p_y = py-.p1y in
  let p2p_x = px-.p2x in
  let p2p_y = py-.p2y in
  let p3p_x = px-.p3x in
  let p3p_y = py-.p3y in
  let p4p_x = px-.p4x in
  let p4p_y = py-.p4y in
  let p2p1_x = p1x-.p2x in
  let p2p1_y = p1y-.p2y in
  let p1p4_x = p4x-.p1x in
  let p1p4_y = p4y-.p1y in
  let p4p3_x = p3x-.p4x in
  let p4p3_y = p3y-.p4y in
  let p3p2_x = p2x-.p3x in
  let p3p2_y = p2y-.p3y in
    cross_product(p2p1_x, p2p1_y, p2p_x, p2p_y) < 0.0 &&
    cross_product(p1p4_x, p1p4_y, p1p_x, p1p_y) < 0.0 &&
    cross_product(p4p3_x, p4p3_y, p4p_x, p4p_y) < 0.0 &&
    cross_product(p3p2_x, p3p2_y, p3p_x, p3p_y) < 0.0   
        
let (step :  Data.subst list -> Data.subst list)=
  fun rabbit_outs -> 
    let x_min = (F (float_of_int 0))
    and x_max = (F (float_of_int (Graphics.size_x ())))
    and y_min = (F (float_of_int 0))
    and y_max = (F (float_of_int (Graphics.size_y ()))) in
    let bounds = [("x_min", x_min);("x_max",x_max);("y_min",y_min);("y_max",y_max)] in 


    (* Drawing the new point onto the Graphics window *)
    let x = get_float "x" rabbit_outs
    and y = get_float "y" rabbit_outs
    and p1x = get_float "p1x" rabbit_outs
    and p2x = get_float "p2x" rabbit_outs
    and p3x = get_float "p3x" rabbit_outs
    and p4x = get_float "p4x" rabbit_outs
    and p1y = get_float "p1y" rabbit_outs
    and p2y = get_float "p2y" rabbit_outs
    and p3y = get_float "p3y" rabbit_outs
    and p4y = get_float "p4y" rabbit_outs
    and freeze = get_bool "freeze" rabbit_outs
    in
      if 
        (is_inside(x,y,p1x,p1y,p2x,p2y,p3x,p3y,p4x,p4y))
      then
        (
          (* ignore (Graphics.read_key ()) *)
        );

    let p1xI = truncate p1x
    and p1yI = truncate p1y
    and p2xI = truncate p2x
    and p2yI = truncate p2y
    and p3xI = truncate p3x
    and p3yI = truncate p3y
    and p4xI = truncate p4x
    and p4yI = truncate p4y
    in
    let point_list = [(p1xI,p1yI);(p2xI,p2yI);(p3xI,p3yI);(p4xI,p4yI)] in 
    let poly = Array.of_list point_list in

          

      Graphics.draw_image !image 0 0;
	   draw x y; 
      image := Graphics.get_image 0 0 (Graphics.size_x ()) (Graphics.size_y ());
      if freeze then (
        Graphics.set_color (Graphics.rgb 255 0 100);
	     Graphics.fill_poly poly
      )
      else (
        Graphics.set_color (Graphics.rgb 0 0 0);
	     Graphics.draw_poly poly
      );      
      bounds

open RdbgPlugin

let plugin = {
  id="ze rabbit plugin";
  inputs = inputs;
  outputs= outputs;
  reset= reset;
  save_state = ss;
  restore_state = rs;
  kill= kill;
  init_inputs=mems_i;
  init_outputs=mems_o;
  step= step;     
  step_dbg=(fun _ _ -> assert false); 
}

let _ =   
  OcamlRM.reg_plugin "rabbit.cmxs" plugin;


