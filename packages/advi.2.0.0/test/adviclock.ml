(***********************************************************************)
(*                               Clock                                 *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Camltk

(** Diameter of the clock. *)
let diametre = ref 200;;

(** Background color. *)
let background_color = ref "White";;

(** Total major time for 360 degrees (in miliseconds) *)
let base_major = ref 3600000.0;;

(** Total minor time for 360 degrees (in miliseconds) *)
let base_minor = ref 60000.0;;

(** Color of the major hand. *)
let color_major = ref "FireBrick";;

(** Color of the minor hand. *)
let color_minor = ref "Blue";;

(** Width of the major hand. *)
let width_major = ref 2;;

(** Width of the major hand. *)
let width_minor = ref 1;;

(** Width of the circle. *)
let width_circle = ref 0;;

(** Refresh frequency (in milliseconds). *)
let frequency = ref 1000.0;;

(** Reverse or not. *)
let reverse = ref false;;

(** Total time, in theory :-), in milliseconds. *)
let total_time = ref 1800000.0;;

(** Color for the authorized time interval. *)
let color_ok = ref "PaleGreen2";;

(** Color for the done time interval. *)
let color_done = ref "Turquoise3";;

(** Color for the 'you're late' time interval. *)
let color_overtime = ref "Red";;

(** Predefined times (in milliseconds). *)
let times = ref ([] : float list);;

(** The filename of the interface file. *)
let filename = ref (None : string option);;

let pi = 4.0 *. atan 1.0;;
let base_x = 2;;
let base_y = 2;;

module Args = struct

  let options = [
    "-d", Arg.Int (fun d -> diametre := d),
      "<d>  Use <d> as the diameter of the clock";
    "-bg", Arg.String (fun c -> background_color := c),
      "<color>  Set the background color";
    "-wc", Arg.Int (fun n -> width_circle := n),
      "<n>  Use <n> as the width of the circle line";
    "-W", Arg.Int (fun n -> width_major := n),
      "<n>  Use <n> as the width of the major hand";
    "-w", Arg.Int (fun n -> width_minor := n),
      "<n>  Use <n> as the width of the minor hand";
    "-C", Arg.String (fun s -> color_major := s),
      "<color>  Set the color name used for the major hand";
    "-c", Arg.String (fun s -> color_minor := s),
      "<color>  Set the color name used for the minor hand";
    "-B", Arg.Float (fun f -> base_major := f),
      "<base>  Set the value for 360° of the major hand \
       (float, in milliseconds)";
    "-b", Arg.Float (fun f -> base_minor := f),
      "<base>  Set the value for 360° of the minor hand \
       (float, in milliseconds)";
    "-rev", Arg.Set reverse, " Countdown style";
    "-time", Arg.Float (fun m -> total_time := (m *. 60000.)),
      "<time>  Theoretical total time in minutes";
    "-f", Arg.Float (fun f -> frequency := f),
      "<f>  Set the refresh frequency (in milliseconds, default is 1000.0)";
    "-file", Arg.String (fun f -> filename := Some f),
      "<file>  Use <file> to read the current slide number \
       when receiving SIGUSR1";
    "-done", Arg.String (fun s -> color_done := s),
      "<color>  Set the color name used for the done sectors";
    "-ok", Arg.String (fun s -> color_ok := s),
      "<color>  Set the color name used for the sectors not done yet";
    "-over", Arg.String (fun s -> color_overtime := s),
      "<color>  Set the color name used for the overtime";
  ]

  let parse () = Arg.parse
      (keywords @ options)
      (fun s ->
        try times := !times @ [ float_of_string s *. 1000.0] with
        | Invalid_argument err ->
            prerr_endline
              (Printf.sprintf "Invalid_argument %s : %s" err s)
      )
      (Printf.sprintf
         "usage : %s [options] t1 t2 ... (in seconds)\nOptions are:"
         Sys.argv.(0))
end;;

let opencamltk () =
  Args.parse ();
  opentk ();;

type clock = {
    widget : Widget.widget; 
    mutable major : tagOrId;
    mutable minor : tagOrId;
    mutable major_elapsed : float;
    mutable minor_elapsed : float;
  };;

let compute_coord rayon base_time time  =
  let angle = (time /. base_time) *. (2. *. pi) in
  let x = cos angle *. float_of_int rayon in
  let y = sin angle *. float_of_int rayon in
  (int_of_float y, - (int_of_float x));;

let options_circle () =
  [Width (Pixels !width_circle)];;
let options_major () =
  [Width (Pixels !width_major); FillColor (NamedColor !color_major)];;
let options_minor () =
  [Width (Pixels !width_minor); FillColor (NamedColor !color_minor)];;

let draw_major c =
  Canvas.delete c.widget [c.major];
  let d = !diametre in
  let r = d / 2 in
  let (x,y) = compute_coord r !base_major c.major_elapsed in
  c.major <-
    Canvas.create_line c.widget
      [ Pixels (base_x + r); Pixels (base_y + r);
	Pixels (base_x + r + x); Pixels(base_y + r + y)]
      (options_major ());;

let draw_minor c =
  Canvas.delete c.widget [c.minor];
  let d = !diametre in
  let r = d / 2 in
  let (x,y) = compute_coord r !base_minor c.minor_elapsed in
  c.minor <-
    Canvas.create_line c.widget
      [ Pixels (base_x + r); Pixels (base_y + r);
	Pixels (r + x); Pixels(r + y)]
      (options_minor ());;


let create_clock () =
  let top = opencamltk () in
  Toplevel.configure top [Background (NamedColor !background_color) ] ;
  let d = !diametre in
  let r = d / 2 in
  let w = Canvas.create top [Background  (NamedColor !background_color)] in
  pack [w] [Expand true; Fill Fill_Both ];
  if !width_circle > 0 then ignore (
    Canvas.create_oval w
      (Pixels base_x) (Pixels base_y)
      (Pixels (base_x + d)) (Pixels (base_y + d))
      (options_circle ()));
  let clock =
    {
      widget = w;
      major =
        Canvas.create_line w
          [ Pixels (base_x + r); Pixels (base_y + r);
	    Pixels (base_x + r); Pixels base_y] (options_major ());
      minor =
        Canvas.create_line w
          [ Pixels (base_x + r); Pixels (base_y + r);
            Pixels (base_x + r); Pixels base_y] (options_minor ());
      major_elapsed = (if !reverse then !total_time else 0.0);
      minor_elapsed = 0.0;
    }
  in
  draw_major clock;
  draw_minor clock;
  clock;;


let draw_overtime_arc c old_major =
  let d = !diametre in
  let inf = if !reverse then 0.0 else -. !total_time in
  let angle_base = 360.0 in
  let angle_inf = (inf /. !base_major) *. angle_base +. 90.0 in
  let angle_sup =
    if !reverse then
      -. ((c.major_elapsed /. !base_major) *. angle_base)
    else
      -. (((c.major_elapsed -. !total_time) /. !base_major) *. angle_base)
      (* -. ((c.major_elapsed /. !base_major) *. angle_base +. inf) *)
  in
  (* prerr_endline
      (Printf.sprintf "overtime inf=%f sup=%f" angle_inf angle_sup); *)
  let arc =
    Canvas.create_arc c.widget
      (Pixels base_x) (Pixels base_y)
      (Pixels (base_x + d)) (Pixels (base_y + d))
      [ Start angle_inf; Extent angle_sup;
	FillColor (NamedColor !color_overtime);
	ArcStyle PieSlice; Outline (NamedColor !color_overtime)]
  in
  ();;


let create_secteurs clock times =
  let rec iter acc = function
  | [] -> []
  | t :: q ->
      let d = !diametre in
      let angle_base = 360.0 in
      let inf = if !reverse then -. !total_time +. acc else -. acc in
      let angle_inf = (inf /. !base_major) *. angle_base +. 90.0 in
      let new_acc = acc +. t in
      if !total_time < (-. new_acc) then
        (prerr_endline
           "The sum of the given times is greater than the total time.";
         [])
      else
        (
         let angle_sup_pre = (-. t /. !base_major) *. angle_base in
         let angle_sup =
           if !reverse then -. angle_sup_pre else angle_sup_pre in
         (*Printf.printf "overtime inf=%f sup%f new_acc=%f"
                       angle_inf angle_sup new_acc;  print_newline ();*)
         let arc =
           Canvas.create_arc clock.widget (Pixels base_x) (Pixels base_y)
             (Pixels (base_x + d)) (Pixels (base_y + d))
             [Start angle_inf; Extent angle_sup;
              FillColor (NamedColor !color_ok);
              ArcStyle PieSlice; Outline (NamedColor "Black")]
         in
         arc :: iter new_acc q
        )
  in
  iter 0.0 times;;

let clock = create_clock ();;

let secteurs = create_secteurs clock !times;;
let current_secteur = ref 0;;
let secteurs_done = ref ([] : int list);;
let last_time = ref (Unix.time ());;


let rec set_time freq c () =
  let new_time = Unix.time () in
  let gain = (new_time -. !last_time) *. 1000.0 in
  c.minor_elapsed <-
    (if !reverse then c.minor_elapsed -. gain else c.minor_elapsed +. gain);
  let old_major = c.major_elapsed in
  c.major_elapsed <-
    (if !reverse then c.major_elapsed -. gain else c.major_elapsed +. gain);

  if c.major_elapsed > !total_time || c.major_elapsed < 0.0
  then draw_overtime_arc c old_major;

  draw_minor c;
  draw_major c;
  last_time := new_time;
  ignore (Timer.add (int_of_float freq) (set_time freq c));;

let handle_signal clock =
  let must_colorize =
    match !filename with
    | None -> true
    | Some f ->
        try
          let ic = open_in f in
          let n = int_of_string (input_line ic) in
          close_in ic;
          let b = List.mem n !secteurs_done in
          if not b then secteurs_done := n :: !secteurs_done;
          not b
        with
        | Sys_error s -> prerr_endline s; false
        | _ -> false
  in
  if must_colorize then
    try
      let sec = List.nth secteurs !current_secteur in
      incr current_secteur;
      Canvas.configure_arc clock.widget sec
        [FillColor (NamedColor !color_done)]
    with _ -> ();;

Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> handle_signal clock));;

ignore (Timer.add (int_of_float !frequency) (set_time !frequency clock));;

mainLoop ();;
