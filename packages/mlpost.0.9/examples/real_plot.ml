open Mlpost
open Real_plot
open Printf

(*parse <<togglescript>> *)

(*parse <<real_plot1 *)

let real_plot1 =
  let graph =
    [
      curve_opt (fun f -> if f < 0. then None else Some (sqrt f)) "sqrt";
      curve ceil "ceil";
      curve floor "floor";
      curve sin "sin";
      curve_l
        [
          (fun f -> if f <= 0. then None else Some (1. /. f));
          (fun f -> if f >= 0. then None else Some (1. /. f));
        ]
        "$\\frac{1}{x}$";
    ]
  in
  draw
    ~label:(fun x -> x)
    ~ymax:5. ~ymin:(-5.) ~xmin:(-5.) ~xmax:5. ~pitch:0.01 ~width:(Num.cm 6.)
    ~height:(Num.cm 4.) graph

(*parse >> <<real_plot2 *)
let real_plot2 =
  let curves =
    List.map
      (fun i -> curve (fun f -> f ** (1. /. float_of_int i)) i)
      [ 2; 3; 4; 8; 12; 15 ]
  in
  draw
    ~label:(sprintf "$\\sqrt[%i]{x}$")
    ~xmin:0. ~xmax:5. ~pitch:0.01 ~width:(Num.cm 6.) ~height:(Num.cm 4.) curves

(*parse >> *)

let () =
  List.iter
    (fun (name, fig) -> Metapost.emit name (Picture.scale (Num.bp 3.) fig))
    [ ("real_plot1", real_plot1); ("real_plot2", real_plot2) ]
