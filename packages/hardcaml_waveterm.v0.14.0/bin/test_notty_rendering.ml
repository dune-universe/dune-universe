(* Quick test of notty rendering to the terminal *)
open! Import
module Ui = Render.Make (Draw_notty)
module Bits = Hardcaml.Bits

let ctx =
  Render.Static.draw
    ~signals:true
    ~values:true
    ~waves:true
    ~style:Render.Styles.colour_on_black
    ~rows:20
    ~cols:80
    Test_data.(create ~prefix:(fun _ -> "") ~length:20 ~num_signals:10)
;;

let image = Draw_notty.to_image ctx
let () = Notty_unix.output_image image
