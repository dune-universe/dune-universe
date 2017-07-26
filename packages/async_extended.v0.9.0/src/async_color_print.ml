(* Async-friendly version of Core_extended.Std.Color_print *)
open Core
module CP = Core_extended.Std.Color_print
include CP

let print_string = Async.print_string

let bold_printf ?override = ksprintf (fun s -> print_string (CP.bold ?override s))
let underline_printf ?override = ksprintf (fun s -> print_string (CP.underline ?override s))
let inverse_printf ?override = ksprintf (fun s -> print_string (CP.inverse ?override s))

let red_printf ?override     = ksprintf (fun s -> print_string (CP.red ?override s))
let yellow_printf ?override  = ksprintf (fun s -> print_string (CP.yellow ?override s))
let green_printf ?override   = ksprintf (fun s -> print_string (CP.green ?override s))
let blue_printf ?override    = ksprintf (fun s -> print_string (CP.blue ?override s))
let magenta_printf ?override = ksprintf (fun s -> print_string (CP.magenta ?override s))
let cyan_printf ?override    = ksprintf (fun s -> print_string (CP.cyan ?override s))

let gray_printf ?override ~brightness = ksprintf (fun s -> print_string (CP.gray ?override ~brightness s))
let rgb_printf ?override ~r ~g ~b = ksprintf (fun s -> print_string (CP.rgb ?override ~r ~g ~b s))
let color_printf ?override ~color = ksprintf (fun s -> print_string (CP.color ?override ~color s))
