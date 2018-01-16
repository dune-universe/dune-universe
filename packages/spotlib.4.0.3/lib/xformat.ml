open Format

type t = formatter

let stdout = std_formatter
let stderr = err_formatter

(* pp_open_* => * *)
let hbox   = pp_open_hbox
let vbox   = pp_open_vbox
let hvbox  = pp_open_hvbox
let hovbox = pp_open_hovbox
let box    = pp_open_box
let tag    = pp_open_tag
let tbox   = pp_open_tbox 

(* pp_close_* => close_* *)
let close_box  = pp_close_box
let close_tag  = pp_close_tag
let close_tbox = pp_close_tbox 

(* pp_print_* => * *)
let string        = pp_print_string
let as_           = pp_print_as 
let int           = pp_print_int 
let float         = pp_print_float 
let char          = pp_print_char 
let bool          = pp_print_bool 
let break         = pp_print_break
let tbreak        = pp_print_tbreak
let cut           ppf = pp_print_cut        ppf ()
let space         ppf = pp_print_space      ppf ()
let force_newline ppf = pp_force_newline    ppf () 
let flush         ppf = pp_print_flush      ppf ()
let newline       ppf = pp_print_newline    ppf ()
let if_newline    ppf = pp_print_if_newline ppf ()
let tab           ppf = pp_print_tab        ppf ()

(* pp_set_* => set_*
   pp_get_* => *      *)
let set_tab = pp_set_tab 
let set_tags = pp_set_tags 
let set_print_tags = pp_set_print_tags 
let set_mark_tags = pp_set_mark_tags 
let print_tags = pp_get_print_tags 
let mark_tags = pp_get_mark_tags 
let set_margin = pp_set_margin 
let margin = pp_get_margin 
let set_max_indent = pp_set_max_indent 
let max_indent = pp_get_max_indent 
let set_max_boxes = pp_set_max_boxes 
let max_boxes = pp_get_max_boxes 
let over_max_boxes = pp_over_max_boxes 
let set_ellipsis_text = pp_set_ellipsis_text 
let ellipsis_text = pp_get_ellipsis_text 
let set_formatter_out_channel = pp_set_formatter_out_channel 
let set_formatter_output_functions = pp_set_formatter_output_functions
let formatter_output_functions = pp_get_formatter_output_functions
let set_formatter_out_functions = pp_set_formatter_out_functions
let formatter_out_functions = pp_get_formatter_out_functions
let set_formatter_tag_functions = pp_set_formatter_tag_functions
let formatter_tag_functions = pp_get_formatter_tag_functions

let of_out_channel = formatter_of_out_channel

let rec list (sep : (unit, formatter, unit) format)  f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> 
      fprintf ppf "@[%a@]%t%a" 
	f x
	(fun ppf -> fprintf ppf sep)
	(list sep f) xs

let option f ppf = function
  | None -> Format.fprintf ppf "None"
  | Some v -> Format.fprintf ppf "Some (%a)" f v 

let lazy_ p ppf v =
  if Xlazy.is_val v then p ppf (Xlazy.(!!) v)
  else Format.fprintf ppf "lazy"

let to_string f v =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  f ppf v;
  flush ppf; 
  Buffer.contents buf

let sprintf fmt =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  kfprintf (fun ppf -> flush ppf; Buffer.contents buf) ppf fmt

let ksprintf f fmt =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  kfprintf (fun ppf -> flush ppf; f (Buffer.contents buf)) ppf fmt

