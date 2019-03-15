

(* This file is free software. See file "license" for more details. *)

(** {1 Simple Formatted Document} *)

module Fmt = Format

type block =
  [ `S of string (* section *)
  | `P of string (* paragraph *)
  | `Pre of string (* formatted paragraph *)
  | `I of string * t (* indented doc with header *)
  | `L of t list (* list of items *)
  | `Tbl of string list * t list list (* table *)
  ]

and t = block list

let ksprintf ~f fmt =
  let buf = Buffer.create 32 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ -> Format.pp_print_flush out (); f (Buffer.contents buf))
    out fmt

let section s = `S s
let paragraph s = `P s
let paragraph_f s = ksprintf ~f:paragraph s
let indent i j = `I (i,j)
let pre s = `Pre s
let pre_f s = ksprintf ~f:pre s
let list l = `L l

let pp_list_ p =
  Format.pp_print_list ~pp_sep:(fun out () -> Format.pp_print_cut out ()) p

let rec pp out (l:t) =
  Fmt.fprintf out "@[<v>%a@]" (pp_list_ pp_block) l

and pp_block out : block -> unit = function
  | `S sec -> Fmt.fprintf out "# @[<h>%s@]@," sec
  | `P msg -> Fmt.fprintf out "@[%a@]" Format.pp_print_text msg
  | `Pre msg -> Fmt.fprintf out "%s" msg
  | `I (head, body) ->
    Fmt.fprintf out "@[<v2>%s::@ %a@]" head pp body
  | `L l ->
    let pp_item out x = Fmt.fprintf out "@[<2>- %a@]" pp x in
    Fmt.fprintf out "@[<v>%a@]" (pp_list_ pp_item) l
  | `Tbl (heads,rows) ->
    let li = String.make 60 '-' in
    let pp_li out () = Format.pp_print_string out li in
    let pp_row = pp_list_ pp in
    Format.fprintf out "@[<v>%a@,%a@," pp_li () (pp_list_ Format.pp_print_string) heads;
    List.iter (fun row -> Format.fprintf out "%a@,%a@," pp_li () pp_row row) rows;
    Format.fprintf out "%a@]" pp_li ()


