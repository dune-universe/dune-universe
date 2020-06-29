include Js_of_ocaml.Js
module Url = Js_of_ocaml.Url
module Dom_html = Js_of_ocaml.Dom_html
module Firebug = Js_of_ocaml.Firebug
module File = Js_of_ocaml.File
module Dom = Js_of_ocaml.Dom

type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b

type window = Dom_html.window

let to_arrayf f a = Array.map f (to_array a)

let of_arrayf f a = array (Array.map f a)

let to_list a = Array.to_list @@ to_array a

let of_list l = array @@ Array.of_list @@ l

let to_listf f a = Array.to_list @@ to_arrayf f a

let of_listf f a = of_arrayf f @@ Array.of_list a

let optdef f = function None -> undefined | Some x -> def (f x)

let to_optdef f x =
  match Optdef.to_option x with None -> None | Some x -> Some (f x)

let unoptdef_f def f x =
  match Optdef.to_option x with None -> def | Some x -> f x

let unoptdef def x = match Optdef.to_option x with None -> def | Some x -> x

let convdef f x =
  match Optdef.to_option x with None -> undefined | Some x -> def (f x)

let to_opt f x =
  match Opt.to_option x with None -> None | Some x -> Some (f x)

let opt f = function None -> null | Some x -> some (f x)

let convopt f x =
  match Opt.to_option x with None -> null | Some x -> some (f x)

let js_log o = Firebug.console##log o

let log_str s = Firebug.console##log (string s)

let log fmt =
  Format.kfprintf
    (fun _fmt -> js_log (string (Format.flush_str_formatter ())))
    Format.str_formatter fmt
