include Js_of_ocaml.Js
module Url = Js_of_ocaml.Url
module Dom_html = Js_of_ocaml.Dom_html
module Firebug = Js_of_ocaml.Firebug
module File = Js_of_ocaml.File
module Dom = Js_of_ocaml.Dom
module Typed_array = Js_of_ocaml.Typed_array
module Regexp = Js_of_ocaml.Regexp

exception JsError = Error

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

let error_of_string s = new%js error_constr (string s)
let catch_exn f = function
  | JsError e -> f e
  | exn -> f @@ error_of_string @@ Printexc.to_string exn

type 'a aopt = 'a

module AOpt = struct
  let null : 'a aopt = Unsafe.pure_js_expr "null"
  external some : 'a -> 'a aopt = "%identity"
  let undefined : 'a aopt = Unsafe.pure_js_expr "undefined"
  external def : 'a -> 'a aopt = "%identity"
  external return : 'a -> 'a aopt = "%identity"
  external js_equals : 'a -> 'b -> bool = "caml_js_equals"
  type 'a t = 'a aopt
  let is_none (x : 'a t) : bool = x == undefined || js_equals x null
  let map ?(none=undefined) (x : 'a t) (f : 'a -> 'b) : 'b t =
    if is_none x then none else return (f x)
  let bind ?(none=undefined) (x : 'a t) (f : 'a -> 'b t) : 'b t =
    if is_none x then none else f x
  let test (x : 'a t) : bool = not (is_none x)
  let iter (x : 'a t) (f : 'a -> unit) : unit = if not (is_none x) then f x
  let case (x : 'a t) (f : unit -> 'b) (g : 'a -> 'b) : 'b = if is_none x then f () else g x
  let get (x : 'a t) (f : unit -> 'a) : 'a = if is_none x then f () else x
  let option ?(none=undefined) (x : 'a option) : 'a t = match x with
    | None -> none
    | Some x -> return x
  let to_option (x : 'a t) : 'a option = case x (fun () -> None) (fun x -> Some x)
  let aopt ?(none=(undefined : 'b t)) (f : 'a -> 'b) : 'a option -> 'b t = function
    | None -> none
    | Some x -> return (f x)
  let to_aopt (f : 'a -> 'b) (x : 'a t) : 'b option = case x (fun () -> None) (fun x -> Some (f x))
end

type 'a case_prop = < get : 'a optdef > gen_prop

let rec choose_case_opt = function
  | [] -> undefined
  | h :: t -> match Optdef.to_option h with None -> choose_case_opt t | Some _ -> h

let rec choose_case l = choose_case_opt (List.map Optdef.return l)

let object_cs = Unsafe.global##._Object
let assign (o1 : _ t) (o2 : _ t) = Unsafe.coerce (object_cs##assign o1 o2)
let assign_list l = Unsafe.coerce (Unsafe.meth_call object_cs "assign" (Array.of_list l))
