let zprintf (_ : ('a, 'b, unit) format) : 'a =
  let rec f = fun _ -> Obj.magic f in
  Obj.magic f

let format_check (template : (_,_,_,_,_,_) format6 as 'a) =
  let template_type = Printf.sprintf "%{%}" (Obj.magic template) in
  fun (str : string) ->
    let str_type = Printf.sprintf "%{%}" (Obj.magic str) in
    if template_type = str_type then (Obj.magic str : 'a)
    else invalid_arg (Printf.sprintf "format_check : incompatible type with %s" template_type)
;;
