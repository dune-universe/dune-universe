open Js_of_ocaml
open Js


class type path =
  object
    method delimiter: js_string t readonly_prop
    method sep:       js_string t readonly_prop
    method basename:  js_string t  ->  js_string t meth
    method dirname:   js_string t  ->  js_string t meth
    method extname:   js_string t  ->  js_string t meth
  end


let path: path t = Unsafe.eval_string "require('path')"

let delimiter: char =
  let str = to_string path##.delimiter in
  assert (String.length str = 1);
  str.[0]

let separator: char =
  let str = to_string path##.sep in
  assert (String.length str = 1);
  str.[0]

let dirname (s:string): string =
  to_string @@ path##dirname (string s)

let basename (s:string): string =
  to_string @@ path##basename (string s)

let extname (s:string): string =
  to_string @@ path##extname (string s)


let _ = dirname
let _ = basename
let _ = extname
