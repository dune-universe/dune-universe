open Html
open Type
open Recursive_type_gen
open Gen_test_lib


let rec print_type_list_string = function
    [] -> ""
  | t::q -> Format.sprintf "%s -> %s" (string_of_compo t) (print_type_list_string q)

let print_func_string f =
  Format.sprintf "val %s : %s%s" f.name (print_type_list_string f.intypes) (string_of_compo f.outtype)

let test_list hash max z n func =
  let rs = random_state () in
  let l = ref [] in
    for i=1 to n do
      let j = 2+ (i*(max-2)/n) in
      let s = call_random hash rs j z func in
      let txt = Format.sprintf "print_endline (\"%s\ = \"^to_string (%s) );" (String.escaped s) s in
      l :=  (text txt):: br () :: !l
    done;
    List.rev !l

let boltz_obj = ref 5.0
let spec = ref "type 'a tree = 
   Empty 
 | Node of 'a tree * 'a * 'a tree
val toto: int list tree -> int"


let html_of_spec td func z =
  let randfun = Hashtbl.create 4 in
  let srand =  gen_random_fun_def randfun func in
  let l = test_list randfun 20 z 20 func in
  let ts = text (Format.sprintf "let to_string = %s\n%s" (gen_string_of_compo [] func.outtype) srand) in
  div ((p [ts]):: l)

let _ =  Js_of_ocaml.Dom_html.window##.onload := Js_of_ocaml.Dom.handler @@ fun _ ->
  let init,result = get_by_id' "result" in
  result [
      let td,func = Parse_from_compiler.parse_string !spec in
      List.iter evaluate td;
      let z,_ = compute_boltzman func !boltz_obj in       
      html_of_spec td func z];
  let alert,alert_up = get_by_id' "alert" in
  let sigt,sig_up = get_by_id' "sigt" in
  let update () = 
  try
    let td,func = Parse_from_compiler.parse_string !spec in
    List.iter evaluate td;
    let z,_ = compute_boltzman func !boltz_obj in
    sig_up [text (print_func_string func )];
    result [html_of_spec td func z];
    alert_up [];
  with
    _ -> (
    result [];
    sig_up [];
    alert_up [text "Not Parsing"]
  ) in
  let _ = get_by_id ~on_change:(fun b ->
                            boltz_obj := float_of_string b;
                            update ()) "b_value" 
(*(string_of_float !boltz_obj);*)
  in
  let _ = get_by_id  ~on_change:(fun nspec ->
                         spec :=nspec;
                         update ()) "funspec" in
  Js_of_ocaml.Js._false
