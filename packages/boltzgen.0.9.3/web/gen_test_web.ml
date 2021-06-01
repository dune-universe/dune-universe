open Html
open Boltzgen_runtime
open Gen_test_lib
open Recursive_type_gen
open Type

(*
let exec b ppf s =
  let lb = Lexing.from_string s in
  try
    List.iter
      (fun phr ->
        if not (Toploop.execute_phrase b ppf phr) then raise Exit)
      (!Toploop.parse_use_file lb)
  with
    | Exit -> ()
    | x    -> Errors.report_error ppf x


let exec_phrase s =
  exec false Format.std_formatter ("Format.pp_print_string Format.str_formatter ("^ s ^")");
  Format.flush_str_formatter ()

let _ =
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "";
  ignore @@ Toploop.use_silently Format.std_formatter "test.ml";
  let str =  exec_phrase "string_of_int (sum 5 3)" in
  Printf.printf "test:'%s'" str
*)

(*let test_list hash max z n func =
  let rs = random_state () in
  let l = ref [] in
    for i=1 to n do
      let j = 2+ (i*(max-2)/n) in
      let s = call_random hash rs j z func in
      let txt = Format.sprintf "print_endline (\"%s\ = \"^to_string (%s) );" (String.escaped s) s in
      l :=  (text txt):: br () :: !l
    done;
    List.rev !l*)

let string_of_print f a =
  f Format.str_formatter a;
  Format.flush_str_formatter ()

let boltz_obj = ref 5.0

let max_size = ref 100.0

let eval_ref = ref false

let gen_caseine = ref false

(*
let html_of_spec2 td func z =
  let randfun = Hashtbl.create 4 in
  let srand =  gen_random_fun_def randfun func in
  let l = test_list randfun 20 z 20 func in
  let ts = text (Format.sprintf "let to_string = %s\n%s" (gen_string_of_compo [] func.outtype) srand) in
  div ((p [ts]):: l)*)

let string_of_spec td func z t =
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  let reference = get_value_by_id "reference" () in
  let count = ref 1 in
  generic_loop
    (fun out_file ~out_err:_ _ sigs ts random_fun_def ->
      Format.fprintf out_file
        "%a\n\
         module TestFunctor (R : EXPSIG ) = struct\n\
         \topen R\n\
         \tlet to_string = %s\n\
        \       %a\tlet _ =\n"
        sigs () ts random_fun_def ())
    (fun out_file ~out_err ?throw:_ ?canonize:_ s ->
      if !gen_caseine then (
        Format.fprintf out_file
          "\t\tprint_endline \"Case = Boltzgen test %i\";@." !count;
        incr count;
        Format.fprintf out_file "\t\tprint_endline \"input = %s\";@."
          (String.escaped s);
        Format.fprintf out_file
          "\t\tprint_endline (\"output = \"^(try to_string (%s) with x -> \
           Printexc.to_string x)^\"\");\n\
           print_newline ();@." s )
      else
        Format.fprintf out_file
          "\t\tprint_endline (\"%s = \"^(try to_string (%s) with x -> \
           Printexc.to_string x)^\"\");@."
          (String.escaped s) s)
    (fun out_file _ ->
      Format.fprintf out_file
        "\t\t()\n\
         end;;\n\
         module Reference = struct@. %a %s end\n\
         module TA = TestFunctor (Reference);;"
        (Format.pp_print_list (fun f t -> Format.fprintf f "%a@." pp_sum t))
        td reference)
    ~boltz_evaluated:(td, func, z) formatter !max_size 20 t ();
  let file_t = Buffer.contents buffer in
  (*Format.fprintf Format.str_formatter "%s" file_t;*)
  if !eval_ref then
    let _ = Js_of_ocaml_toplevel.JsooTop.use Format.str_formatter file_t in
    Format.flush_str_formatter ()
  else file_t

(*let string_of_spec td func z t =
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  let reference = get_value_by_id "reference" () in
  generic_loop
    (fun out_file ~out_err:_ _ sigs ts _ ->
      (*Format.fprintf out_file "%s\n" sigs;
      Format.fprintf out_file "let to_string = %s\n\n" ts;*)
      Format.fprintf formatter "let to_string = %s;;\n%s;;@." ts reference)
    (fun out_file ~out_err:_ ?throw:_ ?canonize:_ s ->
       Format.fprintf formatter
      "\t\tprint_endline (\"%s = \"^(try to_string (%s) with x -> Printexc.to_string \
       x)^\"\");@."
      (String.escaped s) s)
    (fun _ _ -> ())
    ~boltz_evaluated:(td, func, z) formatter !max_size 20 t ();
  let file_t = Buffer.contents buffer in
  (*Format.fprintf Format.str_formatter "%s" file_t;*)
  if !eval_ref then
    let _ = Js_of_ocaml_toplevel.JsooTop.use Format.str_formatter file_t in
    Format.flush_str_formatter ()
  else file_t*)

let _ =
  Js_of_ocaml.Dom_html.window##.onload
  := Js_of_ocaml.Dom.handler @@ fun _ ->
     Js_of_ocaml_toplevel.JsooTop.initialize ();
     Js_of_ocaml.Sys_js.set_channel_flusher stdout
       (Format.fprintf Format.str_formatter "%s");

     let init, result = get_by_id' "result" in
     let alert, alert_up = get_by_id' "alert" in
     let sigt, sig_up = get_by_id' "sigt" in
     let spec = get_value_by_id "funspec" in
     let update () =
       result [];
       sig_up [ text "..." ];
       alert_up [];

       try
         let spec_value = spec () in
         let td, func = Parse_from_compiler.parse_string spec_value in
         List.iter evaluate td;
         List.iter
           (fun t ->
             ignore
             @@ Js_of_ocaml_toplevel.JsooTop.use Format.std_formatter
                  (string_of_print pp_sum t))
           td;
         let z, _ = compute_boltzman func !boltz_obj in
         sig_up
           [
             text (string_of_print pp_func func);
             br ();
             text ("z=" ^ string_of_float z);
           ];
         result [ text @@ string_of_spec td func z spec_value ];
         alert_up []
       with x ->
         result [];
         sig_up [];
         alert_up [ text "Not Parsing : "; text (Printexc.to_string x) ]
     in
     update ();
     let _ =
       get_by_id
         ~on_change:(fun b ->
           boltz_obj := float_of_string b;
           update ())
         "b_value"
       (*(string_of_float !boltz_obj);*)
     in
     let _ =
       get_by_id
         ~on_change:(fun nmax ->
           max_size := float_of_string nmax;
           update ())
         "max_val"
     in
     let _ = get_by_id ~on_change:(fun _ -> update ()) "funspec" in
     let _ = get_by_id ~on_change:(fun _ -> update ()) "reference" in
     let _ =
       get_by_id
         ~on_change:(fun v ->
           eval_ref := v = "true";
           update ())
         "eval_ref"
     in
     let _ =
       get_by_id
         ~on_change:(fun v ->
           gen_caseine := v = "true";
           update ())
         "gen_caseine"
     in

     Js_of_ocaml.Js._false
