open Type
open Sum_type
open Recursive_type_gen

let _ = Type_lib.fill_type_lib ()

let compute_boltzman func init =
  let intype = Prod func.intypes in
  (*let x = newton_raphson_iterate ~bound:(0.0,1.0) (fun y -> let (x,dx) = boltzman_from_compo intype y in x-.init,dx) 0.001 in*)
  let x =
    dichotomie ~up_bound:true (0.0, 1.0)
      (fun z ->
        let x, dx = boltzman_from_compo intype z in
        if x > 1.0e+100 then (nan, 0.0) else (z *. dx /. x, 0.0))
      init
  in
  (*let x = init in*)
  let nv, dnv = boltzman_from_compo intype x in
  let es = x *. dnv /. nv in
  if !verbose > 0 then
    Printf.printf "Boltzman -> obj:%g type:'%s' : z:%g -> G:%g E:%g\n" init
      (string_of_compo intype) x nv es;
  (x, es)

let max_size = ref 20

let set_max_size m = max_size := m

let rand_fun t s l =
  let v = Hashtbl.seeded_hash s (Marshal.to_bytes l [ Marshal.Closures ]) in
  let rs = Random.State.make [| v |] in
  let bt = Name (t, []) in
  let vr, _ = gen_from_compo rs !max_size bt (float !max_size) in
  reveal vr bt

let random_state () = Random.State.make [| Random.bits () |]

let mname_of_string s =
  let sm =
    match String.rindex_opt s '/' with
    | None -> s
    | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  in
  let s2 = String.sub sm 1 (String.length sm - 1) in
  let s3 =
    match String.index_opt s2 '.' with
    | None -> s2
    | Some i -> String.sub s2 0 i
  in
  let c2 = Char.escaped @@ Char.uppercase_ascii @@ sm.[0] in
  c2 ^ s3

let print_typedef f (l : Type.sum_type list) =
  List.iter (fun x -> Printf.fprintf f "\t%s\n" (string_of_sum x)) l

let print_sig f (named, func) =
  (*if named <> [] then Printf.fprintf f "module Ctype = struct \n%a\nend\n" print_typedef named;*)
  Printf.fprintf f "module type EXPSIG = sig\n%a\t%a\nend" print_typedef named
    print_func func

let gen_test ?(out_err = false) ?(ftotest = "rendu.ml") out_file max n td func z
    =
  let randfun = Hashtbl.create 42 in
  let rs = random_state () in
  (*  let td,func = Parsing_types.parse_string t in*)
  Printf.fprintf out_file
    "open Gen_test_lib\n\
     let _ = set_max_size %i;;\n\
     %a\n\
     module TestFunctor (R : EXPSIG ) = struct\n\
     \topen R\n\
     \tlet to_string = %s\n"
    max print_sig (td, func)
    (gen_string_of_compo [] func.outtype);
  Printf.fprintf out_file "%s\tlet _ =\n" (gen_random_fun_def randfun func);
  for i = 1 to n do
    let j = 2 + (i * (max - 2) / n) in
    let s = call_random randfun rs j z func in
    Printf.fprintf out_file
      "\t\t%s (\"%s = \"^(try to_string (%s) with x -> Printexc.to_string \
       x)^\"\");\n"
      (if out_err then "prerr_endline" else "print_endline")
      (String.escaped s) s
  done;
  Printf.fprintf out_file
    "\t\t()\nend;;\n#mod_use \"%s\"\nmodule TA = TestFunctor (%s);;" ftotest
    (mname_of_string ftotest)

let gen_to_string ?(throw = false) ?canonize f =
  let ts = gen_string_of_compo [] f.outtype in
  match (throw, canonize) with
  | true, Some x ->
      Printf.sprintf "fun v -> try %s (%s v) with x -> Printexc.to_string x" ts
        x
  | true, None -> Printf.sprintf "try %s with x -> Printexc.to_string x" ts
  | false, Some x -> Printf.sprintf "fun v -> %s (%s v)" ts x
  | false, None -> ts

let gen_value ?tsrange out_file max n func z =
  let randfun = Hashtbl.create 42 in
  let rs = random_state () in
  for _ = 1 to n do
    let s = call_random ?tsrange randfun rs max z func in
    Printf.fprintf out_file "%s\n" s
  done

let gen_test_direct ?(out_err = false) ?throw ?canonize out_file max n td func z
    =
  let randfun = Hashtbl.create 42 in
  (*let td,func = Parsing_types.parse_string t in*)
  Printf.fprintf out_file
    "open Gen_test_lib\nlet _ = set_max_size %i;;\n%a\nlet to_string = %s;;\n"
    max print_sig (td, func)
    (gen_to_string ?throw ?canonize func);
  Printf.fprintf out_file "%slet _ =\n" (gen_random_fun_def randfun func);
  for i = 1 to n do
    let j = 2 + (i * (max - 2) / n) in
    let s = call_random randfun (random_state ()) j z func in
    Printf.fprintf out_file
      "\t%s (\"%s = \"^(try to_string (%s) with x -> Printexc.to_string \
       x)^\"\");\n"
      (if out_err then "prerr_endline" else "print_endline")
      (String.escaped s) s
  done;
  Printf.fprintf out_file "\t\t();;\n"

(*let print_type_equation f td =
  if td<> [] then (
    Printf.fprintf f "with";
    List.iter (fun ((n,_),_) -> Printf.fprintf f " type %s = Ctype.%s " n n) td
  )*)

let gen_test_diff ?(out_err = false) ?throw ?canonize r1 r2 out_file max n td
    func z =
  let randfun = Hashtbl.create 42 in
  let rs = random_state () in
  let ts = gen_to_string ?throw ?canonize func in
  Printf.fprintf out_file
    "open Gen_test_lib\n\
     %a\n\
     module TestFunctorDiff (R1 : EXPSIG) (R2 : EXPSIG) = struct\n\
     \tlet to_string1 = let open R1 in %s\n\
     \tlet to_string2 = let open R2 in %s\n\
     \tlet ae = assert_equal %s to_string1 to_string2\n"
    print_sig (td, func) ts ts
    (if out_err then "" else "~err:true");
  Printf.fprintf out_file "%s\tlet _ = \n" (gen_random_fun_def randfun func);
  for i = 1 to n do
    let j = 2 + (i * (max - 2) / n) in
    let s = call_random randfun rs j z func in
    Printf.fprintf out_file
      "\t\t(let v1 = let open R1 in (fun () -> %s) and v2 = let open R2 in \
       (fun () -> %s) in ae \"%s\" v1 v2);\n"
      s s (String.escaped s)
  done;
  Printf.fprintf out_file
    "\t\tif !nb_fail>0 then exit 1\n\
     end;;\n\
     #mod_use \"%s\"\n\
     #mod_use \"%s\"\n\
     module TA = TestFunctorDiff (%s) (%s) ;;" r1 r2 (mname_of_string r1)
    (mname_of_string r2)

let nb_test = ref 0

let nb_fail = ref 0

let assert_equal ?(throw = false) ?(err = true) to_string1 to_string2 arg v1 v2
    =
  ignore throw;
  incr nb_test;
  let s1 = try to_string1 (v1 ()) with _ -> "Exception occurs"
  and s2 = try to_string2 (v2 ()) with _ -> "Exception occurs" in
  if s1 <> s2 then (
    incr nb_fail;
    if err then Printf.eprintf "%s = %s instead of %s\n" arg s1 s2
    else Printf.printf "%s = %s instead of %s\n" arg s1 s2;
    exit 1 )

let assert_equal_arg ?throw ?(err = true) to_string1 to_string2 to_string_arg f1
    f2 arg =
  assert_equal ?throw ~err to_string1 to_string2 (to_string_arg arg)
    (fun () -> f1 arg)
    (fun () -> f2 arg)

let assert_equal_string ?(err = true) arg v1 v2 =
  incr nb_test;
  let s1 = v1 () and s2 = v2 () in
  if s1 <> s2 then (
    incr nb_fail;
    if err then Printf.eprintf "%s = %s instead of %s\n" arg s1 s2
    else Printf.printf "%s = %s instead of %s\n" arg s1 s2;
    exit 1 )

let gen_test_t ?out_err max n t =
  let file = open_out "t.ml" in
  let td, func = Parse_from_compiler.parse_string t in
  List.iter evaluate td;
  let z, _ = compute_boltzman func (float max) in
  gen_test ?out_err file max n td func z;
  close_out file

let gen_test_d ?throw ?canonize max n t =
  let file = open_out "t.ml" in
  let td, func = Parse_from_compiler.parse_string t in
  List.iter evaluate td;
  let z, _ = compute_boltzman func (float max) in
  gen_test_diff ~out_err:true ?throw ?canonize Sys.argv.(1) Sys.argv.(2) file
    max n td func z;
  close_out file

let gen ?(out_err = true) max n t = gen_test_t ~out_err max n t

let gen_dir max n t =
  let file = open_out "t.ml" in
  let td, func = Parse_from_compiler.parse_string t in
  List.iter evaluate td;
  let z, _ = compute_boltzman func (float max) in
  gen_test_direct ~out_err:true file max n td func z;
  close_out file

let gen_qbank ?(really_test = true) ?override_login qb id =
  let open Question_type in
  let login =
    match override_login with Some x -> x | None -> Unix.getenv "LOGIN"
  in
  let qbank = load_qbank qb in
  let qbank2 = shuffle login qbank in
  let que = qbank2.questions.(id - 1) in
  match que with
  | Value qu ->
      let qutype = deano_func (Printf.sprintf "fq_%i" id) qu.rtype in
      let file_out = open_out "reference.ml" in
      Printf.fprintf file_out "let fq_%i = %s;;\n" id qu.answer;
      close_out file_out;
      let file = open_out "t.ml" in
      output_string file "open Question_type\n";
      let z, _ = compute_boltzman qutype (float qu.test_max) in
      let canonize =
        match qu.rtype with
        | Name ("qlist", []) -> Some "Question_type.canonize_qlist"
        | _ -> None
      in
      gen_test_diff ~out_err:true ?canonize Sys.argv.(1) "reference.ml" file
        qu.test_max
        (if really_test then qu.test_effort else 0)
        [] qutype z;
      close_out file
  | Type _ -> failwith "Not yet handle"
