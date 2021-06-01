open Type
open Recursive_type_gen

let _ = Type_lib.fill_type_lib ()

let compute_boltzman func target =
  let intype = Prod func.intypes in

  let equations = equations_from_compo intype [] in
  if !verbose > 1 then
    Sum_type.print_equations Format.std_formatter (Array.of_list equations);
  if equations <> [] then (
    let simple_type_boltz bt z =
      match bt with
      | Name (n, _) ->
          if (find_type n).is_simple then Some (boltzman_from_compo bt z)
          else None
      | Abstract _ -> Some (boltzman_from_compo (Name ("int", [])) z)
      | _ -> None
    in

    (*let x = newton_raphson_iterate ~bound:(0.0,1.0) (fun y -> let (x,dx) = boltzman_from_compo intype y in x-.init,dx) 0.001 in*)
    let z =
      Math.dichotomie ~up_bound:false ~low:(0.0, 0.0) (0.0, 1.0)
        (fun z ->
          match Sum_type.compute_size simple_type_boltz equations z with
          | None -> (nan, nan)
          | Some res ->
              List.iter
                (fun (t1, v, dv) -> Recursive_type_gen.add_memoize t1 z (v, dv))
                res;
              let nv, dnv = boltzman_from_compo intype z in
              let es = z *. dnv /. nv in

              (es, 0.0))
        target
    in

    let res =
      match Sum_type.compute_size simple_type_boltz equations z with
      | Some x -> x
      | None -> failwith "fail to compute weigth"
    in

    List.iter
      (fun (t1, v, dv) -> Recursive_type_gen.add_memoize t1 z (v, dv))
      res;

    (*let x = init in*)
    let nv, dnv = boltzman_from_compo intype z in
    let es = z *. dnv /. nv in
    if !verbose > 0 then
      Format.printf "Boltzman -> obj:%g type:'%a' : z:%g -> G:%g E:%g\n" target
        (pp_compo ~use_boltz_id:false)
        intype z nv es;
    (z, es) )
  else (nan, nan)

let max_size = ref 20

let set_max_size m = max_size := m

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

let rand_fun t s l =
  let _, td = Parse_from_compiler.parse_string ("val r:" ^ t) in
  ignore td;
  let v = Hashtbl.seeded_hash s (Marshal.to_bytes l [ Marshal.Closures ]) in
  let rs = Random.State.make [| v |] in
  let bt = td.outtype in
  (*Name (t, []) in*)
  let vr, _ = gen_from_compo rs !max_size bt (float !max_size) in
  reveal vr bt

let print_typedef f (l : Type.sum_type list) =
  Format.pp_print_list (fun _ x -> Format.fprintf f "\t%a@." pp_sum x) f l

let print_sig f (named, func) =
  (*if named <> [] then Printf.fprintf f "module Ctype = struct \n%a\nend\n" print_typedef named;*)
  Format.fprintf f "module type EXPSIG = sig@.%a\t%a@.end" print_typedef named
    (fun f -> pp_func f)
    func

let gen_to_string ?(throw = false) ?canonize f =
  let ts = gen_string_of_compo [] f.outtype in
  match (throw, canonize) with
  | true, Some x ->
      Printf.sprintf "fun v -> try %s (%s v) with x -> Printexc.to_string x" ts
        x
  | true, None -> Printf.sprintf "try %s with x -> Printexc.to_string x" ts
  | false, Some x -> Printf.sprintf "fun v -> %s (%s v)" ts x
  | false, None -> ts

let generic_loop header call footer ?(out_err = false) ?tsrange ?throw ?canonize
    ?boltz_evaluated out_file size n t files =
  let td, func, z =
    match boltz_evaluated with
    | None ->
        let td, func = Parse_from_compiler.parse_string t in
        List.iter evaluate td;
        let z, _ = compute_boltzman func size in
        (td, func, z)
    | Some x -> x
  in
  let max = int_of_float size in
  let randfun = Hashtbl.create 42 in
  let rs = random_state () in
  let ts = gen_to_string ?throw ?canonize func in
  let sigs out () = print_sig out (td, func) in
  let random_fun_def out () = gen_random_fun_def out randfun func in
  header out_file ~out_err max sigs ts random_fun_def;
  for i = 1 to n do
    let j = 2 + (i * (max - 2) / n) in
    let s = call_random ?tsrange randfun rs j z func in
    call out_file ~out_err ?throw ?canonize s
  done;
  footer out_file files

let gen_value ?tsrange ?boltz_evaluated file_name max n t =
  generic_loop
    (fun _ ~out_err:_ _ _ _ _ -> ())
    (fun out_file ~out_err:_ ?throw:_ ?canonize:_ s ->
      Format.fprintf out_file "%s\n" s)
    (fun _ _ -> ())
    ?tsrange ?boltz_evaluated file_name max n t ()

let gen_test ?(out_err = false) ?(ftotest = "rendu.ml") ?tsrange
    ?boltz_evaluated file_name size n t =
  generic_loop
    (fun out_file ~out_err:_ _ sigs ts random_fun_def ->
      Format.fprintf out_file
        "open Boltzgen_runtime.Gen_test_lib@.let _ = set_max_size \
         %i;;@.%a@.module TestFunctor (R : EXPSIG ) = struct@.\topen R@.\tlet \
         to_string = %s@.       %a\tlet _ =@."
        (int_of_float size) sigs () ts random_fun_def ())
    (fun out_file ~out_err ?throw:_ ?canonize:_ s ->
      Format.fprintf out_file
        "\t\t%s (\"%s = \"^(try to_string (%s) with x -> Printexc.to_string \
         x)^\"\");\n"
        (if out_err then "prerr_endline" else "print_endline")
        (String.escaped s) s)
    (fun out_file _ ->
      Format.fprintf out_file
        "\t\t()\nend;;\n#mod_use \"%s\"\nmodule TA = TestFunctor (%s);;" ftotest
        (mname_of_string ftotest))
    ~out_err ?tsrange ?boltz_evaluated file_name size n t ftotest

let gen_test_direct ?(out_err = false) ?throw ?canonize ?boltz_evaluated
    file_name size n t =
  generic_loop
    (fun out_file ~out_err:_ _ sigs ts random_fun_def ->
      Format.fprintf out_file
        "open Boltzgen_runtime.Gen_test_lib\n\
         let _ = set_max_size %i;;\n\
         %a\n\
         let to_string = %s\n\
         %a;;\n"
        (int_of_float size) sigs () ts random_fun_def ())
    (fun out_file ~out_err ?throw:_ ?canonize:_ s ->
      Format.fprintf out_file
        "\t\t%s (\"%s = \"^(try to_string (%s) with x -> Printexc.to_string \
         x)^\"\");\n"
        (if out_err then "prerr_endline" else "print_endline")
        (String.escaped s) s)
    (fun out_file _ -> Format.fprintf out_file "\t\t();;\n")
    ~out_err ?throw ?canonize ?boltz_evaluated file_name size n t ()

let gen_test_diff ?(out_err = false) ?tsrange ?throw ?canonize ?boltz_evaluated
    r1 r2 file_name max n t =
  generic_loop
    (fun out_file ~out_err _ sigs ts random_fun_def ->
      Format.fprintf out_file
        "open Boltzgen_runtime.Gen_test_lib\n\
         %a\n\
         module TestFunctorDiff (R1 : EXPSIG) (R2 : EXPSIG) = struct\n\
         \tlet to_string1 = let open R1 in %s\n\
         \tlet to_string2 = let open R2 in %s\n\
         \tlet ae = assert_equal %s to_string1 to_string2\n\
         %a\tlet _ = \n"
        sigs () ts ts
        (if out_err then "" else "~err:true")
        random_fun_def ())
    (fun out_file ~out_err:_ ?throw:_ ?canonize:_ s ->
      Format.fprintf out_file
        "\t\t(let v1 = let open R1 in (fun () -> %s) and v2 = let open R2 in \
         (fun () -> %s) in ae \"%s\" v1 v2);\n"
        s s (String.escaped s))
    (fun out_file _ ->
      Format.fprintf out_file
        "\t\tif !nb_fail>0 then exit 1\n\
         end;;\n\
         #mod_use \"%s\"\n\
         #mod_use \"%s\"\n\
         module TA = TestFunctorDiff (%s) (%s) ;;" r1 r2 (mname_of_string r1)
        (mname_of_string r2))
    ~out_err ?throw ?canonize ?tsrange ?boltz_evaluated file_name max n t
    (r1, r2)

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
  let outf = Format.formatter_of_out_channel file in
  gen_test ?out_err outf max n t;
  Format.pp_print_flush outf ();
  close_out file

(*
   let td, func = Parse_from_compiler.parse_string t in
   List.iter evaluate td;
   let z, _ = compute_boltzman func (float max) in
   gen_test ?out_err file max n td func z;
   close_out file*)

let gen_test_d ?throw ?canonize max n t =
  let file = open_out "t.ml" in
  let outf = Format.formatter_of_out_channel file in
  gen_test_diff ~out_err:true ?throw ?canonize Sys.argv.(1) Sys.argv.(2) outf
    max n t;
  Format.pp_print_flush outf ();
  close_out file

(*let file = open_out "t.ml" in
  let td, func = Parse_from_compiler.parse_string t in
  List.iter evaluate td;
  let z, _ = compute_boltzman func (float max) in
  gen_test_diff ~out_err:true ?throw ?canonize Sys.argv.(1) Sys.argv.(2) file
    max n td func z;
  close_out file*)

let gen ?(out_err = true) max n t = gen_test_t ~out_err max n t

let gen_dir max n t =
  let file = open_out "t.ml" in
  let outf = Format.formatter_of_out_channel file in
  gen_test_direct ~out_err:true outf max n t;
  Format.pp_print_flush outf ();
  close_out file

(*let file = open_out "t.ml" in
  let td, func = Parse_from_compiler.parse_string t in
  List.iter evaluate td;
  let z, _ = compute_boltzman func (float max) in
  gen_test_direct ~out_err:true file max n td func z;
  close_out file*)

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
      let file_st = open_out "t.ml" in
      let file = Format.formatter_of_out_channel file_st in
      Format.pp_print_string file "open Question_type\n";
      let canonize =
        match qu.rtype with
        | Name ("qlist", []) -> Some "Question_type.canonize_qlist"
        | _ -> None
      in
      pp_func ~use_boltz_id:true Format.str_formatter qutype;
      let funtype = Format.flush_str_formatter () in
      gen_test_diff ~out_err:true ?canonize Sys.argv.(1) "reference.ml" file
        (float_of_int qu.test_max)
        (if really_test then qu.test_effort else 0)
        funtype;
      Format.pp_print_flush file ();
      close_out file_st
  | Type _ -> failwith "Not yet handle"
