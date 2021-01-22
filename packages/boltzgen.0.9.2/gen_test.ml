open Gen_test_lib
open Cmdliner

(*ocamlcommon.cma *)
let load_path path =
  "unix.cma " ^ "-I " ^ path ^ "/../lib/ocaml/compiler-libs -I " ^ path
  ^ "/../lib/ocaml-compiler-libs/common "
  ^ "-I +compiler-libs ocamlcommon.cma -I " ^ path ^ "/../lib/boltzgen" ^ " -I "
  ^ path ^ " -I " ^ path ^ "/.gen_test_lib.objs/byte/"

let gen_test funcspec boltzman exec reference max_val number_of_call v seed
    value_only tsrangeerror path2 =
  (match seed with None -> Random.self_init () | Some s -> Random.init s);
  let path =
    match path2 with
    | None -> Filename.dirname Sys.executable_name
    | Some p -> p
  in

  Gen_test_lib__Type.verbose := v;
  let td, func = Gen_test_lib__Parse_from_compiler.parse_string funcspec in
  List.iter Gen_test_lib__Recursive_type_gen.evaluate td;
  let z, size = compute_boltzman func boltzman in
  let tsrange =
    match tsrangeerror with
    | None -> None
    | Some e ->
        Some
          (int_of_float ((1.0 -. e) *. size), int_of_float ((1.0 +. e) *. size))
  in
  match (exec, reference, value_only) with
  | _, _, true -> gen_value ?tsrange stdout max_val number_of_call func z
  | Some fi, Some fr, _ ->
      let fo = open_out "t.ml" in
      gen_test_diff fi fr fo max_val number_of_call td func z;
      close_out fo;
      let cmd = "ocaml " ^ load_path path ^ " gen_test_lib.cma t.ml" in
      if v > 1 then print_endline cmd;
      exit (Sys.command cmd)
  (*ignore @@ Sys.command "echo \"#use \\\"t.ml\\\";;\" | ocaml -I _build gen_test_lib.cma"*)
  | Some fi, None, _ ->
      let fo = open_out "t.ml" in
      gen_test ~ftotest:fi fo max_val number_of_call td func z;
      close_out fo;
      let cmd = "ocaml " ^ load_path path ^ " gen_test_lib.cma t.ml" in
      if v > 1 then print_endline cmd;
      let _ = Sys.command cmd in
      ()
  (*ignore @@ Sys.command "echo \"#use \\\"t.ml\\\";;\" | ocaml -I _build gen_test_lib.cma"*)
  | None, _, _ -> gen_test ~out_err:true stdout max_val number_of_call td func z

let funspec =
  let doc = "Signature of a function to test" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FUNSPEC")

let boltzman =
  let doc = "Typical term size as a float" in
  Arg.(value & opt float 5.0 & info [ "b" ] ~doc ~docv:"TERMSIZE")

let exec =
  let doc =
    "Implementation containing the implementation of the function to test"
  in
  Arg.(value & pos 1 (some file) None & info [] ~docv:"IMPL" ~doc)

let reference =
  let doc = "Implementation containing the reference implementation" in
  Arg.(value & pos 2 (some file) None & info [] ~docv:"REFERENCE" ~doc)

let max_size =
  let doc = "Maximum size" in
  Arg.(value & opt int 20 & info [ "max-size" ] ~doc ~docv:"SIZE")

let path =
  let doc = "runtime path" in
  Arg.(value & opt (some string) None & info [ "path" ] ~doc ~docv:"PATH")

let number_test =
  let doc = "Number of tests" in
  Arg.(value & opt int 20 & info [ "n" ] ~doc ~docv:"NUMBER")

let verbose =
  let doc = "Verbosity level" in
  Arg.(value & opt int 0 & info [ "v" ] ~doc ~docv:"VERBOSE")

let seed =
  let doc = "Seed of the RNG" in
  Arg.(value & opt (some int) None & info [ "seed" ] ~doc ~docv:"SEED")

let value_only =
  let doc = "only print generated value" in
  Arg.(value & flag & info [ "value-only" ] ~doc ~docv:"VONLY")

let rejection =
  let doc =
    "specify allowed variation in size, warning uses rejection sampling"
  in
  Arg.(
    value & opt (some float) None & info [ "rejection" ] ~doc ~docv:"REJECTION")

let cmd =
  let doc = "generate test based on boltzman sampling" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) generate test following $(i,FUNSPEC).";
    ]
  in
  ( Term.(
      const gen_test $ funspec $ boltzman $ exec $ reference $ max_size
      $ number_test $ verbose $ seed $ value_only $ rejection $ path),
    Term.info "boltzgen" ~version:"v0.9" ~doc ~exits:Term.default_exits ~man )

let () = Term.(exit @@ eval cmd)
