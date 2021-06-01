open Boltzgen_runtime
open Cmdliner

(*ocamlcommon.cma *)
let load_path path =
  "unix.cma " ^ "-I +compiler-libs ocamlcommon.cma -I " ^ path
  ^ "/../lib/boltzgen" ^ " -I " ^ path ^ " -I " ^ path
  ^ "/.boltzgen_runtime.objs/byte/"

(*let _ = Toploop.initialize_toplevel_env ()*)

let gen_test funcspec boltzman exec reference max number_of_call v seed
    value_only iscaseine tsrangeerror path2 =
  let path =
    match path2 with
    | None -> Filename.dirname Sys.executable_name
    | Some p -> p
  in
  Type.verbose := v;
  let td, func = Parse_from_compiler.parse_string funcspec in
  List.iter Recursive_type_gen.evaluate td;
  let z, size = Gen_test_lib.compute_boltzman func boltzman in
  let tsrange =
    match tsrangeerror with
    | None -> None
    | Some e ->
        Some
          (int_of_float ((1.0 -. e) *. size), int_of_float ((1.0 +. e) *. size))
  in

  let gen_t f =
    let fo_st = open_out "t.ml" in
    let fo = Format.formatter_of_out_channel fo_st in
    f fo (float_of_int max) number_of_call funcspec;
    Format.pp_print_flush fo ();
    close_out fo_st;

    let cmd = "ocaml " ^ load_path path ^ " boltzgen_runtime.cma t.ml" in
    if v > 1 then print_endline cmd;
    let x = Sys.command cmd in
    exit x
  in

  (*let gen_t f =
      let buff = Buffer.create 1024 in
      let fo = Format.formatter_of_buffer buff in
      f fo (float_of_int max) number_of_call funcspec;
      Format.pp_print_flush fo ();
      ignore @@ Toploop.use_file Format.std_formatter (Buffer.contents buff);
      ()
    in*)
  (match seed with None -> Random.self_init () | Some s -> Random.init s);
  match (exec, reference, value_only, iscaseine) with
  | Some fi, None, _, true ->
      gen_t
        (Gen_for_caseine.gen_test ?tsrange ~boltz_evaluated:(td, func, z)
           ~ftotest:fi)
  | _, _, _, true -> failwith "Correction file required"
  | _, _, true, _ ->
      Gen_test_lib.gen_value ?tsrange ~boltz_evaluated:(td, func, z)
        Format.std_formatter (float_of_int max) number_of_call funcspec
  | Some fi, Some fr, _, _ ->
      gen_t
        (Gen_test_lib.gen_test_diff ?tsrange ~boltz_evaluated:(td, func, z) fi
           fr)
  (*ignore @@ Sys.command "echo \"#use \\\"t.ml\\\";;\" | ocaml -I _build gen_test_lib.cma"*)
  | Some fi, None, _, _ ->
      gen_t
        (Gen_test_lib.gen_test ?tsrange ~boltz_evaluated:(td, func, z)
           ~ftotest:fi)
  (*ignore @@ Sys.command "echo \"#use \\\"t.ml\\\";;\" | ocaml -I _build gen_test_lib.cma"*)
  | None, _, _, _ ->
      Gen_test_lib.gen_test ?tsrange ~boltz_evaluated:(td, func, z)
        ~out_err:true Format.std_formatter boltzman number_of_call funcspec

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

let gen_for_caseine =
  let doc = "Generate test for Caseine" in
  Arg.(value & flag & info [ "gen-for-caseine" ] ~doc ~docv:"CASEINE")

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
      `P "$(tname) generate tests following $(i,FUNSPEC).";
    ]
  in
  ( Term.(
      const gen_test $ funspec $ boltzman $ exec $ reference $ max_size
      $ number_test $ verbose $ seed $ value_only $ gen_for_caseine $ rejection
      $ path),
    Term.info "boltzgen" ~version:"v0.9.3" ~doc ~exits:Term.default_exits ~man
  )

let () = Term.(exit @@ eval cmd)
