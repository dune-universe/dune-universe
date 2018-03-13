let get_opt f x = function
  | None -> f x
  | Some v -> v

let ctxt = Llvm.global_context ()

let get_triple m =
  match Llvm.target_triple m with
  | "" -> Llvm_target.Target.default_triple ()
  | triple -> triple

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  Llvm_target.TargetMachine.create ~triple target

let get_layout ~target m =
  match Llvm.data_layout m with
  | "" ->
      let layout = Llvm_target.TargetMachine.data_layout target in
      Llvm_target.DataLayout.as_string layout
  | layout ->
      layout

let optimize_target ~triple ~datalayout pm b m =
  match get_opt get_triple m triple with
  | "" ->
      ()
  | triple ->
      Llvm_all_backends.initialize ();
      let target = get_target ~triple in
      let datalayout = get_opt (get_layout ~target) m datalayout in
      Llvm.set_target_triple triple m;
      Llvm.set_data_layout datalayout m;
      Llvm_target.TargetMachine.add_analysis_passes pm target;
      Llvm_passmgr_builder.populate_module_pass_manager pm b

let optimize ~level ~lto ~triple ~datalayout m =
  let pm = Llvm.PassManager.create () in
  let b = Llvm_passmgr_builder.create () in
  optimize_target ~triple ~datalayout pm b m;
  Llvm_passmgr_builder.set_opt_level level b;
  if lto then begin
    Llvm_passmgr_builder.populate_lto_pass_manager
      ~internalize:true
      ~run_inliner:true
      pm
      b;
  end;
  begin match Llvm.PassManager.run_module m pm with
  | true -> () (* Ignored info: module modified by optimizer *)
  | false -> () (* Ignored info: module not modified by optimizer *)
  end;
  Llvm.PassManager.dispose pm

let main level lto triple datalayout file =
  let buf = Llvm.MemoryBuffer.of_file file in
  let m = Llvm_irreader.parse_ir ctxt buf in
  begin match level with
  | (0 | 1 | 2 | 3) as level -> optimize ~level ~lto ~triple ~datalayout m
  | -1 -> ()
  | n -> Printf.eprintf "Error: %d is not a valid optimization level." n
  end;
  Llvm.dump_module m

let term =
  let ($) = Cmdliner.Term.($) in
  let doc_opt = "Sets the optimization level. \
                 Setting it to -1 disables all optimizations." in
  let doc_lto = "Enables Link Time Optimizations." in
  let doc_triple = "Sets the target triple string. \
                    See: https://llvm.org/docs/LangRef.html#target-triple" in
  let doc_datalayout = "Sets the target data layout string. \
                        See: https://llvm.org/docs/LangRef.html#data-layout" in
  Cmdliner.Term.pure main $
  Cmdliner.Arg.(value & opt int 0 & info ~doc:doc_opt ["opt"]) $
  Cmdliner.Arg.(value & flag & info ~doc:doc_lto ["lto"]) $
  Cmdliner.Arg.(value & opt (some string) None & info ~doc:doc_triple ["triple"]) $
  Cmdliner.Arg.(value & opt (some string) None & info ~doc:doc_datalayout ["datalayout"]) $
  Cmdliner.Arg.(required & pos 0 (some file) None & info ~docv:"FILE" [])

let info =
  Cmdliner.Term.info
    ~doc:"Just a tiny LLVM-IR optimizer for testing stuff."
    ~man:[`P "This program just takes a LLVM-IR file (.ll) as input and dumps \
              the optimized LLVM-IR module in the standard output."]
    ~version:Config.version
    Config.name

let () =
  match Cmdliner.Term.eval (term, info) with
  | `Help | `Version | `Ok () -> ()
  | `Error (`Exn | `Parse | `Term) -> exit 1
