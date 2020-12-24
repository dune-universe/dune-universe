
let with_command ~command ~strict test =
  match Sys.command ("command -v " ^ command) with
  | 0 -> test ()
  | _ ->
    if strict then failwith ("Command " ^ command ^ " not present")
    else Printf.printf "!!! Skipping test because command %s is not available" command

(* Each test script takes the path to the NBD CLI as its first command line argument *)
let script name ~requires (cli, strict) =
  with_command
    ~command:requires
    ~strict
    (fun () ->
       Alcotest.(check int)
         name
         0
         (Sys.command (name ^ " " ^ cli))
    )

let opts =
  let cli =
    let doc = "Path to nbd CLI should be first command-line argument" in
    Cmdliner.Arg.(required & opt ~vopt:None (some string) None & info ["cli"] ~docv:"CLI" ~doc)
  in
  let strict =
    let doc = {|If present, the test will fail when the required program is not
                installed. Otherwise the test will simply be skipped.|}
    in
    Cmdliner.Arg.(value & flag & info ["strict"] ~env:(env_var "STRICT") ~doc)
  in
  Cmdliner.Term.(const (fun cli strict -> (cli, strict)) $ cli $ strict)

let () =
  Alcotest.run_with_args
    "Nbd CLI interoperability tests"
    opts
    [ ("NBD CLI interoperability tests",
       [ "data copying with qemu-img", `Quick, script "./test-qemu.sh" ~requires:"qemu-img"
       ; "listing exports with nbd-client", `Quick, script "./test-nbd-client.sh" ~requires:"nbd-client"
       ]
      )
    ; ("Stress tests",
       [ "Misbehaving clients sending random data", `Slow, script "./random_data.sh" ~requires:"nc"
       ]
      )
    ]
