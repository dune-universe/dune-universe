include struct
  let add x y = x + y [@@pbt {| associative[int, int, int] |}]

  let test_add_is_associative =
    QCheck.Test.make
      ~name:"add_is_associative"
      (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))
      (fun (arb_0, (arb_1, arb_2)) ->
        Pbt.Properties.associative add arb_0 arb_1 arb_2)

  let () = Runner.add_tests [ test_add_is_associative ]
end
