open OUnit2

module Core = struct

  let suite =
    "Core" >:::
    [ "Name" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          assert_equal
            ~ctxt
            ~cmp:[%eq: string]
            ~printer:[%show: string]
            name
            (Mock.name mock)
      end
    ; "Not configured (implicit)" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          assert_raises
            (Mock.Mock_not_configured "mock name")
            (Mock.call mock);
          assert_equal
            ~ctxt
            ~cmp:[%eq: unit list]
            ~printer:[%show: unit list]
            []
            (Mock.recorded_calls mock)
      end
    ; "Not configured (explicit)" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock Mock.not_configured;
          assert_raises
            (Mock.Mock_not_configured "mock name")
            (Mock.call mock);
          assert_equal
            ~ctxt
            ~cmp:[%eq: unit list]
            ~printer:[%show: unit list]
            []
            (Mock.recorded_calls mock)
      end
    ; "Return value" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          Mock.call mock ();
          assert_equal
            ~ctxt
            ~cmp:[%eq: unit list]
            ~printer:[%show: unit list]
            [()]
            (Mock.recorded_calls mock)
      end
    ; "Exception" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let exception My_exception in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.raise My_exception);
          assert_raises
            My_exception
            (Mock.call mock);
          assert_equal
            ~ctxt
            ~cmp:[%eq: unit list]
            ~printer:[%show: unit list]
            [()]
            (Mock.recorded_calls mock)
      end
    ; "Call order is preserved" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          Mock.call mock 0;
          Mock.call mock 1;
          Mock.call mock 2;
          assert_equal
            ~ctxt
            ~cmp:[%eq: int list]
            ~printer:[%show: int list]
            [0; 1; 2]
            (Mock.recorded_calls mock)
      end
    ; "Configure resets calls" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          Mock.call mock ();
          Mock.call mock ();
          Mock.call mock ();
          Mock.configure mock (Mock.return ());
          assert_equal
            ~ctxt
            ~cmp:[%eq: unit list]
            ~printer:[%show: unit list]
            []
            (Mock.recorded_calls mock)
      end
    ]
end

module OUnit = struct

  let test_assert_calls_are =
    let setup () =
      let name = "mock name" in
      let mock = Mock.make ~name in
      Mock.configure mock (Mock.return ());
      Mock.call mock ();
      Mock.call mock ();
      mock
    in
    "assert_calls_are" >:::
    [ "They are" >::
      begin
        fun ctxt ->
          let mock = setup () in
          Mock_ounit.assert_calls_are
            ~ctxt
            ~cmp:[%eq: unit]
            ~printer:[%show: unit]
            [(); ()]
            mock
      end
    ; "They are not" >::
      begin
        fun ctxt ->
          let mock = setup () in
          assert_raises
            (OUnitTest.OUnit_failure "expected: [()] but got: [(), ()]")
            (fun () ->
               Mock_ounit.assert_calls_are
                 ~ctxt
                 ~cmp:[%eq: unit]
                 ~printer:[%show: unit]
                 [()]
                 mock
            )
      end
    ]

  let test_assert_called_once_with =
    "assert_called_once_with" >:::
    [ "Called once with correct value" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          Mock.call mock ();
          Mock_ounit.assert_called_once_with
            ~ctxt
            ~cmp:[%eq: unit]
            ~printer:[%show: unit]
            ()
            mock
      end
    ; "Called once with incorrect value" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          Mock.call mock 1;
          assert_raises
            (OUnitTest.OUnit_failure
               "when comparing mock name calls\nexpected: 0 but got: 1"
            )
            (fun () ->
               Mock_ounit.assert_called_once_with
                 ~ctxt
                 ~cmp:[%eq: int]
                 ~printer:[%show: int]
                 0
                 mock
            )
      end
    ; "Never called" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          assert_raises
            (OUnitTest.OUnit_failure
               "expected mock name to be called, but it was never called"
            )
            (fun () ->
               Mock_ounit.assert_called_once_with
                 ~ctxt
                 ~cmp:[%eq: int]
                 ~printer:[%show: int]
                 0
                 mock
            )
      end
    ; "Called several times" >::
      begin
        fun ctxt ->
          let name = "mock name" in
          let mock = Mock.make ~name in
          Mock.configure mock (Mock.return ());
          Mock.call mock 0;
          Mock.call mock 1;
          assert_raises
            (OUnitTest.OUnit_failure
               "expected mock name to be called once, but it was called 2 times"
            )
            (fun () ->
               Mock_ounit.assert_called_once_with
                 ~ctxt
                 ~cmp:[%eq: int]
                 ~printer:[%show: int]
                 0
                 mock
            )
      end
    ]

  let suite =
    "OUnit" >:::
    [ test_assert_calls_are
    ; test_assert_called_once_with
    ]
end

let suite =
  "Mock" >:::
  [ Core.suite
  ; OUnit.suite
  ]

let () = run_test_tt_main suite
