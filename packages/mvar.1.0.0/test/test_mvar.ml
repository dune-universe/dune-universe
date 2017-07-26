open OUnit2

let test_empty _ =
  let mvar = Mvar.create_empty () in
  assert_bool
    "create_empty should create an empty mvar"
    (Mvar.is_empty mvar);
  assert_equal
    ~msg:"try_take on an empty mvar should return None"
    (Mvar.try_take mvar) None;
  assert_bool
    "try_put on an empty mvar should return true"
    (Mvar.try_put mvar "hello")

let test_not_empty _ =
  let mvar = Mvar.create "hello" in
  assert_bool
    "create should not create an empty mvar"
    (not (Mvar.is_empty mvar));
  assert_bool
    "try_put on a non-empty mvar should return false"
    (not (Mvar.try_put mvar "hello"));
  assert_equal
    ~msg:"try_take on a non-empty mvar should return a value"
    (Mvar.try_take mvar) (Some "hello")

let test_take_leaves_empty _ =
  let mvar = Mvar.create "hello" in
  assert_equal
    ~msg:"take on a non-empty mvar should return a value"
    (Mvar.take mvar) ("hello");
  assert_bool
    "take should leave an empty mvar"
    (Mvar.is_empty mvar)

let test_put_leaves_non_empty _ =
  let mvar = Mvar.create_empty () in
  Mvar.put mvar "hello";
  assert_bool
    "put should leave an empty non-empty"
    (not (Mvar.is_empty mvar))

let test_swap _ =
  let mvar = Mvar.create "hello" in
  assert_equal
    ~msg:"swap should return the previous mvar contents"
    (Mvar.swap mvar "goodbye") ("hello");
  assert_equal
    ~msg:"swap should put the new value in the mvar"
    (Mvar.take mvar) ("goodbye")

let test_modify _ =
  let mvar = Mvar.create "hello" in 
  Mvar.modify mvar (fun str -> str ^ str);
  assert_equal
    ~msg:"modify should modify the value in the mvar"
    (Mvar.take mvar) ("hellohello")

let sequential =
  "sequential" >:::
    [
      "test_empty" >:: test_empty;
      "test_not_empty" >:: test_not_empty;
      "test_take_leaves_empty" >:: test_take_leaves_empty;
      "test_put_leaves_non_empty" >:: test_put_leaves_non_empty;
      "test_swap" >:: test_swap;
      "test_modify" >:: test_modify;
    ]

let test_concurrent_put_take _ =
  let mvar = Mvar.create_empty () in
  let put_thread () =
    for _ = 1 to 100 do
      Mvar.put mvar "hello"
    done
  in
  let take_thread () =
    for _ = 1 to 100 do
      ignore(Mvar.take mvar)
    done
  in
  let thread1 = Thread.create put_thread () in
  let thread2 = Thread.create take_thread () in
  Thread.join thread1;
  Thread.join thread2

let test_concurrent_increment _ =
  let mvar = Mvar.create 0 in
  let increment_by_100 () =
    for _ = 1 to 100 do
      Mvar.modify mvar (fun x -> x + 1);
    done
  in
  let thread1 = Thread.create increment_by_100 () in
  let thread2 = Thread.create increment_by_100 () in
  Thread.join thread1;
  Thread.join thread2;
  assert_equal
    ~msg:"mvar should contain 200"
    (Mvar.take mvar) 200

let concurrent =
  "concurrent" >:::
    [
      "test_concurrent_put_take" >:: test_concurrent_put_take;
      "test_concurrent_increment" >:: test_concurrent_increment;
    ]

let base_suite =
  "base_suite" >:::
    [
      sequential;
      concurrent;
    ]

let () =
  OUnit2.run_test_tt_main base_suite
