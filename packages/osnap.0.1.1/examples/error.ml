open Osnap

let add x y = x + y + 1

let test_snapshot_add () =
  let spec = Spec.(int ^> int ^>> string_of_int) in
  let path = ".osnap/error_add.osnap" in

  let test = Test.make ~path ~count:10 ~spec ~name:"add" add in

  Runner.(run_tests ~mode:Error [ test ])

let _ = test_snapshot_add ()
