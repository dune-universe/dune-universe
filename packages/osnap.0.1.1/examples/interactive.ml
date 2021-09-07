open Osnap

let add x y = x + y + 1

let test_snapshot_add () =
  let spec = Spec.(int ^> int ^>> string_of_int) in
  let path = ".osnap/interactive_add.osnap" in

  let test = Test.make ~path ~count:10 ~spec ~name:"add" add in

  Runner.(run_tests ~mode:Interactive [ test ])

let _ = test_snapshot_add ()
