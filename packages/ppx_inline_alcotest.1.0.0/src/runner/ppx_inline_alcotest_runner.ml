let test_map = Hashtbl.create 100

let add_test ~path:filename ~test_name fn =
  let test : unit Alcotest.test_case = Alcotest.test_case test_name `Quick fn in
  match Hashtbl.find_opt test_map filename with
  | None -> Hashtbl.add test_map filename [test]
  | Some tests -> Hashtbl.replace test_map filename (test :: tests)


let build_test_suite () = Hashtbl.fold (fun k v acc -> (k,v) :: acc) test_map []

let get_filename () =
  let exec_name = Sys.argv.(0) in
  String.split_on_char '.' exec_name
  |> List.filter (fun v -> not (String.equal "" v))
  |> List.hd


let run () =
  let filename = get_filename () in
  let test_suite = build_test_suite () in
  Alcotest.run filename test_suite
