let tests = ref []

let add_test t = tests := t :: !tests

let add_tests xs = List.iter add_test xs

let get_last_opt x =
  let l = String.split_on_char '/' x in
  let n = List.length l in
  List.nth_opt l (n - 1)

let check_prefix x pref =
  let n = String.length x in
  let n' = String.length pref in
  n' < n && String.sub x 0 n' = pref

let get_filename () =
  Array.to_list Sys.argv
  |> List.find_map (fun x ->
         match get_last_opt x with
         | Some x
           when Filename.check_suffix x ".exe"
                && check_prefix x "inline_test_runner" ->
             Some x
         | _ -> None)

let run () =
  let tests = List.map (QCheck_alcotest.to_alcotest ~verbose:true) !tests in
  let filename =
    match get_filename () with None -> "<no filename found>" | Some x -> x
  in
  Alcotest.run filename [ ("PBT", tests) ]
