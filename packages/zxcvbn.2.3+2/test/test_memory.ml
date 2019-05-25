(** See https://github.com/cryptosense/ocaml-zxcvbn/issues/4
    Run:
    `dune build test/test_memory.exe && valgrind ./_build/default/test/test_memory.native > /dev/null`
    to detect memory corruption. *)
let test_issue_4 () =
  let random_char () = Char.chr (1 + Random.int 126) in
  let random_pass () = String.init 10 (fun _ -> random_char ()) in
  let check_random_pass () =
    let s = random_pass () in
    let (guesses, _) = Zxcvbn.matches s in
    Printf.printf "%S: %f\n%!" s guesses
  in
  while true do
    check_random_pass ();
  done

let () = test_issue_4 ()
