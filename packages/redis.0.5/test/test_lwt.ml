module Utils = struct
  module IO = Redis_lwt.Client.IO
  let spawn f ~on_complete =
    Lwt.async (fun () ->
        let open Lwt.Infix in
        let fut = f() in
        fut >|= fun x -> on_complete x)
end
module Test_lwt = Test.Make(Redis_lwt.Client)(Utils)
module Test_lwt_cluster = Test.Make(Redis_lwt.ClusterClient)(Utils)

open OUnit2
open Lwt.Infix

(* Compute fibonacci function using Redis as a memoization cache *)
module Test_lwt_fib = struct
  module C = Redis_lwt.Client
  module Cache = Redis_lwt.Cache(struct
      type key = int
      type data = int
      let cache_key i = Printf.sprintf "test_fib_%d" i
      let cache_expiration = Some 500
      let data_of_string = int_of_string
      let string_of_data = string_of_int
    end)

  let fib_ref n =
    let prev = ref 1 in
    let cur = ref 1 in
    for _i = 2 to n do
      let n = !cur in
      cur := !cur + !prev;
      prev := n
    done;
    !cur

  let check_fib n (r:C.connection) =
    let rec fib n =
      if n <= 1 then
        Lwt.return 1
      else Cache.get r n >>= function
        | Some n -> Lwt.return n
        | None ->
          let n1 = fib (n-1) in
          let n2 = fib (n-2) in
          n1 >>= fun n1 ->
          n2 >>= fun n2 ->
          let res = n1 + n2 in
          Cache.set r n res >|= fun _ -> res
    in
    let start = Unix.gettimeofday () in
    fib n >>= fun res ->
    let stop = Unix.gettimeofday () in
    let ref = fib_ref n in
    Printf.eprintf "fib %d = %d (expected: %d) in %.3fs\n" n res ref (stop -. start);
    OUnit.assert_equal ~printer:string_of_int ref res;
    Lwt.return ()

    let test_fib n = Test_lwt.bracket (check_fib n)

    let suite =
      "fib" >::: [
        "10" >:: test_fib 10;
        "20" >:: test_fib 20;
        "30" >:: test_fib 30;
        "40" >:: test_fib 40;
      ]

   let teardown =
     Test_lwt.bracket
       (fun conn ->
         C.keys conn "test_fib_*" >>= fun keys ->
         if keys <> [] then C.del conn keys >>= fun _ -> Lwt.return ()
         else Lwt.return ())
end

let suite =
  "lwt" >::: [
    Test_lwt.suite "simple";
    Test_lwt_cluster.suite "cluster";
    Test_lwt_fib.suite;
  ]

let () =
  Random.self_init ();
  let code = ref 0 in
  OUnit2.run_test_tt_main ~exit:(fun i->code := i) suite;
  Test_lwt.teardown ();
  Test_lwt_fib.teardown();
  exit @@ !code
