module M = Memprof_limits

let token = M.Token.create ()

let a () =
  Thread.delay 1. ;
  M.Token.set token

let b () =
  ignore (Thread.create a ()) ;
  let rec plic l = plic (List.rev l) in
  M.limit_allocations ~limit:10_000_000L @@ fun () ->
  plic (List.init 100 (fun n -> n))

let () =
  Memprof_limits.start_memprof_limits () ;
  match M.limit_with_token ~token b with
  | Ok (Ok _) -> failwith "???"
  | Ok (Error _) -> failwith "not stopped with token"
  | Error _ -> ()
