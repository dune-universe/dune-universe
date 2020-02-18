open Subscribe_sync
open Subscribe_lwt

let host = ref "127.0.0.1"
let port = ref 6379
let n = ref 100_000

let names = [
  "subscribe_sync", (fun () -> subscribe_sync !host !port);
  "subscribe_lwt", (fun () -> subscribe_lwt !host !port);
  "bench_merge_sort", (fun () -> Bench_merge_sort.run ~n:!n !host !port);
]

let () =
  let name = ref (fst @@ List.hd names) in 
  let opts = [
    "--name", Arg.Symbol (List.map fst names, (fun s -> name := s)), " pick example to run";
    "--host", Arg.Set_string host, " host to connect to";
    "--port", Arg.Set_int port, " port to connect to";
    "-n", Arg.Set_int n, " size (for benchmarks)";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> ()) "Example of usage ocaml-redis";
  match List.assoc !name names with
  | f -> f()
  | exception _ ->
    failwith @@ "no such example: " ^ !name
