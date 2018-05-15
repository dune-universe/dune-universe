module SHA1 = Digestif.SHA1.Bytes
module Radix = Radis.Make(struct type t = Bytes.t let get = Bytes.unsafe_get let length _ = Digestif.SHA1.digest_size let equal = Digestif.SHA1.Bytes.eq end)
module Map = Map.Make(Bytes)

let drain buf chan =
  try while true do Buffer.add_channel buf chan 1 done
  with End_of_file -> ()

let output_of_commands ?(env = Unix.environment ()) ?(input = (fun _ -> ())) command =
  let ic, oc, ec = Unix.open_process_full command env in
  let () = input oc in
  close_out oc;
  let buf_out = Buffer.create 16 in
  let buf_err = Buffer.create 16 in
  drain buf_out ic;
  drain buf_err ec;
  let _ = Unix.close_process_full (ic, oc, ec) in
  Buffer.contents buf_out, Buffer.contents buf_err

let objects =
  let out, _ = output_of_commands "git rev-list --objects --all" in
  List.fold_left
    (fun acc line ->
      try match Astring.String.cut ~sep:" " line with
          | Some (hash, value) -> (SHA1.of_hex (Bytes.unsafe_of_string hash), value) :: acc
          | None -> (SHA1.of_hex (Bytes.unsafe_of_string line), "") :: acc
      with _ -> acc)
    []
    (Astring.String.cuts ~sep:"\n" out)

let take lst n =
  let rec go acc rest = function
    | 0 -> acc
    | n -> match rest with
           | [] -> invalid_arg "Not enough data"
           | x :: rest -> go (x :: acc) rest (n - 1) in
  if n < 0 then invalid_arg "take" else go [] lst n

let objects_10 = take objects 10

let radix lst = List.fold_left (fun map (k, v) -> Radix.add k v map) Radix.empty lst
let map lst = List.fold_left (fun map (k, v) -> Map.add k v map) Map.empty lst
let hashtbl ?(size = 128) lst =
  let hashtbl = Hashtbl.create size in
  List.iter (fun (k, v) -> Hashtbl.add hashtbl k v) lst;
  hashtbl

let bench_add ?(time = 3) ?(repeat = 3) n =
  let lst = take objects n in
  let radix ()   = ignore @@ radix lst in
  let map ()     = ignore @@ map lst in
  let hashtbl () = ignore @@ hashtbl lst in
  Benchmark.throughputN time ~repeat
    [ "radix",   radix,   ()
    ; "map",     map,     ()
    ; "hashtbl", hashtbl, () ]

let bench_lookup_one ?(time = 3) ?(repeat = 3) key n =
  let lst = take objects n in

  if not (List.mem_assoc key lst)
  then invalid_arg (Fmt.strf "Invalid key: %a" SHA1.pp key);

  let r = radix lst in
  let m = map lst in
  let h = hashtbl lst in
  let radix ()   = ignore @@ Radix.find_opt key r in
  let map ()     = ignore @@ Map.find_opt key m in
  let hashtbl () = ignore @@ Hashtbl.find_opt h key in
  Benchmark.throughputN time ~repeat
    [ "radix",   radix,   ()
    ; "map",     map,     ()
    ; "hashtbl", hashtbl, () ]

let bench_lookup_all ?(time = 3) ?(repeat = 3) n =
  let lst = take objects n in
  let key = List.map fst lst in
  let r = radix lst in
  let m = map lst in
  let h = hashtbl lst in
  let radix   () = List.iter (fun k -> ignore @@ Radix.find_opt k r) key in
  let map     () = List.iter (fun k -> ignore @@ Map.find_opt k m) key in
  let hashtbl () = List.iter (fun k -> ignore @@ Hashtbl.find_opt h k) key in
  Benchmark.throughputN time ~repeat
    [ "radix",   radix,   ()
    ; "map",     map,     ()
    ; "hashtbl", hashtbl, () ]

let bench_mem_one ?(time = 3) ?(repeat = 3) (n, key) =
  let lst = take objects n in
  let r = radix lst in
  let m = map lst in
  let radix () = ignore @@ Radix.mem key r in
  let map   () = ignore @@ Map.mem key m in
  Benchmark.throughputN time ~repeat
    [ "radix", radix, ()
    ; "map",   map,   () ]

let app_int f n = Benchmark.Tree.(string_of_int n @> lazy (f n))

let () =
  let open Benchmark.Tree in
  let repeat, time = 3, 5 in
  register
    ("radis" @>>>
       [ "add" @>>
           concat
             [ app_int (bench_add ~repeat ~time) 10
             ; app_int (bench_add ~repeat ~time) 100
             ; app_int (bench_add ~repeat ~time) 1000 ]
       ; "lookup_all" @>>
           concat
             [ app_int (bench_lookup_all ~repeat ~time) 10
             ; app_int (bench_lookup_all ~repeat ~time) 100
             ; app_int (bench_lookup_all ~repeat ~time) 1000 ] ])

let () =
  try Benchmark.Tree.run_global ()
  with Arg.Help msg -> print_endline msg
