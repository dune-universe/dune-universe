open Apero_time

let check_if b ?arg line =
  let test_name =
    match arg with
    | None -> Printf.sprintf "test line %d" line
    | Some(s) -> Printf.sprintf "test line %d with %s" line s
  in
    Alcotest.(check bool) test_name b


module HLC_Unix = HLC.Make (Clock_unix)
let hlc = HLC_Unix.create (Apero.Uuid.make_from_alias "id1")

let print_ts ts = Printf.printf "%s\n" (HLC_Unix.Timestamp.to_string ts)

let test1 () =
  let%lwt t1 = HLC_Unix.new_timestamp hlc in
  let%lwt t2 = HLC_Unix.new_timestamp hlc in
  let open HLC_Unix.Timestamp.Infix in
  check_if true  __LINE__ @@ (t2 > t1);
  Lwt.return_unit

let test2 () =
  let l = List.init 10000 (fun _ -> Lwt_main.run @@HLC_Unix.new_timestamp hlc) in
  List.mapi (fun i t -> print_ts t; if i = 0 then true else HLC_Unix.Timestamp.Infix.(t > List.nth l (i-1))) l |>
  List.iter (check_if true  __LINE__)

let all_tests = [
  "test1", `Quick, (fun () -> Lwt_main.run @@ test1 ());
  "test1", `Quick, test2;
]
