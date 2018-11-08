open Record_t
open Lwt.Infix

let database = Sanddb.create_json_database "test.txt" (module Record_j)
(*let database = Sanddb.create_biniou_database "test.txt" (module Record_b)*)
let record = { year = 2018; month = 4; day = 30; data="Some data 1"}
let shadowing_record = { year = 2018; month = 5; day = 1; data="Some data 2"}

let print_record_content record =
  match record with 
  | Ok(_, data) -> Lwt_io.printf "Record: %d-%d-%d %s\n" data.year data.month data.day data.data
  | Error error -> Lwt_io.printf "Error: %s\n" (Printexc.to_string error)

let print_records read_mode records =
  Lwt_io.printf "%s:\n" read_mode >>= fun () ->
  Lwt_list.iter_p print_record_content records

let main () =
  Sanddb.insert_record database record >>= fun id ->
  Sanddb.insert_shadowing_record database id shadowing_record >>= fun _ ->
  Sanddb.read_all_records database () >>= fun records ->
  print_records "All" records >>= fun () ->
  Sanddb.read_visible_records database () >>= fun records ->
  print_records "Visible" records

let _ = Lwt_main.run @@ main()
