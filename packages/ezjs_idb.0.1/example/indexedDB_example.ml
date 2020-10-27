open Ezjs_min
open Ezjs_idb
open Types

let log_test i o =
  Firebug.console##debug_2 (string ("TEST " ^ string_of_int i)) o

module Store = Store(StringTr)(StringTr)

let test_db = "test_db"
let test_table = "test_table"

let () =
  let upgrade (db : iDBDatabase t) e =
    if e.new_version >= 1 && e.old_version = 0 then
      ignore @@ Store.create db test_table in
  let error r = log_test 0 r##.error in
  openDB ~upgrade ~error ~version:1 test_db @@ fun db ->

  let add_button = Dom_html.getElementById "add-button" in
  let remove_button = Dom_html.getElementById "remove-button" in
  let clear_button = Dom_html.getElementById "clear-button" in
  let show_button = Dom_html.getElementById "show-button" in

  (Unsafe.coerce add_button)##.onclick := wrap_callback (fun _e ->
      let st = Store.store ~mode:READWRITE db test_table in
      let key = to_string (Unsafe.coerce @@ Dom_html.getElementById "add-key")##.value in
      let value = to_string (Unsafe.coerce @@ Dom_html.getElementById "add-value")##.value in
      Store.add ~key ~callback:(fun key ->
          log "(%s, %s) added to %s" key value test_table) st value);

  (Unsafe.coerce remove_button)##.onclick := wrap_callback (fun _e ->
      let st = Store.store ~mode:READWRITE db test_table in
      let key = to_string (Unsafe.coerce @@ Dom_html.getElementById "remove-key")##.value in
      Store.delete ~callback:(fun _ ->
        log "%s removed from db" key) st (Store.K key));

  (Unsafe.coerce clear_button)##.onclick := wrap_callback (fun _e ->
      let st = Store.store ~mode:READWRITE db test_table in
      Store.clear ~callback:(fun () -> log_str "db cleared") st);

  (Unsafe.coerce show_button)##.onclick := wrap_callback (fun _e ->
      let lower = to_string (Unsafe.coerce @@ Dom_html.getElementById "show-lower")##.value in
      let upper = to_string (Unsafe.coerce @@ Dom_html.getElementById "show-upper")##.value in
      let key = match lower, upper with
        | "", "" -> None
        | l, u ->
          Some (Store.range
                  ?lower:(if l = "" then None else Some l)
                  ?upper:(if u = "" then None else Some u) ()) in
      let st = Store.store ~mode:READWRITE db test_table in
      Store.fold ?key st (fun key data acc -> acc ^ Printf.sprintf "%s %s<br/>" key data) ""
        (fun s ->
           (Dom_html.getElementById "show-div")##.innerHTML := string s))
