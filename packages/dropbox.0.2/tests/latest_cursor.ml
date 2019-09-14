open Lwt
module D = Dropbox_lwt_unix

let print_delta delta =
  let entries = String.concat ", " (List.map fst delta.D.entries) in
  Lwt_io.printlf "Entries: %s\nReset: %b\nhas_more: %b\n"
                 entries delta.D.reset delta.D.has_more

let main t args =
  match args with
  | [] -> D.latest_cursor t >>= fun cursor ->
          D.delta t ~cursor >>= fun delta ->
          print_delta delta
  | [path] -> D.latest_cursor ~path_prefix:path t >>= fun cursor ->
              D.delta t ~cursor >>= fun delta ->
              print_delta delta
  | _ -> Lwt_io.printlf "Usage: latest_cursor [path]"

let () =
  Common.run main
