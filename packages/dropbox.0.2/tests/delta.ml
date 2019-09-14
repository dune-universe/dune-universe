open Lwt
module D = Dropbox_lwt_unix

(** If there is no entry, we call delta without param.
    If there is one entry, we call delta with the param path_prefix as
       Sys.argv.(0).
    If there is two entries, we call delta with the param cursor as
       Sys.argv.(0) and the param path_prefix as the second. *)

let print_delta delta =
  let entries = String.concat ", " (List.map fst delta.D.entries) in
  Lwt_io.printlf "Entries: %s\nReset: %b\nhas_more: %b\n"
                 entries delta.D.reset delta.D.has_more

let rec print_deltas ?path_prefix t =
  D.delta t ?path_prefix >>= fun delta ->
  print_delta delta >>= fun () ->
  if delta.D.has_more then print_deltas_cursor t delta.D.cursor
  else return()

and print_deltas_cursor t cursor =
  D.delta t ~cursor >>= fun delta ->
  print_delta delta >>= fun () ->
  if delta.D.has_more then print_deltas_cursor t delta.D.cursor
  else return ()

let main t args =
  match args with
  | [] -> print_deltas t
  | [path_prefix] -> print_deltas t ~path_prefix
  | _ -> Lwt_io.printlf "Usage: delta [path_prefix]"

let () =
  Common.run main
