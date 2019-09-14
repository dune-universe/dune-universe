open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the query and Sys.argv.(1) is the path to the folder we want to
    search from *)

let print_search meta_list query =
  match List.length meta_list with
  | 0 -> Lwt_io.printlf "No file or folder whose name contains %S found." query
  | _ -> let get_path m = m.D.path in
         let paths = String.concat "\n" (List.map get_path meta_list) in
         Lwt_io.printlf "%s" paths

let main t args =
  match args with
  | [query] -> D.search t query >>= fun search -> print_search search query
  | [query; fn] -> D.search t ~fn query
                  >>= fun search -> print_search search query
  | _ -> Lwt_io.printf "%s <query> [path]\n" Sys.argv.(0)

let () =
  Common.run main
