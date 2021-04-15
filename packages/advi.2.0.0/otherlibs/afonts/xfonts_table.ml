(* Load and return a hash table for (X) fonts. *)

let get_cache_file_name, set_cache_file_name =
  let cache_file = ref "data" in
  (fun () -> !cache_file),
  (fun fname -> cache_file := fname)
;;

let xfonts_table_get () =
  (* If there is a saved table input it and return it. *)
  if Sys.file_exists (get_cache_file_name ()) then
    let ic = open_in (get_cache_file_name ()) in
    input_value ic else
  let xfonts_table = (Hashtbl.create 1001: Xfonts.x_fonts_table) in
  let fonts = Get_xfonts.all () in
  List.iter (fun (k, f) -> Hashtbl.add xfonts_table k f) fonts;
  let oc = open_out (get_cache_file_name ()) in
  output_value oc xfonts_table;
  close_out oc;
  xfonts_table
;;

let get = xfonts_table_get
;;

let get_fresh () =
  if Sys.file_exists (get_cache_file_name ()) then begin
    begin try Sys.remove (get_cache_file_name ()) with
     | Sys_error s ->
       failwith
         (Printf.sprintf
            "Xfonts_table.get_fresh: can not remove cache file, %s"
            s)
    end;
    xfonts_table_get ()
  end else
  failwith "Xfonts_table.get_fresh: no cache file"
;;
