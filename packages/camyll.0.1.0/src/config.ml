type t = {
    src_dir : string;
    dest_dir : string;
    highlight_dir : string;
    layout_dir : string;
    partial_dir : string;
    date_rformat : string;
    date_wformat : string;
    exclude : Re.re list;
    agda_dir : string;
  }

let default =
  let fmt = "%B %d, %Y" in
  { src_dir = "site"
  ; dest_dir = "public"
  ; highlight_dir = "highlighting"
  ; layout_dir = "layouts"
  ; partial_dir = "partials"
  ; date_rformat = fmt
  ; date_wformat = fmt
  ; exclude = List.map (fun r -> Re.compile (Re.Glob.glob r)) ["*.agdai"]
  ; agda_dir = "lagda" }

let src t = Filename.concat t.src_dir

let dest t = Filename.concat t.dest_dir

let highlight t = Filename.concat t.highlight_dir

let layout t = Filename.concat t.layout_dir

let partial t = Filename.concat t.partial_dir

let agda_dest t = Filename.concat t.dest_dir t.agda_dir

let of_json : Ezjsonm.value -> t = function
  | `O assoc ->
     let src_dir =
       match List.assoc_opt "src_dir" assoc with
       | None -> default.src_dir
       | Some v -> Ezjsonm.get_string v
     and dest_dir =
       match List.assoc_opt "dest_dir" assoc with
       | None -> default.dest_dir
       | Some v -> Ezjsonm.get_string v
     and highlight_dir =
       match List.assoc_opt "highlight_dir" assoc with
       | None -> default.highlight_dir
       | Some v -> Ezjsonm.get_string v
     and layout_dir =
       match List.assoc_opt "layout_dir" assoc with
       | None -> default.layout_dir
       | Some v -> Ezjsonm.get_string v
     and partial_dir =
       match List.assoc_opt "partial_dir" assoc with
       | None -> default.partial_dir
       | Some v -> Ezjsonm.get_string v
     and agda_dir =
       match List.assoc_opt "agda_dir" assoc with
       | None -> default.agda_dir
       | Some v -> Ezjsonm.get_string v
     and date_rformat =
       match List.assoc_opt "date_rformat" assoc with
       | None -> default.date_rformat
       | Some v -> Ezjsonm.get_string v
     and date_wformat =
       match List.assoc_opt "date_wformat" assoc with
       | None -> default.date_wformat
       | Some v -> Ezjsonm.get_string v
     and exclude =
       match List.assoc_opt "exclude" assoc with
       | None -> default.exclude
       | Some v ->
          List.map (fun g -> Re.compile (Re.Glob.glob g))
            (Ezjsonm.get_strings v)
     in
     { src_dir
     ; dest_dir
     ; highlight_dir
     ; layout_dir
     ; partial_dir
     ; agda_dir
     ; date_rformat
     ; date_wformat
     ; exclude }
  | _ -> failwith "Expected an object"
