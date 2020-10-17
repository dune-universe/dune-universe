type 'a doc = {
    name : string;
    subdir : string;
    frontmatter : Yaml.value option;
    content : 'a;
  }

type t = {
    config : Config.t;
    langs : TmLanguage.t;
    layouts : (string, Mustache.t) Hashtbl.t;
    partials : (string, Mustache.t) Hashtbl.t;
  }

let parse_frontmatter chan =
  try
    let line = input_line chan in
    if line = "---" then
      let buf = Buffer.create 100 in
      let rec loop () =
        let line = input_line chan in
        if line = "---" then
          ()
        else (
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
        )
      in
      loop ();
      match Yaml.of_string (Buffer.contents buf) with
      | Ok yaml -> Some yaml
      | Error (`Msg e) -> failwith e
    else (
      seek_in chan 0;
      None
    )
  with
  | End_of_file ->
     seek_in chan 0;
     None

(** Highlight the code blocks *)
let highlight t =
  List.map (fun block ->
      match block with
      | Omd.Code_block(lang, code) ->
         begin match lang with
         | "" -> Omd.Code_block("", code)
         | lang ->
            match
              TmLanguage.find_by_name t.langs lang
            with
            | None ->
               prerr_endline ("Warning: unknown language " ^ lang);
               Omd.Code_block(lang, code)
            | Some grammar ->
               Omd.Raw
                 (Soup.pretty_print
                    (Highlight.highlight_block t.langs grammar code))
         end
      | x -> x)

type doctype =
  | Bin
  | Doc of string doc

let copy_file output_path in_chan =
  (* Copy file exactly *)
  Filesystem.with_out_bin (fun out_chan ->
      output_string out_chan (Filesystem.read_bytes in_chan)
    ) output_path

let process_md t chan =
  Omd.to_html (highlight t (Omd.of_string (Filesystem.read_lines chan)))

let dispatch t subdir name =
  match Filename.chop_suffix_opt ~suffix:".lagda.md" name with
  | Some name ->
     let path =
       if subdir = "" then
         t.config.Config.src_dir ^ "." ^ name
       else
         t.config.Config.src_dir ^ "."
         ^ (String.split_on_char '/' subdir
            |> String.concat (String.make 1 '.'))
         ^ "." ^ name
     in
     Filesystem.with_in_bin (fun chan ->
         let frontmatter = parse_frontmatter chan in
         Doc { name = name ^ ".html"
             ; subdir
             ; frontmatter
             ; content = process_md t chan }
       ) (Filename.concat (Config.agda_dest t.config) path ^ ".md")
  | None ->
     let path = Filename.concat subdir name in
     match Filename.chop_suffix_opt ~suffix:".html" name with
     | Some _ ->
        Filesystem.with_in_bin (fun chan ->
            let frontmatter = parse_frontmatter chan in
            Doc { name
                ; subdir
                ; frontmatter
                ; content = Filesystem.read_lines chan }
          ) (Config.src t.config path)
     | None ->
        match Filename.chop_suffix_opt ~suffix:".md" name with
        | Some name ->
           Filesystem.with_in_bin (fun chan ->
               let frontmatter = parse_frontmatter chan in
               Doc { name = name ^ ".html"
                   ; subdir
                   ; frontmatter
                   ; content = process_md t chan }
             ) (Config.src t.config path)
        | None ->
           Filesystem.with_in_bin (fun chan ->
               let output_path = Config.dest t.config path in
               copy_file output_path chan
             ) (Config.src t.config path);
           Bin

let concat_urls left right =
  let left_len =
    let len = String.length left in
    if len = 0 then
      0
    else if String.get left (len - 1) = '/' then
      len - 1
    else
      len
  in
  let right_len =
    let len = String.length right in
    if len = 0 then
      0
    else if String.get right 0 = '/' then
      len - 1
    else
      len
  in
  let bytes = Bytes.create (left_len + right_len + 1) in
  Bytes.blit_string left 0 bytes 0 left_len;
  Bytes.set bytes left_len '/';
  Bytes.blit_string right 0 bytes (left_len + 1) right_len;
  Bytes.unsafe_to_string bytes

let correct_agda_urls t node =
  let open Soup.Infix in
  Soup.iter (fun node ->
      match Soup.attribute "href" node with
      | None -> failwith "Unreachable: href"
      | Some link ->
         let root_mod = t.config.Config.src_dir in
         let root_len = String.length t.config.Config.src_dir in
         let link_len = String.length link in
         if link_len >= root_len && String.sub link 0 root_len = root_mod then
           (* The link is to an internal module *)
           match String.split_on_char '.' link with
           | [] -> failwith "unreachable"
           | _ :: parts ->
              let rec loop acc = function
                | [] -> failwith "unreachable"
                | [ext] -> acc ^ "." ^ ext
                | x :: xs -> loop (acc ^ "/" ^ x) xs
              in
              Soup.set_attribute "href" (loop "" parts) node
         else
           (* The link is to an external module *)
           let link = "/" ^ (concat_urls t.config.Config.agda_dir link) in
           Soup.set_attribute "href" link node
    ) (node $$ "pre[class=\"Agda\"] > a[href]")

let relativize_urls depth node =
  let open Soup.Infix in
  let replace attr =
    Soup.iter (fun node ->
        match Soup.attribute attr node with
        | None -> failwith ("Unreachable: " ^ attr)
        | Some link ->
           if String.get link 0 = '/' then
             let sub = String.sub link 1 (String.length link - 1) in
             let buf = Buffer.create 5 in
             for _ = 1 to depth do
               Buffer.add_string buf "../"
             done;
             Buffer.add_string buf sub;
             Soup.set_attribute attr (Buffer.contents buf) node
      ) (node $$ ("[" ^ attr ^ "]"))
  in
  replace "href";
  replace "src"

let list_page_metadata t pages =
  List.filter_map (fun page ->
      Option.bind page.frontmatter (fun yaml ->
          let obj = Ezjsonm.get_dict yaml in
          Option.map (fun published ->
              let date =
                published
                |> Ezjsonm.get_string
                |> CalendarLib.Printer.Date.from_fstring t.config.date_rformat
              in (date, page.name, obj))
            (List.assoc_opt "published" obj))
    ) pages
  |> List.sort (fun (d1, _, _) (d2, _, _) -> CalendarLib.Date.compare d2 d1)
  |> List.map (fun (_, url, obj) -> `O (("url", `String url) :: obj))

let render partials template yaml content =
  Mustache.render ~partials template (`O (("content", `String content) :: yaml))

let get_layout t frontmatter =
  match frontmatter with
  | Some (`O attrs) ->
     begin match List.assoc_opt "layout" attrs with
     | None -> None
     | Some (`String name) -> Some (Hashtbl.find t.layouts name)
     | Some _ -> failwith "Template name not a string"
     end
  | Some _ -> failwith "Frontmatter not an object"
  | None -> None

let compile_doc t depth pages { name; subdir; frontmatter; content } =
  let path = Filename.concat subdir name in
  let output_path = Filename.concat t.config.Config.dest_dir path in
  let yaml = match frontmatter with
    | Some yaml -> ["pages", `A pages; "page", yaml]
    | None -> ["pages", `A pages]
  in
  let content = match get_layout t frontmatter with
    | Some template ->
       render (Hashtbl.find_opt t.partials) template yaml content
    | None -> content
  in
  let output = Soup.parse content in
  correct_agda_urls t output;
  relativize_urls depth output;
  Filesystem.with_out (fun out_chan ->
      output_string out_chan (Soup.pretty_print output)
    ) output_path

let rec compile_dir t depth subdir =
  let src = Config.src t.config subdir in
  let dest = Config.dest t.config subdir in
  Filesystem.mkdir dest;
  let pages =
    Filesystem.fold (fun pages name ->
        let path = Filename.concat subdir name in
        if Sys.is_directory (Config.src t.config path) then (
          compile_dir t (depth + 1) path;
          pages
        ) else
          if List.exists (Fun.flip Re.execp path) t.config.Config.exclude then
            pages
          else
            match dispatch t subdir name with
            | Bin -> pages
            | Doc doc -> doc :: pages
      ) [] src
  in
  let metadata = list_page_metadata t pages in
  List.iter (compile_doc t depth metadata) pages

(** Highlight Literate Agda files *)
let preprocess_agda html_dir dir =
  let rec spawn_processes pids dir =
    Filesystem.fold (fun pids name ->
        let path = Filename.concat dir name in
        if Sys.is_directory path then
          spawn_processes pids path
        else
          match Filename.chop_suffix_opt ~suffix:".lagda.md" name with
          | None -> pids
          | Some _ ->
             let pid =
               Unix.create_process "agda"
                 [| "agda"; path; "--html"; "--html-highlight=auto"
                  ; "--html-dir=" ^ html_dir |]
                 Unix.stdin Unix.stdout Unix.stderr
             in pid :: pids
      ) pids dir
  in
  spawn_processes [] dir
  |> List.map (Unix.waitpid [])
  |> List.iter (function
         | _, Unix.WEXITED 0 -> ()
         | _, Unix.WEXITED n ->
            failwith ("Exit code: " ^ Int.to_string n)
         | _, Unix.WSIGNALED _ ->
            failwith "Killed by signal"
         | _, Unix.WSTOPPED _ ->
            failwith "Stopped by signal")

let rec iterate_fixed_point f x =
  let y = f x in
  if x = y then
    x
  else
    iterate_fixed_point f y

let remove_longest_ext = iterate_fixed_point Filename.remove_extension

let build config =
  Filesystem.mkdir config.Config.dest_dir;
  let layouts = Hashtbl.create 11 in
  Filesystem.iter (fun name ->
      let path = Config.layout config name in
      if Sys.is_directory path then
        ()
      else
        let name = remove_longest_ext name in
        try
          Filesystem.read path
          |> Mustache.of_string
          |> Hashtbl.add layouts name
        with
        | Invalid_argument s ->
           failwith ("Invalid_argument " ^ s ^ ": " ^ name)
    ) config.Config.layout_dir;
  let partials = Hashtbl.create 11 in
  Filesystem.iter (fun name ->
      let path = Config.partial config name in
      if Sys.is_directory path then
        ()
      else
        let s = Filesystem.read path in
        let name = remove_longest_ext name in
        Hashtbl.add partials name (Mustache.of_string s)
    ) config.Config.partial_dir;
  let t =
    { config
    ; langs = TmLanguage.create ()
    ; layouts
    ; partials }
  in
  begin
    try
      Filesystem.iter (fun name ->
          if Sys.is_directory (Config.highlight t.config name) then
            ()
          else
            try
              let lang =
                Filesystem.with_in (fun chan ->
                    Markup.channel chan
                    |> Plist_xml.parse_exn
                    |> TmLanguage.of_plist_exn
                  ) (Config.highlight t.config name)
              in
              TmLanguage.add_grammar t.langs lang
            with
            | Plist_xml.Parse_error s ->
               failwith ("Parse_error " ^ s ^ ": " ^ name)
        ) t.config.Config.highlight_dir
    with Unix.Unix_error(Unix.ENOENT, "opendir", "highlighting") -> ()
  end;
  Filesystem.remove_dir t.config.Config.dest_dir;
  preprocess_agda (Config.agda_dest config) (Config.src config "");
  ignore (compile_dir t 0 "")
