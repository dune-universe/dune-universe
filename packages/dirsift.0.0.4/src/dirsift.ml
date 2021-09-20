open Cmdliner

let config_path =
  Printf.sprintf "%s/.config/dirsift/config" (Unix.getenv "HOME")

type config = {
  hot_upper_bound : int64;
  warm_upper_bound : int64;
}

let default_config =
  {
    hot_upper_bound = Timedesc.Span.(For_human.make_exn ~days:7 () |> get_s);
    warm_upper_bound = Timedesc.Span.(For_human.make_exn ~days:30 () |> get_s);
  }

let hot_upper_bound_key = "hot_upper_bound"

let warm_upper_bound_key = "warm_upper_bound"

let config : config ref = ref default_config

let config_of_toml_table (toml : Otoml.t) : (config, string) result =
  let exception Invalid_data of string in
  try
    let hot_upper_bound =
      match Otoml.(find_opt toml get_string [ hot_upper_bound_key ]) with
      | None -> default_config.hot_upper_bound
      | Some s -> (
          match Timere_parse.span s with
          | Ok x -> Timedesc.Span.(get_s x)
          | Error msg ->
              raise
                (Invalid_data
                   (Printf.sprintf "Key: %s, %s" hot_upper_bound_key msg)))
      | exception Otoml.Type_error _ ->
          raise
            (Invalid_data
               (Printf.sprintf "Invalid data for %s" hot_upper_bound_key))
    in
    let warm_upper_bound =
      match Otoml.(find_opt toml get_string [ warm_upper_bound_key ]) with
      | None -> default_config.warm_upper_bound
      | Some s -> (
          match Timere_parse.span s with
          | Ok x -> Timedesc.Span.(get_s x)
          | Error msg ->
              raise
                (Invalid_data
                   (Printf.sprintf "Key: %s, %s" warm_upper_bound_key msg)))
      | exception Otoml.Type_error _ ->
          raise
            (Invalid_data
               (Printf.sprintf "Invalid data for %s" warm_upper_bound_key))
    in
    if warm_upper_bound < hot_upper_bound then
      raise (Invalid_data "Warm upper bound is lower than hot upper bound");
    Ok { hot_upper_bound; warm_upper_bound }
  with Invalid_data msg -> Error msg

type dir_typ =
  | Git
  | Borg
  | Restic
  | Hidden
  | Hot
  | Warm
  | Cold
  | Not of dir_typ

let rec const_of_determining_dir_typ typ =
  match typ with
  | Hidden -> 0
  | Borg -> 1
  | Restic -> 1
  | Git -> 1
  | Hot -> 2
  | Warm -> 2
  | Cold -> 2
  | Not x -> const_of_determining_dir_typ x

let sort_dir_typs_by_cost l =
  List.sort
    (fun x y ->
      compare (const_of_determining_dir_typ x) (const_of_determining_dir_typ y))
    l

let most_recent_mtime_of_files_inside dir =
  FileUtil.(find True dir)
    (fun most_recent_mtime file ->
      let stat = FileUtil.stat file in
      let mtime = Int64.of_float stat.modification_time in
      max most_recent_mtime mtime)
    0L

let diff_most_recent_mtime_of_files_inside ~launch_time dir =
  Int64.sub launch_time (most_recent_mtime_of_files_inside dir)

let rec dir_matches_typ ~launch_time dir typ =
  try
    match typ with
    | Git -> FileUtil.(test Is_dir (Filename.concat dir ".git"))
    | Borg -> (
        let readme = Filename.concat dir "README" in
        try
          FileUtil.(test Is_file readme)
          &&
          let s = CCIO.with_in readme (fun ic -> CCIO.read_all ic) in
          s
          = {|This is a Borg Backup repository.
See https://borgbackup.readthedocs.io/
|}
        with _ -> false)
    | Restic ->
        FileUtil.(
          test Is_file (Filename.concat dir "config")
          && test Is_dir (Filename.concat dir "data")
          && test Is_dir (Filename.concat dir "index")
          && test Is_dir (Filename.concat dir "keys")
          && test Is_dir (Filename.concat dir "locks")
          && test Is_dir (Filename.concat dir "snapshots"))
    | Hidden -> (Filename.basename dir).[0] = '.'
    | Hot ->
        diff_most_recent_mtime_of_files_inside ~launch_time dir
        <= !config.hot_upper_bound
    | Warm ->
        let diff = diff_most_recent_mtime_of_files_inside ~launch_time dir in
        !config.hot_upper_bound < diff && diff <= !config.warm_upper_bound
    | Cold ->
        diff_most_recent_mtime_of_files_inside ~launch_time dir
        > !config.warm_upper_bound
    | Not x -> not (dir_matches_typ ~launch_time dir x)
  with Sys_error _ -> false

let run (typs : dir_typ list) (dir : string) =
  let typs = sort_dir_typs_by_cost typs in
  let subdirs =
    try Sys.readdir dir |> Array.to_list |> List.sort_uniq String.compare
    with _ -> failwith "Failed to read directory"
  in
  let launch_time = Timedesc.Timestamp.(get_s @@ now ()) in
  subdirs
  |> List.to_seq
  |> Seq.filter (fun subdir ->
         let full_path = Filename.concat dir subdir in
         CCIO.File.(exists full_path && is_directory full_path)
         && List.for_all (dir_matches_typ ~launch_time full_path) typs)
  |> (fun s ->
       if dir = "." then s
       else Seq.map (fun subdir -> Filename.concat dir subdir) s)
  |> Seq.iter print_endline

let typ_arg =
  let typs =
    [
      ("git", Git);
      ("not-git", Not Git);
      ("borg", Borg);
      ("not-borg", Not Borg);
      ("restic", Restic);
      ("not-restic", Not Restic);
      ("hidden", Hidden);
      ("not-hidden", Not Hidden);
      ("hot", Hot);
      ("warm", Warm);
      ("cold", Cold);
    ]
  in
  let doc =
    Printf.sprintf
      "$(docv) is one of %s. If multiple types are specified, they are chained \
       together by \"and\"."
      (String.concat ", " (List.map fst typs))
  in
  Arg.(value & opt_all (enum typs) [] & info [ "t"; "type" ] ~doc ~docv:"TYPE")

let dir_arg = Arg.(value & pos 0 dir "." & info [])

let cmd =
  let doc = "Search for directories by type" in
  let version =
    match Build_info.V1.version () with
    | None -> "N/A"
    | Some version -> Build_info.V1.Version.to_string version
  in
  (if CCIO.File.exists config_path && not (CCIO.File.is_directory config_path)
  then
   match Otoml.Parser.from_file_result config_path with
   | Ok table -> (
       match config_of_toml_table table with
       | Ok config' -> config := config'
       | Error msg ->
           print_endline msg;
           exit 1)
   | Error msg ->
       print_endline msg;
       exit 1);
  (Term.(const run $ typ_arg $ dir_arg), Term.info "dirsift" ~version ~doc)

let () = Term.(exit @@ Term.eval cmd)
