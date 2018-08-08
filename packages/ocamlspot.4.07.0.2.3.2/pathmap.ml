open Utils

let build_dir (postfix, dir, conf) =
  let open Filepath in
  match conf.Dotfile.build_dir with
  | None -> None
  | Some build_dir ->
      let postfix' = Filepath.of_string Filepath.os postfix in
      let build_dir' = Filepath.of_string Filepath.os build_dir in
      let dir = Filepath.of_string Filepath.os dir in
      (* path may be already in dir *)
      match Filepath.is_prefix build_dir' postfix' with
      | Some _ -> (* already in build_dir *)
          Format.eprintf "is_prefix check %s %s@." build_dir postfix;
          None
      | None ->
          Some ((dir ^/ build_dir) ^/ postfix)

let src_dir (postfix, dir, conf) =
  match conf.Dotfile.build_dir with
  | None -> None
  | Some build_dir ->
      let postfix' = Filepath.of_string Filepath.os postfix in
      let build_dir' = Filepath.of_string Filepath.os build_dir in
      let dir = Filepath.of_string Filepath.os dir in
      match Filepath.is_prefix build_dir' postfix' with
      | Some ds -> Some (Filepath.concats dir ds)
      | None -> (* out of build_dir *)
          None

let build_loc p =
  let dir = Filename.dirname p in
  let base = Filename.basename p in
  let base_body, _base_ext = Filename.split_extension base in
  match Dotfile.find_and_load dir with
  | None -> dir ^/ base_body
  | Some (postfix, dir', conf) ->
      prerr_endline "aapp";
      let base_body = match conf.Dotfile.module_prefix with
        | None -> base_body
        | Some s -> s ^ "__" ^ String.capitalize_ascii base_body
      in
      match build_dir (postfix, dir', conf) with
      | None -> dir ^/ base_body
      | Some fp -> Filepath.to_string fp ^/ base_body

let src_loc p =
  let dir = Filename.dirname p in
  let base = Filename.basename p in
  let base_body, _base_ext = Filename.split_extension base in
  match Dotfile.find_and_load dir with
  | None -> dir ^/ base_body
  | Some (postfix, dir, conf) ->
      (* Must fix the base_body *)
      match src_dir (postfix, dir, conf) with
      | None -> dir ^/ base_body
      | Some fp -> Filepath.to_string fp ^/ base_body
