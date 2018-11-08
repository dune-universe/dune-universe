open Printf

module String = struct
  include String

  (* Make sure this exists even for OCaml < 4.04.0 *)
  let split_on_char sep s =
    let r = ref [] in
    let j = ref (length s) in
    for i = length s - 1 downto 0 do
      if unsafe_get s i = sep then begin
        r := sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    sub s 0 !j :: !r

  (* Naive substring detection *)
  let rec is_substring_pos j p lenp i s lens =
    if j >= lenp then true
    else if i >= lens then false
    else if p.[j] = s.[i] then is_substring_pos (j+1) p lenp (i+1) s lens
    else false
  let rec is_substring_loop p lenp i s lens =
    if is_substring_pos 0 p lenp i s lens then true
    else if i >= lens then false
    else is_substring_loop p lenp (i+1) s lens
  let is_substring ~sub:p s =
    is_substring_loop p (String.length p) 0 s (String.length s)
end

module List = struct
  include List

  let rec find_map l ~f =
    match l with
    | [] -> None
    | x :: l -> match f x with
                | None -> find_map l ~f
                | Some _ as res -> res
end

(* Adapted from Configurator — until it is exported. *)
module Find_in_path = struct
  let path_sep =
    if Sys.win32 then
      ';'
    else
      ':'

  let get_path () =
    match Sys.getenv "PATH" with
    | exception Not_found -> []
    | s -> String.split_on_char path_sep s

  let exe = if Sys.win32 then ".exe" else ""

  let find prog =
    List.find_map (get_path ()) ~f:(fun dir ->
        let fn = Filename.concat dir (prog ^ exe) in
        if Sys.file_exists fn then Some fn else None)
end

let read_file fname =
  let buf = Buffer.create 4096 in
  let fh = open_in_bin fname in
  Buffer.add_channel buf fh (in_channel_length fh);
  close_in fh;
  Buffer.contents buf

(* Inspired from Dune — until it is exported. *)
let run cmd =
  let stdout_fn = Filename.temp_file "stdout-" ".bin" in
  let stderr_fn = Filename.temp_file "stderr-" ".bin" in
  let exit_code =
    Printf.ksprintf
      Sys.command "cd %s && %s > %s 2> %s"
      (Filename.quote (Filename.get_temp_dir_name()))
      cmd
      (Filename.quote stdout_fn)
      (Filename.quote stderr_fn)
  in
  let stdout = read_file stdout_fn in
  let stderr = read_file stderr_fn in
  Sys.remove stdout_fn;
  Sys.remove stderr_fn;
  if exit_code <> 0 then
    eprintf "Command %s terminated with code %d." cmd exit_code;
  (stdout, stderr)

let () =
  let version =
    match Find_in_path.find "bwrap" with
    | Some bwrap ->
       let (o, _e) = run (bwrap ^ " --version") in
       (match String.split_on_char ' ' o with
        | _ :: v :: _ -> String.trim v
      | _ ->
         eprintf "bwrap --version returned %S, could not extract version.\n" o;
         "0.0.0")
    | None ->
       eprintf "Command \"bwrap\" not found in path\n";
       "0.0.0" in
  let fh = open_out "bwrap_version.txt" in
  fprintf fh "%s" version;
  close_out fh

