(* Extract source files from tarballs and submit them to MOSS, using
   the tarball filename to construct the directory name. *)

open Printf

module Tar = struct
  let extract ~in_dir tarball =
    let cmd = sprintf "tar --force-local -C %s -x -f %s"
                  (Filename.quote in_dir) (Filename.quote tarball) in
    let r = Sys.command cmd in
    if r <> 0 then
      eprintf "Extracting %S terminated with exit code %d\n%!"
        tarball r

end

let mkdir d =
  try Unix.mkdir d 0o777 with _ -> ()

(* Remove the directory [d] (and all its content). *)
let rec rmdir d =
  if Sys.is_directory d then
    let files = Sys.readdir d in
    Array.iter (fun fn -> rmdir(Filename.concat d fn)) files;
    Unix.rmdir d
  else
    Sys.remove d

let rec flatten_into d ~target =
  let c = Array.to_list(Sys.readdir d) in
  let c = List.map (fun fn -> (fn, Filename.concat d fn)) c in
  let dirs, files  = List.partition (fun (_,f) -> Sys.is_directory f) c in
  List.iter (fun (fn, fn_full) ->
      Sys.rename fn_full (Filename.concat target fn)) files;
  List.iter (fun (_, dir) -> flatten_into dir ~target) dirs;
  Unix.rmdir d (* assume the target is not inside [d]. *)

let flatten_dir d =
  let dirs = Array.to_list(Sys.readdir d) in
  let dirs = List.map (fun dir -> Filename.concat d dir) dirs in
  let dirs = List.filter Sys.is_directory dirs in
  List.iter (fun dir -> flatten_into dir ~target:d) dirs


(* Assume the tarball have names such as <name>@extra.tgz *)
let add_files_of_tarball ~dir all_files tarball =
  let tarball_base = Filename.basename tarball in
  printf "Process %s... %!" tarball_base;
  let name =
    match String.index_from tarball_base 1 '@' with
    | i -> String.sub tarball_base 0 i
    | exception _ -> Filename.remove_extension tarball_base in
  let dir = Filename.concat dir name in
  mkdir dir;
  Tar.extract ~in_dir:dir tarball;
  (* Flatten the dir: move all files to the toplevel directory. *)
  flatten_dir dir;
  let all_files =
    Array.fold_left (fun all_files file ->
        Moss.File.of_path (Filename.concat dir file)
          ~name:(Filename.concat name file) :: all_files
      ) all_files (Sys.readdir dir) in
  printf "done\n%!";
  all_files


let () =
  let tarballs = ref [] in
  let specs = [] in
  let anon t = tarballs := t :: !tarballs in
  let name = Filename.basename Sys.argv.(0) in
  Arg.parse specs anon (sprintf "%s <tarballs>" name);
  if List.length !tarballs < 2 then (
    printf "%s: need to be given at least two tarballs.\n" name;
    exit 1;
  );
  let tarballs = List.rev !tarballs in
  (* Create a temporary directory. *)
  let dir = Filename.temp_file "moss-" ".tmp" in
  Sys.remove dir;
  mkdir dir;
  (* Extract the tarballs and submit files. *)
  let files = List.fold_left (add_files_of_tarball ~dir) [] tarballs in
  (* List.iter (fun fn -> printf "- %s\n" (Moss.File.name fn)) files; *)
  let url = Moss.submit Moss.Python files ~by_dir:true in
  printf "→ %s\n" (Uri.to_string url);
  rmdir dir
