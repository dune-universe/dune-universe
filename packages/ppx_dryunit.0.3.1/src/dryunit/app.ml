(*
  Our cmd-free app definitions and models
*)

open Util
open Types

let mkdir_p dir =
  split Filename.dir_sep dir |>
  List.fold_left
  ( fun acc basename ->
    let path = acc ^ Filename.dir_sep ^ basename in
    if not (Sys.file_exists path) then
      Unix.mkdir path 0o755;
    path
  )
  "" |>
  ignore

let gen ~nocache ~framework ~cache_dir ~ignore ~filter ~targets =
  let _ = framework_from framework in
  let detection = "dir" in
  let get_int () =
    (Random.int 9999) + 1 in
  let msg = "This file is supposed to be generated before build with a random ID." in
  let id = format "%d%d%d" (get_int ()) (get_int ()) (get_int ()) in
  ( format "(*\n  %s\n  ID = %s\n*)\nlet () =\n  [%%dryunit\n    { cache_dir = \"%s\"\n    ; cache     = %s\n    ; framework = \"%s\"\n    ; ignore    = \"%s\"\n    ; filter    = \"%s\"\n    ; detection = \"%s\"\n    }\n  ]\n"
    msg id cache_dir (string_of_bool @@ not nocache) framework ignore filter detection
  ) |>
  fun output ->
  if List.length targets == 0 then
    print_endline output
  else
    ( List.iter
      ( fun target ->
        let path = Sys.getcwd () ^ Filename.dir_sep ^ target in
        let dir = Filename.dirname path in
        if not (Sys.file_exists dir) then
          mkdir_p dir;
        let oc = open_out path in
        Printf.fprintf oc "%s" output;
        close_out oc
      )
      targets
    )


let init () =
  print_endline @@ String.trim @@ "
(executables
 ((names (main))
  (libraries (alcotest))
  (preprocess (pps (ppx_dryunit)))))

;; This rule generates the bootstrapping
(rule
 ((targets (main.ml))
  ;;
  ;; Uncomment for change detection:
  ;;
  ;; (deps (FILE1.ml FILE2.ml))
  ;;
  (action  (with-stdout-to ${@} (run
    dryunit gen --framework alcotest
    ;;
    ;; Uncomment to configure:
    ;;
    ;;  --ignore \"space separated list\"
    ;;  --filter \"space separated list\"
  )))))
"


let clean () =
  let dir = ".dryunit" in
  if Sys.file_exists dir && Sys.is_directory dir then
  ( Array.iter
      ( fun v -> Unix.unlink (dir ^ Filename.dir_sep ^ v) )
      ( Sys.readdir dir );
    Unix.rmdir dir
  )
