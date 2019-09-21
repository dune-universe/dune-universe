open Ppxlib
open Backend.Compiler_modules
open Core_kernel
open Expect_test_common.Std
open Expect_test_matcher.Std
open Mlt_parser

module Clflags  = Ocaml_common.Clflags
module Compmisc = Ocaml_common.Compmisc
module Printast = Ocaml_common.Printast
module Warnings = Ocaml_common.Warnings

let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <-
    { pos_fname = fname
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = 0
    };
  Ocaml_common.Location.input_name := fname;
  Parse.use_file lexbuf
;;

let reset_line_numbers = ref false
let line_numbers_delta = ref 0
let () =
  Caml.Hashtbl.add Toploop.directive_table
    "reset_line_numbers"
    (Directive_none (fun () -> reset_line_numbers := true))
;;

let print_line_numbers = ref false
let () =
  Caml.Hashtbl.add Toploop.directive_table
    "print_line_numbers"
    (Directive_bool (fun x -> print_line_numbers := x))
;;

let print_line_number ppf line =
  if !print_line_numbers then
    Format.fprintf ppf "%d" line
  else
    Format.pp_print_string ppf "_"
;;

[%%if ocaml_version < (4, 08, 0)]
let print_loc ppf (loc : Location.t) =
  let line = loc.loc_start.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line %a" print_line_number line;
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@.";
;;

let rec error_reporter ppf ({loc; msg; sub; if_highlight=_} : Ocaml_common.Location.error) =
  print_loc ppf loc;
  Format.fprintf ppf "Error: %s" msg;
  List.iter sub ~f:(fun err ->
    Format.fprintf ppf "@\n@[<2>%a@]" error_reporter err)
;;
[%%endif]

[%%if ocaml_version < (4, 06, 0)]
let warning_printer loc ppf w =
  if Warnings.is_active w then begin
    print_loc ppf loc;
    Format.fprintf ppf "Warning %a@." Warnings.print w
  end
[%%elif ocaml_version < (4, 08, 0)]
let warning_printer loc ppf w =
  match Warnings.report w with
    | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs = _ } ->
    print_loc ppf loc;
    if is_error
    then
      Format.fprintf ppf "Error (Warning %d): %s@." number message
    else Format.fprintf ppf "Warning %d: %s@." number message
[%%elif ocaml_version >= (4, 08, 0)]
let warning_reporter = Ocaml_common.Location.default_warning_reporter
let alert_reporter = Ocaml_common.Location.default_alert_reporter
[%%endif]
;;

[%%if ocaml_version >= (4, 08, 0)]
let report_printer () =
  let printer = Ocaml_common.Location.default_report_printer () in
  let print_loc _ _report ppf loc =
    let line = loc.loc_start.pos_lnum in
    let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "Line %a" print_line_number line;
    if startchar >= 0 then
      Format.fprintf ppf ", characters %d-%d" startchar endchar;
    Format.fprintf ppf ":@."
  in
  { printer with Ocaml_common.Location.pp_main_loc = print_loc; pp_submsg_loc = print_loc }
[%%endif]

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter l ~f:(fun (V (r, v)) -> r := v) in
  fun vars ~f ->
    let backup = List.map vars ~f:(fun (V (r, _)) -> V (r, !r)) in
    set_vars vars;
    protect ~finally:(fun () -> set_vars backup) ~f
;;

[%%if ocaml_version < (4, 08, 0)]
let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Ocaml_common.Location.formatter_for_warnings , ppf            )
    ; V (Ocaml_common.Location.warning_printer        , warning_printer)
    ; V (Ocaml_common.Location.error_reporter         , error_reporter )
    ]
    ~f
[%%else]
let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Ocaml_common.Location.formatter_for_warnings , ppf             )
    ; V (Ocaml_common.Location.warning_reporter       , warning_reporter)
    ; V (Ocaml_common.Location.report_printer         , report_printer  )
    ; V (Ocaml_common.Location.alert_reporter         , alert_reporter  )
    ]
    ~f
[%%endif]
;;

let apply_rewriters = function
  | Ptop_dir _ as x -> x
  | Ptop_def s ->
    Ptop_def (Driver.map_structure s
              |> Migrate_parsetree.Driver.migrate_some_structure
                   (module Ppxlib_ast.Selected_ast))
;;

let verbose = ref false
let () =
  Caml.Hashtbl.add Toploop.directive_table
    "verbose"
    (Directive_bool (fun x -> verbose := x))
;;

let shift_line_numbers = object
  inherit [int] Ast_traverse.map_with_context
  method! position delta pos =
    { pos with pos_lnum  = pos.pos_lnum + delta }
end

let exec_phrase ppf phrase =
  if !reset_line_numbers then begin
    match phrase with
    | Ptop_def (st :: _) ->
      reset_line_numbers := false;
      line_numbers_delta := 1 - st.pstr_loc.loc_start.pos_lnum
    | _ -> ()
  end;
  let phrase =
    match !line_numbers_delta with
    | 0 -> phrase
    | n -> shift_line_numbers#toplevel_phrase n phrase
  in
  let phrase = apply_rewriters phrase in
  let module Js = Ppxlib_ast.Selected_ast in
  let ocaml_phrase = Js.to_ocaml Toplevel_phrase phrase in
  if !Clflags.dump_parsetree then Printast. top_phrase ppf ocaml_phrase;
  if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
  Toploop.execute_phrase !verbose ppf ocaml_phrase
;;

let count_newlines : _ Cst.t Expectation.Body.t -> int =
  let count s = String.count s ~f:(Char.(=) '\n') in
  function
  | Unreachable | Output -> 0
  | Exact s -> count s
  | Pretty cst ->
    match cst with
    | Empty       e -> count e
    | Single_line s -> count s.trailing_spaces
    | Multi_lines m ->
      List.length m.lines - 1 +
      count m.leading_spaces  +
      count m.trailing_spaces
;;

let canonicalize_cst : 'a Cst.t -> 'a Cst.t = function
  | Empty _ -> Empty "\n"
  | Single_line s ->
    Multi_lines
      { leading_spaces  = "\n"
      ; trailing_spaces = "\n"
      ; indentation     = ""
      ; lines           =
          [ Not_blank
              { trailing_blanks = ""
              ; orig            = s.orig
              ; data            = s.data
              }
          ]
      }
  | Multi_lines m ->
    Multi_lines
      { leading_spaces  = "\n"
      ; trailing_spaces = "\n"
      ; indentation     = ""
      ; lines           = List.map m.lines ~f:Cst.Line.strip
      }
;;

let reconcile ~actual ~expect ~allow_output_patterns : _ Reconcile.Result.t =
  match
    Reconcile.expectation_body
      ~expect
      ~actual
      ~default_indent:0
      ~pad_single_line:false
      ~allow_output_patterns
  with
  | Match -> Match
  | Correction c -> Correction (Expectation.Body.map_pretty c ~f:canonicalize_cst)
;;

let redirect ~f =
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stderr in
  let filename = Caml.Filename.temp_file "expect-test" "stdout" in
  let fd_out = Unix.openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  let ic = In_channel.create filename in
  let read_up_to = ref 0 in
  let capture buf =
    Out_channel.flush stdout;
    Out_channel.flush stderr;
    let pos = Unix.lseek fd_out 0 SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Caml.Buffer.add_channel buf ic len
  in
  protect ~f:(fun () -> f ~capture)
    ~finally:(fun () ->
      In_channel.close ic;
      Unix.close fd_out;
      Unix.dup2 stdout_backup Unix.stdout;
      Unix.dup2 stderr_backup Unix.stderr;
      Unix.close stdout_backup;
      Unix.close stderr_backup;
      Sys.remove filename)
;;

type chunk_result =
  | Matched
  | Didn't_match of Fmt.t Cst.t Expectation.Body.t

let eval_expect_file fname ~file_contents ~capture ~allow_output_patterns =
  (* 4.03: Warnings.reset_fatal (); *)
  let chunks, trailing_code =
    parse_contents ~fname file_contents
    |> split_chunks ~fname ~allow_output_patterns
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  reset_line_numbers := false;
  line_numbers_delta := 0;
  let exec_phrases phrases =
    (* So that [%expect_exact] nodes look nice *)
    Buffer.add_char buf '\n';
    List.iter phrases ~f:(fun phrase ->
      let snap = Ocaml_common.Btype.snapshot () in
      match exec_phrase ppf phrase with
      | (_ : bool) -> ()
      | exception exn ->
        Location.report_exception ppf exn;
        Ocaml_common.Btype.backtrack snap
    );
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
      (* So that [%expect_exact] nodes look nice *)
      Buffer.add_char buf '\n';
    capture buf;
    if Buffer.nth buf (len - 1) <> '\n' then
      Buffer.add_char buf '\n';
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in
  let results =
    capture_compiler_stuff ppf ~f:(fun () ->
      List.map chunks ~f:(fun chunk ->
        let actual = exec_phrases chunk.phrases in
        match
          reconcile ~actual ~expect:chunk.expectation.body ~allow_output_patterns
        with
        | Match -> (chunk, actual, Matched)
        | Correction correction ->
          line_numbers_delta :=
            !line_numbers_delta +
            count_newlines correction -
            count_newlines chunk.expectation.body;
          (chunk, actual, Didn't_match correction)))
  in
  let trailing =
    match trailing_code with
    | None -> None
    | Some (phrases, pos_start, part) ->
      let actual, result =
        capture_compiler_stuff ppf ~f:(fun () ->
          let actual = exec_phrases phrases in
          (actual, reconcile ~actual ~expect:(Pretty Cst.empty)
                     ~allow_output_patterns))
      in
      Some (pos_start, actual, result, part)
  in
  (results, trailing)
;;

let interpret_results_for_diffing ~fname ~file_contents (results, trailing) =
  let corrections =
    List.filter_map results ~f:(fun (chunk, _, result) ->
      match result with
      | Matched -> None
      | Didn't_match correction ->
        Some (chunk.expectation,
              Matcher.Test_correction.Node_correction.Correction correction))
  in
  let trailing_output =
    match trailing with
    | None -> Reconcile.Result.Match
    | Some (_, _, correction, _) -> correction
  in
  Matcher.Test_correction.make
    ~location:{ filename    = File.Name.of_string fname
              ; line_number = 1
              ; line_start  = 0
              ; start_pos   = 0
              ; end_pos     = String.length file_contents
              }
    ~corrections
    ~trailing_output
    ~uncaught_exn:Match
;;

module T = Toplevel_expect_test_types

(* Take a part of a file, trimming spaces at the beginning as well as ';;' *)
let sub_file file_contents ~start ~stop =
  let rec loop start =
    if start >= stop then
      start
    else
      match file_contents.[start] with
      | ' ' | '\t' | '\n' -> loop (start + 1)
      | ';' when start + 1 < stop && file_contents.[start+1] = ';' ->
        loop (start + 2)
      | _ -> start
  in
  let start = loop start in
  String.sub file_contents ~pos:start ~len:(stop - start)
;;

let generate_doc_for_sexp_output ~fname:_ ~file_contents (results, trailing) =
  let rev_contents =
    List.rev_map results ~f:(fun (chunk, resp, _) ->
      let loc = chunk.phrases_loc in
      (chunk.part,
       { T.Chunk.
         ocaml_code = sub_file file_contents ~start:loc.loc_start.pos_cnum
                        ~stop:loc.loc_end.pos_cnum
       ; toplevel_response = resp
       }))
  in
  let rev_contents =
    match trailing with
    | None -> rev_contents
    | Some (pos_start, resp, _, part) ->
      (part,
       { ocaml_code = sub_file file_contents ~start:pos_start.Lexing.pos_cnum
                        ~stop:(String.length file_contents)
       ; toplevel_response = resp
       }) :: rev_contents
  in
  let parts =
    List.group (List.rev rev_contents) ~break:(fun (a, _) (b, _) -> a <> b)
    |> List.map ~f:(function chunks ->
      { T.Part.
        name = Option.bind (List.hd chunks) ~f:fst |> Option.value ~default:""
      ; chunks = List.map chunks ~f:snd
      })
  in
  let matched =
    List.for_all results ~f:(fun (_, _, r) -> r = Matched) &&
    match trailing with
    | None | Some (_, _, Reconcile.Result.Match, _) -> true
    | Some (_, _, Reconcile.Result.Correction _, _) -> false
  in
  { T.Document. parts; matched }
;;

let diff_command = ref None

let process_expect_file fname ~use_color ~in_place ~sexp_output ~use_absolute_path
      ~allow_output_patterns =
  (* Captures the working directory before running the user code, which might change it *)
  let cwd = Sys.getcwd () in
  let file_contents = In_channel.read_all fname in
  let result =
    redirect ~f:(eval_expect_file fname ~file_contents ~allow_output_patterns)
  in
  if sexp_output then begin
    let doc = generate_doc_for_sexp_output ~fname ~file_contents result in
    Format.printf "%a@." Sexp.pp_hum (T.Document.sexp_of_t doc)
  end;
  let corrected_fname = fname ^ ".corrected" in
  let remove_corrected () =
    if Sys.file_exists corrected_fname then
      Sys.remove corrected_fname
  in
  match interpret_results_for_diffing ~fname ~file_contents result with
  | Correction correction ->
    Matcher.write_corrected [correction]
      ~file:(if in_place then fname else corrected_fname)
      ~file_contents ~mode:Toplevel_expect_test;
    if in_place then begin
      remove_corrected ();
      true
    end else begin
      if not sexp_output then begin
        let maybe_use_absolute_path file =
          if use_absolute_path
          then Filename.concat cwd file
          else file
        in
        Ppxlib_print_diff.print ()
          ~file1:(maybe_use_absolute_path fname)
          ~file2:(maybe_use_absolute_path corrected_fname)
          ~use_color
          ?diff_command:!diff_command
      end;
      false
    end
  | Match ->
    if not in_place then remove_corrected ();
    true
;;

let setup_env () =
  (* Same as what run-tests.py does, to get repeatable output *)
  List.iter ~f:(fun (k, v) -> Unix.putenv k v)
    [ "LANG"        , "C"
    ; "LC_ALL"      , "C"
    ; "LANGUAGE"    , "C"
    ; "TZ"          , "GMT"
    ; "EMAIL"       , "Foo Bar <foo.bar@example.com>"
    ; "CDPATH"      , ""
    ; "COLUMNS"     , "80"
    ; "GREP_OPTIONS", ""
    ; "http_proxy"  , ""
    ; "no_proxy"    , ""
    ; "NO_PROXY"    , ""
    ; "TERM"        , "xterm"
    ]

[%%if ocaml_version < (4, 08, 0)]
let warnings = "@a-4-29-40-41-42-44-45-48-58"
[%%else]
let warnings = "@a-4-29-40-41-42-44-45-48-58-66"
[%%endif]

let setup_config () =
  Clflags.real_paths      := false;
  Clflags.strict_sequence := true;
  Clflags.strict_formats  := true;
  Clflags.unsafe_string   := Backend.unsafe_string ();
  Warnings.parse_options false warnings;
;;

let use_color   = ref true
let in_place    = ref false
let sexp_output = ref false
let use_absolute_path = ref false
let allow_output_patterns = ref false

[%%if ocaml_version < (4, 09, 0)]
let init_path () = Compmisc.init_path true
[%%else]
let init_path () = Compmisc.init_path ()
[%%endif]

let main fname =
  let cmd_line =
    Array.sub Sys.argv ~pos:!Arg.current ~len:(Array.length Sys.argv - !Arg.current)
  in
  setup_env ();
  setup_config ();
  Core.Sys.override_argv cmd_line;
  Toploop.set_paths ();
  init_path ();
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  Backend.init ();
  let success =
    process_expect_file fname ~use_color:!use_color ~in_place:!in_place
      ~sexp_output:!sexp_output ~use_absolute_path:!use_absolute_path
      ~allow_output_patterns:!allow_output_patterns
  in
  exit (if success then 0 else 1)
;;

let args =
  Arg.align
    [ "-no-color", Clear use_color, " Produce colored diffs"
    ; "-in-place", Set in_place,    " Overwirte file in place"
    ; "-diff-cmd", String (fun s -> diff_command := Some s), " Diff command"
    ; "-sexp"    , Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-absolute-path", Set use_absolute_path, " Use absolute path in diff-error message"
    ; "-allow-output-patterns", Set allow_output_patterns,
      " Allow output patterns in tests expectations";
    ]

let main () =
  let usage =
    Printf.sprintf "Usage: %s [OPTIONS] FILE [ARGS]\n"
      (Filename.basename Sys.argv.(0))
  in
  try
    Arg.parse args main (usage ^ "\nOptions are:");
    Out_channel.output_string Out_channel.stderr usage;
    exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
;;
