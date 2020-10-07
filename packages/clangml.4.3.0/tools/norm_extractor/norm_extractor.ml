let search_ill_formed = Kmp.On_string.find "ill-formed"

let section_table_sz = 17

let set_filename (lexbuf : Lexing.lexbuf) filename =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }

let open_lexbuf filename f =
  let channel = open_in filename in
  Fun.protect begin fun () ->
    let lexbuf = Lexing.from_channel channel in
    set_filename lexbuf filename;
    f lexbuf
  end
  ~finally:begin fun () ->
    close_in channel
  end

type title_buffer = Tex_lexer.section_title list

let rec remove_first_empty_lines lines =
  match lines with
  | "" :: tl -> remove_first_empty_lines tl
  | _ -> lines

let remove_last_empty_lines lines =
  lines |> List.rev |> remove_first_empty_lines |> List.rev

let rec strip_title_buffer base_level (title_buffer : title_buffer) =
  match title_buffer with
  | { level; _ } :: tail when level >= base_level ->
      strip_title_buffer base_level tail
  | _ ->
      title_buffer

module String_hashtbl = Hashtbl.Make (struct
  type t = string

  let equal : string -> string -> bool = ( = )

  let hash : string -> int = Hashtbl.hash
end)

let brace_count s =
  s |> String.to_seq |> Seq.fold_left begin fun counter c ->
    match c with
    | '{' -> succ counter
    | '}' -> pred counter
    | _ -> counter
  end 0

let rec find_line_comment_delim line from =
  match String.index_from_opt line from '/' with
  | Some index ->
      if index + 1 < String.length line then
        if String.unsafe_get line (index + 1) = '/' then
          Some index
        else
          find_line_comment_delim line (index + 2)
      else
        None
  | None -> None

let split_line_comment line =
  match find_line_comment_delim line 0 with
  | Some index ->
      String.sub line 0 index,
      Some (String.sub line index (String.length line - index))
  | None -> line, None

let has_prefix prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

let rec comment_ill_formed buffer in_commented_block lines =
  match lines with
  | [] -> ()
  | line :: lines ->
      let body, line_comment = split_line_comment line in
      let brace = brace_count body in
      let in_commented_block =
        match line_comment with
        | Some comment when
            has_prefix "// error:" comment ||
            has_prefix "// error," comment ||
            has_prefix "// Error:" comment ||
            search_ill_formed (String.to_seq comment) () <> Nil ->
              Buffer.add_string buffer "// ";
              in_commented_block || brace > 0
        | _ ->
            let rec has_ill_formed_in_comment lines =
              match lines with
              | [] -> false
              | line :: tl ->
                  let body, line_comment = split_line_comment line in
                  if String.trim body = "" then
                    match line_comment with
                    | Some comment when
                        search_ill_formed (String.to_seq comment) () <> Nil ->
                        true
                    | _ -> has_ill_formed_in_comment tl
                  else
                    false in
            if has_ill_formed_in_comment lines then
              begin
                Buffer.add_string buffer "// ";
                in_commented_block || brace > 0
              end
            else if in_commented_block && brace < 0 then
              begin
                Buffer.add_string buffer "// ";
                false
              end
            else
              in_commented_block in
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n';
      comment_ill_formed buffer in_commented_block lines

let format_comment fmt pp =
  Format.fprintf fmt "@[(*@ %a@ *)@]@." pp ()

type context = {
    section_table : string String_hashtbl.t;
    target : Format.formatter;
    command_line_args : string list;
  }

let outside_decl (decl : Clang.Ast.decl) =
  not (Filename.is_relative
    (Clang.Ast.concrete_of_source_location Presumed
       (Clang.Ast.location_of_node decl)).filename)

let inside_decl (decl : Clang.Ast.decl) =
  Filename.is_relative
    (Clang.Ast.concrete_of_source_location Presumed
       (Clang.Ast.location_of_node decl)).filename

let rec filter_outside list =
  match list with
  | hd :: tl when outside_decl hd ->
      filter_outside tl
  | _ -> list

let format_check_pattern fmt (ast : Clang.Ast.translation_unit) =
  let hook : type a . a Refl.Lift.Pat.hook_fun =
    fun refl lifter x ->
      match refl, x with
      | Clang.Ast.Refl_integer_literal, CXInt _
      | Clang.Ast.Refl_floating_literal, CXFloat _ ->
          Ast_helper.Pat.any ()
      | _ ->
          lifter x in
  let hook = { Refl.Lift.Pat.hook } in
  let items = ast.desc.items |> List.filter inside_decl in
  let loc = !Ast_helper.default_loc in
  let decls = items |> List.mapi begin fun i decl ->
    (Printf.sprintf "d%d" i, decl)
  end in
  let pattern = decls |> List.fold_left begin
    fun pat (binder, _) : Parsetree.pattern ->
      [%pat? [%p Ast_helper.Pat.var { loc; txt = binder }] :: [%p pat]]
  end (Ast_helper.Pat.any ()) in
  let checkers = decls |> List.map begin
    fun (binder, decl) : Parsetree.expression ->
      [%expr check_pattern_decl
         [%e Ast_helper.Exp.ident { loc; txt = Lident binder }]
         [%pattern? [%p Refl.Lift.Pat.lift ~hook [%refl: Clang.Ast.decl] []
           decl]]]
  end in
  let checkers : Parsetree.expression =
    match List.rev checkers with
    | [] -> [%expr ()]
    | hd :: tl ->
        List.fold_left begin fun acc item : Parsetree.expression ->
          [%expr [%e item]; [%e acc]]
        end hd tl in
  let expression : Parsetree.expression =
    [%expr match List.rev ast with [%p pattern] -> [%e checkers]] in
  Format.fprintf fmt "%a@." Pprintast.expression expression

let rec find_map f list =
  match list with
  | [] -> None
  | hd :: tl ->
      match f hd with
      | None -> find_map f tl
      | result -> result

let parse_code context contents =
  let { target; command_line_args } = context in
  let tu = Clang.parse_string ~command_line_args contents in
  let alternative_contexts =
        [(fun s -> "void in_a_function() {\n" ^ s ^ "\n}");
         (fun s -> s ^ ";");
         (fun s -> "void in_a_function() {\n" ^ s ^ ";\n}");
         (fun s -> "struct in_a_struct {\n" ^ s ^ "};");
         (fun s -> "struct in_a_struct {\n" ^ s ^ ";};");
         (fun s -> "void in_a_function() { int a, b;\n" ^ s ^ "\n}");
         (fun s -> "#include <cstring>
#include <cassert>

void in_a_function() {\n" ^ s ^ "\n}");
         (fun s -> "#include <string>
void in_a_function() {\n" ^ s ^ "\n}");
         (fun s -> "struct X;
namespace N {};
" ^ s);
        (fun s -> "struct X {
  X();
  X& operator=(X&);
};
" ^ s);
        (fun s -> "#include <array>\n" ^ s);
        (fun s -> "int i;\n" ^ s);
        (fun s -> "int i;
namespace M {
  class B {};
};
" ^ s);
        ] in
      let contents, tu =
        match
          if Clang.has_severity Clang.error tu then
            alternative_contexts |> find_map begin fun context ->
              let contents = context contents in
              let unsaved_files : Clang.cxunsavedfile list =
                [{ filename = "a.h"; contents = {|
class A {
  A();
  void Use();
};
|} };
                 { filename = "b.h"; contents = {|
class B {
  B();
  void Use();
};
|} };
                 { filename = "usefullib.h"; contents = ""; };
                 { filename = "myprog.h"; contents = ""; };
                 { filename = "versN.h"; contents = ""; };
                 { filename = "vers2.h"; contents = ""; };
                 { filename = "Date"; contents = "#include <ctime>\n"; };
                 { filename = "jctype"; contents = ""; }] in
              let tu =
                Clang.parse_string ~unsaved_files ~command_line_args contents in
              if Clang.has_severity Clang.error tu then
                None
              else
                Some (contents, tu)
            end
          else
            None
        with
        | None -> (contents, tu)
        | Some result -> result in
      let pp pp fmt () =
        Format.fprintf fmt "@[(*@ @[<v>%a@] *)@]" pp () in
      Clang.format_diagnostics ~pp Clang.warning_or_error target tu;
      if Clang.has_severity Clang.error tu then
        Format.fprintf target "ignore ast"
      else
        let ast = Clang.Ast.of_cxtranslationunit tu in
        Format.fprintf target "
let () =
  let ast = parse_string {|%s|} in
" contents;
        format_check_pattern target ast

let rec loop context title_buffer lexbuf =
  match Tex_lexer.main lexbuf with
  | EOF -> ()
  | Begin_codeblock ->
      Gc.full_major ();
      let { target; section_table } = context in
      title_buffer |> List.rev |> List.iter begin fun
        ({ title; ident; _ } : Tex_lexer.section_title) ->
          match String_hashtbl.find_opt section_table ident with
          | None ->
              Format.eprintf "Unknown section label %s@." ident
          | Some number ->
              format_comment target begin fun fmt () ->
                Format.fprintf fmt "%s@ %s" number title
              end
      end;
      let buffer = Buffer.create 17 in
      Tex_lexer.code_block buffer lexbuf;
      let s = Buffer.contents buffer in
      let lines = String.split_on_char '\n' s in
      Buffer.clear buffer;
      let lines = remove_last_empty_lines lines in
      let _ = comment_ill_formed buffer false lines in
      let contents = Buffer.contents buffer in
      begin
        try
          parse_code context contents
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          Format.eprintf "@[Error while processing:@ %s@]@." contents;
          Printexc.raise_with_backtrace e bt
      end;
      loop context [] lexbuf
  | Rsec section_title ->
      let title_buffer = strip_title_buffer section_title.level title_buffer in
      loop context (section_title :: title_buffer) lexbuf

let rec read_aux_file section_table lexbuf =
  match Aux_lexer.main lexbuf with
  | EOF -> ()
  | Reference { ident; number } ->
      String_hashtbl.add section_table ident number;
      read_aux_file section_table lexbuf

let extract_code command_line_args target filename =
  prerr_endline filename;
  let aux_filename = Filename.chop_suffix filename ".tex" ^ ".aux" in
  let section_table = String_hashtbl.create section_table_sz in
  open_lexbuf aux_filename (read_aux_file section_table);
  open_lexbuf filename (loop { section_table; target; command_line_args } [])

let main files target =
  Clangml_tools_common.command_line begin fun _language command_line_args ->
    let target =
      match target with
      | None -> failwith "Missing target name"
      | Some target -> target in
    let target_channel = open_out target in
    Fun.protect begin fun () ->
      let target_fmt = Format.formatter_of_out_channel target_channel in
      files |> List.iter (extract_code command_line_args target_fmt);
      Format.pp_print_flush target_fmt
    end
    ~finally:begin fun () ->
      close_out target_channel
    end
  end

let files =
  let doc = "Source files" in
  Cmdliner.Arg.(
    value & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc)

let target =
  let doc = "Target file name" in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["o"] ~docv:"TARGET" ~doc)

let options =
  Clangml_tools_common.options Cmdliner.Term.(const main $ files $ target)

let info =
  let doc = "Extract code from norms" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "norm_extractor" ~doc ~exits:Cmdliner.Term.default_exits ~man

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (options, info))
