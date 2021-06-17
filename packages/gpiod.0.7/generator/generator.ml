open Core

exception InvalidFunction of string

let ml_prelude = "open Ctypes\n\nmodule Stubs (F : Ctypes.FOREIGN) = struct\n  open F\n"
let ml_suffix = "end"

let rec matches_chr chr t =
  match chr with
  | [] -> false
  | x :: _ when Char.( = ) x t -> true
  | _ :: xs -> matches_chr xs t
;;

let rec split_inclusive chr = function
  | [] -> [], []
  | x :: xs when matches_chr chr x -> [ x ], xs
  | x :: xs ->
    let before_split, after_split = split_inclusive chr xs in
    x :: before_split, after_split
;;

let rec full_split chr xs =
  let split_header, xs = split_inclusive chr xs in
  match xs with
  | [] -> [ split_header ]
  | xs -> split_header :: full_split chr xs
;;

let rec remove_tabs xs =
  match xs with
  | [] -> []
  | '\t' :: xs -> remove_tabs xs
  | x :: xs -> x :: remove_tabs xs
;;

let rec remove_double_spaces xs =
  match xs with
  | [] -> []
  | ' ' :: ' ' :: xs -> remove_double_spaces (' ' :: xs)
  | x :: xs -> x :: remove_double_spaces xs
;;

(* This method uses a regular expression to extract the return type, type name and arguments from a function definition, returning an array of the captured parts if the line includes a function definition or None otherwise *)
let scan_line_for_function_definition line =
  let matcher =
    Re.Pcre.regexp "^\\s*([a-zA-Z0-9_ ]*?[\\s*])([a-zA-Z][a-zA-Z0-9_]*)[(](.*)[)]"
  in
  let matched = Re.exec_opt matcher line in
  match matched with
  | Some matched -> Some (Re.Group.all matched)
  | _ -> None
;;

(* This method uses a regular expression to extract the name of a declared structure, returning the captured name if the line is a structure definition or None otherwise *)
let scan_line_for_struct_definition line =
  let matcher = Re.Pcre.regexp "struct\\s+([a-zA-Z0-9_ ]*)\\s*[;{]" in
  let matched = Re.exec_opt matcher line in
  match matched with
  | Some matched -> Some (String.strip (Array.get (Re.Group.all matched) 1))
  | _ -> None
;;

(* Strip the gpiod_ prefix from methods *)
let strip_gpiod xs =
  let gpiod_prefix = "gpiod_" in
  if String.is_prefix ~prefix:gpiod_prefix xs
  then String.drop_prefix xs (String.length gpiod_prefix)
  else xs
;;

let extract_arguments args_str =
  if String.( = ) (String.strip args_str) "void"
  then []
  else (
    let unprocessed_list = String.split_on_chars ~on:[ ',' ] args_str in
    List.map unprocessed_list ~f:(fun arg_item ->
        match String.rsplit2 arg_item ~on:'*' with
        | Some v ->
          let l, r = v in
          String.strip (String.concat [ l; "*" ]), String.strip r
        | _ ->
          let l, r = String.rsplit2_exn arg_item ~on:' ' in
          String.strip l, String.strip r))
;;

(* This method removes all newlines from a source file and then reinserts them at the end of a structure or function definition so that we can run a regular expression to extract arguments *)
let split_header_at_functions_and_structs header =
  let header = String.filter header ~f:(fun x -> not (Char.( = ) x '\n')) in
  let header = String.to_list header in
  let header = remove_tabs header in
  let header = remove_double_spaces header in
  List.map ~f:(fun x -> String.of_char_list x) (full_split [ ';'; '/' ] header)
;;

(* Take a C argument or return type and yield a Ctypes Ocaml type *)
let to_ocaml_typename arg_type =
  match arg_type with
  | "void" -> "void"
  | "int" -> "int"
  | "unsigned int" -> "int"
  | "bool" -> "bool"
  | "size_t" -> "nativeint"
  | "char *" | "const char *" | "char const *" -> "string_opt"
  | "const char **" ->
    "ptr void" (* Arrays of strings are treated as voidptr and sent with CArray *)
  | "const int *" | "int *" | "const unsigned int *" | "unsigned int *" ->
    "ptr void" (* Arrays of ints are treated the same as ^ *)
  | _ ->
    if String.is_prefix ~prefix:"struct" arg_type && String.is_suffix ~suffix:"*" arg_type
    then
      sprintf
        "ptr_opt %s"
        (String.strip (String.drop_suffix (String.drop_prefix arg_type 6) 1))
    else raise (InvalidFunction (sprintf "Could not to_ocaml_typename '%s'" arg_type))
;;

(* Build a Ctypes ocaml type signature from the arguments extracted from a function definition *)
let as_ocaml_type_signature args return_type =
  let args = if List.length args = 0 then [ "void", "unit_arg" ] else args in
  let signature_parts =
    List.map args ~f:(fun x ->
        let ctype, _ = x in
        to_ocaml_typename ctype)
  in
  let signature_parts =
    List.concat
      [ signature_parts; [ sprintf "(returning (%s))" (to_ocaml_typename return_type) ] ]
  in
  String.concat ~sep:" @-> " signature_parts
;;

let process_identified_method out_ml return_type function_name arguments =
  fprintf
    out_ml
    "let %s = foreign \"%s\" (%s)\n\n"
    (strip_gpiod function_name)
    function_name
    (as_ocaml_type_signature arguments return_type)
;;

let process_header_line seen_names out_ml line =
  match scan_line_for_function_definition line with
  | Some regex_parts ->
    (try
       let return_type = String.strip (Array.get regex_parts 1) in
       let function_name = String.strip (Array.get regex_parts 2) in
       let arguments = extract_arguments (Array.get regex_parts 3) in
       if String.is_prefix ~prefix:"static" return_type
       then raise (InvalidFunction "cannot link to static methods");
       process_identified_method out_ml return_type function_name arguments
     with
    | InvalidFunction x ->
      printf
        "Could not synthesize bindings for %s because %s\n"
        (Array.get regex_parts 2)
        x;
      ())
  | _ ->
    (match scan_line_for_struct_definition line with
    | Some struct_name ->
      if not (Hash_set.mem seen_names struct_name)
      then (
        fprintf
          out_ml
          "type %s\nlet %s : %s structure typ = structure \"%s\"\n\n"
          struct_name
          struct_name
          struct_name
          struct_name;
        Hash_set.add seen_names struct_name;
        ())
      else printf "Saw %s twice\n" struct_name
    | _ -> ())
;;

let rec process_header_lines seen_names out_ml = function
  | [] -> ()
  | line :: remaining_lines ->
    process_header_line seen_names out_ml line;
    process_header_lines seen_names out_ml remaining_lines
;;

(* Take the gpiod.h header file and generate Ctype bindings to the given .ml file under the module Stubs.
 * This is a naive binding generator, we just use regular expressions to spot function signatures and generate type definitions or bindings with the equivalent ocaml signatures.
 * This works for libgpiod only because the signatures to methods are very straightforward *)
let process_header_file filepath output_ml =
  let out_ml = Out_channel.create output_ml in
  let header = In_channel.read_all filepath in
  let lines = split_header_at_functions_and_structs header in
  fprintf out_ml "%s" ml_prelude;
  process_header_lines (Hash_set.create (module String)) out_ml lines;
  fprintf out_ml "%s" ml_suffix
;;

let () = process_header_file "/usr/include/gpiod.h" "gpiod_bindings.ml"
