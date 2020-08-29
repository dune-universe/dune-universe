(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open SimpleConfigTypes

open Genlex

let lexer = make_lexer ["="; "{"; "}"; "["; "]"; ";"; "("; ")"; ","; "."; "@"]

let once_values = Hashtbl.create 13

let parse_config_file str =

  let rec parse_top options =
    match Stream.peek str with
    | Some (Ident s | String s) ->
      begin
        Stream.junk str;
        match Stream.next str with
        | Kwd "=" ->
          let v = parse_option () in
          parse_top ( (s,v) :: options)
        | _ -> failwith "Operator '=' expected"
      end
    | tok -> List.rev options, tok

  and parse_option () =
    match Stream.next str with
    | Ident s | String s -> StringValue s
    | Int i -> IntValue i
    | Float f -> FloatValue f
    | Char c -> StringValue (String.make 1 c)
    | Kwd "[" -> parse_list "]" []
    | Kwd "(" -> parse_list ")" []
    | Kwd "{" ->
      begin
      let (options, tok) = parse_top [] in
      match tok with
      | Some (Kwd "}") ->
        Stream.junk str;
        Module options
      | _ -> failwith "Symbol '}' expected"
      end
    | Kwd "@" ->
      begin
        match Stream.next str with
        | Int i ->
          let v = parse_once_value i in
          OnceValue v
        | _ -> failwith "expected int"
      end
    | _ -> failwith "expected value"

  and parse_once_value i =
    match Stream.next str with
    | Kwd "@" ->
      begin
        try Hashtbl.find once_values i with Not_found ->
          Printf.kprintf failwith "once value @%d@ is unknown" i
      end
    | Kwd "=" ->
      let v = parse_option () in
      Hashtbl.add once_values i v;
      v
    | _ -> failwith "operators '=' or '@' expected"

  and parse_list end_kwd values =
    match Stream.peek str with
    | None ->
      Printf.kprintf failwith "reached end of file before %s" end_kwd
    | Some (Kwd ( ";" | "," | ".") ) ->
      Stream.junk str;
      parse_list end_kwd values
    | Some (Kwd kwd) when kwd = end_kwd ->
      Stream.junk str;
      List (List.rev values)
    | _ ->
      let v = parse_option () in
      parse_list end_kwd (v :: values)

  in
  let (options, tok) = parse_top [] in
  match tok with
  | Some _ -> failwith "ident or string expected"
  | None -> options

let parse filename ic =
      Hashtbl.clear once_values;
    let s = Stream.of_channel ic in
    let stream = lexer s in
    let list =
      try parse_config_file stream with
      | e ->
        raise (LoadError (filename,
                          ParseError (Stream.count s, Printexc.to_string e)))
    in
    Hashtbl.clear once_values;
    list



let exit_exn = Exit


let once_values_counter = ref 0
let once_values_rev = Hashtbl.create 13

let reset () =
  once_values_counter := 0;
  Hashtbl.clear once_values_rev

let safe_string s =
  if s = "" then "\"\""
  else
    try
      match s.[0] with
        'a'..'z' | 'A'..'Z' ->
          for i = 1 to String.length s - 1 do
            match s.[i] with
              'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> ()
            | _ -> raise exit_exn
          done;
          s
      | _ ->
          if Int64.to_string (Int64.of_string s) = s ||
             string_of_float (float_of_string s) = s
          then
            s
          else raise exit_exn
    with
      _ -> Printf.sprintf "\"%s\"" (String.escaped s)

let comment option_comment =
  let option_comment = Printf.sprintf "%s" option_comment in
  let lines = EzString.split option_comment '\n' in
  let max_length = ref 10 in
  List.iter (fun line ->
      let len = String.length line in
      if len > !max_length then max_length := len) lines;
  let max_length =
    if !max_length > 74 then 74 else !max_length in
  let spaces = String.make max_length ' ' in
  let lines = List.map (fun line ->
      let len = String.length line in
      if len < max_length then
        line ^ String.sub spaces 0 (max_length - len)
      else line) lines in
  Printf.sprintf "(* %s *)" (String.concat " *)\n(* " lines)

let compact_string oc f =
  let b = Buffer.create 100 in
  f b;
  let s = Buffer.contents b in
  let b = Buffer.create 100 in
  let rec iter b i len s =
    if i < len then
      let c = s.[i] in
      if c = ' ' || c = '\n' || c = '\t' then begin
          iter_space b (i+1) len s
        end
      else begin
          Buffer.add_char b c;
          iter b (i+1) len s
        end
  and  iter_space b i len s =
    if i < len then
      let c = s.[i] in
      if c = ' ' || c = '\n' || c = '\t' then begin
          iter_space b (i+1) len s
        end
      else begin
          Buffer.add_char b ' ';
          Buffer.add_char b c;
          iter b (i+1) len s
        end
  in
  iter b 0 (String.length s) s;
  let ss = Buffer.contents b in
  Buffer.add_string oc (if String.length ss < 80 then ss else s)

let rec save_module with_help indent oc list =
  let subm = ref [] in
  List.iter
    (fun (name, help, value) ->
       match name with
         [] -> assert false
       | [name] ->
           if with_help && help <> "" then
             Printf.bprintf oc "\n%s\n" (comment help);
           Printf.bprintf oc "%s%s = " indent (safe_string name);
           save_value indent oc value;
           Printf.bprintf oc "\n"
       | m :: tail ->
           let p =
             try List.assoc m !subm
             with
             | Not_found -> let p = ref [] in subm := (m, p) :: !subm; p
           in
           p := (tail, help, value) :: !p)
    list;
  List.iter
    (fun (m, p) ->
       Printf.bprintf oc "%s%s = {\n" indent (safe_string m);
       save_module with_help (indent ^ "  ") oc !p;
       Printf.bprintf oc "%s}\n" indent)
    !subm
and save_list indent oc list =
  match list with
    [] -> ()
  | [v] -> save_value indent oc v
  | v :: tail ->
      save_value indent oc v; Printf.bprintf oc ", "; save_list indent oc tail
and save_list_nl indent oc list =
  match list with
    [] -> ()
  | [v] -> Printf.bprintf oc "\n%s" indent; save_value indent oc v
  | v :: tail ->
      Printf.bprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.bprintf oc ";";
      save_list_nl indent oc tail
and save_value indent oc v =
  match v with
    StringValue s -> Printf.bprintf oc "%s" (safe_string s)
  | IntValue i -> Printf.bprintf oc "%d" i
  | FloatValue f -> Printf.bprintf oc "%F" f
  | List l ->
      compact_string oc (fun oc ->
          Printf.bprintf oc "[";
          save_list_nl (indent ^ "  ") oc l;
          Printf.bprintf oc "\n%s]" indent)
  | DelayedValue f -> f oc indent
  | SmallList l ->
      Printf.bprintf oc "(";
      save_list (indent ^ "  ") oc l;
      Printf.bprintf oc ")"
  | Module m ->
      compact_string oc (fun oc ->
          Printf.bprintf oc "{";
          save_module_fields (indent ^ "  ") oc m;
          Printf.bprintf oc "%s}" indent)
  | OnceValue v ->
      try
        let i = Hashtbl.find once_values_rev v in Printf.bprintf oc "@%Ld@" i
      with
        Not_found ->
          incr once_values_counter;
          let i = Int64.of_int !once_values_counter in
          Hashtbl.add once_values_rev v i;
          Printf.bprintf oc "@%Ld = " i;
          save_value indent oc v
and save_module_fields indent oc m =
  match m with
    [] -> ()
  | (name, v) :: tail ->
      Printf.bprintf oc "%s%s = " indent (safe_string name);
      save_value indent oc v;
      Printf.bprintf oc "\n";
      save_module_fields indent oc tail

let save_binding oc name value =
  Printf.bprintf oc "%s = " (safe_string name);
  save_value "  " oc value;
  Printf.bprintf oc "\n"

let save_module ~with_help ~indent buf list =
  save_module with_help indent buf list
let save_value ~indent buf v = save_value indent buf v
