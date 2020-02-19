open Core
open Idds
open Async

type policy = Katbb_lib.Ast.exp

type showable =
  (* usage: policy
   * Shows the policy that is currently active. *)
  | Policy
  (* usage: table_html
   * Shows the flow-table produced by current policy in HTML *)
  | Table_HTML
  (* usage: table_text
   * Shows the flow-table produced by the current policy in text *)
  | Table_Text
  (* usage: idd_pdf
   * Shows the idd produced by current policy as a separate pdf *)
  | IDD_PDF
  (* usage: idd_text
   * Shows the idd produced by current policy in text *)
  | IDD_Text
  (* usage: help
   * Displays a helpful message. *)
  | Help


type command =
  (* usage: update <policy>
   * Compiles the specified policy *)
  | Update of (policy * string)
  (* usage: <p1> = <p2>
   * Tests whether p1 equals p2 *)
  | Equiv of ((policy * string) * (policy * string))
  (* usage: load <filename>
   * Loads the specified file as a local policy and compiles it updating
     the controller with the new flow table. *)
  | Load of string
  (* See showables for more details *)
  | Show of showable
  (* usage: exit
   * Exits the shell. *)
  | Exit
  (* usage: quit
   * Exits the shell. *)
  | Quit

 

module Parser = struct

  open MParser

  module Tokens = MParser_RE.Tokens

  (* Mostly useless error message for parsing policies *)
  let string_of_position (p : Lexing.position) : string =
    sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

  (* Use the netkat parser to parse policies *)
  let parse_policy ?(name = "") (pol_str : string) : (policy, string) Result.t =
    Ok (Katbb_lib.Parser.parse_string pol_str)

  (* Consumes input until [till] is reached then compiles input into policy *)
  let policy_till till : ((policy * string), bytes list) MParser.t =
    many_until any_char till >>= fun pol_chars ->
    let pol_str = String.of_char_list pol_chars in
    match parse_policy pol_str with
    | Ok pol -> return (pol, pol_str)
    | Error msg -> fail msg

  (* Parser for the Update command *)
  let update : (command, bytes list) MParser.t =
    Tokens.symbol "update" >>
    (policy_till eof) >>=
    (fun pol -> return (Update pol))
  
  (* Parser for the Equiv command *)
  let equiv : (command, bytes list) MParser.t =
    pipe2 (policy_till (Tokens.symbol "=")) (policy_till eof) (fun p1 p2 -> Equiv (p1, p2))


  (* Parser for the load command *)
  let load : (command, bytes list) MParser.t =
    Tokens.symbol "load" >>
    many_until any_char eof >>=
    (fun filename -> return (Load (String.of_char_list filename)))

  (* Parser for the policy command *)
  let policy : (command, bytes list) MParser.t =
    Tokens.symbol "policy" >> return (Show Policy)

  (* Parser for the table_html command *)
  let table_html : (command, bytes list) MParser.t =
    Tokens.symbol "table_html" >>
    eof >> 
    return (Show Table_HTML)

  (* Parser for the table_text command *)
  let table_text : (command, bytes list) MParser.t =
    Tokens.symbol "table_text" >>
    eof >> 
    return (Show Table_Text)

  (* Parser for the idd_pdf command *)
  let idd_pdf : (command, bytes list) MParser.t =
    Tokens.symbol "idd_pdf" >>
    eof >> 
    return (Show IDD_PDF)

  (* Parser for the idd_text command *)
  let idd_text : (command, bytes list) MParser.t =
    Tokens.symbol "idd_text" >>
    eof >> 
    return (Show IDD_Text)

  (* Parser for the help command *)
  let help : (command, bytes list) MParser.t =
    Tokens.symbol "help" >> return (Show Help)

  (* Parser for the exit command *)
  let exit : (command, bytes list) MParser.t =
    Tokens.symbol "exit" >> return Exit

  (* Parser for the exit command *)
  let quit : (command, bytes list) MParser.t =
    Tokens.symbol "quit" >> return Quit


 (* Parser for commands *)
  let command : (command, bytes list) MParser.t = choice [
    update;
    load;
    policy;
    table_html;
    table_text;
    idd_pdf;
    idd_text;
    help;
    exit;
    quit;
    equiv;
  ]

end

(* A reference to the current policy and the associated string. *)
let policy : (policy * string) ref = 
  ref (Kat.Ast.Assert (Kat.Ast.True), "1")

let show_policy () = 
  printf "Policy: \n%s\n%!" (snd !policy)

let map_var_tbl tbl =
  Hashtbl.to_alist tbl
  |> List.map ~f:(fun (x,y) -> y,x)
  |> Hashtbl.of_alist_exn (module Int)

let get_idd ?(p=(fst !policy)) ?(mgr=Idd.manager ()) () =
  let tbl: int Hashtbl.M(String).t = Hashtbl.create (module String) in
  let next = ref (-1) in
  let map_var var =
    Hashtbl.find_or_add tbl var ~default:(fun () ->
      incr next;
      !next
    )
  in
  (Katbb_lib.Idd_compiler.compile_exp ~mgr ~map_var p, tbl, mgr)


let get_var_name tbl var =
    Format.sprintf "%s%s"
      (Hashtbl.find_exn tbl (Var.index var))
      (if Var.is_inp var then "?" else "!")

let show_idd_pdf () =
  let idd, tbl, _ = get_idd () in
  let tbl' = map_var_tbl tbl in
  Dd.render (idd :> Dd.t) ~var_name:(get_var_name tbl')

let show_idd_text () =
  let idd, tbl, _ = get_idd () in
  let tbl' = map_var_tbl tbl in
  printf "IDD: \n%s\n%!"
    (Dd.to_string ~var_name:(get_var_name tbl') (idd :> Dd.t))

let show_table_html () =
  let idd, tbl, _ = get_idd () in
  let tbl' = map_var_tbl tbl in
  Tables.to_table idd 
  |> Tables.render ~var_name:(fun var ->
    Format.sprintf "%s" (Hashtbl.find_exn tbl' (Var.index var))
  )

let show_table_text () =
  let idd, tbl, _ = get_idd () in
  let tbl' = map_var_tbl tbl in
  Tables.to_table idd
  |> Tables.to_string ~var_name:(fun var ->
    Format.sprintf "%s" (Hashtbl.find_exn tbl' (Var.index var))
  )
  |> printf "Table: \n%s\n%!"

let parse_command (line : string) : command option =
  match (MParser.parse_string Parser.command line []) with
  | Success command -> Some command
  | Failed (msg, e) -> (print_endline msg; None)

let help =
  String.concat ~sep:"\n\t" [
  "";
  "commands:";
  "  policy              - Displays the policy that is currently active.";
  "";
  "  <p1> = <p2>         - Tests whether policies p1 and p2 are equivalent";
  "";
  "  update <policy>     - Compiles the specified policy.";
  "";
  "  load <file>         - Loads local policy from the specified file, compiles it.";
  "";
  "  table_html          - Renders the flow-table produced by the current policy in html.";
  "";
  "  table_text          - Displays the flow-table produced by the current policy as text in the console.";
  "";
  "  idd_pdf             - Renders the IDD produced by the current policy in a pdf and opens the pdf.";
  "";
  "  idd_text            - Displays the IDD produced by the current policy as text in the console.";
  "";
  "  help                - Displays this message.";
  "";
  "  exit                - Exits the REPL.";
  "";
  "  quit                - Exits the REPL.  Equivalent to CTRL-D.";
  ""
  ]

let print_help () : unit =
  printf "%s\n%!" help

let load_file filename = 
  try
    let open In_channel in
    let chan = create filename in
    let policy_string = input_all chan |> String.strip in
    let pol = Katbb_lib.Parser.parse_string policy_string in
    close chan;
    policy := (pol, policy_string);
    show_policy ()
  with
  | Sys_error msg -> printf "Load failed: %s\n%!" msg

let equiv (p1, s1) (p2, s2) = 
  let idd1, _, mgr = get_idd ~p:p1 () in
  let idd2, _, _ = get_idd ~p:p2 ~mgr () in
  let b = Idds.Idd.equal idd1 idd2 in
  printf "Expression 1: %s \nExpression 2: %s\nEquivalent: %b\n%!" s1 s2 b


let rec repl () = 
  printf "netkat> %!";
  Reader.read_line (Lazy.force Reader.stdin) >>= fun input ->
  let handle line =
    try match line with
    | `Eof -> Shutdown.shutdown 0
    | `Ok line -> 
      match parse_command line with
		  | Some Exit | Some Quit ->
		     print_endline "Goodbye!";
		     Shutdown.shutdown 0
		  | Some (Show Policy) -> show_policy ()
		  | Some (Show Help) -> print_help ()
      | Some (Show Table_HTML) -> show_table_html ()
      | Some (Show Table_Text) -> show_table_text ()
      | Some (Show IDD_PDF) -> show_idd_pdf ()
      | Some (Show IDD_Text) -> show_idd_text ()
		  | Some (Update (pol, pol_str)) ->
          policy := (pol, pol_str)
      | Some (Equiv ((pol1, pol_str1), (pol2, pol_str2))) -> 
          equiv (pol1, pol_str1) (pol2, pol_str2)
      | Some (Load filename) -> load_file filename
		  | None -> ()
    with exn -> Location.report_exception Format.std_formatter exn
  in handle input; repl ()


let main () : unit =
  printf "Welcome to the KAT+B! REPL!\n";
  printf "Type `help` for a list of commands\n";
  ignore (repl () : 'a Deferred.t)