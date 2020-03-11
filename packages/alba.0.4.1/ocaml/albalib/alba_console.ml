open Fmlib
open Common

module Parser = Parser_lang
module Repl_parser = Parser.Make (Parser.Command)

module Position = Character_parser.Position
type pos = Position.t
type range = pos * pos









module Pretty_make (Io:Io.SIG) =
  struct
    module Position = Character_parser.Position
    module Located = Character_parser.Located
    module Expression = Ast.Expression
    module Out = Fmlib.Io.Output (Io)
    module PP =  Pretty_printer.Pretty (Out)
    include PP

    let put_left (width:int) (s:string): t =
      let len = String.length s in
      if len < width then
        chain [string s; fill (width - len) ' ']
      else
        string s

    let print_list (l:'a list) (f:'a -> t): t =
      List.fold_right
        (fun a pr ->
          pr <+> f a)
        l
        empty

    let indented_paragraph (p:t): t =
      cut <+> nest 4 p <+> cut


    let paragraphs (lst:t list): t =
      chain_separated lst cut


    let error_header
          (error_type:string)
        : t
      =
      (* Print the error header. *)
      let err = " ERROR " in
      let nfill =
        max 0 (80 - 3 - (String.length error_type + String.length err))
      in
      chain [fill 2 '-';
             char ' ';
             string error_type;
             string err;
             fill nfill '-';
             cut;
             cut]


    let print_source (source:string) ((pos1,pos2):range): t =
      (* Print the source code in [range] with line numbers and error markers
       *)
      let start_line = Position.line pos1
      and end_line   = Position.line pos2
      and start_col  = Position.column pos1
      and end_col    = Position.column pos2
      and len        = String.length source
      in
      let end_col =
        if start_line = end_line && start_col = end_col then
          end_col + 1
        else
          end_col
      in
      assert (start_line <= end_line);
      assert (start_line < end_line || start_col < end_col);
      let number_width =
        String.length (string_of_int (end_line + 1))
      in
      let print_line start beyond line_no =
        let line_no_str = string_of_int (line_no + 1) in
        fill (number_width - String.length line_no_str) ' '
        <+> string line_no_str
        <+> string "| "
        <+> substring source start (beyond - start)
        <+> cut
      and skip_line_no =
        fill (number_width + 2) ' '
      in
      let rec print (char_offset:int) (line_no:int): t =
        if len <= char_offset then
          empty
        else
          let pos_newline = String.find (fun c -> c = '\n') char_offset source
          in
          (if line_no = start_line && start_line = end_line then
             print_line char_offset pos_newline line_no
             <+> skip_line_no
             <+> fill start_col ' '
             <+> fill (end_col - start_col) '^'
             <+> cut
           else if line_no = start_line && start_line < end_line then
             skip_line_no
             <+> fill start_col ' '
             <+> char 'v'
             <+> fill 3 '.'
             <+> cut
             <+> print_line char_offset pos_newline line_no
           else if line_no = end_line && start_line <> end_line then
             print_line char_offset pos_newline line_no
             <+> skip_line_no
             <+> fill (end_col - 1) '.'
             <+> char '^'
             <+> cut
           else if pos_newline + 1 = len && start_line = line_no + 1 then
             print_line char_offset pos_newline line_no
             <+> skip_line_no
             <+> fill (pos_newline - char_offset) ' '
             <+> char '^'
             <+> cut
           else
             (* normal line *)
             print_line char_offset pos_newline line_no
          )
          <+> print (pos_newline + 1) (line_no + 1)
      in
      print 0 0


    let explain_operator_precedence_error (op1: string) (op2: string): t =
      let source_text op1 op2 =
        chain [string "_ ";
               string op1;
               string " _ ";
               string op2;
               string " _"]
      and left op1 op2 =
        chain [string "( _ ";
               string op1;
               string " _ ) ";
               string op2;
               string " _"]
      and right op1 op2 =
        chain [string "_ ";
               string op1;
               string " ( _ ";
               string op2;
               string " _ )"]
      in
      paragraphs
        [string "I am not able to group your operator expression.";
         indented_paragraph @@
           source_text op1 op2;
         string "I can either group the first two";
         indented_paragraph @@
           left op1 op2;
         string "or group the second two";
         indented_paragraph @@
           right op1 op2;
         fill_paragraph
           "However the precedence and associativity of these operators \
            don't give me enough information. Please put parentheses to \
            indicate your intention."
        ]



    let print (fd:Io.File.Out.fd) (width:int) (pp:t): unit Io.t =
      Out.run fd (PP.run 0 width width pp)
  end (* Pretty_make *)









module Located = Character_parser.Located












module Make (Io:Io.SIG) =
  struct
    module Pretty = Pretty_make (Io)

    type command_line = {
        command: string option;
        workspace: string;
        package_paths: string list;
        verbosity: int;
        force: bool;
        arguments: string list (* reversed *)
      }

    module CLP =
      Argument_parser.Make (struct type t = command_line end)

    let find_in_array (p:'a->bool) (arr: 'a array): int =
      Interval.find
        (fun i -> p arr.(i))
        0
        (Array.length arr)

    let find_elem_in_array (a:'a) (arr:'a array): int =
      find_in_array (fun e -> e = a) arr

    let has_file (name:string) (arr:string array): bool =
      find_elem_in_array name arr < Array.length arr

    let rec find_workspace
          (path:string)
        : (string * string array) option Io.t =
      (* Find a directory with a file named "alba-workspace.yml" in the
         directory [path] and all its parent directories.

         Return the path to the directory, the directory entries and the
         position of the file "alba-workspace.yml" in the entries.  *)
      let open Io in
      Directory.read path >>= function
      | None ->
         return None
      | Some arr ->
         let len = Array.length arr in
         let pos = find_elem_in_array "alba-workspace.yml" arr
         in
         if pos = len then (* not the root of the workspace *)
           match Path.split path with
           | None ->
              return None
           | Some (dir,_) ->
              find_workspace dir
         else
           return @@ Some (path,arr)

    let find_packages
          (ws_path:string) (entries:string array)
        : string list Io.t =
      (* Find the packages in the workspace [ws_path].

         Return a list of paths
       *)
      let open Io in
      let rec find path entries lst =
        let len = Array.length entries in
        let rec find_in_entries i lst =
          if i = len then
            return lst
          else
            let path1 = Path.join path entries.(i) in
            Directory.read path1 >>= function
            | None ->
               find_in_entries (i+1) lst
            | Some entries1  ->
               find path1 entries1 lst >>= fun lst ->
               find_in_entries (i+1) lst
        in
        if has_file "alba-package.yml" entries then
          return @@ path :: lst
        else
          find_in_entries 0 lst
      in
      find ws_path entries []


    let explore_workspace (cmd:command_line)
        : (string * string list) option Io.t =
      (* Find the root of the workspace and a list of package directories in
         the workspace.  *)
      let open Io in
      Stdout.line "explore workspace ..." >>= fun _ ->
      Path.absolute cmd.workspace >>= fun path ->
      find_workspace (Path.normalize path) >>= function
      | None ->
         return None
      | Some (path, entries) ->
         find_packages path entries >>= fun pkgs ->
         return @@ Some (path,pkgs)





    let compile (cmd:command_line): unit Io.t =
      Io.(Stdout.line "compile ..." >>= fun _ ->
          explore_workspace cmd >>= function
          | None ->
             Stdout.line "no workspace found"
          | Some (ws_path, _) ->
             Stdout.line ("workspace <" ^ ws_path ^ ">"))

    let status (_:command_line): unit Io.t =
      Io.Stdout.line "status ..."







    let report_parse_problem
          (src: string)
          (p: Repl_parser.parser)
        : Pretty.t
      =
     let open Pretty in
     let error = Repl_parser.error p in
     if Repl_parser.Error.is_semantic error then
       match Repl_parser.Error.semantic error with
       | Parser.Problem.Operator_precedence (range, op1, op2) ->
          chain
            [ error_header "SYNTAX";
              print_source src range;
              cut;
              explain_operator_precedence_error op1 op2;
              cut
            ]

       | Parser.Problem.Illegal_name (range, expect) ->
          chain
            [ error_header "SYNTAX";
              print_source src range;
              cut;
              fill_paragraph
                ("I was expecting " ^ expect);
              cut
            ]

       | Parser.Problem.Illegal_command (range, _) ->
          chain
            [ error_header "SYNTAX";
              print_source src range;
              cut;
              string "Illegal command";
              cut
            ]
       | Parser.Problem.Ambiguous_command (range, _) ->
          chain
            [ error_header "SYNTAX";
              print_source src range;
              cut;
              string "Ambiguous command";
              cut
            ]
        | Parser.Problem.Duplicate_argument range ->
            chain [ error_header "SYNTAX";
                    print_source src range;
                    cut;
                    wrap_words "I found a duplicate argument name. All names \
                                of formal arguments must be different.";
                    cut; cut]
     else
       let pos = Repl_parser.position p in
       chain
         [ error_header "SYNTAX";
           print_source src (pos, pos);
           match Repl_parser.Error.expectations error with
           | [] ->
              assert false (* Illegal call! *)
           | [e] ->
              chain [string "I was expecting";
                     nest_list 4 [cut; cut; string e];
                     cut]
           | lst ->
              chain [string "I was expecting one of";
                     cut;
                     indented_paragraph @@
                       print_list lst
                         (fun e ->
                           chain [
                               string "- ";
                               string e;
                               cut])
                ]
         ]



    let build_and_compute
        (src: string)
        (e: Ast.Expression.t)
        (compute: bool)
        : Pretty.t
        =
        let std_context = Context.standard () in
        match Builder.build e std_context with
        | Error (range, descr) ->
            let module Builder_print = Builder.Print (Pretty) in
            let open Pretty in
            error_header "TYPE"
            <+> print_source src range
            <+> cut
            <+> Builder_print.description descr
            <+> cut
        | Ok lst ->
            Pretty.(
                cut
                <+> paragraphs
                (List.map
                    (fun (t,tp) ->
                        let t =
                            if compute then
                                Context.compute t std_context
                            else
                            t
                        in
                        let module P = Context.Pretty (Pretty) in
                        P.print (Term.Typed (t, tp)) std_context
                        <+> cut)
                    lst)
                <+> cut
         )



    let repl (_:command_line): unit Io.t =
      let module State =
        struct
          type t = string option
          let string (s: t): string =
            Option.value s
          let init: t =  Some ""
          let exit: t =  None
          let prompt (s:t): string option =
            Option.map
              (fun s ->
                if s = "" then "> " else "| ")
              s
          let is_last (line: string): bool =
            let len = String.length line in
            len = 0 || line.[len-1] <> ' '
          let add (line: string) (s: t): t =
            Option.map
              (fun s ->
                if s = "" then
                  line
                else
                  s ^ "\n" ^ line)
              s
        end
      in
      let parse (s:string): Repl_parser.parser =
        let len = String.length s in
        let rec parse i p =
          let more = Repl_parser.needs_more p in
          if i < len && more then
            parse (i+1) (Repl_parser.put_char p s.[i])
          else if more then
            Repl_parser.put_end p
          else
            p
        in
        parse 0 Repl_parser.(make command)
      in
      let command (src: string): State.t Io.t =
        let print pr =
          Io.(Pretty.print File.stdout 80 pr)
        in
        let p = parse src in
        assert (Repl_parser.has_ended p);
        match Repl_parser.result p with
        | Some Parser.Command.Do_nothing ->
            Io.return State.init
        | Some Parser.Command.Exit ->
            Io.return State.exit
        | Some (Parser.Command.Evaluate e) ->
            Io.(print (build_and_compute src e true)
                >>= fun _ -> return State.init )
        | Some (Parser.Command.Type_check e) ->
            Io.(print (build_and_compute src e false)
                >>= fun _ -> return State.init )
        | None ->
            Io.(print (report_parse_problem src p)
                >>= fun _ -> return State.init)
      in
      let next (s:State.t) (line:string): State.t Io.t =
        let s = State.add line s in
        if State.is_last line then
          command (State.string s)
        else
          Io.return s
      and stop (s:State.t): State.t Io.t =
        Io.return s
      in
      Io.(cli_loop State.init State.prompt next stop >>= fun _ -> return ())





    let commands: (string*(command_line->unit Io.t)*string) list =
      ["compile", compile,
       "Compile the modules provided on the command line and all its \
        dependencies if compilation is required. If no modules are provided \
        all modules of the package which require compilation are compiled."
      ;
        "status", status,
        "Display all modules which require compilation or recompilation."
      ;
        "repl", repl,
        "Start an interactive programming session."
      ]

    let command_options: (CLP.key*CLP.spec*CLP.doc) list =
      let open CLP in
      [("-verbosity",
        Int (fun i a -> {a with verbosity = i}),
        "Verbosity level (default 1)"
       );

       ("-w", String (fun s a -> {a with workspace = s}),
        "Path into an Alba workspace (default: current working directory)");

       ("-I", String (fun s a -> {a with package_paths =
                                           s :: a.package_paths}),
        "Add argument to search path for used packages");

       ("-force", Unit (fun a -> {a with force = true}),
        "Force compilation, even if it is not necessary")
      ]

    let parse (args:string array): (command_line,CLP.error) result =
      let open CLP in
      parse
        args
        {command = None;
         workspace = "";
         package_paths = [];
         verbosity = 1;
         force = false;
         arguments = []}
        command_options
        (fun s a ->
          match a.command with
          | None ->
             {a with command = Some s}
          | Some _ ->
             {a with arguments = s :: a.arguments})






    let print_options: Pretty.t =
      let open Pretty in
      chain
        (List.map
           (fun (key,spec,doc) ->
             chain [cut; put_left 20 (key ^ CLP.argument_type spec);
                    nest 20 @@ fill_paragraph doc])
           command_options)


    let print_commands: Pretty.t =
      let open Pretty in
      chain
        (List.map
           (fun (cmd,_,doc) ->
             chain [cut; put_left 10 cmd; nest 10 @@ fill_paragraph doc])
           commands)


    let print_usage: Pretty.t =
      let open Pretty in
      chain
        [string "Usage: alba command options arguments";
         cut; cut;
         nest_list 4 [string "Commands:"; print_commands];
         cut; cut;
         nest_list 4 [string "Options:";  print_options];
         cut]

    let print_error (s:string): unit Io.t =
      let open Pretty in
      print
        Io.File.stderr
        80
        (string "Error: " <+> string s <+> cut <+> cut <+> print_usage)






    let run (): unit =
      let open Io in
      Process.execute
        (Process.command_line >>= fun args ->
         match parse args with
         | Ok cl ->
            begin
              match cl.command with
              | None ->
                 print_error "no commands given"
              | Some cmd ->
                 match
                   List.find (fun (c,_,_) -> c = cmd) commands
                 with
                 | None ->
                    print_error ("Unknown command '" ^ cmd ^ "'")
                     | Some (_,f,_) ->
                        f cl
            end
         | Error e ->
            print_error (CLP.string_of_error e) >>= fun _ ->
            Process.exit 1
        )
  end
