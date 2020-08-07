open Fmlib
open Common

module Parser = Parser_lang
module Repl_parser = Parser.Make (Parser.Command)










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






    let evaluate (_:command_line): unit Io.t =
        let module Repl = Repl.Make (Io) in
        Repl.run_eval ()

    let compile_module _: unit Io.t =
        let module Compile = Module.Make (Io) in
        Compile.run ()


    let repl _: unit Io.t =
        let module Repl = Repl.Make (Io) in
        Repl.run_cli ()



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
        ;
        "evaluate", evaluate,
        "Read an expression from standard input and evaluate it."
        ;
        "module", compile_module,
        "Compile a module from standard input. The module might have commands \
        like ':evaluate <expression>' or ':typecheck <expression>' in it."
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
