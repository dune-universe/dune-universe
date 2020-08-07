(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support

let fill_string (n:int) (str:string): string =
  let len = String.length str in
  if len < n then
    str ^ String.make (n - len) ' '
  else
    str


let print_string_wrapped (f:Format.formatter) (str:string): unit =
  let len = String.length str in
  Format.fprintf f "@[<hov>";
  for i = 0 to len - 1 do
    let c = str.[i] in
    if c = ' ' || c = '\n' then
      Format.fprintf f "@ "
    else
      Format.fprintf f "%c" c
  done;
  Format.fprintf f "@]"






type t = {
    mutable command: string list;  (* in reversed order *)
    mutable work_dir: string;
    mutable package_paths: string list;
    mutable verbosity: int;
    mutable force: bool;
    mutable arguments: string list (* in reversed order *)
  }

let command (cmd_line:t): string list =
  List.rev cmd_line.command

let working_directory (cmd_line:t): string =
  cmd_line.work_dir

let verbosity (cmd_line:t): int =
  cmd_line.verbosity

let arguments (cmd_line:t): string list =
  List.rev cmd_line.arguments

let package_paths (cmd_line:t): string list =
  List.rev cmd_line.package_paths

let is_forced (nme: int) (cmd:t): bool =
  cmd.force && (cmd.arguments = [] || List.mem (ST.string nme) cmd.arguments)


type alba_option = {
    key:  string;
    key_text:string;
    text: string
  }


let compile_text = "Compile the modules provided on the command line and all its \
dependencies if compilation is required. If no modules are provided all modules \
of the package which require compilation are compiled."

let help_text = "Give general help or help for a specific command."

let status_text = "Display all modules which require compilation or recompilation."

let init_text = "Initialize the current directory as an Albatross directory."

let verbosity_option = {
  key  = "-verbosity";
  key_text = "-verbosity <level>";
  text = "Verbosity level (default 1)"
}

let work_dir_option = {
  key  = "-work-dir";
  key_text  = "-work-dir <path>";
  text = "Path to the directory where the Alba package is located \
(default: current working directory)"
}

let force_option = {
  key = "-force";
  key_text = "-force";
  text = "Forced compilation, even if it is not necessary"
}


let package_path_option = {
    key = "-I";
    key_text = "-I <path>";
    text = "Add <path> to search path for used packages"
  }


type command = {
    name: string;
    details: command_details
  }
and command_details =
    Final of (string*alba_option list)
  | Subcommands of command list


let rec command_options0 (cmd:command) (lst:alba_option list): alba_option list =
  let has_key key lst =
    try
      ignore (List.find (fun opt -> opt.key = key) lst);
      true
    with Not_found ->
      false
  in
  match cmd.details with
    Final (_,opt_lst) ->
      List.fold_left
        (fun lst opt ->
          if has_key opt.key lst then
            lst
          else
            opt :: lst
        )
        lst
        opt_lst
  | Subcommands menu ->
      menu_options0 menu lst

and menu_options0 (menu:command list) (lst: alba_option list): alba_option list =
  List.fold_left
    (fun lst cmd -> command_options0 cmd lst)
    lst
    menu

let menu_options (menu:command list): alba_option list =
  menu_options0 menu []

let command_options (cmd:command): alba_option list =
  command_options0 cmd []


let initial_menu = [
  { name = "compile";
    details = Final(compile_text,
                    [work_dir_option;force_option;
                     verbosity_option;package_path_option])
  };
  { name = "status";
    details = Final(status_text,[work_dir_option;package_path_option]) };
  { name = "init";    details = Final(init_text,[work_dir_option]) };
  { name = "version";
    details = Final("Display the version of the Albatross compiler",[])};
  { name = "help";    details = Final(help_text,[]) }
]


let find_command_in_menu (name:string) (menu:command list): command =
  List.find
    (fun cmd -> cmd.name = name)
    menu


let find_command (cmds: string list): command =
  match cmds with
    [] ->
      raise Not_found
  | name :: tail ->
      List.fold_left
        (fun cmd name ->
          match cmd.details with
            Final _ ->
              raise Not_found
          | Subcommands menu ->
              find_command_in_menu name menu
        )
        (find_command_in_menu name initial_menu)
        tail




let print_commands (f:Format.formatter) (menu:command list): unit =
  let rec print_menu (menu:command list) (lst: string list): unit  =
    List.iter
      (fun cmd -> print_command cmd lst)
      menu
  and print_command (cmd:command) (lst:string list): unit =
    let lst = cmd.name :: lst
    in
    match cmd.details with
      Final (text,_) ->
        let cmd_string = fill_string 20 (String.concat " " (List.rev lst))
        in
        Format.fprintf f "  %s " cmd_string;
        print_string_wrapped f text;
        Format.fprintf f "@,"
    | Subcommands menu ->
        print_menu menu lst
  in
  print_menu menu []



let print_options (f:Format.formatter) (options: alba_option list): unit =
  List.iter
    (fun opt ->
      Format.fprintf f "  %s " (fill_string 20 opt.key_text);
      print_string_wrapped f opt.text;
      Format.fprintf f "@,"
    )
    options



let print_menu_help (f:Format.formatter) (menu:command list): unit =
  Format.fprintf f "@[<v>Usage: alba command options arguments@,@,";
  Format.fprintf f "Commands:@,";
  print_commands f menu;
  let opts = menu_options menu in
  if opts = [] then
    ()
  else begin
    Format.fprintf f "@,Options:@,";
    print_options f opts
  end;
  Format.fprintf f "@]@."


let print_general_help (f:Format.formatter): unit =
  print_menu_help f initial_menu



let print_help (f:Format.formatter) (cmd_line:t): unit =
  let cmds = arguments cmd_line in
  match cmds with
    [] ->
      print_general_help f
  | _ ->
      try
        let cmd = find_command cmds in
        print_menu_help f [cmd]
      with Not_found ->
        Format.fprintf
          f
          "Unknown command \"%s\"@.@."
          (String.concat " " cmds);
        print_general_help f


let get (): t =
  let menu = ref initial_menu
  and cmd_line = {
    command = [];
    work_dir = "";
    package_paths = [];
    verbosity = 1;
    force = false;
    arguments = [];
  }
  in
  let anon_fun (str:string): unit =
    match !menu with
      [] ->
        cmd_line.arguments <- str :: cmd_line.arguments
    | _ ->
        begin
          cmd_line.command <- str :: cmd_line.command;
          try
            let cmd = find_command_in_menu str !menu in
            match cmd.details with
              Final(_) ->
                menu := []
            | Subcommands lst ->
                menu := lst
          with Not_found ->
            Format.eprintf "Unknown command \"%s\"@.@."
              (String.concat " " (command cmd_line));
            print_general_help Format.err_formatter;
            exit 1
        end
  in
  let do_nothing () = () in
  Arg.parse
    [(work_dir_option.key,
      Arg.String (fun str -> cmd_line.work_dir <- str),
      work_dir_option.text);
     (verbosity_option.key,
      Arg.Int (fun i -> cmd_line.verbosity <- i),
      verbosity_option.text);
     (force_option.key,
      Arg.Unit (fun () -> cmd_line.force <- true),
      force_option.text);
     (package_path_option.key,
      Arg.String
        (fun str -> cmd_line.package_paths <- str :: cmd_line.package_paths),
      package_path_option.text);
     ("-help", Arg.Unit do_nothing, "");
     ("--help", Arg.Unit do_nothing, "");
   ]
    anon_fun
    "usage: alba command option arguments";
  begin
    match cmd_line.command with
      [] ->
        print_general_help Format.err_formatter;
        exit 1
    | ["help"] ->
        print_help Format.std_formatter cmd_line;
        exit 0
    | _ ->
        ()
  end;
  cmd_line
