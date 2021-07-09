(* Time-stamp: <modified the 07/07/2021 (at 16:09) by Erwan Jahier> *)
(** A wrapper around rdbg-top to 
    - avoid typing "()" and ";;" 
    - generate initial sessions
    - manage/load previous sessions
*)
(* open Rdbg *)
open RdbgWrapperArgs

(** control the automatic addition of "()" and ";;" before
   sending to rdbg-top *)
let add_semi_ref = ref true

let rdbg_top = "rdbg-top" 

let url_doc="http://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/rdbg/"
              
let _ =
  RdbgWrapperArgs.lurette := false;
  RdbgWrapperArgs.usage := (Sys.argv.(0) ^
               " is a Reactive programs debugger. 
"^Sys.argv.(0)^" is extensible: debugging commands or functions can be programmed
"^Sys.argv.(0)^" is meant to be easily pluggable. Currently, existing plugins are: 
    "^
            (String.concat ", " GenSessions.supported_plugins)
            ^"

Examples of usage:
  rdbg -sut \"lv6 file.lus -n node\" -env \"lutin f.lut -n main\"
  rdbg -sut \"ecexe-rif f.ec node\" -env \"lutin f.lut -n main\"
  lv6 -2c -cc f.lus -n n ;rdbg -sut \"./n.exec\" -env \"lutin f.lut -n main\"

Hint: preprend rdbg by 'ledit' (or 'ledit -h rdbg-history -x').

More information: "^url_doc^"

usage: "^ Sys.argv.(0) ^" [<options>] 
where <options> are:")


let get_extension f =
  try
    let bf = Filename.chop_extension f in
    let lf,lbf = String.length f, String.length bf in
    String.sub f (lbf+1) (lf - lbf - 1)
  with _ -> ""

let args= ref []
let ml_files = ref []

let add_args arg = 
  if get_extension arg = "ml" 
  then 
    ml_files := arg::!ml_files 
  else
    args := (arg::!args)


let first_line b = (
  try (
    let f = String.index b '\n' in
    String.sub b 0 f
  ) with Not_found -> b
)

let _ =
  mkoptab ();
  try Arg.parse_argv Sys.argv !arg_options add_args !usage
  with
  | Arg.Bad msg -> 
    Printf.fprintf stderr "%s\n" (first_line msg); flush_all(); exit 2
  | Arg.Help _msg -> help  (); flush_all(); exit 2

let rdbg_top_list = [ rdbg_top; "-I"; "+rdbg" ] 
let rdbg_top_array = Array.of_list rdbg_top_list
let rdbg_top_cmd = String.concat " " rdbg_top_list

  

(**********************************************************************************)

let rdbg_cmd = ref "*"
                                             
let rec my_read_line ic =
  try 
    let str = flush stdout; input_line ic  in
    match str with
    | "#auto" -> 
       add_semi_ref:= true;
       print_string "type '#auto_off' to disable automatic semicolumn mode\n";
       flush stdout;
       my_read_line ic
    | "#auto_off" ->
       add_semi_ref := false; 
       print_string "type '#auto' to enable automatic semicolumn mode\n";
       flush stdout;
       my_read_line ic
    | "" -> if !rdbg_cmd = "" then my_read_line ic else !rdbg_cmd 
    | _ -> str
  with End_of_file -> Unix.sleep 1; "q"

         
let log_oc = if !log then open_out "rdbg.log" else stdout

(* Read  as long as  there is something to read and exit 
- block: read in blocking mode (well during 2 seconds), when I'm 
  sure something will be written on stderr, eg after the second step
- stop_logging: stop writing on rdbg.log, typically after
  the "#use rdbg-session.ml" is sent to rtop
*) 
let rec read_rdbgtop_err block stop_logging ic =
  let buff = Bytes.create 256 in
  let res  = ref "" in
  let cond = ref true in
  while !cond do
    try
      let n = input ic buff 0 256 in
      let str = Bytes.sub_string buff 0 n in
      res := !res ^ str;
      Printf.fprintf log_oc "%s%!" str; 
      if n < 256 then cond := false;
    with Sys_blocked_io -> cond := false
  done;
  if stop_logging then log := false;
  if block && !res = "" then (
    (* Printf.printf "W: nothing to on stderr. On more try after some sleep...\n%!"; *)
    Unix.sleep 2;
    read_rdbgtop_err false true ic)
  else (
    Printf.sprintf "%s%!" !res;
  )

let has_semicol str = Str.string_match (Str.regexp ".*;;[ \t]*$") str 0

(* (blocking) read until the next ";;". *)
let read_until_semicol str0 =
  let res = ref str0 in
  let cond = ref true in
  Printf.printf "[Entering the multi-line mode: type ';;' to leave it.]\n%!";
  while !cond do
    let str = read_line () in
    Printf.fprintf log_oc "%s\n%!" str;
    res := !res ^ str ;
    cond := not (has_semicol !res);
  done;
  Printf.printf "%s\n%!" !res;
  !res

let my_exit ic_stdout ic_stderr oc outstr errstr i =
  let erref = ref errstr in
  flush_all();
  Printf.fprintf log_oc "\nExit rdbg with Error %d\n%!" i;
  Printf.fprintf log_oc "%s\n%!" errstr;
  while !erref <> "" do (* make sure no error msg is lost before exit *)
    flush_all();
    erref := (read_rdbgtop_err false true ic_stderr) ^
             (read_rdbgtop_err false true ic_stdout);
    Printf.fprintf log_oc "%s\n%!" !erref;
  done;
  Printf.fprintf log_oc "\nThe end.\n%!";
  if i <> 0 then Printf.printf "\nError %d: '%s'\n%!" i outstr;
  Printf.printf "Bye\n%!" ;
  flush log_oc;
  close_out log_oc;
  close_out oc;
  exit i

  
let strsub str init len =
  try String.sub str init len
  with Invalid_argument _ ->
    Printf.printf "pb in String.sub '%s' %i %i\n%!"str init len;
    assert false

(* (blocking) read until the next "(rdbg) " prompt string. 
- first: 1st time it is called
- use_use: "#use " just have been sent to rdbg-top
*)
let use_use_reg = ref false
let first_reg = ref true
let read_rdbgtop_out oc ic_stdout ic_stderr first use_use =
  let falling_edge_use_use = !use_use_reg && not use_use in
  let falling_edge_fisrt = !first_reg && not first in
  if !verbose then Printf.printf "Start reading the rdbg-top output.\n%!";
  let buff = Bytes.create 256 in
  let res = ref "" in
  let cond = ref true in
  let s = ref 0 in
  let err = ref "" in
  while !cond do
    let n = try input ic_stdout buff 0 256 with Sys_blocked_io -> 0 in
    (* Printf.printf  "n=%i s=%i cpt=%i \n%!" n !s !cpt ; *)
    res := !res ^ (Bytes.sub_string buff 0 n);
    s := !s+n;
    (* When the rdbg prompt can not be find, it generally means that 
       something bad happened. 
    *)
    if n=0 then (
      err := !err ^ read_rdbgtop_err false use_use ic_stderr;
      if !verbose then Printf.printf "W: nothing to read on stdout\n%!";
      cond := false
    );
    if !s>=7 && strsub !res (!s-7) 7 = "(rdbg) " then (
      cond := false;
    );
    if !s>=7 && strsub !res (!s-7) 7 = "inputs:" then (
      Printf.printf "%s%!" !res;
      let str = input_line stdin in
      Printf.fprintf oc "%s\n%!" str; 
    );
  done;
  if !verbose then Printf.printf "read from rdbg-top: '%s'\n%!" !res;
  if
    Str.string_match (Str.regexp_string "Exception: Sys_error") !res 0
  then (
    let msg = "Error: Something bad happened in the ocaml toplevel:\nBye\n%!" in
    my_exit ic_stdout ic_stderr oc msg !res 2
  );
  let stderr_str = if first then "" else
      read_rdbgtop_err use_use 
        (falling_edge_use_use || ((not use_use) && falling_edge_fisrt)) ic_stderr
  in
  if !log then (
    Printf.fprintf log_oc "stderr:%sstdout: %s\n%!" stderr_str !res;  
    Printf.printf "nb: some messages are written to the rdbg.log file\n";
    if not (GenSessions.empty_session ()) then
      Printf.printf  "--> type 'l' to obtain the list of Level 0 commands\n    or"; 
    Printf.printf  " type 'h' to get a short manual\n%!"; 
    Printf.printf "(rdbg) ";
    flush_all() ;
  ) else (
    (* remove boring strings from stdout *)
    let boring_list = ["- : unit = ()"; "nil"] in
    List.iter (fun boring ->
        try
          let s = String.length !res in
          let n = Str.search_backward (Str.regexp_string boring) !res s in
          let m = Str.search_forward (Str.regexp_string "\n") !res n in
          res := Printf.sprintf "%s%s"
              (strsub !res 0 (max 0 (n-1))) (strsub !res m (s-m))
        with Not_found -> ()
      ) boring_list ;
    Printf.printf "%s%s" stderr_str !res
  );
  if !log then 
    (
      (* when "#use " is used, some errors/warnings may be hidden in rdbg.log
         Try to print them on the rdbg oc
      *) 
      try
        let i1 = Str.search_forward (Str.regexp_string "Error") !res 0 in
        let i2 = try Str.search_backward (Str.regexp_string "File ") !res i1
          with _  -> i1
        in
        let i3 = try
            Str.search_forward (Str.regexp_string "- : unit = ()") !res i1
          with _ -> String.length !res
        in
        let msg = Printf.sprintf
            "\n%s\nAn error occured when reading the my-rdbg-tuning.ml file\n\
             cf \"rdbg.log\" for more messages\n%!"
            (strsub !res i2 (i3-i2))
        in
        flush_all(); 
        my_exit ic_stdout ic_stderr oc msg "" 3
      with Not_found -> (
          try 
            let i1 = Str.search_forward (Str.regexp_string "Exception") !res 0 in
            let i2 = Str.search_forward (Str.regexp_string "\n") !res i1 in
            let msg = Printf.sprintf
                "\n%s\nAn error occured when reading the my-rdbg-tuning.ml file\n\
                 cf \"rdbg.log\" for more messages\n%!"
                (strsub !res i1 (i2-i1))
            in
            flush_all(); 
            my_exit ic_stdout ic_stderr oc msg !res 4
          with Not_found -> (
              try
                (* No errors. Let's search for warning *)
                let i1 = Str.search_forward (Str.regexp_string "Warning") !res 0 in
                let i2 = try Str.search_backward (Str.regexp_string "File ") !res i1
                  with _  -> i1 
                in
                let i3 = try
                    Str.search_forward (Str.regexp_string "- : ") !res i1
                  with _ -> String.length !res
                in
                Printf.printf
                  "\n%s\nAt least a Warning occured when reading the \
                   my-rdbg-tuning.ml file\ncf \"rdbg.log\" for more messages\n%!"
                  (strsub !res i2 (i3-i2));
              with Not_found -> ()));
        try 
          let n = Str.search_forward (Str.regexp_string "  1   1") !res 0 in
          let m = Str.search_forward (Str.regexp_string "\n") !res n in
          Printf.printf "\n%s\n(rdbg) %!" (strsub !res n (m-n));
        with Not_found -> ()
    );
  use_use_reg := use_use;
  first_reg := first;
  if !verbose then Printf.printf "reading the rdbg-top output: done.\n%!" else ()

let string_use_use str = Str.string_match (Str.regexp ".*#use.*") str 0

let start_with str1 str2 =
  let l1, l2 = String.length str1, String.length str2 in
  l1 >= l2 && String.sub str1 0 l2 = str2

let rec myloop ic_stdout ic_stderr ic oc str =
  let str = if str = "" then my_read_line ic else str in
  let s = String.length str in
  let str = 
    (* a clutch I'd prefer to avoid *)
    if start_with str "man" then "RdbgMain.man" 
    else
      (* sugaring help and apropos *)
      if start_with str "help " && not (String.contains str '\"') then
        Printf.sprintf "help \"%s\"" (String.sub str 5 (s-5))
      else if start_with str "h " && not (String.contains str '\"') then
        Printf.sprintf "help \"%s\"" (String.sub str 2 (s-2))
      else if start_with str "apropos " && not (String.contains str '\"') then
        Printf.sprintf "apropos \"%s\"" (String.sub str 8 (s-8))
      else if start_with str "a " && not (String.contains str '\"') then
        Printf.sprintf "apropos \"%s\"" (String.sub str 2 (s-2))
      else if str ="a" || str = "apropos" then
        Printf.sprintf "apropos \"\""
      else if str ="h" || str = "help" then
        Printf.sprintf "RdbgMain.man"
      else str
  in
  let str =
    if ((str.[0] = ' ' || start_with str "let ") && not (has_semicol str))
    then read_until_semicol str else str
  in
  let add_par = if String.contains str ' ' then "" else "()" in
  let semicol = if (!add_semi_ref && str.[0] <> ' ') && not (has_semicol str) 
                then ";;" else ""
  in 
  if str = "q" || str = "quit" then (
    ignore (Unix.close_process_full (ic_stdout,  oc, ic_stderr)); flush_all();
    my_exit ic_stdout ic_stderr oc "Bye" "" 0)  
  else (
    rdbg_cmd := if !rdbg_cmd = "*" then "" else str;
    let str = str ^ add_par ^ semicol in
    if !verbose then 
      Printf.printf "rdbg sends to the ocaml toplevel the string: \"%s\"\n%!" str;
    output_string oc (str ^ "\n");
    flush oc;
    let str =
      if string_use_use str then
        (" emacs := " ^ (if !emacs_mode then "true" else "false") ^ ";;" )
      else my_read_line ic
    in
    myloop ic_stdout ic_stderr ic oc str
  ) 

let ls str = Mypervasives.run ("ls -v "^str) (fun s -> Some s)
let get_info session_file =
  let cmd = Printf.sprintf "head -n 3 %s | tail -n 2" session_file in
  match Mypervasives.run cmd (fun s -> Some s) with
  | [ l1; l2 ]  -> (
      try
        let ll1 = Str.split (Str.regexp " ") l1 in
        let date = List.hd (List.tl (List.tl (List.tl (List.tl ll1)))) in
        let l2 = String.sub l2 2 ((String.length l2) -4) in
        let res = Printf.sprintf "(%s: %s)" date l2 in
        if String.length res < 46 then res else  (String.sub res 0 45) ^ "[...])"
      with _ -> ""
    )
  | _ -> ""

let rdbg_session_manager outc = 
  (* Deals with existing rdbg-sessions, 
         read user input, 
         create a new session if asked
         launch the chosen session
   *)
  let ic, close_ic = match !RdbgWrapperArgs.input_file with
    | None -> stdin, fun () -> ()
    | Some f ->
       if Sys.file_exists f then
         let ic = open_in f in
         ic, fun () -> close_in ic
       else (
         Printf.printf "Error: file %s not found.\n%!" f;
         exit 2
       )
  in
  let call_myloop str =
    let ic_stdout, oc, ic_stderr =
      Unix.open_process_full rdbg_top_cmd (Unix.environment ())
    in
    Unix.set_nonblock (Unix.descr_of_in_channel ic_stdout); 
    Unix.set_nonblock (Unix.descr_of_in_channel ic_stderr);   

    (* *)
    let first = ref true in
    let rec loop_read () =
      flush_all();
      if !first then Thread.delay 1.5;
      (* not  a big deal, but  otherwise, the ocamltop process  do not
         have the time to write on its stdout, and only the first read
         can take case of hiding the ocaml loading msgs.  *)
      flush_all(); 
      Thread.delay 0.05;
      read_rdbgtop_out oc ic_stdout ic_stderr !first !first;
      first := false;
      loop_read ()
    in
    let _t = Thread.create loop_read () in
    myloop ic_stdout ic_stderr ic oc (str^";;\n"^ !cmd^"\n")
  in
  let ml_files = ls "rdbg-session*.ml" in
  let dir = List.mapi (fun i f ->
                let info = get_info f in
                if i = 0 then
                  Printf.sprintf "\n []  #use \"%s\"  %s"  f info
                else
                  Printf.sprintf "\n [%i] #use \"%s\" %s" i f info)
              ml_files
  in
  let dir = dir@["\n ["^(if dir = [] then "" else "c")^ "] create a fresh session\n";
                 " [q] quit\n"]
  in
  let dir = "Enter one of the following key (the first is the default one):" ^
              (String.concat "" dir)^ "\n" in
  let info() = print_string dir in
  let numbers = (List.mapi (fun i _ -> Printf.sprintf "%i" i) ml_files) in
  let cmds = ""::(List.tl (numbers @ ["c";"q"])) in
  info();
  let rec read_c () =
    Printf.printf "[%s]: " (String.concat "/" cmds);
    let c = read_line() in
    if List.mem c cmds then c else
      (Printf.printf "invalid key (%s), try again\n%!" c; read_c ())
  in
  try
    if !go then (* non-interactive mode *)
      let ns_fn,_ = GenSessions.file outc in
      let ns = Printf.sprintf "#use \"%s.ml\"" ns_fn in
      output_string outc ns;
      flush stdout;
      call_myloop ns
    else (* interactive mode *)
      let c = read_c () in
      if c = "c" || (ml_files = [] && c = "") then (
        let ns_fn,_ = GenSessions.file outc in
        let ns = Printf.sprintf "#use \"%s.ml\"" ns_fn in
        output_string outc ns;
        flush stdout;
        call_myloop ns
      )
      else
        if  c = "" then (
          let last_ml = List.hd ml_files in
          let last = Printf.sprintf "#use \"%s\"" last_ml in
          if !log then Printf.fprintf log_oc "%s" last else Printf.printf "%s" last;
          flush stdout;
          call_myloop last
        ) else
          if List.mem c ["q"] then (flush_all(); close_ic (); exit 0)
          else (
            try
              let i = int_of_string c in
              let f = List.nth ml_files i in
              let str = Printf.sprintf "#use \"%s\"" f  in
              output_string outc str;
              flush stdout;
              call_myloop str
            with
            | Invalid_argument _
              | Failure _ ->
               call_myloop  ""
          )
  with 
  | End_of_file -> Printf.printf "bye.\n"; flush stdout; exit 2
  | e  -> 
     Printf.eprintf "Error in rdbg: %s\n" (Printexc.to_string e);
     flush stderr;
     exit 2


let _ =   
    if !verbose then Printf.printf "  %s communicates with the process \"%s\"\n%!"
        Sys.argv.(0) rdbg_top_cmd;
    let outc = if !log then log_oc else stdout in
    if !RdbgWrapperArgs.lurette then
      GenSessions.lurette outc
    else
      rdbg_session_manager outc
