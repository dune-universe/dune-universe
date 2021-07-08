(* Time-stamp: <modified the 23/08/2019 (at 15:44) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
*)

open LocalGenlex

let rif_file = ref ""
let plot_file = ref ""
let dynamic = ref false
let grid = ref true
let min_step = ref None
let max_step = ref None
let window_size = ref 40
let vars_to_hide = ref []
let vars_to_show = ref []

(********************************************************************************)
                       
let (readfile: string -> string) = 
  fun name -> 
    let chanin = open_in_bin name in 
    let len = 1024 in 
    let s = Bytes.create len in 
    let buf = Buffer.create len in 
    let rec iter () = 
      try 
	     let n = input chanin s 0 len in 
	       if n = 0 then () else ( 
            Buffer.add_subbytes buf s 0 n; 
            iter () 
          )
      with 
	       End_of_file -> () 
    in 
    let remove_control_m str =
      Str.global_replace (Str.regexp "\013") "" str
    in
    let str =
      iter (); 
      close_in chanin; 
      Buffer.contents buf 
    in
      if str = "" then (
	     output_string stderr ("*** File " ^ name ^ " is empty!\n"); "")
      else (
	     remove_control_m str)

(********************************************************************************)
(* Well, using reg expr would have been simpler actually... *)
let lexer = make_lexer ["#"; "outputs"; "inputs"; "columns"; ":"; "|"; ")";"("]
type tok = token Stream.t

let verbose = ref false

let debug_msg msg =     
  if !verbose then (output_string stdout ("\ngnuplot-rif: "^msg); flush stdout)

let (_print_debug : string -> tok -> unit) =
  fun msg tok ->
    if !verbose then (
	   output_string stdout ((string_of_int (Stream.count tok)) ^ ": " ^ msg);
	   flush stdout)
    else
      ()

(* for debuging *)
let tok2str = function
  | Kwd  s -> s
  | Ident  s -> s
  | Int i -> string_of_int i
  | Float  f -> string_of_float f
  | String  s -> s
  | Char c -> Char.escaped c

let ftok2str stream =
  match Stream.peek stream with
    | None -> "" | Some tok -> tok2str tok

(********************************************************************************)
(* get var type in the rif file *)
(* name, type, position (starting from 0) *)
type vtypes_tbl = (string * (string * int * bool)) list

let (get_var_types : string -> vtypes_tbl) =
  fun rif_file -> 
    let _ = debug_msg "try to find the variable names and types" in
    let tbl = ref [] in
    let s_ref = ref (lexer (Stream.of_string rif_file)) in (* recovering stupid errors *)
    let pos_ref = ref 0 in
    let is_input = ref true in
    let rec aux pos s = (* aux looks for the first "string" *)
      s_ref := s;
      pos_ref := pos;
      let _ = debug_msg ("\t aux: "^(string_of_int (Stream.count s))^" pos="^
                         (string_of_int pos)^" ; token='"^ (ftok2str s) ^"'") 
      in
      match Stream.next s with 
      | String(id)  -> aux2 pos id  s
      | Ident(_)  ->  aux pos s 
      | Int(_)  ->  aux pos s 
      | Float(_)  ->  aux pos s 
      | Kwd("inputs")   -> is_input:=true;aux pos s 
      | Kwd("outputs")  -> is_input:=false;aux pos s 
      | Kwd(_) ->  aux pos s 
      | Char(_)  -> aux pos s 

    and aux2 pos id s =  (* aux2 looks for the next ":" *)
      let _ = debug_msg ("aux2: "^(string_of_int (Stream.count s))^" pos="^
                         (string_of_int pos)^" ; token='"^ (ftok2str s) ^"'\n") in
      match Stream.next s with 
      | Kwd( ":") -> aux3 pos id s
      | Ident(_) ->  aux pos s 
      | Int(_) ->  aux pos s 
      | Float(_) ->  aux pos s 
      | Kwd(_) ->  aux pos s 
      | Char(_) -> aux pos s 
      | String(_)  -> aux pos s
    and aux3 pos id s =  (* aux3 looks for the next ident *)
      let _ = debug_msg ("aux3: "^(string_of_int (Stream.count s))^" pos="^
                         (string_of_int pos)^" ; token='"^(ftok2str s)^"'\n") in
      match Stream.next s with 
      | Ident(t)-> tbl:=(id, (t, pos, !is_input))::!tbl; aux  (pos+1) s
      | Kwd(_) ->  aux pos s 
      | Int(_) ->  aux pos s 
      | Float(_) ->  aux pos s 
      | String(_) ->  aux pos s 
      | Char(_) -> aux pos s 
    in
    let rec aux_ignore_error pos s =
      try aux pos s with
      | Stream.Error _ ->
        if !verbose then (
          output_string stderr ("ignore (harmless?) stream errors at pos "^
                                (string_of_int (Stream.count s))^" in gnuplot-rif.\n");
          flush stderr);
        aux_ignore_error !pos_ref !s_ref
    in
    try
      aux_ignore_error 0 !s_ref
    with 
    | Stream.Failure -> List.rev !tbl
    | Stream.Error(msg) -> 
      print_string ("gnuplot-rif:"^msg^"\n"); flush stdout;
      List.rev !tbl

let ressource_file_usage = "
gnuplot-rif first reads the content of a file named .gnuplot-rif in the 
current directory, if it exists. If it contains:

   hide T
   hide toto*

gnuplot-rif will ignore all I/O which names begin by 'toto', as well as 
the variable 'T'. If it contains:

   show xx*

it will show show only I/O beginning by 'xx'. If it contains:

  plot_range 12 42

it will plot data from step 12 to 42 only. If it contains:

  dynamic
  window_size 56

it will show only the last of 56 steps of the simulation (40 by default).

If one 'show' statement is used, all hide statements are ignored.
If several plot_range or window_size are used, the last one win.

All these values can be overriden by setting options.
"

(* Returns the list of var names to hide *)
let (read_ressource_file_name : string -> unit) =
  fun file ->
    try
      let str = if Sys.file_exists file then readfile file else "" in
      let strl = Str.split (Str.regexp "\n") str in
      let strll = List.map (Str.split (Str.regexp "[ \t]+")) strl in
      (* XXX I could use a less rustic parsing methodology... *)
      let hide_strll (* lines beggining by 'hide' *), strll (* other lines *) = 
        List.partition (fun l -> if l = [] then false else List.hd l = "hide")
          strll 
      in
      let show_strll (* lines beggining by 'show' *), strll (* other lines *) = 
        List.partition (fun l -> if l = [] then false else List.hd l = "show")
          strll 
      in
      List.iter 
        (function 
          | ["plot_range" ; min_s ; max_s] -> (
            print_string ("Plotting from step " ^min_s ^ " to step " ^ max_s ^ "\n") ;
            min_step := (try Some (int_of_string min_s) with _ -> None);
            max_step := (try Some (int_of_string max_s) with _ -> None)
          )
          | ["window_size" ; size] -> (
            (try window_size := (int_of_string size) with _ -> ()))
          | _ -> ()
        )
        strll;
      debug_msg "read .gnuplot-rif: ok.\n";
      vars_to_hide:= !vars_to_hide @ (List.flatten (List.map List.tl hide_strll));
      vars_to_show:= !vars_to_show @ (List.flatten (List.map List.tl show_strll))
    with _ ->
      if !verbose then (
      print_string "No valid .gnuplot-rif file has been found.\n";
      flush stdout
      )

let (read_ressource_file : unit -> unit) =
  fun () ->   
    read_ressource_file_name ".gnuplot-rif"


(********************************************************************************)
type terminal_kind =
    Jpg | Ps | Pdf | Cps | Eps | Latex | X11| Wxt | Qt | NoDisplay | Default
let terminal_kind_to_string tk file =
  let base_fn = Filename.chop_extension file in
    match tk with
      | Jpg -> "set term jpeg \nset output \"" ^ base_fn ^ ".jpg\"\n"
      | Pdf -> "set term pdf \nset output \"" ^ base_fn ^ ".pdf\"\n"
      | Ps -> "set term post solid \nset output \"" ^ base_fn ^ ".ps\"\n"
      | Cps  -> "set term post color solid \nset output \"" ^ base_fn ^ ".ps\"\n"
      | Eps  -> "set term post color solid  eps\nset output \"" ^ base_fn ^ ".eps\"\n"
      | Latex  -> "set term latex\nset output \"" ^ base_fn ^ ".tex\"\n"
      | X11 -> "set terminal x11"
      | Qt -> "set terminal qt size 1600,400"
      | Default -> ""
      | Wxt -> "set terminal wxt persist font \"Arial,12\" size 1600,400 "
      | NoDisplay -> " "

let terminal = ref Default

let gen_gnuplot_file vars to_hide ttbl file tk =
  let oc = open_out file in
  let put str = output_string oc str in
  (*   let flip = ref true in *)
  let bool_var_nb = ref 0 in
  let put_one_var (var:string) (i:int) =
    let is_num,is_input =
      try let t,_,ii= (List.assoc var ttbl) in (t <> "bool"),ii with _  -> 
        let var_tbl_str = String.concat ", " (List.map (fun (n,_) -> n) ttbl) in
        output_string stderr
          ("Warning: cannot find  "^var^" in "^var_tbl_str^"\n");
        flush stderr;
        false,false (* fake values *) 
    in 
    put ("\"< read-rif.sh "^ !rif_file^ " ");
    if !dynamic then put (" | tail -n "^ (string_of_int !window_size)) else
      (match !min_step,!max_step with
       | None, None -> ()
       | Some l, None -> put (" | tail -n +"^ (string_of_int l))
       | None, Some h -> put (" | head -n "^ (string_of_int h))
       | Some l, Some h ->  put (" | head -n "^ (string_of_int h) ^ 
                                 " | tail -n +"^(string_of_int l))
      );
    put ("\"  using 1:" ^ 
         (if is_num then
            ("(getminmax($"^(string_of_int i)^")) title \""^var^"\"  with linespoints") 
          else
            ("(scale_bool($"^(string_of_int i)^","^
             (incr bool_var_nb;string_of_int (!bool_var_nb-1))^"))  lc rgb \""^
             (if is_input then "blue" else "red")^"\" notitle")
         )
        )
  in
  let bool_nb = 
    List.fold_left
      (fun cpt (id,  (t,_,_ii)) -> if t = "bool" && not (to_hide id) then cpt+1 else cpt)
      0
      ttbl
  in
  let num_nb = 
    List.fold_left
      (fun cpt (id,  (t,_,_ii)) -> if t <> "bool" && not (to_hide id) then cpt+1 else cpt)
      0
      ttbl
  in
  debug_msg ("Generating "^ file ^ " file...\n");
  put ("
# defaults
set title \"A visualisation of "^(!rif_file)^"\"
set style data steps
set pointsize 0.2");
  if !grid then put "
set grid back";
  put ("
set mouse 
" ^
       (if num_nb = 0 then "" else
          "set key outside title \"Numeric variables\"   box 3")^ 
       "
set xlabel \"steps\"

# Set parameters
bool_nb="^(string_of_int bool_nb)^ "

# Initialise the global vars
max=" ^ (if num_nb = 0 then "-1" else "-1e36") ^"
min=" ^ (if num_nb = 0 then "1" else "1e36") ^"
range=0
range_10=1
delta=1

# Various stuff to be able to display booleans
update_delta(x) = (range=max-min,range_10=ceil(range/10),(delta2=((range)/(bool_nb+2)), (delta2<1?1:delta=delta2)))
getminmax(x)= ((x<min ? min=x : x>max ? (max=x) : x),update_delta(x),x)
scale_bool(x,i) = min + 1.7*i*delta + (x*delta)
label_pos(i)=min + i*delta*1.7+delta*0.5

"^(if !dynamic then ("set xtics " ^ (string_of_int (!window_size / 10))) else
     match !min_step,!max_step with
     | None, None -> ""
     | Some _l, None -> ""
     | None, Some h -> "set xtics " ^ (string_of_int (h / 10))
     | Some l, Some h ->"set xtics " ^ (string_of_int ((h-l) / 10))
  )^"\n");   
  put (terminal_kind_to_string tk file);
  put "\nplot ";
  ignore 
    (List.fold_left 
	    (fun (i,sep) var -> 
          if to_hide var then (
            if !verbose then 
              print_string ("\n Skipping hidden var "^var) ;
            (i+1,sep) 
          )
          else ( 
	         put sep;
	         put_one_var var i;
	         (i+1,", \\\n     ")
          )
	    )
	    (2,"")
	    vars
    );
  put "\n\nunset label\n";
  bool_var_nb:=0;
  List.iter 
    (fun  (id, (t,_pos,_ii))  -> 
       if  (to_hide id) then  debug_msg ("Hidding " ^ id ^ "\n"); 
       if t = "bool" then (
         if  (to_hide id) then () else (
           put ("set label \""^ id ^ "\" at  3, (label_pos("^
                (incr bool_var_nb;string_of_int (!bool_var_nb-1))
                ^")) front left \n")
         )
       ) ;
    )
    ttbl;
  put "
unset ytics
set ytics range_10
";
  flush stderr;
  if (not (List.mem tk [Wxt; Default; Qt; X11 ;NoDisplay])) then put "set size 1.3,1\n";
  put "unset output\nreplot\n";
  if (List.mem tk [Qt; X11; Default; NoDisplay]) then put "pause mouse close\n";
  close_out oc;
  file


(********************************************************************************)
let gnuplot =
  try Unix.getenv "GNUPLOT"
  with _ -> "gnuplot"

(********************************************************************************)
let (f:unit -> out_channel * int) = fun () -> 
  let rif_file_content = readfile !rif_file in (* XXX I only need the first lines ! *)
  let ttbl = get_var_types rif_file_content in
  let to_hide v = 
    if !vars_to_show = [] then
      List.exists
        (fun patt -> 
          let patt = Str.global_replace (Str.regexp_string "*") ".*"  patt in
          Str.string_match (Str.regexp patt) v 0) 
        !vars_to_hide 
    else
      not (List.exists
             (fun patt -> 
               let patt = Str.global_replace (Str.regexp_string "*") ".*"  patt in
               Str.string_match (Str.regexp patt) v 0) 
             !vars_to_show)
  in
  let vars = fst (List.split ttbl) in
  let gp_file =  (Filename.chop_extension !rif_file) ^ ".gp" in

  let (pipe_in,  pipe_out) = Unix.pipe () in
  let oc = Unix.out_channel_of_descr pipe_out in
  let gnuplot_out = open_out "gnuplot.log" in
  let _ = plot_file := gen_gnuplot_file vars to_hide ttbl gp_file !terminal in
  let plot_file_content = readfile !plot_file in
  if !verbose then (
	 output_string stderr "\nvar names are :\n"; 
	 List.iter (fun x -> output_string stderr (x ^ "\n")) vars; 
	 flush stderr
  );
  if !terminal = NoDisplay then oc,0 else (
	 let pid = 
	   debug_msg ("Launching "^ gnuplot ^ " " ^ !plot_file ^ "...\n");
	   Unix.create_process  
	     gnuplot
	     (Array.of_list [gnuplot; "-"]) 
(* 	     (Array.of_list [gnuplot; !plot_file])  *)
        (pipe_in) 
        (Unix.descr_of_out_channel gnuplot_out) 
	     (Unix.descr_of_out_channel stderr) 
	 in
    output_string oc plot_file_content;
    flush oc;
    oc,pid)

(********************************************************************************)


