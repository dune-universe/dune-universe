(* Time-stamp: <modified the 11/06/2020 (at 11:06) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
** This file may only be copied under the terms of the CeCILL
** Public License
**-----------------------------------------------------------------------
**
** File: rifIO.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)


open List

let lexer = LocalGenlex.make_lexer ["q"; "nil"; "?"; "ERROR";"Error";"error"; "#"; 
                                    "x"; "load_luc"; "#@"; "@#";"end"]

(* xxx Which pragmas should be defined ? *)

let dflt_pragmas = ["inputs";"reset";"quit"]
(* let dflt_pragmas = ["outs";"outputs";"program";"inputs";"step";"reset" ] *)


type stream = LocalGenlex.token Stream.t

let rec (_parse_string_list : stream -> string list -> string list) =
  fun stream sl ->
    try (
      match (Stream.next stream) with
          (LocalGenlex.String(str)) -> _parse_string_list stream (str::sl)
        | _  -> failwith ("### rif parse error. A \"string\" (wrapped with double" ^
                          "quotes) was expected. \n")
    )
    with Stream.Failure ->
      sl

open Data

(*------------------------------------------------------------------------*)
exception Bye
exception Reset

let read_line debug label ic oc =
  if debug then (
    prerr_string ("["^label^"] RifIO.read_line: wait for something to read...\n");
    flush stderr); 
  let str = input_line ic in
  let _ = 
    if debug then (
      prerr_string ("["^label^"] RifIO.read_line:'"^str^"'\n"); flush stderr) 
  in
  let str = Str.global_replace (Str.regexp "\013") "" str in
  (match oc with
   | Some oc -> output_string oc str; flush oc
   | None -> ());
  str

let get_stream debug label ic oc = 
  try 
    let str = read_line debug label ic oc in 
    str, lexer (Stream.of_string str)
  with e  ->
    print_string ("*** Error when parsing RIF: " ^ (Printexc.to_string e) ^ "\n");
    flush stdout;
    exit 2


let (rm_blank : string -> string) =
  fun s -> 
    let buff = ref "" in
      for i = 0 to String.length s - 1 do
        match s.[i] with
          | ' ' | '\t' | '\n' | '\"' -> ()
          | c -> buff:=!buff^(String.make 1 c)
      done;
      !buff        

let (to_pair : string -> string * Data.t) =
  fun s ->
    match Str.split (Str.regexp ":") s with
      | [n;t] -> rm_blank n, Data.type_of_string (rm_blank t)
      | _ -> failwith ("Rif parse error: Cannot split '"^s^
                          "'. I expect a string of the form <ident>:<ident>")


let _ = assert (to_pair "T:bool" = ("T",Data.Bool))

let strsub str i j = 
  try String.sub str i j 
  with _  -> 
    Printf.printf "invalid arg in 'String.sub %s %i %i'\n" str i j;
    flush stdout;
    exit 2

let rec (read_until_pragma_end :?debug:(bool) -> in_channel -> out_channel option -> 
         string -> string) =
  fun ?(debug=false) ic oc str -> 
    let line = read_line debug "" ic oc in
    try
      let i = Str.search_forward (Str.regexp "#@") line 0 in
      (String.sub line 0 i) ^ str
    with Not_found -> 
      read_until_pragma_end ~debug:debug ic oc (str^" "^line)

(* exported *)
let (read_interface : ?debug:(bool) -> ?label:(string) -> in_channel ->
         out_channel option -> vntl * vntl) =
  fun ?(debug=false) ?(label="") ic oc -> 
    let rec loop ins outs in_done out_done =
      if in_done && out_done then ins, outs else
        let line =  read_line debug label ic oc in
        try
          if (Str.string_match (Str.regexp "#end") line 0) || line = "q" || line = "bye"
          then (
            Printf.printf "\n*** RifIO.read_interface: The process %s died before " label;
            Printf.printf "sending its interface declarations.\n*** Hara-Kiring!\n" ;
            flush stdout;
            raise Bye
          )
          else if Str.string_match (Str.regexp "#inputs") line 0 then
            let str = strsub line 7 (String.length line - 7) in
            let l = Str.split (Str.regexp " ") str in
            let l = List.filter (fun str -> str <> "") l in
              loop (List.map to_pair l) outs true out_done
                
          else if Str.string_match (Str.regexp "@#inputs") line 0 then          
            let str = strsub line 8 (String.length line - 8) in
            let str = read_until_pragma_end ~debug:debug ic oc str in
            let l = Str.split (Str.regexp " ") str in
            let l = List.filter (fun str -> str <> "") l in
              loop (List.map to_pair l) outs true out_done
                
          else if Str.string_match (Str.regexp "#outputs") line 0 then
            let str = strsub line 8 (String.length line - 8) in
            let l = Str.split (Str.regexp " ") str in
            let l = List.filter (fun str -> str <> "") l in
              loop ins (List.map to_pair l) in_done true
                
          else if Str.string_match (Str.regexp "@#outputs") line 0 then          
            let str = strsub line 9 (String.length line - 9) in
            let str = read_until_pragma_end ~debug:debug ic oc str in
            let l = Str.split (Str.regexp " ") str in
            let l = List.filter (fun str -> str <> "") l in
              loop ins (List.map to_pair l) in_done true
          else
            loop ins outs in_done out_done
        with e -> 
          print_string ("#" ^line ^"\n");
          flush stdout;
          raise e
    in
      loop [] [] false false


(* exported *)
(**  Reads input values on ic. It should follow the rif format. *)
let rec (read : ?debug:(bool) -> ?label:(string) -> ?pragma:(string list) -> 
         in_channel -> out_channel option -> vntl -> subst list) =
  fun ?(debug=false) ?(label="") ?(pragma = dflt_pragmas) ic oc vntl  ->
  let tbl = [] in
  if vntl = [] then tbl else 
    let str,stream = get_stream debug label ic oc in
    parse_rif_stream ~debug:debug label ic oc vntl (str,stream) tbl pragma

and (parse_rif_stream : 
       ?debug:(bool) -> string -> in_channel -> out_channel option -> vntl ->
     string * stream -> subst list -> string list -> subst list) =
  fun ?(debug=false) label ic oc vntl (str,stream) tbl pragma ->
  if vntl = [] then tbl else
    let tok_list = Stream.npeek 2 stream in
    match tok_list with
    | [LocalGenlex.Kwd ("#"); LocalGenlex.Ident (id)] ->
      if List.mem id pragma then (
        Stream.junk stream ;
        Stream.junk stream ;
        if id = "quit" || id = "q" then raise Bye;
        if id = "reset" then raise Reset;
        parse_rif_stream label ic oc vntl (str,stream) tbl pragma
      ) else (
        (* We skip everything that occurs after a [#], until the next eol. *)
        Stream.junk stream ;
        (* prerr_endline (">>" ^str);   print the ignored string on stderr *)
        parse_rif_stream label ic oc 
          vntl (get_stream debug label ic oc) tbl pragma
      )
    | (LocalGenlex.Kwd ("ERROR"|"Error"|"error"))::_ -> 
      print_string ("#ERROR value read. bye! ("^str^")\n"); 
      flush stdout;
      raise Bye
    | (LocalGenlex.Kwd ("#"))::(LocalGenlex.Kwd ("ERROR"|"Error"|"end"))::_ -> 
      print_string ("#ERROR value read. bye! ("^str^")\n"); 
      flush stdout;
      raise Bye 
    | (LocalGenlex.Kwd ("#"))::_ ->
      Stream.junk stream ;
      (* prerr_endline (">>>" ^str);  print the ignored string on stderr *)
      parse_rif_stream label ic oc vntl (get_stream debug label ic oc)  tbl pragma 
    | (LocalGenlex.Kwd ("q"))::_ -> print_string "# bye!\n"; raise Bye
    | (LocalGenlex.Kwd ("#@"))::_ ->
      (* Beginning of multi-line comment. Note that here,
         unlike the rif format, we ignore multi line pragmas;
         namely, we handle them as a multi-line comment. *)
      (
        Stream.junk stream ;
        ignore_toks_until_end_of_pragmas 
          debug label ic oc vntl (str,stream) tbl pragma 
      )
    | (LocalGenlex.Kwd ("nil"))::_ 
    | (LocalGenlex.Kwd ("?"))::_ -> 
      Stream.junk stream ;
      let tbl = tbl @[fst (hd vntl), U] in
      parse_rif_stream label ic oc (tl vntl) (str,stream) tbl pragma

    | (LocalGenlex.Float (f))::_ ->
      (
        Stream.junk stream ;
        (* Hashtbl.add tbl (Var.name (hd vntl)) (N(F(f))) ; *)
	let v =
          match snd (hd vntl) with
          | Data.Bool -> B(f<>0.0)
          | Data.Real -> F(f)
          | Data.Int  -> 
            let i = int_of_float f in
            print_string ("\n*** Warning: type error, " ^ (string_of_float f)
			  ^" is an real, but an int is expected. I convert it to '"^
                          (string_of_int i)^"'\n");
            I(i)
          | e -> 
            print_string ("\n*** Type Error: float found, "^ 
                          (Data.type_to_string e) ^ " expected\n");
            exit 2
	in
        let tbl = tbl@ [fst (hd vntl), v] in
        parse_rif_stream label ic oc (tl vntl) (str,stream) tbl pragma 
      )
    | (LocalGenlex.Int (i))::_ -> (
	Stream.junk stream ;
	let v =
          match snd (hd vntl) with
          | Data.Bool -> B(i<>0)
          | Data.Int  -> I(i)
          | Data.Real ->
            let f = float_of_int i in
            print_string "\n*** Warning: type error, ";
	    print_string ((string_of_int i)
			  ^ " is an int, but a real is expected. I convert it to '"^
                          (string_of_float f)^"'\n");
            F(f)
          | Data.String -> Data.Str (string_of_int i)
          | e -> 
            print_string ("\n*** Type Error: int found, "^ (Data.type_to_string e) 
                          ^ "e xpected \n");
            exit 2
	in
	let tbl = tbl @[fst (hd vntl), v] in
        parse_rif_stream label ic oc (tl vntl) (str,stream) tbl pragma
      )
    | (LocalGenlex.String (b))::_ -> (
	Stream.junk stream ;
	let v = Str(b) in
	let tbl = tbl @ [fst (hd vntl), v] in
        parse_rif_stream label ic oc (tl vntl) (str,stream) tbl pragma
      )
    | (LocalGenlex.Ident (b))::_ -> (
	Stream.junk stream ;
	let v = if mem b ["f"; "F";"false"] then B(false)
          else if  mem b ["t"; "T";"true"] then B(true)
	  else Str(b)
	in
	let tbl = tbl @ [fst (hd vntl), v] in
        parse_rif_stream label ic oc (tl vntl) (str,stream) tbl pragma
      )
    | [] ->
      (* Eol is is reached; proceed with the next one *)
      parse_rif_stream label ic oc vntl (get_stream debug label ic oc)
        tbl pragma 
    | _ -> failwith ("### rif parse error: not in RIF format ("^str^").\n")

and (ignore_toks_until_end_of_pragmas : bool -> string -> 
     in_channel -> out_channel option-> vntl -> string * stream -> subst list -> 
     string list -> subst list) =
  fun debug label  ic oc vntl (str,stream) tbl pragma ->
  (* ignore all tokens until "@#" is reached *)
  let tok_opt = Stream.peek stream in
  match tok_opt  with
  | Some(LocalGenlex.Kwd ("@#")) ->
    (
      Stream.junk stream ;
      parse_rif_stream label ic oc vntl (str,stream) tbl pragma
    )
  | Some(_) ->
    (
      Stream.junk stream ;
      ignore_toks_until_end_of_pragmas debug label ic oc vntl (str,stream) tbl pragma
    )
  | None ->
    (* Eol is is reached; proceed with the next one *)
    (ignore_toks_until_end_of_pragmas debug label ic oc vntl
       (get_stream debug label ic oc) tbl pragma)
        



(*------------------------------------------------------------------------*)
(* exported *)
let (write : out_channel -> string -> unit) =
  fun oc str -> 
    output_string oc str

let (flush : out_channel -> unit) =
  fun oc -> 
    flush oc

(*------------------------------------------------------------------------*)
(* exported *)
let (write_interface :
       out_channel -> vntl -> vntl -> vntl option -> vntl list option -> unit) =
  fun oc in_vars out_vars loc_vars_opt oracle_vars_opt -> 
    let str =  
      (List.fold_left
         (fun acc (vn,vt) ->
           acc ^ "\"" ^ vn ^ "\":" ^ (Data.type_to_string vt) ^ " ")
         "#inputs "
         in_vars) ^

        "\n#outputs " ^
        
        (List.fold_left 
           (fun acc (vn,vt) ->
             acc ^ "\"" ^ vn ^ "\":" ^  (Data.type_to_string vt) ^ " ")
           ""
           out_vars) ^
        
        (match loc_vars_opt with
          | None -> "\n"
          | Some loc_vars ->  
            ((List.fold_left
                (fun acc (vn,vt) -> 
                  acc^"\"" ^ vn ^ "\":" ^  (Data.type_to_string vt) ^ " ")
                "\n#locals "
                loc_vars
             ) ^ "\n")
        )
      ^
        (match oracle_vars_opt with
          | None -> ""
          | Some vars_l ->  
            (List.fold_left
               (fun acc vars -> 
                 ((List.fold_left
                     (fun acc (vn,vt) -> 
                       acc^"\"" ^ vn ^ "\":" ^  (Data.type_to_string vt) ^ " ")
                     "#oracle_outputs "
                     vars
                  ) ^ "\n" ^ acc)
               )
               ""
               vars_l
            )
        )
    in
    write oc str

(*------------------------------------------------------------------------*)
(* exported *)
let (write_outputs : out_channel -> (float -> string) -> vntl -> subst list -> unit) =
  fun oc s2f vntl sl ->
    let str = 
      List.fold_left
        (fun acc (vn, _vt) ->
          acc^ (try Data.val_to_string s2f (List.assoc vn sl) 
            with 
              | Not_found -> 
                Printf.eprintf ("\n*** RifIO: %s not found in {%s} \n") vn
                              (String.concat "," (List.map (fun (n,_) -> n) sl));
                flush stderr;
                "nil"
          ) ^ " "
        )
        ""
        vntl
    in
    output_string oc str
