(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

open Utils
open Internals

(** a few values imported from Internals *)

type blank = Internals.blank
type 'a grammar = 'a Internals.grammar
type 'a fpos = 'a Internals.fpos
exception Parse_error = Internals.Parse_error
let warn_merge = Internals.warn_merge
let debug_lvl = Internals.debug_lvl
let keep_all_names = Internals.keep_all_names

(** The user visible function to reject a parsing rule from its action *)
let give_up () = raise Error

(** Three predefined blank functions *)

let no_blank : Input.buffer -> int -> Input.buffer * int =
  fun str pos -> (str, pos)

let blank_regexp : string -> blank =
  fun str ->
    let (re, _) = Regexp.regexp_from_string str in
    Regexp.read_regexp re

(** blank grammar take another blank function. This is usefull
    to define blank as list of comments separated by blanks *)
let blank_grammar : unit grammar -> blank -> blank
  = fun grammar blank buf pos ->
      let save_debug = !debug_lvl in
      debug_lvl := !debug_lvl / 10;
      let (_,buf,pos) =
        internal_parse_buffer ~blank_after:true blank grammar buf pos
      in
      debug_lvl := save_debug;
      (buf,pos)

(** Smart constructors for rules *)
let nonterm name info rules =
  NonTerm{info;rules;name;memo=Container.Ref.create ()}

let next_aux s r = mkrule (Next(compose_info s r, s, Arg r))
let next_pos_aux s r = mkrule (Next(compose_info s r, s, Pos r))
let next_ign_aux s r = mkrule (Next(compose_info s r, s, Ign r))

let next : type a c. a grammar -> (a -> c) rule -> c rule =
  fun ((i,rs) as g) r ->
    match rs with
    | [{rule = Next(_,s0, Arg {rule = Empty Idt; _}); _}] ->  next_aux s0 r
    | _                                                   ->
        next_aux (nonterm (grammar_name ~delim:true g) i rs) r

let next_pos : type a c. a grammar -> (a -> c) fpos rule -> c rule =
  fun (i,rs as g) r ->
    match rs with
    | [{rule = Next(_,s0, Arg {rule = Empty Idt; _}); _}] -> next_pos_aux s0 r
    | _                                                   ->
        next_pos_aux (nonterm (grammar_name ~delim:true g) i rs) r

let next_ign : type a c. a grammar -> c rule -> c rule = fun (i,rs as g) r ->
  match rs with
  | [{rule = Next(_,s0, Arg {rule = Empty Idt; _}); _}] -> next_ign_aux s0 r
  | _                                                   ->
      next_ign_aux (nonterm (grammar_name ~delim:true g) i rs) r

let emp f = mkrule (Empty f)
let ems f = emp (Simple f)

let mkterm name info input =
  Term{input;info;memo=Container.Ref.create ();name}

let mkter2 name info input =
  Ter2{input;info;memo=Container.Ref.create ();name}

let mktest name info input =
  Test{input;info;memo=Container.Ref.create ();name}

let mkgrammar s = (grammar_info s, s)

(** Helper to build a terminal symbol *)
let solo : string -> ?accept_empty:bool -> Charset.t -> 'a input
    -> 'a grammar = fun name ?(accept_empty=false) set s ->
  let j = Fixpoint.from_val (accept_empty,set) in
  (j, [mkrule (Next(j,mkterm name j s, Arg(idtEmpty ())))])

(** Function used to call a grammar as a terminal. Its input
    takes more arguments, in particular to record error position *)
let solo2 =
  fun name i s ->
    let s = fun errpos blank b p b' p' ->
      s errpos blank b p b' p'
    in
    (i, [mkrule (Next(i,mkter2 name i s, Arg (idtEmpty ())))])

(** Combinator for test at current position *)
let test : ?name:string -> Charset.t
           -> (Input.buffer -> int -> 'a * bool) -> 'a grammar =
  fun ?(name="") set f ->
    let j = Fixpoint.from_val (true,set) in
    (j, [mkrule (Next(j,mktest name j (fun _ _ -> f), Arg (idtEmpty ())))])

(** Combinator for test blank before the current position *)
let blank_test : ?name:string -> Charset.t -> 'a test -> 'a grammar =
  fun ?(name="") set f ->
  let j = Fixpoint.from_val (true,set) in
  (j, [mkrule (Next(j,mktest name j f, Arg(idtEmpty ())))])

(** A test that always pass *)
let success a = test ~name:"SUCCESS" Charset.full (fun _ _ -> (a, true))

(** A test that blank exists before the current position *)
let with_blank_test a = blank_test ~name:"BLANK" Charset.full
  (fun buf' pos' buf pos -> (a, not (Input.buffer_equal buf' buf) || pos' <> pos))

(** A test that blank do not exists before the current position *)
let no_blank_test a = blank_test ~name:"NOBLANK" Charset.full
  (fun buf' pos' buf pos -> (a, Input.buffer_equal buf' buf && pos' = pos))

(** Used for unset recursive grammars *)
let unset : string -> 'a grammar = fun msg ->
  let fn _ _ = failwith msg in
  solo msg Charset.empty fn (* make sure we have the message *)

(** Alternatives between many grammars *)
let alternatives : 'a grammar list -> 'a grammar = fun g ->
  mkgrammar (List.flatten (List.map snd g))

(** Declare a recusive grammar *)
let declare_grammar name =
  let g = snd (unset (name ^ " not set")) in
  let nt = nonterm name (Fixpoint.from_val (false, Charset.empty)) g in
  let j =
    Fixpoint.from_ref nt (
      function
      | NonTerm{rules; _} -> grammar_info rules
      | _                 -> assert false)
  in
  begin
    match nt with
    | NonTerm r -> r.info <- j
    | _ -> assert false
  end;
  mkgrammar [mkrule (Next(j,nt, Arg(idtEmpty ())))]

(** Set the value of a recursive grammar *)
let set_grammar : type a.a grammar -> a grammar -> unit = fun p1 (_,rules2) ->
      match snd p1 with
      | [{rule=Next(_,NonTerm({info; _} as r),Arg {rule=Empty Idt; _}); _}] ->
         r.rules <- rules2; Fixpoint.update info;
      (*Printf.eprintf "setting %s %b %a\n%!" name ae Charset.print set;*)
      | _ -> invalid_arg "set_grammar"

let grammar_family ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = EqHashtbl.create 8 in
  let is_set = ref None in
  (fun p ->
    try EqHashtbl.find tbl p
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string p) in
      EqHashtbl.add tbl p g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f p);
      );
      g),
  (fun f ->
    is_set := Some f;
    EqHashtbl.iter (fun p r ->
      set_grammar r (f p);
    ) tbl)

let grammar_prio ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = EqHashtbl.create 8 in
  let is_set = ref None in
  (fun p ->
    try EqHashtbl.find tbl p
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string p) in
      EqHashtbl.add tbl p g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f p);
      );
      g),
  (fun (gs,gp) ->
    let f = fun p ->
      alternatives (List.map snd (List.filter (fun (f,_) -> f p) gs) @ (gp p))
    in
    is_set := Some f;
    EqHashtbl.iter (fun p r ->
      set_grammar r (f p);
    ) tbl)

let grammar_prio_family ?(param_to_string=(fun _ -> "<...>")) name =
  let tbl = EqHashtbl.create 8 in
  let tbl2 = EqHashtbl.create 8 in
  let is_set = ref None in
  (fun args p ->
    try EqHashtbl.find tbl (args,p)
    with Not_found ->
      let g = declare_grammar (name^"_"^param_to_string (args,p)) in
      EqHashtbl.add tbl (args, p) g;
      (match !is_set with None -> ()
      | Some f ->
         set_grammar g (f args p);
      );
      g),
  (fun f ->
    let f = fun args ->
      (* NOTE: to make sure the tbl2 is filled soon enough *)
      let (gs, gp) = f args in
      try
        EqHashtbl.find tbl2 args
      with Not_found ->
        let g = fun p ->
            alternatives (List.map snd (List.filter (fun (f,_) -> f p) gs) @ gp p)
        in
        EqHashtbl.add tbl2 args g;
        g
    in
    is_set := Some f;
    EqHashtbl.iter (fun (args,p) r ->
      set_grammar r (f args p);
    ) tbl)

(** Parse the end of file *)
let eof : 'a -> 'a grammar
  = fun a ->
    let fn buf pos =
      if Input.is_empty buf pos then (a,buf,pos) else raise Error
    in
    solo "EOF" (Charset.singleton '\255') fn

(** Give a name to a grammar *)
let give_name name (i,_ as g) =
  (i, [grammar_to_rule ~name g])

(** Change the action of the grammar by applying a function *)
let apply : type a b. (a -> b) -> a grammar -> b grammar =
  fun f g -> mkgrammar [next g (emp (Simple f))]

(** Idem, with positions *)
let apply_position : type a b. (a -> b) fpos
                          -> a grammar -> b grammar =
  fun f g ->
    mkgrammar [next g (emp (WithPos f))]

(** Build a tuple with positions *)
let position g =
  apply_position (fun buf pos buf' pos' a ->
    (Input.filename buf, Input.line_num buf, pos, Input.line_num buf', pos', a)) g


(** An always failing grammar *)
let fail : unit -> 'a grammar = fun () ->
  let fn _ _= raise Error in
  solo "FAIL" Charset.empty fn

(** Accept only one char *)
let char : ?name:string -> char -> 'a -> 'a grammar
  = fun ?name c a ->
    let msg = Printf.sprintf "%C" c in
    let name = match name with None -> msg | Some n -> n in
    let fn buf pos =
      let c', buf', pos' = Input.read buf pos in
      if c = c' then (a,buf',pos') else give_up ()
    in
    solo name (Charset.singleton c) fn

(** Accept any char in a given char set *)
let in_charset : ?name:string -> Charset.t -> char grammar
  = fun ?name cs ->
    let msg = Printf.sprintf "[%s]" (Charset.show cs) in
    let name = match name with None -> msg | Some n -> n in
    let fn buf pos =
      let c, buf', pos' = Input.read buf pos in
      if Charset.mem cs c then (c,buf',pos') else give_up ()
    in
    solo name cs fn

(** Test that the current char is not in a given char set (do not parse it) *)
let not_in_charset : ?name:string -> Charset.t -> unit grammar
  = fun ?name cs ->
    let msg = Printf.sprintf "^[%s]" (Charset.show cs) in
    let name = match name with None -> msg | Some n -> n in
    let fn buf pos =
      let c = Input.get buf pos in
      if Charset.mem cs c then ((), false) else ((), true)
    in
    test ~name (Charset.complement cs) fn

(** Test the charactere at the beginning of the blank.
    TODO: should not it test all blank char ? *)
let blank_not_in_charset : ?name:string -> Charset.t -> unit grammar
  = fun ?name cs ->
    let msg = Printf.sprintf "^[%s]" (Charset.show cs) in
    let name = match name with None -> msg | Some n -> n in
    let fn buf pos _ _ =
      let c = Input.get buf pos in
      if Charset.mem cs c then ((), false) else ((), true)
    in
    blank_test ~name (Charset.complement cs) fn

(** Accept exactly one char *)
let any : char grammar
  = let fn buf pos =
      let c, buf', pos' = Input.read buf pos in
      if c = '\255' then give_up ();
      (c,buf',pos')
    in
    solo "ANY" Charset.(del full '\255') fn

(** Print a debugging message, with the position *)
let debug msg : unit grammar
    = let fn buf pos =
        log "%s file:%s line:%d col:%d\n%!"
            msg (Input.filename buf) (Input.line_num buf) pos;
        ((), true)
      in
      test ~name:msg Charset.empty fn

(** Accept a string *)
let string : ?name:string -> string -> 'a -> 'a grammar
  = fun ?name s a ->
    let name = match name with None -> s | Some n -> n in
    let fn buf pos =
      let buf = ref buf in
      let pos = ref pos in
      let len_s = String.length s in
      for i = 0 to len_s - 1 do
        let c, buf', pos' = Input.read !buf !pos in
        if c <> s.[i] then give_up ();
        buf := buf'; pos := pos'
      done;
      (a,!buf,!pos)
    in
    solo name ~accept_empty:(s="") (Charset.singleton s.[0]) fn

(** Accept a keyword: the charter after the parsed string should
    return false for the given function *)
let keyword : ?name:string -> string -> (char -> bool) -> 'a -> 'a grammar
  = fun ?name s test a ->
    let name = match name with None -> s | Some n -> n in
    let fn buf pos =
      let buf = ref buf in
      let pos = ref pos in
      let len_s = String.length s in
      for i = 0 to len_s - 1 do
        let c, buf', pos' = Input.read !buf !pos in
        if c <> s.[i] then give_up ();
        buf := buf'; pos := pos'
      done;
      let c, _, _ = Input.read !buf !pos in
      if test c then give_up ();
      (a,!buf,!pos)
    in
    solo name ~accept_empty:(s="") (Charset.singleton s.[0]) fn

(** option combinator *)
let option : 'a -> 'a grammar -> 'a grammar
  = fun a (_,l) -> mkgrammar (mkrule (Empty (Simple a))::l)

(** Regexp (use our own regexp, look at [Earley_str] for Str regexp support *)
let regexp : ?name:string -> string -> string array grammar =
  fun ?name str ->
    let name = match name with None -> String.escaped str | Some n -> n in
    let (re, grps) = Regexp.regexp_from_string str in
    let fn buf pos =
      let (buf, pos) =
        try Regexp.read_regexp re buf pos
        with Regexp.Regexp_error(_,_) -> give_up ()
      in
      (Array.map (!) grps, buf, pos)
    in
    let accept_empty = Regexp.accept_empty re in
    let charset = Regexp.accepted_first_chars re in
    solo name ~accept_empty charset fn

(** Allow to write any terminal, by supplying a function *)
let black_box : (Input.buffer -> int -> 'a * Input.buffer * int) -> Charset.t -> bool
                  -> string -> 'a grammar
  = fun fn set accept_empty name -> solo name ~accept_empty set fn

(** Parse the empty string *)
let empty : 'a -> 'a grammar = fun a -> (iempty,[ems a])

let empty_pos : 'a fpos -> 'a grammar
  = fun f -> (iempty,[emp (WithPos f)])

(** Various wy to make sequence of parsing *)
let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
    mkgrammar [next l1 (next l2 (ems (fun b a -> f a b)))]

let sequence_position : 'a grammar -> 'b grammar
                        -> ('a -> 'b -> 'c) fpos -> 'c grammar
  = fun l1 l2 f ->
  mkgrammar [next l1 (next l2
    (emp (WithPos (fun b p b' p' a a' -> f b p b' p' a' a))))]

let sequence3 : 'a grammar -> 'b grammar -> 'c grammar
                -> ('a -> 'b -> 'c -> 'd) -> 'd grammar
  = fun l1 l2 l3 f ->
    sequence l1 (sequence l2 l3 (fun x y z -> f z x y)) (fun z f -> f z)

let fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> mkgrammar [next l1 (grammar_to_rule l2)]

let fsequence_position : 'a grammar -> ('a -> 'b) fpos grammar -> 'b grammar
  = fun l1 l2 -> mkgrammar [next_pos l1 (grammar_to_rule l2)]

let fsequence_ignore : 'a grammar -> 'b grammar -> 'b grammar
  = fun l1 l2 -> mkgrammar [next_ign l1 (grammar_to_rule l2)]

let simple_dependent_sequence
    : 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun l1 f2 ->
      mkgrammar [next l1 (mkrule (Dep (fun a -> grammar_to_rule (f2 a))))]

let dependent_sequence
    : ('a * 'b) grammar -> ('a -> ('b -> 'c) grammar) -> 'c grammar
  = fun f1 f2 ->
        simple_dependent_sequence f1 (fun (a,b) -> apply (fun g -> g b) (f2 a))

(** A nice one !*)
let iter : 'a grammar grammar -> 'a grammar
  = fun g -> simple_dependent_sequence g (fun f -> f)

(** Various fixpoints *)
let fixpoint :  'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    let name = grammar_delim_name f1 ^ "*" in
    let res = declare_grammar name in
    let _ = set_grammar res
      (mkgrammar [ems a; next res (next f1 (idtEmpty ()))]) in
    res

let fixpoint' :  type a b.a -> b grammar -> (b -> a -> a) -> a grammar
  = fun a f1 f ->
    let name = grammar_delim_name f1 ^ "*" in
    let res = declare_grammar name in
    let _ = set_grammar res
      (mkgrammar [ems a; next res (next f1 (ems f))]) in
    res

let fixpoint1 :  'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    let name = grammar_delim_name f1 ^ "+" in
    let res = declare_grammar name in
    let _ = set_grammar res
      (mkgrammar [next f1 (ems (fun f -> f a));
       next res (next f1 (idtEmpty ()))]) in
    res

let fixpoint1' :  'a -> 'b grammar -> ('b -> 'a -> 'a) -> 'a grammar
  = fun a f1 f ->
    let name = grammar_delim_name f1 ^ "+" in
    let res = declare_grammar name in
    let _ = set_grammar res
      (mkgrammar [next f1 (ems (fun b -> f b a));
       next res (next f1 (ems f))]) in
    res


(** General lists with seprator *)
let list1 g sep =
  fsequence g
    (apply (fun xs x -> x :: xs [])
       (fixpoint' (fun l -> l)
                  (fsequence_ignore sep g)
                  (fun x f l -> f (x::l))))

let list0 g sep =
  option [] (list1 g sep)

let list2 g sep =
  fsequence g
    (apply (fun xs x -> x :: xs [])
       (fixpoint1' (fun l -> l)
                   (fsequence_ignore sep g)
                   (fun x f l -> f (x::l))))

(** A combinator to change the notion of blank *)
let change_layout : ?old_blank_before:bool -> ?new_blank_after:bool
                      -> 'a grammar -> blank -> 'a grammar
  = fun ?(old_blank_before=true) ?(new_blank_after=true) l1 blank1 ->
    let i = Fixpoint.from_val (false, Charset.full) in
    (* compose with a test with a full charset to pass the final charset test in
       internal_parse_buffer *)
    let l1 = mkgrammar [next l1 (next (success ()) (ems (fun _ a -> a)))] in
    let fn errpos _ buf pos buf' pos' =
      let buf,pos = if old_blank_before then buf', pos' else buf, pos in
      let (a,buf,pos) = internal_parse_buffer ~errpos
        ~blank_after:new_blank_after blank1 l1 buf pos in
      (a,buf,pos)
    in
    let name = grammar_name l1 in
    solo2 name i fn

(** A combinator to parse with no blank at all *)
let no_blank_layout : 'a grammar -> 'a grammar
  = fun l1 ->
    (* compose with a test with a full charset to pass the final charset test in
       internal_parse_buffer *)
    let l1 = mkgrammar [next l1 (next (success ()) (ems (fun _ a -> a)))] in
    let fn errpos _ _ _ buf pos =
      let (a,buf,pos) = internal_parse_buffer ~errpos
        ~blank_after:false no_blank l1 buf pos in
      (a,buf,pos)
    in
    let name = grammar_name l1 in
    solo2 name (fst l1) fn

(** Calls a grammar "greedy": retains only the longuest match *)
let greedy : 'a grammar -> 'a grammar
  = fun l1 ->
    (* compose with a test with a full charset to pass the final charset test in
       internal_parse_buffer *)
    let l1 = mkgrammar [next l1 (next (success ()) (ems (fun _ a -> a)))] in
    (* FIXME: blank are parsed twice. internal_parse_buffer should have one
              more argument *)
    let fn errpos blank buf pos _ _ =
      let (a,buf,pos) = internal_parse_buffer ~errpos blank l1 buf pos in
      (a,buf,pos)
    in
    let name = grammar_delim_name l1 ^ "$" in
    solo2 name (fst l1) fn

(** How to call the parser *)

let partial_parse_buffer
    : type a.a grammar -> blank -> ?blank_after:bool -> Input.buffer -> int
           -> a * Input.buffer * int
   = fun g bl ?(blank_after=false) buf pos ->
       parse_buffer_aux blank_after bl g buf pos

let parse_buffer : 'a grammar -> blank -> Input.buffer -> 'a =
  fun g blank buf ->
    let g = sequence g (eof ()) (fun x _ -> x) in
    let (a, _, _) = partial_parse_buffer g blank buf 0 in
    a

let parse_string ?(filename="") grammar blank str =
  let str = Input.from_string ~filename str in
  parse_buffer grammar blank str

let parse_channel ?(filename="") grammar blank ic  =
  let str = Input.from_channel ~filename ic in
  parse_buffer grammar blank str

let parse_file grammar blank filename  =
  let str = Input.from_file filename in
  parse_buffer grammar blank str

(** A helper to hangle exceptions *)
let fail_no_parse () = exit 1

let handle_exception ?(error=fail_no_parse) f a =
  try f a with Parse_error(buf, pos) ->
    let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!" in
    Printf.eprintf (red "Parse error: file %S, line %d, character %d.\n")
      (Input.filename buf) (Input.line_num buf) (Input.utf8_col_num buf pos);
    error ()

(** A module to call a parser with a preprocessor (see Input) *)
module WithPP(PP : Input.Preprocessor) =
  struct
    module InPP = Input.WithPP(PP)

    let parse_string ?(filename="") grammar blank str =
      let str = InPP.from_string ~filename str in
      parse_buffer grammar blank str

    let parse_channel ?(filename="") grammar blank ic  =
      let str = InPP.from_channel ~filename ic in
      parse_buffer grammar blank str

    let parse_file grammar blank filename  =
      let str = InPP.from_file filename in
      parse_buffer grammar blank str
  end

(** Collect info a bout grammars *)
let grammar_info : type a. a grammar -> bool * Charset.t
  = fun g -> (force (fst g))

(** A test on grammar *)
let accept_empty : 'a grammar -> bool
  = fun grammar -> fst (grammar_info grammar)
