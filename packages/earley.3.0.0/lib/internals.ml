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
open Container

(*
let _ = Printexc.record_backtrace true
*)

(** Flags. *)
let debug_lvl  = ref 0
let warn_merge = ref true
let log = Printf.eprintf

(** Exception raised for parse error. Can be raise in the action of a
   grammar using [give_up] *)
exception Error

(** identity is often used *)
let idt x = x

(** A blank function is just a function progressing in a buffer *)
type blank = Input.buffer -> int -> Input.buffer * int

(** Positions *)
type position = Input.buffer * int

(** type of a function waiting for positions *)
type 'a fpos = Input.buffer -> int -> Input.buffer -> int -> 'a

(** Type for action with or without position and its combinators *)
type _ pos =
  | Idt : ('a -> 'a) pos
  | Simple : 'a -> 'a pos
  | WithPos : 'a fpos -> 'a pos
  | Error : 'a pos

(** Common combinators, easy from their types *)
let apply_pos: type a.a pos -> a fpos =
  fun f buf col buf' col' ->
    match f with
    | Idt -> idt
    | Simple f -> f
    | WithPos f -> f buf col buf' col'
    | Error -> raise Error

(** For terminals: get the start position and returns a value with the final
    position *)
type 'a input = Input.buffer -> int -> 'a * Input.buffer * int

(** A reference to record the last error useful when a terminal calls another
    parsing *)
type errpos = (Input.buffer * int) option ref

(** Type for Ter2 terminals: get both the position before and after blank *)
type 'a input2 = errpos -> blank -> Input.buffer -> int -> 'a input

(** Type for tests: get also both position and return a boolean and a value *)
type 'a test  = ('a * bool) fpos

(** Position record stored in the elements of the earley table.  We
    store the position before and after the blank *)
type pos2 = {buf : Input.buffer; col : int; buf_ab : Input.buffer; col_ab : int }

(** Some function on pos2 *)
let eq_pos p1 p2 = p1.col = p2.col && Input.buffer_equal p1.buf p2.buf

(** Get the position before and after the parsed text annd apply it to
    the combinator *)
let apply_pos_start =
  fun f ({buf_ab; col_ab; _} as p1) ({buf;col;_} as p2) ->
    (* parse nothing : only position before blank *)
    if eq_pos p1 p2 then apply_pos f buf col buf col
    (* parse something *)
    else apply_pos f buf_ab col_ab buf col

let apply_start =
  fun f ({buf_ab; col_ab; _} as p1) ({buf; col; _} as p2) ->
    (* parse nothing : only position before blank *)
    if eq_pos p1 p2 then f buf col buf col
    (* parse something *)
    else f buf_ab col_ab buf col

(** Type of the information computed on a rule: the boolean tells if
    the grammar can parse an empty string and the charset, the first
    accepted characteres when the rule is used to parse something. *)
type info = (bool * Charset.t) Fixpoint.t

(** THE MAIN TYPES *)

(** Use the container references, because they can be reset *)
type 'a cref = 'a Ref.container

(** a reference to the store the result of reading a terminal *)
type 'a tref = ('a * Input.buffer * int) option cref

(** A BNF grammar is a list of rules. The type parameter ['a]
    corresponds to the type of the semantics of the grammar. For
    example, parsing using a grammar of type [int grammar] will
    produce a value of type [int]. *)
module rec Types : sig
  (** The type of a grammar, with its information *)
  type 'a grammar = info * 'a rule list

   (** The symbol, a more general concept that terminals *)
   and _ symbol =
     | Term : { input : 'a input;  info : info
              ; memo : 'a tref; name : string } -> 'a symbol
     (** terminal symbol just read the input buffer *)
     | Ter2 : { input : 'a input2; info : info
              ; memo : 'a tref; name : string  } -> 'a symbol
     (** Ter2 correspond to a recursive call to the parser. We
         can change the blank function for instance, or parse
         input as much as possible. In fact it is only in the
         combinators in earley.ml that we see the use Ter2 to call
         the parser back. *)
     | Test : { input : 'a test; info : info
              ; memo : 'a tref; name : string  } -> 'a symbol
     (** Test on the input, can for instance read blanks, usefull for
         things like ocamldoc (but not yet used by earley-ocaml). *)
     | NonTerm : { mutable rules : 'a rule list
                 ; mutable info : info
                 ; memo : 'a ntref; name : string  } -> 'a symbol
     (** Non terminals trough a mutable field to define recursive rule lists *)

   (** Type of a container ref to memoize treatment of non terminal *)
   and 'a ntref = 'a rule list cref


   (** BNF rule. *)
   and _ prerule =
     | Empty : 'a pos -> 'a prerule
     (** Empty rule. *)
     | Next : info * 'a symbol * ('a, 'c) next -> 'c prerule
     (** Sequence of a symbol and a rule, with a possible name for debugging,
         the information on the rule, the symbol to read, an action and
         the rest of the rule *)
     | Dep : ('a -> 'b rule) -> ('a -> 'b) prerule
     (** Dependant rule, gives a lot of power! but costly! use only when
         no other solution is possible *)

   and ('a, 'c) next =
     | Arg : ('a -> 'c) rule -> ('a, 'c) next
     | Pos : ('a -> 'c) fpos rule -> ('a, 'c) next
     | Ign : 'c rule -> ('a, 'c) next

   (** Each rule holds a container to associate data to the rule in O(1).
       see below the description of the type ('a,'b) pre_stack *)
   and 'a rule = { rule : 'a prerule
                 ; ptr : 'a StackContainer.container
                 ; adr : int }

   (** Type of an active element of the earley table. In a description of
       earley, each table element is [(start, end, done * rest)] meaning we
       parsed the string from [start] to [end] with the rule [done] and it
       remains to parse [rest]. The [*] therefore denotes the current
       position. Earley is basically a dynamic algorithm producing all
       possible elements.

       We depart from this representation in two ways:

       - we do not represent [done], we keep the whole whole rule:
         [full = done rest]

       - we never keep [end]. It is only used when we finish parsing of a
         rule and we have an element [(start, end, done * Empty)] then, we
         look for other elements of the form [(start', end', done' * rest')]
         where [end' = start], [rest' = nonterm' rest''] and [nonterm'] is a
         non terminal containing the [done] rule. We represent this situation
         by a stack in the element [(start, end, done * Empty)], which is
         maintained to lists all the elements satisfying the above property
         (no more, no less, each element only once)

         The type ['a final] represent an element of the earley table
         where [end] is the current position in the string being parsed.
    *)
   type _ final = D :
      { start : pos2           (** position in buffer, before and after blank *)
      ; stack : ('c, 'r) stack (** tree of stack representing what should be
                                done after reading the [rest] of the rule *)
      ; acts  : 'b -> 'c       (** action to produce the final 'c. *)
      ; rest  : 'b rule        (** remaining to parse, will produce 'b *)
      ; full  : 'c rule        (** full rule. rest is a suffix of full. *)
      } -> 'r final

   (** Type of the element that appears in stacks. Note: all other
       elements no reachable from stacks will be collected by the GC,
       which is what we want to release memory.

       The type is similar to the previous: [('a, 'r) element], means
       that from a value of type 'a, comming from our parent in the
       stack, we could produce a value of type ['r] using [rest].

       The action needs to be prametrised by the future position which
       is unknown.  *)
   and (_,_) element =
     | B : ('a -> 'r) pos -> ('a,'r) element
     (** End of the stack, with an action to produce the final parser value *)
     | C : { start : pos2
           ; stack : ('c, 'r) stack
           ; acts  : ('a -> 'b -> 'c) pos (** Here we wait [x:'a] from parents *)
           ; rest  : 'b rule
           ; full  : 'c rule
           } -> ('a ,'r) element
     (** Cons cell of the stack with a record similar to D's *)

   (** Stack themselves are an acyclic graph of elements (sharing is
       important to be preserved). We need a reference for the stack
       construction.  *)
   and ('a,'b) stack = ('a,'b) element list ref

  (** For the construction of the stack, all elements of the same list
      ref of type ('a,'b) stack have the same en position [end'].  And
      all elements that points to this stack have their begin position
      [start = end']. Moreover, all elements with the same [full] and
      [start] must point to the same stack.  Recall that [end] is not
      represented in elements.

      We call the position [end'] associated to a stack (as said the
      [start] of the element that point to this stack, the "stack
      position". An important point: new stack elements are
      constructed when the stack position is the current position.

      Moreover, when we add a point from an element [(start, end, rest, full)]
      to a stack (which is therefore at position [start], we have
      [start = end] and [rest = full]. The rule has not parsed anything! This
      is the "prediction" phase of earley.

      To do this construction, we use the record below with a hook
      that we run on all elements added to that stack.  This record is
      only used with stack whose position are the current position: all
      these records will become inaccessible when we advance in
      parsing.

      Morevoer, a direct pointer (thanks to the Container module) is
      kept from the [full] rule of all elements that point to these
      stack and that have the current position as [end].  This is the
      role of the functor call below.  *)

  (** stack in construction ... they have a hook, to run on
      elements added to the stack later ! *)
   type ('a,'b) pre_stack =
     { stack : ('a, 'b) stack
     ; mutable hooks : (('a, 'b) element -> unit) list }

end = Types

   (** Use container to store a point to the rule in stack, we
       use recursive module for that *)
and StackContainer : Param
                   with type ('b,'a) elt = ('a,'b) Types.pre_stack =
                  Make(struct type ('b,'a) elt = ('a,'b) Types.pre_stack end)

include Types

(** Recall the INVARIANTS:

1° Consider two C elements (or two D elements) of a stack.  If their
   have the same start and full is means we can to the same after
   a complete parsing of the rule.

   Then, the two elements MUST HAVE PHYSICALLY EQUAL stack

2° For D nodes, we keep only one for each (start, rest, full) triple
   with a merge warning when a node only differs by its actions
   (compared looking inside closure).

   So D nodes with the same (start, rest, full) triple always have
   equal action. This propagates to each stack (C node with the same
   (start, rest, full) in the same stack have phisically equal actions.
 *)

(** Counter defined outside [mkrule] due to value restriction. *)
let count_rule = ref 0

(** A function to build rule from pre_rule *)
let mkrule : type a. a prerule -> a rule = fun rule ->
  let adr = let c = !count_rule in count_rule := c+1; c in
  { rule; ptr = StackContainer.create (); adr  }

(** Rule equlity *)
let eq_rule : type a b. a rule -> b rule -> (a, b) eq =
  fun r1 r2 -> StackContainer.eq r1.ptr r2.ptr

(** Equality for stack element: as we keep the invariant, we only
    need physical equality. You may uncomment the code below
    to check this. *)
let eq_C : type a b.(a,b) element -> (a,b) element -> bool =
  fun c1 c2 -> c1 == c2

(** Equality on a final needs to do a comparison as it is used to test
    if a new element is already present.*)
let eq_D (D {start; rest; full; stack; acts})
         (D {start=start'; rest=rest'; full=full'; stack=stack'; acts=acts'}) =
  eq_pos start start' &&
    match eq_rule rest rest', eq_rule full full' with
    | Y, Y -> assert(acts == acts'); assert(stack == stack'); true
    | _ -> false

(** Some rules/grammar contruction that we need already here *)
let idtEmpty : type a.unit -> (a->a) rule = fun () -> mkrule (Empty Idt)

(** Handling of symbol, rule and grammar names *)
let symbol_name:type a.a symbol -> string  = function
  | Term{name;_} | Ter2{name;_} | Test{name;_} | NonTerm{name;_} -> name

let rule_name : type a. a rule -> string = fun r ->
  let rec fn : type a.a rule -> string list = fun r ->
    match r.rule with
    | Empty _ -> []
    | Next(_,s,Arg r) ->
       symbol_name s :: fn r
    | Next(_,s,Pos r) ->
       symbol_name s :: fn r
    | Next(_,s,Ign r) ->
       symbol_name s :: fn r
    | Dep _ -> ["DEP"] (* FIXME *)
  in
  String.concat " " (List.filter (fun x -> x <> "") (fn r))

let grammar_name : type a. ?delim:bool -> a grammar -> string =
  fun ?(delim=true) p1 ->
    match snd p1 with
    | [{rule = Next(_,s,Arg{rule=Empty _; _}); _}] -> symbol_name s
    | [{rule = Next(_,s,Pos{rule=Empty _; _}); _}] -> symbol_name s
    | [{rule = Next(_,s,Ign{rule=Empty _; _}); _}] -> symbol_name s
    | []                                           -> "EMPTY"
    | [r]                                          -> rule_name r
    | rs                                           ->
       let name = String.concat " | " (List.map rule_name rs) in
       if delim then "{"^name^"}" else name

let grammar_delim_name : type a. a grammar -> string = fun g ->
  "{"^grammar_name ~delim:false g^"}"

let keep_all_names = ref false

(** Converting a grammar to a rule *)
let grammar_to_rule : type a. ?name:string -> a grammar -> a rule
  = fun ?name (info,rules as g) ->
    match rules with
    | [r] when name = None || not !keep_all_names -> r
    | _ ->
       let name = match name with None -> grammar_name g
                                | Some n -> n in
       mkrule (Next(info,
                    NonTerm{info;rules;name
                           ;memo=Ref.create ()},Arg(idtEmpty ())))

(** Basic constants/functions for rule information *)
let force  = Fixpoint.force
let iempty = Fixpoint.from_val (true, Charset.empty)
let any    = Fixpoint.from_val (true, Charset.full)

(** managment of info = accept empty + charset accepted as first char *)
let rule_info:type a.a rule -> info = fun r ->
  match r.rule with
  | Next(i,_,_) -> i
  | Empty _ -> iempty
  | Dep(_) -> any

let symbol_info:type a.a symbol -> info = function
  | Term{info;_} | Ter2{info;_} | Test{info;_} | NonTerm{info;_} -> info

let compose_info i1 i2 =
  let i1 = symbol_info i1 in
  match i2.rule with
    Empty _ -> i1
  | _ ->
     let i2 = rule_info i2 in
     Fixpoint.from_fun2 i1 i2 (
       fun (accept_empty1, c1 as i1) (accept_empty2, c2) ->
         if not accept_empty1 then i1 else
           (accept_empty1 && accept_empty2, Charset.union c1 c2))

let grammar_info:type a.a rule list -> info = fun g ->
  let or_info (accept_empty1, c1) (accept_empty2, c2) =
    (accept_empty1 || accept_empty2, Charset.union c1 c2)
  in
  let g = List.map rule_info g in
  Fixpoint.from_funl g (false, Charset.empty) or_info

(** Printing *)
let rec print_rule : type a b. ?rest:b rule -> out_channel -> a rule -> unit =
  fun ?rest ch rule ->
    begin
      match rest with
      | None -> ()
      | Some rest ->
         match eq_rule rule rest with
         | Y -> Printf.fprintf ch "\x1B[31m*\x1B[0m " | N -> ()
    end;
    match rule.rule with
    | Next(_,s,Arg rs) -> Printf.fprintf ch "%s %a" (symbol_name s)
                                     (print_rule ?rest) rs
    | Next(_,s,Pos rs) -> Printf.fprintf ch "%s %a" (symbol_name s)
                                     (print_rule ?rest) rs
    | Next(_,s,Ign rs) -> Printf.fprintf ch "%s %a" (symbol_name s)
                                     (print_rule ?rest) rs
    | Dep _ -> Printf.fprintf ch "DEP"
    | Empty _ -> ()

let print_pos ch {buf; col; buf_ab; col_ab} =
  Printf.fprintf ch "%5d:%3d-%5d:%3d"
                 (Input.line_num buf) col (Input.line_num buf_ab) col_ab

let print_final pos ch (D {start; rest; full; _}) =
  if pos then Printf.fprintf ch "%a == " print_pos start;
  print_rule ~rest ch full;
  let (ae,set) = force (rule_info rest) in
  if !debug_lvl > 2 then Printf.fprintf ch "(%a %b)" Charset.print set ae

let print_final_no_pos ch f = print_final false ch f
let print_final ch f = print_final true ch f

let print_element : type a b.out_channel -> (a,b) element -> unit = fun ch el ->
  match el with
  | C {start; rest; full; _} ->
     Printf.fprintf ch "%a == " print_pos start;
     print_rule ~rest ch full;
     let (ae,set) = force (rule_info rest) in
     if !debug_lvl > 2 then Printf.fprintf ch "(%a %b)" Charset.print set ae
  | B _ ->
    Printf.fprintf ch "B"

let print_rule ch rule = print_rule ?rest:None ch rule

(** Here are the 3 type for tables used by out algorithm *)

(** This type is the state of the parsing table for the current position
    it only hold ['a final] elements whose [end] are the current position,
    the keys are the buffer uid's, column position and uid of the
    [full] and [rest] rules  *)
module K =
  struct
    type t = int * int * int
    let equal (a,b,h) (a',b',h') = a = a' && b == b' && h = h'
    let hash (_,_,h) = h
  end

module HK = Hashtbl.Make(K)

type 'a cur = 'a final HK.t

(** Type of a table with pending reading, that is elements resulting from
    reading the string with some symbols. We need this table, because two
    terminal symbols could read different length of the input from the
    same point *)
type 'a reads = 'a final Input.OrdTbl.t ref

(** Stack construction. The type below, denotes table associate stack
    to rule. Recall we construct stack for elements whose [end] are the
    current position *)
type 'a sct = 'a StackContainer.table

(** Table to memoize the treatment of terminals and non terminals. This
    is important to garanty that results of actions are physicaly equals
    when they comes from the same parsing. To do this, terminals must read
    the input only once and the result must be memoized. *)
type tmemo = unit Ref.table

(** [add_stack_hook sct rule fn] adds in [table] a hook [fn] for the given
    [rule]. [fn] will be called each time an element is added to the stack
    pointed by that [rule]. The hook is run on the existing elements
    if the stack. The stack is created if it did not exists yet *)
let add_stack_hook
    : type a b. b sct -> a rule -> ((a, b) element -> unit) -> unit =
  fun sct r f ->
    try
      let {stack; hooks } as p = StackContainer.find sct r.ptr in
      p.hooks <- f::hooks; List.iter f !stack
    with Not_found ->
      StackContainer.add sct r.ptr {stack = ref []; hooks = [f]}

(** [add_stack sct rule element] add the given [element] to the stack of
    the given [rule] in the table [sct]. Runs all hooks if any. Creates
    the stack if needed *)
let add_stack : type a b. b sct -> a rule -> (a, b) element -> (a, b) stack =
  fun sct r el ->
    try
      let { stack; hooks } = StackContainer.find sct r.ptr in
      if not (List.exists (eq_C el) !stack) then (
        (*if !debug_lvl > 3 then
          log "    Add stack %a ==> %a\n%!"
                         print_rule r print_element el;*)
        stack := el :: !stack;
        List.iter (fun f -> f el) hooks); stack
    with Not_found ->
      (*if !debug_lvl > 3 then
        log "    New stack %a ==> %a\n%!" print_rule r print_element el;*)
      let stack = ref [el] in
      StackContainer.add sct r.ptr {stack; hooks=[]};
      stack

(** [find_stack sct rule] finds the stack to associate to the given rule,
    again the stack is created if needed *)
let find_stack : type a b. b sct -> a rule -> (a, b) stack = fun sct r ->
  try (StackContainer.find sct r.ptr).stack with Not_found ->
    let stack = ref [] in
    StackContainer.add sct r.ptr {stack; hooks=[]};
    stack

(** Computes the size of an element stack, taking in account sharing *)
let size : 'a final -> (Obj.t, Obj.t) stack -> int = fun el adone ->
  let cast_elements
      : type a b.(a,b) element list -> (Obj.t, Obj.t) element list
    = Obj.magic
  in
  let res = ref 1 in
  let rec fn : (Obj.t, Obj.t) element list -> unit = fun els ->
    List.iter (fun el ->
      if List.exists ((==) el) !adone then () else begin
        res := !res + 1;
        adone := el :: !adone;
        match el with
        | C {stack; _} -> fn (cast_elements !stack)
        | B _          -> ()
      end) els
  in
  match el with D {stack; _} -> fn (cast_elements !stack); !res

(** Computes the size of the two tables, forward reading and the current
    elements. The other tables do not retain stack pointers *)
let size_tables els forward =
  if !debug_lvl > 0 then
    let adone = ref [] in
    let res = ref 0 in
    Hashtbl.iter (fun _ el -> res := !res + 1 + size el adone) els;
    Input.OrdTbl.iter forward (fun el -> res := !res + 1 + size el adone);
    !res
  else 0

(** wrap up the size function *)
let size el = size el (ref [])

(** A fonction to fetch the key of the tail of a rule, needed
    to get the key of an element representing a complete parsing *)
let rec tail_key : type a. a rule -> int = fun rule ->
  match rule.rule with
  | Next(_,_,Arg rest) -> tail_key rest
  | Next(_,_,Pos rest) -> tail_key rest
  | Next(_,_,Ign rest) -> tail_key rest
  | Empty _ -> rule.adr
  | Dep _ -> rule.adr

(** Constructor for the final type which includes its hash ley *)
let final : type b c r. pos2 -> (b -> c) -> (c, r) stack ->
                 b rule -> c rule -> r final =
  fun start acts stack rest full ->
    D {start; acts; stack; rest; full }

(** Get the key of an element *)
let elt_key (D {start={buf;col;_}; rest; full; _}) =
  let a = full.adr in
  let b = rest.adr in
  let c = Input.line_offset buf + col in
  let h = a lxor b lxor c in
  (a,b,h)

(** Get the key of the final element when parsing is finished *)
let final_key {buf;col;_} full =
  let a = full.adr in
  let b = tail_key full in
  let c = Input.line_offset buf + col in
  let h = a lxor b lxor c in
  (a,b,h)

(** Test is a given char is accepted by the given rule *)
let good c rule =
  let i = rule_info rule in
  let (ae,set) = force i in
  ae || Charset.mem set c

let max_stack = ref 0

(** Adds an element in the current table of elements, return true if new *)
let add : string -> pos2 -> char -> 'a final -> 'a cur -> bool =
  fun msg pos_final _ (D {rest; _} as element) elements ->
    let key = elt_key element in
    try
      let e = HK.find elements key in
      (match e, element with
         D {start=s; rest; full; stack; acts},
         D {start=_; rest=r'; full=fu'; stack = stack'; acts = acts'} ->
         match eq_rule rest r', eq_rule full fu' with
         | Y, Y ->
            if !warn_merge && not (acts == acts') then
              begin
                let fname = Input.filename s.buf_ab in
                let ls = Input.line_num s.buf_ab in
                let lc = Input.utf8_col_num s.buf_ab s.col_ab in
                let es = Input.line_num pos_final.buf in
                let ec = Input.utf8_col_num pos_final.buf pos_final.col in
                log "\027[31mmerging %a at %s %d:%d-%d:%d\027[0m\n%!"
                    print_final_no_pos element fname ls lc es ec
              end;
            assert(stack == stack');
            false
         | _ -> assert false)
    with Not_found ->
         if !debug_lvl > 1 then
           begin
             let (ae,cs) = Fixpoint.force (rule_info rest) in
             let size = size element in
             if size > !max_stack then max_stack := size;
             log "add %-6s: %a (%d/%d,%b,%a)\n%!" msg print_final element
                 size !max_stack ae Charset.print cs;
           end;
         HK.add elements key element;
         true

(** Combinators for actions, these are just the combinators we need, contructed
    from there types *)

(** This one for completion *)
let cns : type a b c.a -> (b -> c) -> ((a -> b) -> c) = fun a f g -> f (g a)

let cns_pos : type a b c.(a -> (b -> c) -> ((a -> b) fpos -> c)) fpos =
  fun b p b' p' a f g -> f (g b p b' p' a)

let cns_ign : type a b c.a -> (b -> c) -> (b -> c) =
  fun _ f -> f

(** This one for prediction *)
let combine : type a c d.(c -> d) -> (a -> (a -> c) -> d) pos =
  fun acts -> Simple(fun a f -> acts (f a))

let combine_pos : type a c d.(c -> d) -> (a -> (a -> c) fpos -> d) pos =
  fun acts -> WithPos(fun b p b' p' a f -> acts (f b p b' p' a))

let combine_ign : type a c d.(c -> d) -> (a -> c -> d) pos =
  fun acts -> Simple(fun _ f -> acts f)

(** Exception for parse error, can also be raise by
    Ter2 terminals, but no other terminal handles it *)
exception Parse_error of Input.buffer * int

let update_errpos errpos buf col =
  match !errpos with
  | None -> errpos := Some(buf,col)
  | Some(buf',col') ->
     if Input.buffer_before buf' col' buf col
     then errpos := Some(buf,col)

(** This is now the main function computing all the consequences of the
   element given at first argument.
   It needs
   - the element [elt0] being added
   - the [errpos] to record advaces in the parsing
   - the four tables: [elements forward sct tmemo]
   - the [blank] (to pass it to Ter2 terminals)
   - the position [cur_pos] and current charater [c] for the action
     and the good test

   It performs prediction/completion/lecture in a recursive way.
 *)
let pred_prod_lec : type a. errpos -> a final -> a cur -> a reads
    -> a sct -> tmemo -> blank -> pos2 -> char -> unit =
  fun errpos elt0 elements forward sct tmemo blank cur_pos c ->
  let rec fn elt0 =
    match elt0 with
    | D {start; acts; stack; rest; full} ->
       match rest.rule with

       (* A non terminal : prediction *)
       | Next(_,NonTerm{rules;memo;_},rest2) ->
          if !debug_lvl > 0 then
            log "Prediction: %a\n%!" print_final elt0;
          (* Create one final elements for each rule and calls fn if not new *)
          let rules =
            try let res = Ref.find tmemo memo in
                res (* Check if this was done *)
            with Not_found ->
              let rules = List.filter (good c) rules in
              Ref.add tmemo memo rules;
              List.iter
                (fun rule ->
                  let start = cur_pos in
                  let stack = find_stack sct rule in
                  let elt = final start idt stack rule rule in
                  let b = add "P" cur_pos c elt elements in
                  if b then fn elt;
                ) rules;
              rules
          in
          (* Computes the elements to add in the stack of all created rules *)
          let c = match rest2 with
            | Arg rest -> C {rest; acts=combine acts; full; start; stack}
            | Pos rest -> C {rest; acts=combine_pos acts; full; start; stack}
            | Ign rest -> C {rest; acts=combine_ign acts; full; start; stack}
          in
          List.iter (fun rule -> ignore (add_stack sct rule c)) rules

       (* Nothing left to parse in the current rule: completion/production *)
       | Empty(a) ->
          begin try
            if !debug_lvl > 0 then
              log "Completion: %a\n%!" print_final elt0;
            (* run the action *)
            let is_empty = eq_pos start cur_pos in
            (* make the action lazy, essatial for rules like "a*" which
                are quadratic otherwise *)
            if is_empty then (* no thunk if empty, not needed and could break give_up ? *)
              let x = acts (apply_pos_start a start cur_pos) in
              (* create a new element in the table for each element
                 in the stack *)
              let complete = fun element ->
                try
                  match element with
                  | C {start=start0; stack=els'; acts; rest; full} ->
                     if good c rest then begin
                         let acts = apply_pos_start acts start cur_pos x in
                         let elt = final start0 acts els' rest full in
                         let b = add "C" cur_pos c elt elements in
                         if b then fn elt
                       end
                  | B _ -> ()
                with Error -> ()
              in
              (* use hook if D starts at current position because element
                 might still be added to the stack *)
              add_stack_hook sct full complete
            else
              let x = Lazy.from_fun (fun () -> acts (apply_pos_start a start cur_pos)) in
              (* create a new element in the table for each element
                 in the stack *)
              let complete = fun element ->
                try
                  match element with
                  | C {start=start0; stack=els'; acts; rest; full} ->
                     if good c rest then begin
                         let acts = apply_pos_start acts start cur_pos (Lazy.force x) in
                         let elt = final start0 acts els' rest full in
                         let b = add "C" cur_pos c elt elements in
                         if b then fn elt
                       end
                  | B _ -> ()
                with Error -> ()
              in
              List.iter complete !stack;
            with Error -> () end

       (* A terminal, we try to read *)
       | Next(_,(Term{memo;_}|Ter2{memo;_}|Test{memo;_} as t),rest) ->
          begin try
            if !debug_lvl > 0 then log "Read      : %a\n%!" print_final elt0;
            let {buf; col; buf_ab; col_ab} = cur_pos in
            let a, buf, col =
              try
                match Ref.find tmemo memo with
                | Some v -> v
                | None -> raise Error
              with Not_found ->
                try
                  let res =
                    match t with
                    | Term{input=f;_} -> f buf_ab col_ab
                    | Ter2{input=f;_} -> f errpos blank buf col buf_ab col_ab
                    | Test{input=f;_} ->
                        let (a,b) = f buf col buf_ab col_ab in
                        if b then (a,buf_ab,col_ab) else raise Error
                    | _               -> assert false
                  in
                  Ref.add tmemo memo (Some res); res
                with Error -> Ref.add tmemo memo None; raise Error
            in
            let empty_parse = Input.buffer_before buf col buf_ab col_ab in
            let elt, gd = match rest with
              | Arg rest ->
                 final start (cns a acts) stack rest full
                , (fun () -> good c rest)
              | Pos rest ->
                 let end_pos =
                   if empty_parse then cur_pos else
                     { buf_ab = buf; col_ab = col; buf; col }
                 in
                 final start (apply_start cns_pos cur_pos end_pos a acts) stack rest full
                , (fun () -> good c rest)
              | Ign rest ->
                 final start (cns_ign a acts) stack rest full
                , (fun () -> good c rest)
            in
            if empty_parse then
              begin
                if gd () then
                  let b = add "T" cur_pos c elt elements in
                  if b then fn elt
              end
            else (* otherwise write in the forward table for the next cycles *)
              forward := Input.OrdTbl.add buf col elt !forward
          with Error -> () end

       (* A dependant rule: computes a rule while parsing ! *)
       | Dep(fn_rule) ->
          begin try
            if !debug_lvl > 0 then log "dependant rule\n%!";
            let a =
              let a = ref None in
              try
                let _ = acts (fun x -> a := Some x; raise Exit) in
                assert false
              with Exit ->
                match !a with None -> assert false | Some a -> a
            in
            let rule = fn_rule a in
            if good c rule then
              let elt = C { start; rest = idtEmpty (); full
                            ; acts = Simple(fun g f -> f (acts (fun _ -> g)))
                            ; stack }
              in
              let stack = add_stack sct rule elt in
              let start = cur_pos in
              let elt = final start idt stack rule rule in
              let b = add "P" cur_pos c elt elements in
              if b then fn elt
          with Error -> () end

  in fn elt0

let count = ref 0

(** The main parsing loop *)
let parse_buffer_aux : type a. ?errpos:errpos -> bool -> blank -> a grammar
                            -> Input.buffer -> int -> a * Input.buffer * int =
  fun ?errpos blank_after blank main buf0 col0 ->
    let internal, errpos = match errpos with
      | None -> (false, ref None)
      | Some e -> (true, e)
    in
    (* get a fresh parse_id *)
    let parse_id = incr count; !count in
    (* contruction of the 3 tables *)
    let elements : a cur = HK.create 8 in
    let forward = ref Input.OrdTbl.empty in
    let sct = StackContainer.create_table () in
    let tmemo = Ref.create_table () in
    (* contruction of the initial elements and the refs olding the position *)
    let main_rule = grammar_to_rule main in
    (* the key of a final parsing *)
    let s0 : (a, a) stack = ref [B Idt] in
    let start = { buf = buf0; col = col0; buf_ab = buf0; col_ab = col0 } in
    let col = ref col0 and buf = ref buf0 in
    let col' = ref col0 and buf' = ref buf0 in
    (* the initial elements *)
    let main_key = final_key start main_rule in
    let init = final start idt s0 main_rule main_rule in
    (* old the last success for partial_parse *)
    let last_success = ref [] in
    (* the list of elements to be treated at the next step *)
    let todo = ref [init] in
    if !debug_lvl > 0 then log "STAR=%5d: %a\n%!" parse_id print_pos start;

    (* The function calling the main recursice function above *)
    let one_step l =
      let c,_,_ = Input.read !buf' !col' in
      let cur_pos = { buf = !buf; col = !col
                      ; buf_ab = !buf'; col_ab = !col' }
      in
      if !debug_lvl > 0 then
        begin
          log "NEXT=%5d: %a\n%!" parse_id print_pos cur_pos;
          if !debug_lvl > 1 then  log "MAXSTACK=%d\n%!" !max_stack
        end;
      List.iter (fun s ->
        if add "I" cur_pos c s elements then
          pred_prod_lec errpos s elements forward sct tmemo blank cur_pos c) l;
    in
    (* Searching a succes *)
    let search_success () =
      try
        let success = HK.find elements main_key in
        last_success := (!buf,!col,success) :: !last_success
      with Not_found -> ()
    in

    (* Main loop *)
    while !todo <> [] do
      StackContainer.clear sct;
      HK.clear elements;
      Ref.clear tmemo;
      (* read blanks *)
      let buf'', col'' = blank !buf !col in
      buf' := buf''; col' := col'';
      (* treat the next elements *)
      one_step !todo;
      (* search success for internal (partial) parse *)
      if internal then search_success ();
      (* advance to the next elements *)
      try
         (* pop the next elements and position *)
         let (new_buf, new_col, l, forward') = Input.OrdTbl.pop !forward in
         todo := l;
         forward := forward';
         (* advance positions *)
         col := new_col; buf := new_buf;
       with Not_found -> todo := []
    done;
    (* search succes at the end for non internal parse *)
    if not internal then search_success ();
    let parse_error () =
      update_errpos errpos !buf' !col';
      if internal then raise Error else
        match !errpos with
        | None -> assert false
        | Some(buf,col) -> raise (Parse_error (buf, col))
    in
    let cur_pos = { buf = !buf; col = !col; buf_ab = !buf'; col_ab = !col' } in
    if !debug_lvl > 0 then log "ENDS=%5d: %a\n%!" parse_id print_pos cur_pos;
    (* Test if the final element contains a B note, too long code
       to accomodate GADT *)
    let fn : type a.a final -> a = function
      | D {stack=s1; rest={rule=Empty f;_}; acts; full; _} ->
         (match eq_rule main_rule full with N -> raise Error | Y ->
           let x = acts (apply_pos f buf0 col0 !buf !col) in
           let gn : type a b. b -> (b,a) element list -> a =
            fun x l ->
              let rec hn =
                function
                | B (ls)::l ->
                   (try apply_pos ls buf0 col0 !buf !col x
                    with Error -> hn l)
                | C _:: l ->
                   hn l
                | [] -> raise Error
              in
              hn l
           in
           gn x !s1)
      | _ -> assert false
    in
    (* Apply the above function to all last success, further position first *)
    let result =
      let rec kn = function
        | [] -> parse_error ()
        | (b,p, elt) :: rest ->
           try
             let a = fn elt in
             if blank_after then
               let (b, p) = blank b p in (a, b, p)
             else (a, b, p)
           with
             Error -> kn rest
      in kn !last_success
    in
    if !debug_lvl > 0 then log "EXIT=%5d: %a\n%!" parse_id print_pos cur_pos;
    result

(** Function to call the parser in a terminal *)
let internal_parse_buffer
    : type a. ?errpos:errpos -> ?blank_after:bool -> blank
           -> a grammar -> Input.buffer -> int -> a * Input.buffer * int
   = fun ?errpos ?(blank_after=false) bl g buf col ->
       parse_buffer_aux ?errpos blank_after bl g buf col
