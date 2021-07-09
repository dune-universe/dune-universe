(* Time-stamp: <modified the 10/06/2021 (at 14:19) by Erwan Jahier> *)

 
(** main:Launches the debuggees, calls hooks (by default), and returns the first event *)
val run : ?call_hooks:bool -> unit -> RdbgEvent.t

(** {1 Moving forwards } *)

(** move:Computes the next event, calls the hooks functions on it, and returns that event *)
val next : RdbgEvent.t -> RdbgEvent.t

(** move:as next, without printing the current event *)
val next_np : RdbgEvent.t -> RdbgEvent.t


(** move:Returns the ith next event *)
val nexti : RdbgEvent.t -> int -> RdbgEvent.t

(** move:Returns the ith next event without printing events *)
val nexti_np : RdbgEvent.t -> int -> RdbgEvent.t

(** move:Goes to the next simulation step of the current node *)
val step : RdbgEvent.t -> RdbgEvent.t

(** move:Goes to the ith simulation step of the current node *)
val stepi : RdbgEvent.t -> int -> RdbgEvent.t

(** move:Returns the next event that satisfies a condition *)
val next_cond : RdbgEvent.t -> (RdbgEvent.t -> bool) -> RdbgEvent.t

type ckptl = RdbgEvent.t list
  
val next_cond_gen :
  RdbgEvent.t -> (RdbgEvent.t -> bool) -> (RdbgEvent.t -> RdbgEvent.t) -> RdbgEvent.t
  
(** move:Returns the next event s.t. the string arg is a substring of the event name   *)
val next_match : RdbgEvent.t -> string -> RdbgEvent.t

(** move:Returns the previous event s.t. the string arg is a substring of the event name *)
val previous_match : RdbgEvent.t -> string -> RdbgEvent.t

(** move:Loops forever *)
val loopforever : RdbgEvent.t -> unit


(** {1 Moving backwards } *)

(** misc:Turns on/on the possibility to run backwards *)
val time_travel : bool -> unit

(** move:Moves one event backwards *)
val back : RdbgEvent.t -> RdbgEvent.t

(** move:Moves i events backwards *)
val backi : RdbgEvent.t -> int -> RdbgEvent.t

(** move:Moves one step backwards *)
val prev : RdbgEvent.t -> RdbgEvent.t

(** move:Moves i steps backwards *)
val previ : RdbgEvent.t -> int -> RdbgEvent.t

(** move:Returns the previous event satisfying a condition *)
val rev_cond : RdbgEvent.t -> (RdbgEvent.t -> bool) -> RdbgEvent.t

(** move:Returns the  previous event satisfying a condition  p using a
   custom next.

Since the next function may modify some (external) state, rev_cond_gen
   is parameterized  by the  restore_state:int->unit function  that is
   able to  restore the external  state at its  value at event  i (the
   save_state i ought to be done by the caller) *)
val rev_cond_gen :
  RdbgEvent.t -> (RdbgEvent.t -> bool) -> (RdbgEvent.t -> RdbgEvent.t) ->
  (int -> unit) -> RdbgEvent.t


(** move:Goes to step number i  *)
val goto_s : RdbgEvent.t -> int -> RdbgEvent.t

(** move:Goes to event nb i *)
val goto : RdbgEvent.t -> int -> RdbgEvent.t


(** Checkpoint rate (that can be changed). *)
val ckpt_rate : int ref

(** A predicate that controls when check-point are done.
By default, it is done periodically every !ckpt_rate event. 

But of course one can change it (hence the ref)
*)
val check_ref : (RdbgEvent.t -> bool) ref

(** List of checkpoints made when !check_ref is true. 
    This list is used and modified by the commands
    - rev
    - rev_cond
    - back/backi
    - goto

 *)
val ckpt_list : RdbgEvent.t list ref

(* [goto_last_ckpt i] returns the ckpt immediately preceding event nb i 

ZZZ It modifies the checkpoints list by poping out checkpoints while moving backwards.
*)
val goto_last_ckpt : int -> RdbgEvent.t

                                                 
(** {1 Breakpoints } *)
                             
(** move:Adds a breakpoint on a node or a file.
A breakpoint is a string of the form:
       "node"
       "node@line"
       "file"
       "file@line"
 *)
val break : string -> unit

(** move:The list of breakpoints *)
val breakpoints : string list ref

(** move:Removes all breakpoints *)
val delete : unit -> unit

(** move:continues to the next breakpoint  *)
val continue : RdbgEvent.t -> RdbgEvent.t

(** move:runs backwards until the next breakpoint *)
val rev : RdbgEvent.t -> RdbgEvent.t

(** move:Goes to to a specific explicit breakpoint (cf 'break' for breakpoints syntax) *)
val goto_brk : RdbgEvent.t -> string -> RdbgEvent.t

(** {1 View data} *)

(** data:Gets the value of an int variable *)
val v : string -> RdbgEvent.t -> Data.v

(** data:Gets the value of an int variable *)
val vi : string -> RdbgEvent.t -> int

(** data:Gets the value of a float variable *)
val vf : string -> RdbgEvent.t -> float

(** data:Gets the value of a boolean variable *)
val vb : string -> RdbgEvent.t -> bool

  
(** data:Prints some source code information attached to an event *)
val print_src : RdbgEvent.t -> unit

(** data:Prints the call stack *)
val print_call_stack : RdbgEvent.t -> unit

(** {1 Hooks} *)

(** misc:Hooks are  functions that are called each time  a new event is
    obtained with  the next function  and thus with functions that
    use next, such as nexti, next_cond, rev_conv, step, stepi, and 
    more). 
       
    [add_hook] adds (or replaces) a hook function in the hooks list.    
*)
val add_hook : string -> (RdbgEvent.t -> unit) -> unit

(** misc:Gets a hook function (may raise [Not_found]) *)
val get_hook : string -> (RdbgEvent.t -> unit) 

(** misc:Removes a hook  *)
val del_hook : string -> unit

(** misc:Removes all hooks *)
val del_all_hooks : unit -> unit

(** misc:Lists hooks *)
val list_hooks : unit -> string list

(** {1 Misc} *)

(** Controls the printing of skipped events *)
val show_trace : bool ref

(** Controls the behavior of print_event *)
val show_data : bool ref

(** Controls the behavior of print_event *)
val show_src : bool ref

(** data:Prints an event  *)
val string_of_event : RdbgEvent.t -> string
val print_event : RdbgEvent.t -> unit
