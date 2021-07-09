
(** Runs the debuggees and returns the first event.  *)
val run : unit -> RdbgEvent.t


(** main:Displays info about the test parameters *)
val info : unit -> unit
val info_string : unit -> string

(** main:A Shortcut for info *)
val i : unit -> unit
 
(** main:Lists functions related to a string *)
val apropos : string -> unit

(** main:A Shortcut for apropos *)
val a : string -> unit

(** main:Displays help about rdbg commands *)
val help : string -> unit
val help_string:string -> string

(** main:A Shortcut for help *)
val h : string -> unit

(** main:Displays a small online manual *)
val man : unit -> unit

(** main:Stops the interaction with the rdbg (behaves as lurette after that) *)
val off : unit -> unit


(** main:Quits rdbg *)
val quit : unit -> unit

(** main:Quits rdbg *)
val q : unit -> unit

(** misc:Adds an entry in the doc [add_doc_entry name profile message category file]. *)
val add_doc_entry : string -> string -> string -> string -> string -> unit
  
(** Gets the profile of a documented function *)
val doc_prof : string -> string

(** Gets the documentation message of a documented function *)
val doc_msg : string -> string

(** Gets the documentation label of a documented function *)
val doc_label : string -> string

(** Gets the filename where the function is defined *)
val doc_file : string -> string


(**/**)
(* for internal use *)
val get_prompt : unit -> string 
                   
(*  Experimental function.
 
 Display a caml expr that, once evaluated, binds variable value
  in the current toplevel. XXX How can it be automated?
*)
val get_val_event : RdbgEvent.t -> unit

