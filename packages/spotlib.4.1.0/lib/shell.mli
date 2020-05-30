(** Execution of some unix commands *)

val com : string -> string list -> Unix.process_status
(** [com exec args] executes the executable file [exec] with [args],
    printing its stdout and stderr outputs.  It waits until the end
    of the command execution then returns its final process status.
 *)
  
val cp : string list -> Unix.process_status
(** Run '/bin/cp' with the given arguments *)

val mv : string list -> Unix.process_status
(** Run '/bin/mv' with the given arguments *)
      
val rm : string list -> Unix.process_status
(** Run '/bin/rm' with the given arguments *)

val cat : string list -> Unix.process_status
(** Run '/bin/cat' with the given arguments *)

val file : string -> (string option, Unix.process_status) result
(** Run '/usr/bin/file' with the given arguments. The last line of
    the stdout is returned if successful. *)

val grep 
    : string list 
      -> init:'a 
      -> f: (Command.output -> 'a -> 'a) 
      -> 'a * Unix.process_status
(** Run '/bin/grep' with the given arguments.  It folds over the output
    lines of the execution's stdout and stderr.
 *)

val grep_ : string list -> Unix.process_status
(** Simpler version of [grep].  Simply returns the final status *)
  
val cmp : string -> string -> [> `Different | `Error | `Same ]
(** Run '/bin/cmp' *)
