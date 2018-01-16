open Unix

module Find : sig

  class type path = object
    method base : string
    method depth : int (* 0 for paths given directly to [find] *)
    method dev_inode : (int * int, [`Exn of exn]) result
    method dir : string
    method is_dir : bool
    method is_ldir : bool
    method is_reg : bool
    method kind : (Unix.file_kind, [`Exn of exn]) result
    method lkind : (Unix.file_kind, [`Exn of exn]) result
    method path : string
    method stat : (Unix.stats, [`Exn of exn]) result
    method lstat : (Unix.stats, [`Exn of exn]) result
  end

  val prune : unit -> 'a

  val find : ?follow_symlink:bool -> f:(path -> unit) -> string list -> unit

  val fold 
    : ?follow_symlink:bool  (*+ default false *)
    -> string list 
    -> 'st 
    -> ('st -> path -> [`Continue | `Exit | `Prune] * 'st)
    -> 'st

  val files : ?follow_symlink:bool -> string list -> path list
  (** simply Grabs all the files under the specified directories *)
end

module CommandDeprecated : sig

  (** Command invocation 

      Typical usage is:

      [ starter |> scanner ], for example
      [ shell "ls /" |> print ]

      or

      [ starter |> scanner |> result_checker ], for example
      [ shell "ls /" |> get_stdout |> must_exist_with ~name:"ls /" 0 ]
  *)

  (** Starter 

      Starters invoke processes and returns a value of ['st t]. 
      You do not need to care about ['st]: it is determined by a output scanner. 
  *)
    
  type 'st t

  val shell : string -> 'st t
  (** Execute a shell command using /bin/sh *)

  val execvp : string list -> 'st t
  (** Same as [shell_command] but it takes the command and arguments as a list of string,
      then directly executed by [Unix.execvp]. *)


  (** Output scanner

      Output scanner takes the result of a starter and scans its stdout+stderr output.
      After all the stdout+stderr outputs are sent to the scanner, it returns
      the final result of the scan and the process status

  *)

  type 'a result = Unix.process_status * 'a

  val fold : 
    'st t
    -> init:'st 
    -> f:('st -> [`Out | `Err] * [`Read of string | `EOF] -> 'st) 
    -> 'st result
  (** Generic scanner *)

  val iter : 
    unit t
    -> f:([`Out | `Err] * [`Read of string | `EOF] -> unit) 
    -> unit result
  (** Iteration over stdout+stderr outputs *)

  val print :
    ?prefix:  string
    -> unit t
    -> unit result
  (** Output to stdout and stderr of the command are printed to stdout and stderr
      resp.ly. *)
    
  val ignore_output :
    unit t
    -> unit result
  (** Completely ignore the outputs *)
    
  val get_stdout :
    string list t -> string list result
  (** Gather stdout lines. Stderr outputs are printed to stderr. *)

  val get_all :
    string list t -> string list result
  (** Gather stdout and stderr lines. *)

  (** Result tools
  *)

  val fail : ?name:string -> 'res result -> 'no_return
  val from_exit : ?name:string -> 'res result -> int * 'res
  val must_exit_with : ?name:string -> int -> 'res result -> 'res
  val should_exit_with : int -> 'res result -> ('res, Unix.process_status * 'res) Vresult.t
end

val timed : ('a -> 'b) -> 'a -> 'b * float (* in sec *)

module Process_times : sig
  type t = process_times
  val (-) : t -> t -> t
  val timed : ('a -> 'b) -> 'a -> 'b * t
end

val mkdir : ?perm:Unix.file_perm -> ?recursive:bool -> string ->
  (unit,
   string * [> `Unix of Unix.error
            |  `Already_exists of Unix.stats
            |  `Not_a_directory of Unix.stats ]) result
  (** Create a directory of the given name. Does nothing if the
      directory exists already. 

      Bug: No chmod if the directroy already exists.
  *)

val mkdtemp : string -> string
(** Like mkdtemp(3). It must take a template whose postfix is "XXXXXX".
    It creates a new directory name by replacing "XXXXXX" by random number then creates a directory
    of permission 0o700 
*)

val with_dtemp : string -> (string -> 'a) -> 'a
(** [with_dtemp template f] creates a temp directory [dir] using 
    [mkdtemp template] then runs [f dir]. 
    The temp directory [dir] is automatically removed 
    (even if it is not empty) after [f dir].
*)
 
val with_chdir : ?at_failure:(exn -> 'a) -> string -> (unit -> 'a) -> 'a
(** [with_chdir ?at_failure dir f] may raises an exception when:
    - [at_failure] raises an exception when the first chdir failure is reported to it
    - [f ()] raises an exception
    - chdir back to the original directory fails (reported by [Exn.Finally])
*)

val with_dir : string -> (dir_handle -> 'a) -> 'a
(** [with_dir dir f] runs [f] with the [dir_handle] against [dir] *)

val fold_dir : string -> 'a -> ('a -> string -> 'a) -> 'a
(** [fold_dir dir init f] runs [f] folding over the files 
    under the directory [dir], with the initial value [init] *)

val timed_message : string -> ('a -> 'b) -> 'a -> 'b
(** [timed_message mes f] returns a function as same as [f] but
    with printing computation times to stderr.
*)

module Pervasives : sig
  (** values accessible simply via [open Spotlib.Spot] *)
    
  val timed_message : string -> ('a -> 'b) -> 'a -> 'b
  (** [timed_message mes f] returns a function as same as [f] but
      with printing computation times to stderr.
  *)
end

