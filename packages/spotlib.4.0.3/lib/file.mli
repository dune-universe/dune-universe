(** Some simple file content iterations and 
    /usr/bin/test style file status checking functions. *)

(** {6 Iterators over lines } *)

val iter_lines_exn : string -> (string -> 'a) -> unit
(** [iter_lines_exn filename f] iters [f] over lines of contets of [filename].

    Note: lines are obtained by [input_line]: therefore the newline chars are
    removed from the lines.
 *)

val iter_lines : string -> (string -> unit) -> (unit, [> `Exn of exn]) result
(** [iter_lines filename f] iters [f] over lines of contets of [filename].

    Note: lines are obtained by [input_line]: therefore the newline chars are
    removed from the lines.
*)

val to_lines : string -> (string list, [> `Exn of exn]) result
(** [to_lines filename] returns the contens of the file. 

    Note: lines are obtained by [input_line]: therefore the newline chars are
    removed from the lines.
*)

val to_string : string -> (string, [> `Exn of exn]) result
(** [to_string filename] returns the contens of the file. *)

val open_out : string -> (out_channel -> 'a) -> 'a
(** [open_out filename f] opens [filename] then runs [f] over the opened channel. 
    The channel is closed automatically no matter whether [f] terminates normally
    or raises an exception.
*)

val write_lines : string -> string list -> unit

val get_inode : string -> int option
(** Get file inode. Works for Unix and MinGW. Cygwin? Never tried. *)

val equal : string -> string -> bool
(** Check two file names share the same inode *)
(* XXX bad name *)

(** [contains p1 p2] return [Some compos] when [p2] is equal to [p1]
    or under [p1]. [compos] is the directory components from [p2] to reach [p1].

    If [p1] is non existent, the search is performed only by comparison of
    path names. If [p1] exists, the search is performed also with 
    the file's inodes.
*)
val contains : string -> string -> string list option
  
(** /usr/bin/test like file testing *)
module Test : sig
  type test_unary = 
      string  (*+ file name *)
      -> ( [ `TRUE  of Unix.stats | `FALSE of Unix.stats ], Unix.error ) result
	    
  (*     FILE1 -ef FILE2
                FILE1 and FILE2 have the same device and inode numbers
  
         FILE1 -nt FILE2
                FILE1 is newer (modification date) than FILE2
  
         FILE1 -ot FILE2
                FILE1 is older than FILE2
  
  val _b : string -> bool (* FILE exists and is block special *)
  val _b' : test_unary (* FILE exists and is block special *)
  val _c : string -> bool (* FILE exists and is character special *)
  val _c' : test_unary (* FILE exists and is character special *)
  *)
  
  val _d : string -> bool
  val _d' : test_unary 
  (** -d: FILE exists and is a directory *)

  val _e : string -> bool
  val _e' : string -> ([ `TRUE  of Unix.stats | `FALSE], Unix.error) result
  (** -e: FILE exists *)

  val _f : string -> bool
  val _f' : test_unary 
  (** -f: FILE exists and is a regular file *)

  (*
  val _g : string -> bool (* FILE exists and is set-group-ID *)
  val _g' : test_unary (* FILE exists and is set-group-ID *)
  val _G : string -> bool (* FILE exists and is owned by the effective group ID *)
  val _G' : test_unary (* FILE exists and is owned by the effective group ID *)
  *)

  val _h : string -> bool
  val _h' : test_unary 
  (** -h: FILE exists and is a symbolic link (same as -L) *)

  (*
  val _k : string -> bool (* FILE exists and has its sticky bit set *)
  val _k' : test_unary (* FILE exists and has its sticky bit set *)
  *)

  val _L : string -> bool
  val _L' : test_unary 
  (** -L: FILE exists and is a symbolic link (same as -h) *)

  (*
  val _O : string -> bool (* FILE exists and is owned by the effective user ID *)
  val _O' : test_unary (* FILE exists and is owned by the effective user ID *)
  val _p : string -> bool (* FILE exists and is a named pipe*)
  val _p' : test_unary (* FILE exists and is a named pipe*)
  val _r : string -> bool (* FILE exists and read permission is granted*)
  val _r' : test_unary (* FILE exists and read permission is granted*)
  *)

  val _s : string -> bool
  val _s' : test_unary 
  (** -s: FILE exists and has a size greater than zero*)

  (*
  val _S : string -> bool (* FILE exists and is a socket*)
  val _S' : test_unary (* FILE exists and is a socket*)
  
  val _t : string -> bool (* file descriptor FD is opened on a terminal *)
  val _t' : test_unary (* file descriptor FD is opened on a terminal *)
  		       IT TAKES FD not FILE!
  
  val _u : string -> bool (* FILE exists and its set-user-ID bit is set*)
  val _u' : test_unary (* FILE exists and its set-user-ID bit is set*)
  val _w : string -> bool (* FILE exists and write permission is granted*)
  val _w' : test_unary (* FILE exists and write permission is granted*)
  val _x : string -> bool (* FILE exists and execute (or search) permission is granted*)
  val _x' : test_unary (* FILE exists and execute (or search) permission is granted*)
  *)
end
