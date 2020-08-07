(** Definition of an enviroment for console applications which can run natively
or under nodejs or any other module which satisfies the signature [SIG].



The Fmlib helps you to develop Ocaml programs which run natively and can be
compiled to javascript without changing the code of the application.

You develop your program as a functor accepting a module argument of type
{!module-type: SIG}. The program does all input/ouptut via the module [Io].

{[
    (* Content of file [program.ml] *)
    module Make (Io: Io.SIG) =
    struct
        let run _: unit =
        let open Io in
        Process.execute
            (Stdout.line "Hello world")
    end

]}

The library provides two instances of type [Io.SIG]. One is [Fmlib_native.Io]
for native applications and the other is [Fmlib_node.Io] for nodejs
    applications.

You can make a native application by
{[
    module P = Program.Make (Fmlib_native.Io)

    let _ =
        P.run ()

]}

and a nodejs application by
{[
    module P = Program.Make (Fmlib_node.Io)

    let _ =
        P.run ()

]}




*)


open Module_types


(** IO Errors *)
module Error:
sig
    type t
    val code:    t -> string
    val message: t -> string
    val make: string -> string -> t
end




(** Statistic data about a file/directory. *)
module type STAT =
  sig
    (** Type of the data. *)
    type t

    (** Time stamp. *)
    type time

    (** Compare time stamps. *)
    val compare: time -> time -> int

    (** Is the node a directory? *)
    val is_directory: t -> bool

    (** Is the node a file? *)
    val is_file:      t -> bool

    (** Time of last modification. *)
    val modification: t -> time
  end









(** Signature of an IO environment. *)
module type SIG =
  sig
    module M: MONAD
    include MONAD

    module Path:
    sig
      (** [absolute path] converts [path] into an absolute path. *)
      val absolute: string -> string t


      (** [split path] splits [path] into a dirname and a basename if
         possible.

         Examples:
         {[
           split ""                = None
           split "/"               = None
           split "/hello"          = Some ("/", "hello")
           split "/User/name/xxx/" = Some("/User/name", "xxx")
           split "/User/name/xxx"  = Some("/User/name", "xxx")
         ]}

       *)
      val split: string -> (string * string) option


      (** [normalize path] removes duplicate path separators and normalizes
         "." and ".." segments.


         Examples:
         {[
           normalize ""            = "."
           normalize "/"           = "/"
           normalize "////"        = "/"
           normalize "a//b"        = "a/b"
           normalize "a/./b"       = "a/b"
           normalize "a/../b"      = "b"
           normalize "a/b/../../c" = "c"
           normalize "../a"        = "../a"
         ]}
       *)
      val normalize: string -> string

      (** [join dir file] joins the directory name [dir] with the file name
         [file]. *)
      val join: string -> string -> string
    end



    module Process:
    sig
        val exit: int -> 'a t
        val execute: unit t -> unit
        val command_line: string array t
        val current_working_directory: string  t
    end


    module Directory:
    sig
      (** [read path] reads the entries (files and subdirectories) of the
         directory [path] and returns it in an array ([..] and [.] are not
         included). *)
      val read: string -> string array option t
    end



    module File:
    sig
        module In:
        sig
            type fd
            val open_: string -> (fd, Error.t) result t
            val close: fd -> unit t
        end

        module Out:
        sig
            type fd
            val open_:  string -> (fd, Error.t) result t
            val create: string -> (fd, Error.t) result t
            val close: fd -> unit t

            val putc: char -> fd -> unit t
            val substring: string -> int -> int -> fd -> unit t
            val string: string -> fd -> unit t
            val line: string -> fd -> unit t
            val newline: fd -> unit t
            val fill: int -> char -> fd -> unit t
        end

        val stdin:  In.fd
        val stdout: Out.fd
        val stderr: Out.fd


        module Read (W: WRITABLE):
        sig
            val read_buffer: In.fd -> W.t -> W.t t
            val read:        In.fd -> W.t -> (W.t, W.t * Error.t) result t
        end
    end






    (** Write to standard output (usually the screen). *)
    module Stdout:
    sig
      val putc: char -> unit t
      val string: string -> unit t
      val line: string -> unit t
      val newline: unit t
      val fill: int -> char -> unit t
    end



    (** Write to standard error (usually the screen). *)
    module Stderr:
    sig
      val putc: char -> unit t
      val string: string -> unit t
      val line: string -> unit t
      val newline: unit t
      val fill: int -> char -> unit t
    end




    val cli_loop:
        'a
        -> ('a -> string option)
        -> ('a -> string -> 'a t)
        -> ('a -> 'a t)
        -> 'a t
    (** [cli_loop state prompt next stop] runs a cli loop which starts in state
    [state] and prompts its users with the string returned by [prompt state]. If
    [prompt state] returns [None], the cli_loop is ended.

    If the user enters a line, the command [next state line] is performed.

    If [prompt state] returns [None] or the user ends its input by pressing
    [ctrl-d], the command [stop state] is performed.

    Example:

    The following command starts a cli_loop and prompts the user at most 5 times
    with [i> ] and echoes back the input of the user.

    {[
        cli_loop
            0
            (fun i ->
                if i < 5 then
                    Some (string_of_int i ^ "> ")
                else
                    None)
            (fun i line ->
                Stdout.line line >>= fun _ -> return (i + 1))
            (fun _ -> return ())
    ]}
    *)





  end








module Output (Io:SIG):
sig
    type t
    val empty: t
    val (<+>): t -> t -> t
    val char: char -> t
    val string: string -> t
    val line: string -> t
    val newline: t
    val substring: string -> int -> int -> t
    val fill: int -> char -> t
    val run: Io.File.Out.fd -> t -> unit Io.t
end
