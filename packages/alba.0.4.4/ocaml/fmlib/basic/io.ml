open Module_types


module Error =
struct
    type t = {
        code: string;
        message: string;
    }

    let code (error: t): string =
        error.code

    let message (error: t): string =
        error.message

    let make (code: string) (message: string): t =
        {code; message}
end




module type STAT =
  sig
    type t
    type time

    val compare: time -> time -> int
    val is_directory: t -> bool
    val is_file:      t -> bool
    val modification: t -> time
  end










module type SIG =
  sig
    module M: MONAD
    include MONAD

    module Path:
    sig
      val absolute: string -> string t
      val split: string -> (string * string) option
      val normalize: string -> string
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



    module Stdout:
    sig
      val putc: char -> unit t
      val string: string -> unit t
      val line: string -> unit t
      val newline: unit t
      val fill: int -> char -> unit t
    end

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
  end



















module Output(Io: SIG) =
    struct
        type fd = Io.File.Out.fd

        type t = fd -> unit Io.t

        let empty: t =
            fun _ -> Io.return ()

        let (<+>) (a: t) (b: t): t =
            fun fd ->
            Io.(a fd >>= fun () -> b fd)

        let char (c: char): t =
            Io.File.Out.putc c

        let fill (n: int) (c: char): t =
            Io.File.Out.fill n c

        let substring (str: string) (start: int) (len: int): t =
            Io.File.Out.substring str start len

        let string (str: string): t =
            Io.File.Out.string str

        let line str =
            Io.File.Out.line str

        let newline =
            Io.File.Out.newline

        let run (fd: fd) (p: t): unit Io.t =
            p fd
    end
