open Module_types
open Common


module type SIG =
sig
    type in_file
    type out_file

    val stdin:  in_file
    val stdout: out_file
    val stderr: out_file

    module M: MONAD
    include MONAD


    val exit: int -> 'a t
    val execute: unit t -> unit
    val command_line: string array t
    val current_working_directory: string  t



    val cli_loop:
        'a
        -> ('a -> string option)
        -> ('a -> string -> 'a t)
        -> ('a -> 'a t)
        -> 'a t


    val path_separator: char
    val path_delimiter: char

    val read_directory: string -> string array option t

    module Read: functor (W:WRITABLE) ->
                 sig
                   val read_buffer: in_file -> W.t -> W.t t
                   val read: in_file -> W.t -> W.t t
                 end
    module Write: functor (R:READABLE) ->
                  sig
                    val write_buffer: out_file -> R.t -> R.t t
                    val write: out_file -> R.t -> R.t t
                  end
end

module Make (Base: SIG): Io.SIG =
  struct
    include Base

    module Path =
      struct
        let absolute (path:string): string t =
          let len = String.length path
          in
          if 0 < len && path.[0] = path_separator then
            return path
          else
            current_working_directory >>= fun cwd ->
            return (if len = 0
                    then cwd
                    else cwd ^ String.one path_separator ^ path)

        let split (path:string): (string * string) option =
          Path.split path_separator path

        let normalize (path:string): string =
          Path.normalize path_separator path

        let join (dir:string) (base:string): string =
          dir ^ String.one path_separator ^ base
      end


    module Process = struct
        let exit = exit
        let execute = execute
        let command_line = command_line
        let current_working_directory = current_working_directory
    end

    module Directory =
      struct
        let read = read_directory
      end


    (*let read_file
          (path:string) (cannot_open:'a t) (read:in_file -> 'a t)
        : 'a t =
      open_for_read path >>= fun fd ->
      match fd with
      | None ->
         cannot_open
      | Some fd ->
         read fd >>= fun a ->
         close_in fd >>= fun _ ->
         return a

    let write_file
          (path:string) (cannot_open:'a t) (write:out_file -> 'a t)
        : 'a t =
      open_for_write path >>= fun fd ->
      match fd with
      | None ->
         cannot_open
      | Some fd ->
         write fd >>= fun a ->
         close_out fd >>= fun _ ->
         return a

    let create_file
          (path:string) (cannot_create:'a t) (write:out_file -> 'a t): 'a t =
      create path >>= fun fd ->
      match fd with
      | None ->
         cannot_create
      | Some fd ->
         write fd >>= fun a ->
         close_out fd >>= fun _ ->
         return a
     *)

    module File =
      struct
        module In =
          struct
            type fd = in_file
          end
        module Out =
          struct
            type fd = out_file
            let substring
                  (s:string) (start:int) (len:int) (fd:out_file)
                : unit t =
              let module W = Write (String_reader) in
              W.write fd (String_reader.of_substring s start len)
              >>= fun _ ->
              return ()

            let string (s:string) (fd:out_file): unit t =
              substring s 0 (String.length s) fd

            let putc (c:char) (fd:out_file): unit t =
              let module W = Write (Char_reader) in
              W.write fd (Char_reader.make c)
              >>= fun _ ->
              return ()

            let newline (fd:out_file): unit t =
              putc '\n' fd

            let line (s:string) (fd:out_file): unit t =
              string s fd >>= fun _ ->
              newline fd

            let fill (n:int) (c:char) (fd:out_file): unit t =
              let module W = Write (Fill_reader) in
              W.write fd (Fill_reader.make n c)
              >>= fun _ ->
              return ()
          end

        let stdin:  In.fd  = stdin
        let stdout: Out.fd = stdout
        let stderr: Out.fd = stderr
      end


(*
    let getc_in: char option t =
      getc stdin

    let get_line_in: string option t =
      get_line stdin
 *)


    module Stdout =
      struct
        open File
        let putc (c:char): unit t =
          Out.putc c stdout

        let string (s:string): unit t =
          Out.string s stdout

        let line (s:string): unit t =
          Out.line s stdout

        let newline: unit t =
          Out.newline stdout

        let fill n c = Out.fill n c stdout
      end


    module Stderr =
      struct
        open File
        let putc (c:char): unit t =
          Out.putc c stderr

        let string (s:string): unit t =
          Out.string s stderr

        let line (s:string): unit t =
          Out.line s stderr

        let newline: unit t =
          Out.newline stderr

        let fill n c = Out.fill n c stderr
      end
  end

