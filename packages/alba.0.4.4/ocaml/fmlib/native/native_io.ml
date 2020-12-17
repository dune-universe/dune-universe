open Fmlib

open Module_types


module Error = Io.Error

type 'a io_result = ('a, Error.t) result



module Buffer:
sig
    type t

    val make: int ->
              (Bytes.t -> int -> int -> int io_result) ->
              (Bytes.t -> int -> int -> int) ->
              t


    val is_ok:    t -> bool
    val is_empty: t -> bool
    val is_full:  t -> bool

    val fill:  t -> unit io_result
    val flush: t -> unit



    module Read: functor (W: WRITABLE) ->
    sig
        val read: t -> W.t -> W.t
    end


    module Write: functor (R: READABLE) ->
    sig
        val write: t -> R.t -> R.t
    end
end
=
struct
    type t = {
        mutable rp: int; (* The content of the buffer is between the read and
                            the write pointer. *)
        mutable wp: int;
        mutable flag: bool; (* ok flag, set to false if (a) refilling a buffer
                               adds 0 bytes, (b) flushing a nonempty buffer
                               does not write anything to the filesystem. *)
        read:  Bytes.t -> int -> int -> int io_result; (* refill function *)
        write: Bytes.t -> int -> int -> int; (* flush function *)
        bytes: Bytes.t}


    let make
        (n:int)
        (read:  Bytes.t -> int -> int -> int io_result)
        (write: Bytes.t -> int -> int -> int)
        : t
        =
        assert (n > 0);
        assert (n <= Sys.max_string_length);
        {
                rp = 0;
                wp = 0;
                bytes = Bytes.create n;
                flag = true;
                read;
                write;
        }


    let is_ok (b:t): bool =
      b.flag


    let is_empty (b:t): bool =
      b.rp = b.wp


    let reset (b:t): unit =
      b.rp <- 0;
      b.wp <- 0


    let is_full (b:t): bool =
      b.wp = Bytes.length b.bytes


    let fill (b: t): unit io_result =
        assert (is_ok b);
        assert (is_empty b);
        reset b;
        Result.map
            (fun n ->
                if n = 0 then
                    b.flag <- false
                else
                    b.wp <- n)
            (b.read b.bytes b.rp (Bytes.length b.bytes))



    let flush (b:t): unit =
      if not (is_empty b) && is_ok b then
        let n = b.write b.bytes b.rp (b.wp - b.rp) in
        if n = 0 then
          b.flag <- false
        else
          (* BUG: what if n > 0 && n <> b.wp - b.rp ?? Only a part of the
             buffer has been written!! *)
          reset b


    let get (b:t): char =
        assert (not (is_empty b));
        let c = Bytes.get b.bytes b.rp in
        b.rp <- b.rp + 1;
        c


    let put_character (b:t) (c:char): unit =
      assert (is_ok b);
      if is_full b then
        flush b;
      assert (not (is_full b));
      Bytes.set b.bytes b.wp c;
      b.wp <- b.wp + 1


    module Read (W: WRITABLE) =
    struct
        let read (b: t) (w: W.t): W.t =
            let rec read w =
                if
                    not (is_empty b)
                    && W.needs_more w
                then
                    read (W.put_character w (get b))
                else
                    w
            in
            read w
    end


    module Write (R: READABLE) =
    struct
        let write (b: t) (r: R.t): R.t =
            let rec write r =
                if not (is_full b) && R.has_more r then
                    (put_character b (R.peek r);
                     write (R.advance r))
                else
                    r
            in
            write r
    end
end (* Buffer *)






module File_system:
sig
    type t
    val make: unit -> t
    val flush_all: t -> unit

    type in_file
    type out_file
    val stdin: in_file
    val stdout: out_file
    val stderr: out_file

    val flush: t -> out_file -> unit

    val open_for_read:  t -> string -> in_file io_result
    val open_for_write: t -> string -> out_file io_result
    val create:         t -> string -> out_file io_result
    val close_in:  t -> in_file  -> unit
    val close_out: t -> out_file -> unit

    module Read: functor (W:WRITABLE) ->
    sig
        val read_buffer: t -> in_file -> W.t -> W.t
        val read: t -> in_file -> W.t -> (W.t, W.t * Error.t) result
    end


    module Write: functor (R:READABLE) ->
    sig
        val write_buffer: t -> out_file -> R.t -> R.t
        val write: t -> out_file -> R.t -> R.t
    end
end
=
struct
    type file =
      | Read of Unix.file_descr * Buffer.t
      | Write of Unix.file_descr * Buffer.t
      (*| Free of int*)

    type t = {
        mutable standard_files: file array;
        buffers: file Pool.t
    }


    type in_file = int
    type out_file = int

    let buffer_size = 4096


    let unix_read
        (fd: Unix.file_descr)
        (b: Bytes.t)
        (ofs: int)
        (n: int)
        : int io_result
        =
        try
            Ok (
                let nread =
                Unix.read fd b ofs n
                in
                nread
            )
        with Unix.Unix_error (error, _, _) ->
            Error ( Error.make "NOCODE" (Unix.error_message error))



    let unix_write (fd:Unix.file_descr) (b:Bytes.t) (ofs:int) (n:int): int =
      try
        Unix.write fd b ofs n
      with Unix.Unix_error _ ->
        0


    let readable_file (fd: Unix.file_descr): file =
        Read (
            fd,
            Buffer.make
                buffer_size
                (unix_read fd)
                (fun _ _ _ -> assert false))


    let writable_file (fd:Unix.file_descr): file =
      Write (fd, Buffer.make
                   buffer_size
                   (fun _ _ _ -> assert false)
                   (unix_write fd))


    let make (): t =
        {
            standard_files =
                [| readable_file Unix.stdin;
                   writable_file Unix.stdout;
                   writable_file Unix.stderr |];

            buffers = Pool.make_empty ();
        }


    let iter (f: file -> unit) (fs: t): unit =
        f fs.standard_files.(0);
        f fs.standard_files.(1);
        f fs.standard_files.(2);
        Pool.iter f fs.buffers


    let get_file (fs: t) (fd: int): file =
        if fd < 3 then
            fs.standard_files.(fd)
        else (
            assert (Pool.has fs.buffers (fd - 3));
            Pool.elem fs.buffers (fd - 3)
        )


    let writable_buffer (fs: t) (fd: int): Buffer.t =
        match get_file fs fd with
        | Write (_,b) ->
            b
        | _ ->
            assert false




    let readable_buffer (fs:t) (fd:int): Buffer.t =
        match get_file fs fd with
        | Read (_,b) ->
            b
        | _ ->
            assert false



    let close (fs: t) (fd: int): unit =
        assert (3 <= fd); (* standard files cannot be closed. *)
        assert (Pool.has fs.buffers (fd - 3));
        let file = Pool.elem fs.buffers (fd - 3) in
        (
            match file with
            | Read (unix_fd, _) ->
                Unix.close unix_fd
            | Write (unix_fd, buffer) ->
                Buffer.flush buffer;
                Unix.close unix_fd
        );
        Pool.release fs.buffers (fd - 3)



    let open_for_read (fs: t) (path: string): in_file io_result =
        try
            let file =
                  readable_file (Unix.openfile path [Unix.O_RDONLY] 0o640)
            in
            let fd =
                Pool.occupy fs.buffers file
            in
            Ok (fd + 3)

        with Unix.Unix_error (error, _, _) ->
            Error (Error.make "NOCODE" (Unix.error_message error))


    let open_for_write (fs: t) (path: string): out_file io_result =
        try
            let file =
                  writable_file (Unix.openfile path [Unix.O_WRONLY] 0o640)
            in
            let fd =
                Pool.occupy fs.buffers file
            in
            Ok (fd + 3)

        with Unix.Unix_error (error, _, _) ->
            Error (Error.make "NOCODE" (Unix.error_message error))


    let create (fs: t) (path: string): out_file io_result =
        try
            let file =
                  writable_file (Unix.openfile path [Unix.O_CREAT] 0o640)
            in
            let fd =
                Pool.occupy fs.buffers file
            in
            Ok fd

        with Unix.Unix_error (error, _, _) ->
            Error (Error.make "NOCODE" (Unix.error_message error))


    let close_in (fs: t) (fd: in_file): unit =
        close fs fd


    let close_out (fs: t) (fd: out_file): unit =
        close fs fd





    let flush (fs:t) (fd:out_file) : unit =
        match get_file fs fd with
        | Write (_, b) ->
            Buffer.flush b
        | _ ->
            ()



    let flush_all (fs: t): unit =
        iter
            (function
                | Write (_, b) ->
                    Buffer.flush b
                | _ ->
                    ())
            fs



    let stdin: in_file = 0

    let stdout: out_file = 1

    let stderr: out_file = 2



    module Read (W: WRITABLE) =
    struct
        module BR = Buffer.Read (W)

        let read_buffer (fs: t) (fd: in_file) (w: W.t): W.t =
            BR.read (readable_buffer fs fd) w

        let read (fs: t) (fd: in_file) (w: W.t): (W.t, W.t * Error.t) result =
            let b =
                readable_buffer fs fd
            in
            let rec read w =
                let more =
                    W.needs_more w
                in
                if more && not (Buffer.is_empty b) then
                    read (BR.read b w)

                else if more then
                (
                    match Buffer.fill b with
                    | Error error ->
                        Error (w, error)
                    | Ok () ->
                        if Buffer.is_ok b then
                            read w
                        else
                            Ok (W.put_end w)
                )
                else
                    Ok w
            in
            read w
    end


    module Write (R: READABLE) =
    struct
        module BW = Buffer.Write (R)

        let write_buffer (fs:t) (fd:out_file) (r:R.t): R.t =
            BW.write (writable_buffer fs fd) r


        let write (fs:t) (fd:out_file) (r:R.t): R.t =
            let b = writable_buffer fs fd in
            let rec write r =
                let more = R.has_more r in
                if not (Buffer.is_full b) && more then
                  write @@ BW.write b r
                else if Buffer.is_ok b && more then
                  (Buffer.flush b;
                   write r)
                else
                  r
            in
            write r
    end
end (* File_system *)













module IO0: Make_io.SIG =
struct
    type program =
      | More of (File_system.t * (File_system.t -> program))
      | Done

    let rec execute_program: program -> unit = function
      | Done ->
         ()
      | More (fs, f) ->
         execute_program (f fs) (* [f fs] does one execution step and returns
                                   the remainder of the program. *)

    (* The same as an iteration
       ========================
    let execute_program (p:program): unit =
      let pref = ref p in
      while !pref <> Done do
        match !pref with
        | Done ->
           assert false (* cannot happen *)
        | More (fs, f) ->
           pref := f fs
      done
     *)

    type 'a cont = 'a -> File_system.t -> program

    module M =
      Monad.Of_sig_min(
          struct
            type 'a t = File_system.t -> 'a cont -> program

            let return (a:'a): 'a t =
              fun fs k -> k a fs

            let (>>=) (m:'a t) (f:'a -> 'b t): 'b t =
              fun fs k ->
              m fs (fun a fs -> More (fs, fun fs -> f a fs k))
          end)

    include M


    type in_file = File_system.in_file
    type out_file = File_system.out_file

    let stdin:  in_file = File_system.stdin
    let stdout: out_file = File_system.stdout
    let stderr: out_file = File_system.stderr


    let exit (code:int): 'a t =
      fun fs _ ->
      File_system.flush_all fs;
      Stdlib.exit code



    let execute (p:unit t): unit =
      let fs = File_system.make ()
      in
      let _ =
        try
          execute_program
            (p
               fs
               (fun () _ -> Done))
        with e ->
          File_system.flush_all fs;
          raise e
      in
      File_system.flush_all fs;
      Stdlib.exit 0

    let command_line: string array t =
      fun fs k  -> k Sys.argv fs

    let current_working_directory: string t =
      fun fs k -> k (Sys.getcwd ()) fs




    let path_separator: char =
      if Sys.win32 then '\\' else '/'

    let path_delimiter: char =
      if Sys.win32 then ';' else ':'




    let read_directory (path:string): string array option t =
      fun fs k ->
      k
        (try
           Some (Sys.readdir path)
         with _ ->
           None)
        fs






    let cli_prompt (prompt: string): string option t =
      fun fs k ->
      File_system.flush fs stdout;
      k
        (let res = LNoise.linenoise prompt in
         match res with
         | None ->
            res
         | Some str ->
            ignore (LNoise.history_add str);
            res)
        fs

    let cli_loop
        (state: 'a)
        (get_prompt: 'a -> string option)
        (next: 'a -> string -> 'a t)
        (stop: 'a -> 'a t)
        : 'a t
        =
        let rec loop state =
            match get_prompt state with
            | None ->
                return state
            | Some prompt_string ->
                cli_prompt prompt_string >>= function
                | None ->
                    stop state
                | Some line ->
                    next state line >>= loop
        in
        loop state







    let open_for_read (path: string): in_file io_result t =
        fun fs k ->
        k
            (File_system.open_for_read fs path)
            fs


    let open_for_write (path: string): out_file io_result t =
        fun fs k ->
        k
            (File_system.open_for_write fs path)
            fs


    let create (path: string): out_file io_result t =
        fun fs k ->
        k
            (File_system.create fs path)
            fs


    let close_in (fd: in_file): unit t =
        fun fs k ->
        k
            (File_system.close_in fs fd)
            fs


    let close_out (fd: out_file): unit t =
        fun fs k ->
        k
            (File_system.close_out fs fd)
            fs


    module Read (W: WRITABLE) =
    struct
        module FS_Read = File_system.Read (W)

        let read_buffer (fd:in_file) (w:W.t): W.t t =
            fun fs k ->
            k
                (FS_Read.read_buffer fs fd w)
                fs

        let read (fd:in_file) (w:W.t): (W.t, W.t * Error.t) result t =
            fun fs k ->
            k
                (FS_Read.read fs fd w)
                fs
    end


    module Write (R: READABLE) =
    struct
        module FS_Write = File_system.Write (R)

        let write_buffer (fd:out_file) (r:R.t): R.t t =
            fun fs k ->
            k
                (FS_Write.write_buffer fs fd r)
                fs

        let write (fd:out_file) (r:R.t): R.t t =
            fun fs k ->
            k
                (FS_Write.write fs fd r)
                fs
    end
end (* IO0 *)


module IO: Io.SIG = Make_io.Make (IO0)
