open Fmlib

open Module_types


module Buffer:
sig
  type t
  val make: int ->
            (Bytes.t -> int -> int -> int) ->
            (Bytes.t -> int -> int -> int) ->
            t
  val is_ok: t -> bool
  val is_full: t -> bool
  val flush: t -> unit

  module Read: functor (W:WRITABLE) ->
               sig
               end
  module Write: functor (R:READABLE) ->
               sig
                 val write: t -> R.t -> R.t
               end
end =
  struct
    type t = {
        mutable rp: int; (* The content of the buffer is between the read and
                            the write pointer. *)
        mutable wp: int;
        mutable flag: bool; (* ok flag, set to false if (a) refilling a buffer
                               adds 0 bytes, (b) flushing a nonempty buffer
                               does not write anything to the filesystem. *)
        read:  Bytes.t -> int -> int -> int; (* refill function *)
        write: Bytes.t -> int -> int -> int; (* flush function *)
        bytes: Bytes.t}

    let make (n:int)
          (read:Bytes.t -> int -> int -> int)
          (write:Bytes.t -> int -> int -> int)
        : t =
      assert (n > 0);
      assert (n <= Sys.max_string_length);
      {rp = 0; wp = 0; bytes = Bytes.create n; flag = true; read; write}

    let is_ok (b:t): bool =
      b.flag

    let is_empty (b:t): bool =
      b.rp = b.wp

    let reset (b:t): unit =
      b.rp <- 0;
      b.wp <- 0

    let is_full (b:t): bool =
      b.wp = Bytes.length b.bytes

    let flush (b:t): unit =
      if not (is_empty b) && is_ok b then
        let n = b.write b.bytes b.rp (b.wp - b.rp) in
        if n = 0 then
          b.flag <- false
        else
          (* BUG: what if n > 0 && n <> b.wp - b.rp ?? Only a part of the
             buffer has been written!! *)
          reset b

    (*let get (b:t): char =
      assert (not (is_empty b));
      let c = Bytes.get b.bytes b.rp in
      b.rp <- b.rp + 1;
      c*)

    let putc (b:t) (c:char): unit =
      assert (is_ok b);
      if is_full b then
        flush b;
      assert (not (is_full b));
      Bytes.set b.bytes b.wp c;
      b.wp <- b.wp + 1

    module Read (W:WRITABLE) =
      struct
        (*let read (b:t) (w:W.t): W.t =
          let rec read w =
            if not (is_empty b) && W.needs_more w then
              read (W.putc w (get b))
            else
              w
          in
          read w*)
      end

    module Write (R:READABLE) =
      struct
        let write (b:t) (r:R.t): R.t =
          let rec write r =
            if not (is_full b) && R.has_more r then
              (putc b (R.peek r);
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
  (*val getc: t -> in_file -> char option
  val getline: t -> in_file -> string option
  val putc: t -> out_file -> char -> unit
  val open_for_read: t -> string -> in_file option
  val open_for_write: t -> string -> out_file option
  val create: t -> string -> out_file option
  val close_in:   t -> in_file -> unit
  val close_out:  t -> out_file -> unit*)
  val flush: t -> out_file -> unit

  module Read: functor (W:WRITABLE) ->
               sig
                 val read_buffer: t -> in_file -> W.t -> W.t
                 val read: t -> in_file -> W.t -> W.t
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

    type t = {mutable files: file array;
              mutable first_free: int;
              line_buf: Buffer.t}

    type in_file = int
    type out_file = int

    let buffer_size = 4096

    let unix_read (fd:Unix.file_descr) (b:Bytes.t) (ofs:int) (n:int): int =
      try
        Unix.read fd b ofs n
      with Unix.Unix_error _ ->
        0

    let unix_write (fd:Unix.file_descr) (b:Bytes.t) (ofs:int) (n:int): int =
      try
        Unix.write fd b ofs n
      with Unix.Unix_error _ ->
        0

    let readable_file (fd:Unix.file_descr): file =
      Read (fd, Buffer.make
                  buffer_size
                  (unix_read fd)
                  (fun _ _ _ -> assert false))

    let writable_file (fd:Unix.file_descr): file =
      Write (fd, Buffer.make
                   buffer_size
                   (fun _ _ _ -> assert false)
                   (unix_write fd))

    let make (): t =
      {first_free = -1;
       files =
         [| readable_file Unix.stdin;
            writable_file Unix.stdout;
            writable_file Unix.stderr |];
       line_buf =
         let fr _ _ _ = assert false in
         let fw _ _ _ = assert false in
         Buffer.make 200 fr fw
      }


    (*let put_to_files (fs:t) (file:file): int option =
      if fs.first_free >= 2 then
        begin
          let fd = fs.first_free in
          match fs.files.(fd) with
          | Free n ->
             fs.first_free <- n;
             fs.files.(fd) <- file;
             Some fd
          | _ ->
             assert false (* Cannot happen, must be free! *)
        end
      else
        begin
          let nfiles = Array.length fs.files in
          let files = Array.make (nfiles + 1) file in
          Array.blit fs.files 0 files 0 nfiles;
          fs.files <- files;
          Some nfiles
        end*)

    let writable_buffer (fs:t) (fd:int): Buffer.t =
      assert (fd < Array.length fs.files);
      match fs.files.(fd) with
      | Write (_,b) ->
         b
      | _ ->
         assert false

    (*let readable_buffer (fs:t) (fd:int): Buffer.t =
      assert (fd < Array.length fs.files);
      match fs.files.(fd) with
      | Read (_,b) ->
         b
      | _ ->
         assert false*)


    (*let getc (fs:t) (fd:in_file): char option =
      Buffer.getc (readable_buffer fs fd)

    let putc (fs:t) (fd:out_file) (c:char): unit =
      Buffer.putc (writable_buffer fs fd) c


    let open_for_read (fs:t) (path:string): in_file option =
      try
        put_to_files
          fs
          (readable_file (Unix.openfile path [Unix.O_RDONLY] 0o640))
      with Unix.Unix_error _ ->
        None

    let open_for_write (fs:t) (path:string): out_file option =
      try
        put_to_files
          fs
          (writable_file (Unix.openfile path [Unix.O_WRONLY] 0o640))
      with Unix.Unix_error _ ->
        None

    let create (fs:t) (path:string): out_file option =
      try
        put_to_files
          fs
          (writable_file (Unix.openfile path [Unix.O_CREAT] 0o640))
      with Unix.Unix_error _ ->
        None*)

    (*let unix_file_descriptor (fs:t) (fd:int): Unix.file_descr =
      assert (fd < Array.length fs.files);
      match fs.files.(fd) with
      | Read (fd,_) -> fd
      | Write (fd,_) -> fd*)


    (*let close_file (fs:t) (fd:int): unit =
      assert (fd < Array.length fs.files);
      match fs.files.(fd) with
      | Read (fd,_) ->
         Unix.close fd
      | Write (fd,b) ->
         Buffer.flush b;
         Unix.close fd
      | Free _ ->
         ()

    let close_in (fs:t) (fd:in_file): unit =
      close_file fs fd

    let close_out (fs:t) (fd:out_file): unit =
      close_file fs fd*)


    let flush (fs:t) (fd:out_file) : unit =
      assert (fd < Array.length fs.files);
      match fs.files.(fd) with
      | Write (_,b) ->
         Buffer.flush b
      | _ ->
           ()

    let flush_all (fs:t): unit =
      for i = 0 to Array.length fs.files - 1 do
        flush fs i
      done

    let stdin: in_file = 0

    let stdout: out_file = 1

    let stderr: out_file = 2

    (*let stdin_buffer (fs:t): Buffer.t =
      readable_buffer fs stdin


    let stdout_buffer (fs:t): Buffer.t =
      writable_buffer fs stdout


    let stderr_buffer (fs:t): Buffer.t =
      writable_buffer fs stderr


    let getline (fs:t) (fd:in_file): string option =
      assert (fd < Array.length fs.files);
      let b = readable_buffer fs fd in
      Buffer.reset fs.line_buf;
      let content () = Some (Buffer.content fs.line_buf) in
      let len = Buffer.size fs.line_buf
      in
      let rec read (i:int): string option =
        if i = len then
          content ()
        else
          begin
            match Buffer.getc b with
            | None ->
               if i = 0 then
                 None
               else
                 content ()
            | Some c ->
               if c = '\n' then
                 content ()
               else
                 begin
                   Buffer.putc fs.line_buf c;
                   read (i+1)
                 end
          end
      in
      read 0
     *)

    module Read (W:WRITABLE) =
      struct
        let read_buffer (_:t) (_:in_file) (_:W.t): W.t =
          assert false
        let read (_:t) (_:in_file) (_:W.t): W.t =
          assert false
      end

    module Write (R:READABLE) =
      struct
        module BW = Buffer.Write (R)

        let write_buffer (fs:t) (fd:out_file) (r:R.t): R.t =
          assert (fd < Array.length fs.files);
          BW.write (writable_buffer fs fd) r

        let write (fs:t) (fd:out_file) (r:R.t): R.t =
          assert (fd < Array.length fs.files);
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







    module Read (W:WRITABLE) =
      struct
        module FS_Read = File_system.Read (W)

        let read_buffer (fd:in_file) (w:W.t): W.t t =
          fun fs k -> k (FS_Read.read_buffer fs fd w) fs

        let read (fd:in_file) (w:W.t): W.t t =
          fun fs k -> k (FS_Read.read fs fd w) fs
      end


    module Write (R:READABLE) =
      struct
        module FS_Write = File_system.Write (R)

        let write_buffer (fd:out_file) (r:R.t): R.t t =
          fun fs k -> k (FS_Write.write_buffer fs fd r) fs

        let write (fd:out_file) (r:R.t): R.t t =
          fun fs k -> k (FS_Write.write fs fd r) fs
      end
  end


module IO: Io.SIG = Make_io.Make (IO0)
