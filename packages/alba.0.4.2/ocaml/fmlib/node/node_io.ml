open Fmlib
open Module_types
open Common



module type BUFFER =
  sig
    type t
    val alloc: int -> t
  end




module type BUFFERS =
  functor (B:BUFFER) ->
  sig
    type t
    val make: int -> t
    val is_open_write: t -> int -> bool
    val capacity: t -> int
    val occupy_readable: t -> int -> int
    val occupy_writable: t -> int -> int
    val writable_file: t -> int -> int * B.t
  end



module Buffers: BUFFERS =
  functor (B:BUFFER) ->
  struct
    type file =
      | Read  of int * B.t
      | Write of int * B.t


    type t = {size: int;
              files: file Pool.t}

    let make (size:int): t =
      {size;
       files = Pool.make_empty ()}

    let is_open_write (s:t) (i:int): bool =
      Pool.has s.files i
      && match Pool.elem s.files i with
         | Read _  -> false
         | Write _ -> true

    let capacity (s:t): int =
      Pool.capacity s.files

    let occupy (s:t) (f:B.t -> file): int =
      let buf = B.alloc s.size in
      Pool.occupy s.files (f buf)

    let occupy_readable (s:t) (fd:int): int =
      occupy s (fun b -> Read(fd,b))

    let occupy_writable (s:t) (fd:int): int =
      occupy s (fun b -> Write(fd,b))

    let writable_file (s:t) (i:int): int * B.t =
      match Pool.elem s.files i with
      | Read _ -> assert false (* Illegal call! *)
      | Write (fd,b) -> fd,b
  end (* Buffers *)




module World =
  struct
    include Buffers (Io_buffer)

    let buffer_size = 4096 (* 16K: 16384, 32K: 32768, 64K: 65536, 2000 loc ~ 56K,
                              3000 loc ~ 85K *)

    let stdin:  int = 0
    let stdout: int = 1
    let stderr: int = 2

    let init () =
      let w = make buffer_size in
      let i0 = occupy_readable w stdin in
      assert (i0 = stdin);
      let i1 = occupy_writable w stdout in
      assert (i1 = stdout);
      let i2 = occupy_writable w stderr in
      assert (i2 = stderr);
      w
  end





module IO0: Make_io.SIG =
  struct
    type in_file = int
    type out_file = int

    let stdin:  in_file =  0
    let stdout: out_file = 1
    let stderr: out_file = 2

    type program =
      | More of (World.t * (World.t -> program))
      | Done

    module M =
      Monad.Of_sig_min(
          struct
            type 'a t = World.t -> ('a -> World.t -> program) -> program

            let return (a:'a): 'a t =
              fun w k ->
              More (w, k a)

            let (>>=) (m:'a t) (f:'a -> 'b t): 'b t =
              fun w k ->
              More (w,
                    fun w ->
                    m w
                      (fun a w -> f a w k))
          end)

    include M


    let rec execute_program (p:program): unit =
      match p with
      | Done ->
         ()
      | More (w,f) ->
         execute_program (f w)


    let world: World.t t =
      fun w k -> k w w


    let write1
          (fd:int)
          (buf:Io_buffer.t)
          (w:World.t)
          (k:int -> World.t -> program): program =
      File_system.write
        fd
        buf
        (fun n -> execute_program @@  k n w);
      Done


    let flush_buffer (fd:int) (buf:Io_buffer.t): unit option t =
      let rec write () =
        if Io_buffer.is_empty buf then
          return (Some ())
        else
          write1 fd buf >>= fun n ->
          if n = 0 then
            return None
          else
            write ()
      in
      write ()


    let flush (fd:int): unit option t =
      fun w k ->
      assert (World.is_open_write w fd);
      let fd,buf = World.writable_file w fd in
      flush_buffer fd buf w k



    let writable_file (fd:out_file): (int * Io_buffer.t) t =
      fun w k ->
      assert (World.is_open_write w fd);
      let fd,buf = World.writable_file w fd in
      k (fd,buf) w


    let write (fd:out_file): unit option t =
      writable_file fd >>= fun (fd,buf) ->
      flush_buffer fd buf


    let flush_all: unit t =
      world >>= fun w ->
      let rec flush i =
        if i = World.capacity w then
          return ()

        else if World.is_open_write w i then
          write i >>= fun _ ->
          flush (i + 1)
        else
          flush (i + 1)
      in
      flush 0


    let make_program (m:unit t): program =
      (m >>= fun _ -> flush_all)
        (World.init ())
        (fun _ _ -> Done)


    let execute (m:unit t): unit =
      execute_program @@ make_program m


    let exit (code:int): 'a t =
      flush_all >>= fun _ -> Process.exit code

    let command_line: string array t =
      return Process.command_line

    let current_working_directory: string t =
      return (Process.current_working_directory ())



    let path_separator: char =
      Path.separator

    let path_delimiter: char =
      Path.delimiter



    let read_directory (path:string): string array option t =
      fun w k ->
      File_system.readdir
        path
        (fun arr ->
          Printf.printf "read_directory %s\n" path;
          execute_program @@ k arr w);
      Done









    let cli_loop
        (s: 'a)
        (get_prompt: 'a -> string option)
        (next: 'a -> string -> 'a t)
        (stop: 'a -> 'a t)
        : 'a t
        =
        let rl = Readline.create_interface ()
        in
        let rec loop s: 'a t =
          flush stdout >>= fun _ ->
          fun w k ->
          match get_prompt s with
          | None ->
             Readline.close rl;
             execute_program @@ stop s w k;
             Done
          | Some prompt_str ->
             Readline.question
               rl
               prompt_str
               (fun answer ->
                 execute_program @@
                   (next s answer >>= loop) w k)
               (fun () -> execute_program @@ stop s w k);
             Done
        in
        loop s





    module Read (W:WRITABLE) =
      struct
        let read_buffer (_:in_file) (_:W.t): W.t t =
          assert false

        let read (_:in_file) (_:W.t): W.t t =
          assert false
      end


    module Write (R:READABLE) =
      struct
        let rec extract_readable (n_max:int) (r:R.t): string =
          if n_max <> 0 && R.has_more r then
            String.one (R.peek r) ^ extract_readable (n_max - 1) (R.advance r)
          else
            ""
        let _ = extract_readable (* might be used for debugging *)

        module BW = Io_buffer.Write (R)
        let write_buffer (fd:out_file) (r:R.t): R.t t =
          writable_file fd >>= fun (_,buf) ->
          return @@ BW.write buf r

        let write (fd:out_file) (r:R.t): R.t t =
          writable_file fd >>= fun (fd,buf) ->
          let rec write i r =
            assert (i < 100);
            if R.has_more r then
              if Io_buffer.is_full buf then
                flush_buffer fd buf >>= function
                | None ->
                   return r
                | Some () ->
                   assert (not (Io_buffer.is_full buf));
                   write (i+1) r
              else
                return @@ BW.write buf r >>= write (i+1)
            else
              return r
          in
          write 0 r
      end
  end


module IO: Io.SIG = Make_io.Make (IO0)
