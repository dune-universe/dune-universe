open Js_of_ocaml
open Js



class type error =
object
    method code: js_string t readonly_prop
    method message: js_string t readonly_prop
end


let make_io_error (js_error: error Js.t): Fmlib.Io.Error.t =
    Fmlib.Io.Error.make
        (to_string js_error##.code)
        (to_string js_error##.message)


class type stats =
    object
      method dev     : int readonly_prop
      method ino     : int readonly_prop
      method mode    : int readonly_prop
      method nlink   : int readonly_prop
      method uid     : int readonly_prop
      method gid     : int readonly_prop
      method rdev    : int readonly_prop
      method size    : float t readonly_prop
      method blksize : int readonly_prop
      method blocks  : int readonly_prop
      method atime   : date t readonly_prop
      method mtime   : date t readonly_prop
      method ctime   : date t readonly_prop
    end




class type fs =
  object
    method mkdir: js_string t -> (error t opt -> unit) callback -> unit meth
    method rmdir: js_string t -> (error t opt -> unit) callback -> unit meth

    method readdir:
             js_string t
             -> (error t opt -> js_string t js_array t -> unit) callback
             -> unit meth

    method unlink : js_string t -> (error t opt -> unit) callback -> unit meth

    method mkdirSync: js_string t -> unit meth
    method rmdirSync: js_string t -> unit meth

    method statSync: js_string t -> stats t meth

    method open_: js_string t  (* path *)
                  -> js_string t (* flags 'r', 'w', ... *)
                  -> (error t opt -> int -> unit) callback
                  -> unit meth
    method close: int -> (error t opt -> unit) callback -> unit meth

    method read:
             int -> Io_buffer.js_buffer
             -> int -> int -> int opt
             -> (error t opt -> int -> Io_buffer.js_buffer -> unit) callback
             -> unit meth

    method write:
             int -> Io_buffer.js_buffer -> int -> int
             -> (error t opt -> int -> Io_buffer.js_buffer -> unit) callback
             -> unit meth
  end

(*  Js_of_ocaml callback
    --------------------

    ('a->'b) callback = (unit,'a->'b) meth_callback

    val wrap_callback: ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
                       = ('a -> 'b) callback
 *)

let node_fs: fs t = Unsafe.eval_string "require('fs')"




let mkdir (path:string) (f:unit option -> unit): unit =
  node_fs##mkdir
    (string path)
    (wrap_callback @@
       fun eopt ->
       match Opt.to_option eopt with
       | Some _ ->
          f None
       | None ->
          f @@ Some () )



let rmdir (path:string) (f:unit option -> unit): unit =
  node_fs##rmdir
    (string path)
    (wrap_callback @@
       fun eopt ->
       match Opt.to_option eopt with
       | Some _ ->
          f None
       | None ->
          f @@ Some () )


let readdir (path:string) (f:string array option -> unit): unit =
  node_fs##readdir
    (string path)
    (wrap_callback @@
       fun eopt arr ->
       match Opt.to_option eopt with
       | Some _ ->
          f None
       | None ->
          f @@ Some (Array.map to_string (to_array arr)))


let stat (path:string): _ option =
  try
    let _ = node_fs##statSync (string path) in
    assert false
  with
  | Error _ ->
     None
  | _ ->
     assert false


let open_
    (path:string)
    (flags:string)
    (k: (int, Fmlib.Io.Error.t) result -> unit):
    unit
=
    node_fs##open_
        (string path)
        (string flags)
        (wrap_callback
            (fun error fd ->
                match Opt.to_option error with
                | None ->
                    k (Ok fd)
                | Some js_error ->
                    k (Error (make_io_error js_error))
            )
        )



let close (fd:int) (k: (unit, Fmlib.Io.Error.t) result -> unit): unit =
    node_fs##close
        fd
        (wrap_callback
            (fun error ->
                match Opt.to_option error with
                | None ->
                    k (Ok ())
                | Some js_error ->
                    k (Error (make_io_error js_error))
            )
        )


let read
    (fd: int)
    (buf: Io_buffer.t)
    (f: (int, Fmlib.Io.Error.t) result -> unit)
    : unit
    =
    assert (Io_buffer.is_empty buf);
    node_fs##read
        fd
        (Io_buffer.js_buffer buf)
        0
        (Io_buffer.capacity buf)
        null (* no explicit position, use the current file position *)
        (wrap_callback (
            fun error n _ ->
                match Opt.to_option error with
                | None ->
                    Io_buffer.reset buf;
                    Io_buffer.set_write_pointer buf n;
                    f (Ok n)
                | Some js_error ->
                    f (Error (make_io_error js_error))
        ))



let write (fd:int) (buf:Io_buffer.t) (f: int -> unit): unit =
  assert (Io_buffer.read_pointer buf = 0);
  let len = Io_buffer.length buf
  and offset = Io_buffer.read_pointer buf
  in
  node_fs##write
    fd
    (Io_buffer.js_buffer buf)
    offset
    len
    (wrap_callback
     @@ fun _ n _ ->
        assert (n <= len);
        if n = len then
          Io_buffer.reset buf
        else
          Io_buffer.set_read_pointer buf (offset + n);
        f n)
