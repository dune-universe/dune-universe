open Js_of_ocaml
open Js


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

type file_descriptor = int
type offset = int
type length = int

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
             file_descriptor -> Io_buffer.js_buffer
             -> offset -> length -> int opt
             -> (error t opt -> int -> Io_buffer.js_buffer -> unit) callback
             -> unit meth
    method write:
             file_descriptor -> Io_buffer.js_buffer -> offset -> length
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


let open_ (path:string) (flags:string) (k:int option -> unit): unit =
  node_fs##open_
    (string path)
    (string flags)
    (wrap_callback
       (fun err fd ->
         if Opt.test err then
           k None
         else
           k @@ Some fd))

let close (fd:int) (k:unit option -> unit): unit =
  node_fs##close
    fd
    (wrap_callback
       (fun err ->
         if Opt.test err then
           k None
         else
           k @@ Some ()))

let read (fd:int) (buf:Io_buffer.t) (f: int -> unit): unit =
  node_fs##read
    fd
    (Io_buffer.js_buffer buf)
    0
    (Io_buffer.capacity buf)
    Opt.empty  (* no explicit position, use the current file position *)
    (wrap_callback
       (fun _ n _ ->
         Io_buffer.reset buf;
         Io_buffer.set_write_pointer buf n;
         f n))

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
