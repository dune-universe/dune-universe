(* open Core *)
open Async

module Stdio_attr = struct

  type t = {
    attr_in : Unix.Terminal_io.t;
    attr_out: Unix.Terminal_io.t;
  }

  let setattr_stdout ~attr_out =
    Unix.Terminal_io.tcsetattr attr_out
      ~mode:Unix.Terminal_io.TCSAFLUSH (Unix.Fd.stdout ())

  let setattr_stdin ~attr_in =
    Unix.Terminal_io.tcsetattr attr_in
      ~mode:Unix.Terminal_io.TCSADRAIN (Unix.Fd.stdin ())

  let get () =
    Unix.Terminal_io.tcgetattr (Unix.Fd.stdin ()) >>= fun attr_in ->
      Unix.Terminal_io.tcgetattr (Unix.Fd.stdout ()) >>= fun attr_out ->
        return {
          attr_in = attr_in;
          attr_out = attr_out;
        }

  let set t =
    setattr_stdin ~attr_in:t.attr_in >>= fun () ->
      setattr_stdout ~attr_out:t.attr_out

  let mod_cbreak t = {
    attr_in =
      {
        t.attr_in with Core.Unix.Terminal_io.
          c_echo = false;
          c_icanon = false;
          c_vmin = 1;
          c_ixon = false;
      };
    attr_out =
      {
        t.attr_out with Core.Unix.Terminal_io.
          c_echo = false;
          c_icanon = false;
          c_vmin = 1;
      }
  }

end

let with_cbreak ~f =
  Stdio_attr.get() >>= fun orig ->
    let cbreak = Stdio_attr.mod_cbreak orig in
    let restore () = Stdio_attr.set orig in
    let set_cbreak () = Stdio_attr.set cbreak in
    Monitor.protect
      (fun () -> set_cbreak() >>= f)
      ~finally:restore
