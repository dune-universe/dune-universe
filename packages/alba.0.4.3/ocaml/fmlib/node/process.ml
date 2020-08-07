open Js_of_ocaml
open Js


class type stream =
object
    method fd: int readonly_prop
end


class type process =
  object
    method argv: (js_string t) js_array t readonly_prop
    method cwd: js_string t meth
    method exit: int -> 'a meth
    method nextTick: (unit -> unit) callback -> unit meth
    method stdin:  stream t readonly_prop
    method stdout: stream t readonly_prop
    method stderr: stream t readonly_prop
  end



let process: process t = Unsafe.eval_string "require('process')"



let next_tick (k:unit -> unit): unit =
  process##nextTick
    (wrap_callback k)



let exit (code:int): 'a  =
  Printf.printf "exiting with code %d\n" code;
  process##exit code



let command_line: string array =
  let arr = to_array process##.argv in
  let len = Array.length arr in
  assert (0 < len);
  Array.map to_string (Array.sub arr 1 (len - 1))



let current_working_directory (_:unit): string =
  to_string process##cwd
