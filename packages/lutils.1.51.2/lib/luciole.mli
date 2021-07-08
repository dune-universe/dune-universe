
(* Var name and C type *)
type vn_ct = string * string  

(* 
   [gen_stubs lutin str inputs outputs]
 generates a C file that will be compiled into a .dro file.

   - str a just a string used to invent file names
   - inputs is the list or the luciole process var names and types
   - outputs ditto for outputs
   
if the optional argument boot is set to false (it is true by default), inputs are 
read before output are produced

*)

val gen_stubs : ?boot: bool -> string -> vn_ct list -> vn_ct list -> unit

