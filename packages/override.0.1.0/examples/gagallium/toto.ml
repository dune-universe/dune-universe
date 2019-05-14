(* Example taken from Gabriel Scherer's post about Overriding submodules
   on Gagallium blog.
   http://gallium.inria.fr/blog/overriding-submodules/
*)

let x = 1
module Tata = struct
  let y = 2
end
module Titi = struct
  let z = 3
end
