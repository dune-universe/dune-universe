(* Example taken from Gabriel Scherer's post about Overriding submodules
   on Gagallium blog.
   http://gallium.inria.fr/blog/overriding-submodules/
*)

module%override Toto : sig
  module%override Tata : sig
    val y : string
  end
end
