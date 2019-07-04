(* Example taken from Gabriel Scherer's post about Overriding submodules
   on Gagallium blog.
   http://gallium.inria.fr/blog/overriding-submodules/
*)

module%override Toto = struct
  module%override Tata = struct
    let y = "2"
  end
end

let test () =
  assert (Toto.x = 1);
  assert (Toto.Tata.y = "2")

let () = test ()
