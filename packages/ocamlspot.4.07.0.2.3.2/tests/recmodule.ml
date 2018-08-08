module rec (* M => *) M (* <= M *) : sig val f : unit -> unit end = struct

  include N (* ? N *)

  let (* M.f => *) f (* <= M.f *) () = N.g (* ? N.g *) ()

end and (* N => *) N (* <= N *) : sig val g : unit -> unit end = struct

  include M (* ? M *) 

  let (* N.g => *) g (* <= N.g *) () = M.f (* ? M.f *) ()

end 
