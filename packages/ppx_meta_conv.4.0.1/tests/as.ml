open Ocaml_conv.Default

module M : sig
  type connection = [ `Following          
                    | `Follwing_requested 
                    | `Followed_by        
                    | `None               
                    ] [@@deriving conv{ocaml}]

  type v [@@deriving conv{ocaml}]
end = struct
  type connection = [ `Following          [@conv.as following]
                    | `Follwing_requested [@conv.as "following_requested"]
                    | `Followed_by        [@conv.as followed_by]
                    | `None               [@conv.as {ocaml="none"}]
                    ] [@@deriving conv{ocaml}]

  type v = Foo of int [@conv.as foo] [@@deriving conv{ocaml}]

end
