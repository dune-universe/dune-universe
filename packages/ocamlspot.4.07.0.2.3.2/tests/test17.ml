(*
let module G = Make ...
in
G.x

module M = struct
  type t = { .... hoge : G.t }
end
*)

let _ = 
  let module (* module G => *) G (* <= module G *) = 
    struct let (* G.x => *) x (* <= G.x *) = 1 end 

  in
  let module H = G (* ? module G *) in
  G.x (* ? G.x *) + H.x (* ? G.x *)

let y = 1
