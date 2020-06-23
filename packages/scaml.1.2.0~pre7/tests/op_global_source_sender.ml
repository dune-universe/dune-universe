(* INPUT= ()
   STORAGE= (None : (address * address) option)
*)
open SCaml
let [@entry] main () _ =
  [],
  Some (Global.get_source (), Global.get_sender ())
