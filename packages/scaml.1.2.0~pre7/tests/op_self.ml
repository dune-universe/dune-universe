(* INPUT= ()
   STORAGE= Some (Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
*)
open SCaml
let [@entry] main () _ =
  [], Some (Contract.address Contract.self)
(* In test, it seems always  
   "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi"
*)
