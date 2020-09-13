(*
  STORAGE=([] : address list)
  ENTRY=main
*)

open SCaml
    
let [@entry name="hello"] init () ads = [], ads

let [@entry] main () ads =
  let a = Contract.address Contract.self in
  [], [a;
       Contract.address (Option.get (Contract.contract' a "hello"))
      ]

  
  
