open SCaml

(* chain_id is not comparable *)
let [@entry] main param storage =
  [], assert (
    Obj.pack (Global.get_chain_id ())
    = Obj.pack (Global.get_chain_id ()) 
               (* chain_id changes in networks 
                  Obj.pack (Chain_id "NetXdQprcVkpaWU") *)
  )

