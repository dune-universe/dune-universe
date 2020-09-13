(*
   INPUT= ()
   STORAGE= (None : bytes option)
*)
open SCaml

type storage = 
  { stored_counter : nat
  ; threshold : nat
  ; keys : key list
  }

type parameter =
  { payload : payload
  ; sigs : signature option list 
  }

and payload = 
  { counter : nat
  ; action : action 
  }

and action =
  | Transfer of transfer
  | Delegate of key_hash option
  | Change_keys of change_keys

and transfer = 
  { amount : tz 
  ; dest : unit contract 
  }                    

and change_keys = 
  { threshold : nat 
  ; keys : key list 
  }

(*
  (Some 0x0507070707002a050507070080897a0a00000016000002298c03ed7d454a101eb7022bc95f7e5f41ac7807070a00000016011d23c1d3d2f8a4ea5e8784b8f7ecf2ad304c0fe6000a000000047a06a770)
*)

let main parameter (storage : bytes option) : operations * bytes option =

  (* pair the payload with the current contract address, to ensure signatures
     can't be replayed accross different contracts if a key is reused. *)
  let signature_target = 
    Obj.pack ( parameter.payload
             , Contract.address Contract.self
             , Global.get_chain_id ()
             )
  in
  [], Some signature_target

let parameter = 
  { payload= { counter= Nat 42
             ; action= Transfer { amount= Tz 1.0
                                ; dest= Contract.implicit_account (Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                                }
             }
  ; sigs= [ Some (Signature "edsigu4chLHh7rDAUxHyifHYTJyuS8zybSSFQ5eSXydXD7PWtHXrpeS19ds3hA587p5JNjyJcZbLx8QtemuJBEFkLyzjAhTjjta"); None ]
  }

(*
let storage = 
  { stored_counter= Nat 42
  ; threshold= Nat 1
  ; keys= [ Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"
          ; Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"
          ]
  }
*)
  
let [@entry] test () _ =
  let ops, res = main parameter None in
  ops, res

