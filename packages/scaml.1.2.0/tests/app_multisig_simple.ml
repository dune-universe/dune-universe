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

let [@entry] main () () =

  let parameter = 
    { payload= { counter= Nat 42
               ; action= Transfer { amount= Tz 1.0
                                  ; dest= Contract.implicit_account (Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                                  }
               }
    ; sigs= [ Some (Signature "edsigu4chLHh7rDAUxHyifHYTJyuS8zybSSFQ5eSXydXD7PWtHXrpeS19ds3hA587p5JNjyJcZbLx8QtemuJBEFkLyzjAhTjjta"); None ]
    }
  in

  let storage = 
    { stored_counter= Nat 42
    ; threshold= Nat 1
    ; keys= [ Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"
            ; Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"
            ]
    }
  in
  let signature_target = Bytes "68656c6c6f" (* hello *) in
  let nsigs = Loop.left (fun (x, keys, sigs) ->

      match keys, sigs with
      | [], [] -> Right x
      | [], _::_ -> assert false
      | _::_, [] -> assert false
      | key::keys, Some sig_::sigs ->
          (* Checks signatures, fails if invalid *)
          assert (Crypto.check_signature key sig_ signature_target);
          Left (x, keys, sigs)
      | key::keys, None::sigs ->
          Left (x, keys, sigs)) 
      (Nat 100, storage.keys, parameter.sigs)
  in
  (* Assert that the threshold is less than or equal to the
     number of valid signatures.
  *)
  ([],
   assert (storage.threshold <= nsigs))

