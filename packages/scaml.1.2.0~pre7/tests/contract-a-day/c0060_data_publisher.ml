open SCaml

type storage = 
  { key : key
  ; counter : nat
  ; string : string
  }
    
let [@entry] get () storage =
  if Global.get_amount () < Tz 1.0 then failwith "too low";
  [], storage (* Unlike the original example, we cannot return the string *)

let [@entry] set (sign, s, n) storage =
  if storage.counter <> n then failwith "invalid counter";
  if not (Crypto.check_signature storage.key sign (Obj.pack (s, n))) then
    failwith "invalid signature";
  [],
  { storage 
    with counter= storage.counter +^ Nat 1
       ; string= s
  }
  

   
