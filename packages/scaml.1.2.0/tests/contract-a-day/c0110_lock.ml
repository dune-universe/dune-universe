open SCaml

type storage =
  { until : Timestamp.t
  ; amount : tz
  ; destination : unit Contract.t
  }
    
let main () storage =
  let now = Global.get_now () in
  if now < storage.until then failwith "too early to pay out";
  [ Operation.transfer_tokens () storage.amount storage.destination ],
  storage
  
  

