(* 
INPUT=Int 42 
*)
open SCaml

type event =
  | Foo of int

let [@entry] default i () =
  (* XXX We need self' : string -> 'a Contract.t *)
  [ Operation.transfer_tokens (Foo i) (Tz 0.) (Option.get (Contract.(contract' (address self) "event"))) ], ()

let [@entry] event (_:int) () = [], ()
                                    
