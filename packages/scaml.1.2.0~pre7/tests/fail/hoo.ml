(* hoo.ml *)
open SCaml
type t = 
  { name   : string
  ; age    : nat
  ; salary : tz
  }

and u = 
   | Foo of int * tz * string
   | Bar
   | Boo of t list
   | Far

let v = Boo [ { name= "jon"; age= Nat 18; salary= Tz 10000.0 }
            ; { name= "dow"; age= Nat 50; salary= Tz 1.0 }
            ]
