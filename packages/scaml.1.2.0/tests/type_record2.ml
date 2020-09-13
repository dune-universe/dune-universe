[@@@SCaml iml_optimization=false]
open SCaml

type t = 
  { title : string
  ; candidates : (string, int) map
  ; voters : address set
  ; beginning_time : timestamp
  ; finish_time : timestamp
  }

let f () =
  let t =
    { title= "hello"
    ; finish_time= Global.get_now ()
    ; beginning_time= Global.get_now ()
    ; voters= Set []
    ; candidates= Map [ ("hello", Int 0); ("world", Int 1) ]
    }
  in
  let t' = { t with candidates= Map []; voters= Set [] } in
  let finish_time = t'.finish_time in
  true

let [@entry] main x y =
  [],
  assert (f ())
