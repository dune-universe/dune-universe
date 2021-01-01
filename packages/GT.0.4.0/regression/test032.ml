(* Should be merged in another test *)
open GT

@type test = string with stateful,eval,compare,eq,foldl, foldr,gmap,fmt, html,show

let _ =
  Printf.printf "%s\n" (transform(test) (new @test[show]) () "abc")
