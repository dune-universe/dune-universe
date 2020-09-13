(* MUST_FAIL *)
open SCaml

let [@entry] default () () =
  let () = (failwith "fail" : unit) in
  [], ()
      
