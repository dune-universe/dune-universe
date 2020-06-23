(* MUST_FAIL *)
open SCaml

let [@entry] default () () =
  let () = if true then failwith "fail" in
  [], ()
      
