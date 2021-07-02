(** Main entry point for our application. *)

let () =
  Dream.run ~debug:true
  @@ Dream.logger
  @@ Dream_encoding.compress
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.html "Hello World!") ]
  @@ Dream.not_found
