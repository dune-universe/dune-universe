external c__init : unit -> unit = "caml__init"
external c__radamsa : string -> int -> string = "caml__radamsa"

let radamsa ?seed:(seed=0) input =
  c__radamsa input seed

let () = 
  c__init ();
  Random.self_init ()
