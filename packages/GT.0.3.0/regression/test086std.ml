(* The same as test 805 but in camlp5 syntax *)

module T : sig
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful,html;;

  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,html,foldl,eq,compare,eval,stateful;;

  @type 'a t1 = 'a with show,gmap,html,foldl,eq,compare,eval,stateful;;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,html,foldl,eq,compare,eval,stateful;;

  @type 'a u1 = 'a GT.option with show,gmap,html,foldl,eq,compare,eval,stateful;;
  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;

  @type 'a u3 = {aa: GT.int; bb:GT.string} with show,gmap,foldl,eq,compare,eval;;
  @type ('a,'b) arrows = ('a -> 'b) GT.list  with show,gmap,foldl,eq,compare,eval;;
end = struct
  @type t2 = GT.int * GT.string with show,gmap,html,foldl,eq,compare,eval,stateful;;

  let () = ();;

  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,html,foldl,eq,compare,eval,stateful;;

  let () = ();;
  @type 'a t1 = 'a with show,gmap,html,foldl,eq,compare,eval,stateful;;

  let () = ();;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,html,foldl,eq,compare,eval,stateful;;

  let () = ();;

  @type 'a u1 = 'a GT.option with show,gmap,html,foldl,eq,compare,eval,stateful;;

  let () = ();;

  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;

  let () = ();;
  (* TODO: implement stateful for records *)

  @type 'a u3 = {aa: GT.int; bb:GT.string} with show,gmap,foldl,eq,compare,eval;;
  let () = ();;

  @type ('a,'b) arrows = ('a -> 'b) GT.list  with show,gmap,foldl,eq,compare,eval;;
end
