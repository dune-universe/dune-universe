(* @type 'a option = Some of 'a | None
 *   with show,html,gmap,fmt,eval,stateful,foldl,foldr,compare,eq *)


(* @type ('a,'b,'c) triple = Triple of 'a*'b*'c
 *   with foldr,foldl,eq,compare,stateful,eval,gmap,html,fmt,show *)


(* @type ('a) t = Lazy of 'a
 *   with foldr,foldl,eq,compare,stateful,eval,gmap,html,fmt,show *)

let () = ();;

module A = struct
  @type 'a t = [ `A ] with stateful
end
(* module B = struct
 *   @type 'b t = [ `B ] with stateful
 * end
 *
 * module Z = struct
 *   @type 'x t = [ GT.int A.t | GT.string B.t ] with stateful
 *
 * end *)
