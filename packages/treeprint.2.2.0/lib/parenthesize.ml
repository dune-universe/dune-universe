open Spotlib.Spot

(** Primitive operators *)

type assoc = Left | Right | Noassoc
type level = float
type 'a t = assoc -> level -> 'a (* monadic *)
type 'a t_ = 'a t

include Monad.Make(struct 
  type 'a t = 'a t_
  let bind at f = fun a l -> f (at a l) a l
  let return a = fun _ _ -> a
end)

module Make(A : sig
  type t
end) = struct

  type ppr = A.t t

  type parens = A.t -> A.t
    
  let left    : 'a t -> 'a t          = fun p _a l -> p Left    l
  let right   : 'a t -> 'a t          = fun p _a l -> p Right   l
  let noassoc : 'a t -> 'a t          = fun p _a l -> p Noassoc l
  let level : level -> 'a t -> 'a t = fun l p a _l -> p a l
  let reset   : 'a t -> 'a t = fun t -> noassoc (level 0.0 t)
  
  let need_parens : assoc -> level -> bool t = fun assoc lev out_pos out_lev ->
    match compare out_lev lev with
    | 1 -> true
    | -1 -> false
    | 0 ->
        begin match out_pos, assoc with
        | Left, Left -> false
        | Right, Right -> false
        | _ -> true
        end
    | _ -> assert false
  
  let maybe_parens : parens -> assoc -> level -> ppr -> ppr = fun parens assoc lev t ->
    need_parens assoc lev >>= function
      | true  -> fmap parens & reset t
      | false -> t

  let atom a = return a

  let binop parens build assoc lev l r = 
    maybe_parens parens assoc lev
    & level lev
    & left l >>= fun l ->
      right r >>= fun r ->
      return (build l r)

  (* a1 sep a2 sep .. sep an *)          
  let list parens build lev xs =
    maybe_parens parens Noassoc lev & fmap build & mapM (level lev) xs
  
  let prefix parens build lev x =
    maybe_parens parens Right lev & fmap build & level lev & right x
  
  let postfix parens build lev x =
    maybe_parens parens Left lev & fmap build & level lev & left x
  
  let parens ps x = fmap ps & level (-1.0) x
end

