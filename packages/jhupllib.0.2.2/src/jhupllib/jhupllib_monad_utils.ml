open Batteries;;
open Interfaces;;

module type Utils =
sig
  type 'a m
  
  val sequence : 'a m Enum.t -> 'a Enum.t m
  val mapM : ('a -> 'b m) -> 'a Enum.t -> 'b Enum.t m
end;;

module Make(M : Monad) : Utils with type 'a m = 'a M.m =
struct
  type 'a m = 'a M.m;;
  let rec sequence xms =
    let open M in
    match Enum.get xms with
    | None -> return @@ Enum.empty ()
    | Some xm ->
      let%bind x = xm in
      let%bind xs = sequence xms in
      return @@ Enum.append (Enum.singleton x) xs
  ;;
  let mapM f xs = sequence (Enum.map f xs);;
end;;
