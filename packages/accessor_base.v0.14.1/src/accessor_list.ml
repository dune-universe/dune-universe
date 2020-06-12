open! Base
open! Import

let reversed = [%accessor Accessor.isomorphism ~get:List.rev ~construct:List.rev]

(* The reasoning for naming the following functions as [nil] and [cons] is that if [list]
   was defined as a normal ADT, it would probably be:

   {[
     type 'a t =
       | Nil
       | Cons of 'a * 'a t
   ]}

   The convention for [variant] accessors is to name them the same way we would name
   native OCaml constructors. *)

let nil =
  [%accessor
    Accessor.variant
      ~match_:(function
        | [] -> First ()
        | _ :: _ as list -> Second list)
      ~construct:(fun () -> [])]
;;

let cons =
  [%accessor
    Accessor.variant
      ~match_:(function
        | [] -> Second []
        | x :: xs -> First (x, xs))
      ~construct:(fun (x, xs) -> x :: xs)]
;;

let split_n i =
  Accessor.isomorphism
    ~get:(fun xs -> List.split_n xs i)
    ~construct:(fun (prefix, suffix) -> prefix @ suffix)
;;

let nth i = split_n i @> Accessor_tuple2.snd @> cons @> Accessor_tuple2.fst

let prefixed prefix ~equal =
  [%accessor
    Accessor.variant
      ~match_:(fun xs ->
        let p, s = List.split_n xs (List.length prefix) in
        if List.equal equal prefix p then First s else Second xs)
      ~construct:(fun s -> prefix @ s)]
;;

let suffixed suffix ~equal = reversed @> prefixed (List.rev suffix) ~equal @> reversed

(* All this is so that [each] will not overflow the stack. The traversal visits the
   elements in a balanced tree instead of in a list, that way no matter what applicative
   is used the stack usage is limited to the height of the tree instead of the length of
   the list.

   It might be possible to change the [Many] applicative to do this regardless of the data
   structure, but that would be a lot more work and complicated types, and I kind of doubt
   there are that many data structures that need this. If it comes up again, we should
   consider generalizing. *)
module Tree : sig
  type 'a t

  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val each : ('i -> 'a -> 'b, 'i -> 'a t -> 'b t, [< many ]) Accessor.t
end = struct
  module Bounded_length_list : sig
    type 'a t = private 'a list

    val empty : 'a t
    val of_list : 'a list -> 'a t * 'a list
    val traverse : 'a t -> ('b t, 'a, 'b) Accessor.Many.t
  end = struct
    type 'a t = 'a list

    let empty = []
    let max_len = 1_000
    let of_list xs = List.split_n xs max_len

    let rec traverse xs =
      let open Accessor.Many.Let_syntax in
      match xs with
      | [] -> return []
      | hd :: tl ->
        let%map_open hd = access hd
        and tl = traverse tl in
        hd :: tl
    ;;
  end

  type 'a t =
    | Leaf of 'a Bounded_length_list.t
    | Branch of 'a Bounded_length_list.t t

  let rec of_list : type a. a list -> a t =
    fun xs ->
    let chunks =
      Sequence.unfold_step ~init:xs ~f:(function
        | [] -> Done
        | xs ->
          let prefix, suffix = Bounded_length_list.of_list xs in
          Yield (prefix, suffix))
      |> Sequence.to_list
    in
    match chunks with
    | [] -> Leaf Bounded_length_list.empty
    | [ chunk ] -> Leaf chunk
    | chunks -> Branch (of_list chunks)
  ;;

  let rec to_list : type a. a t -> a list = function
    | Leaf xs -> (xs :> a list)
    | Branch t -> List.concat (to_list t :> a list list)
  ;;

  let rec traverse : type a b. a t -> (b t, a, b) Accessor.Many.t =
    let open Accessor.Many.Let_syntax in
    function
    | Leaf xs ->
      let%map xs = Bounded_length_list.traverse xs in
      Leaf xs
    | Branch t ->
      let%map t =
        Accessor.Many.Accessed.bind (traverse t) ~f:Bounded_length_list.traverse
      in
      Branch t
  ;;

  let each = [%accessor Accessor.many traverse]

  let%test_module "tree" =
    (module struct
      let%test_unit "to_list of_list" =
        List.iter
          [ 0; 1; 999; 1_000; 1_001; 999_999; 1_000_000; 1_000_001 ]
          ~f:(fun len ->
            let xs = List.init len ~f:Fn.id in
            [%test_eq: int list] (xs |> of_list |> to_list) xs)
      ;;
    end)
  ;;
end

let tree = [%accessor Accessor.isomorphism ~get:Tree.of_list ~construct:Tree.to_list]

(* This is not a well-behaved accessor on its own, but its use in [eachi] is fine. *)
let with_indices =
  [%accessor
    Accessor.isomorphism
      ~get:(List.mapi ~f:(fun i x -> i, x))
      ~construct:(List.map ~f:snd)]
;;

let each = [%accessor tree @> Tree.each]
let eachi = [%accessor with_indices @> each @> Accessor_tuple2.sndi]

include Accessor.Of_monad (struct
    include List

    let apply = `Define_using_bind
  end)
