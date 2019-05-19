open Batteries;;

(** This exception should be raised when code has not yet been implemented. *)
exception Not_yet_implemented of string;;

(** This exception should be raised if a defensive runtime check determines that
    an invariant has been violated. *)
exception Invariant_failure of string;;

let rec natural_compare_seq (parts : (unit -> int) list) =
  match parts with
  | [] -> 0
  | h::t ->
    let r = h () in
    if r = 0 then natural_compare_seq t else r
;;

let uniq_enum : 'a. ('a -> 'a -> int) -> 'a Enum.t -> 'a Enum.t =
  fun comparator e ->
    let empty_set = Set.PSet.create comparator in
    let all_set = Enum.fold (fun a e -> Set.PSet.add e a) empty_set e in
    Set.PSet.enum all_set
;;

let rec cartesian_product_of_list lst =
  match lst with
  | [] ->  [[]]
  | hh::tt ->
    let rec_results = cartesian_product_of_list tt in
    rec_results
    |> List.enum
    |> Enum.map
      (fun rec_result ->
         hh
         |> List.enum
         |> Enum.map (fun h -> h::rec_result)
      )
    |> Enum.concat
    |> List.of_enum
;;

let pairwise_enum_fold f e =
  if Enum.is_empty e then Enum.empty () else
    let e' = Enum.clone e in
    let _ = Enum.get_exn e' in
    if Enum.is_empty e' then Enum.empty () else
      Enum.combine (e,e') |> Enum.map (uncurry f)
;;

let set_file_contents filename s =
  File.with_file_out ~mode:[`create;`trunc;`text] filename
    (fun output -> IO.nwrite output s; IO.write output '\n')
;;
