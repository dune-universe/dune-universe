open Batteries;;

module type Multimap_sig =
sig
  type t
  type key
  type value

  module M : Map.S with type key = key
  module S : Set.S with type elt = value

  val empty : t

  val is_empty : t -> bool

  val num_keys : t -> int

  val num_values : t -> int

  val add : key -> value -> t -> t

  val add_all : key -> value Enum.t -> t -> t

  val find : key -> t -> value Enum.t

  val remove : key -> value -> t -> t

  val remove_all : key -> t -> t

  val mem : key -> value -> t -> bool

  val mem_any : key -> t -> bool

  val singleton : key -> value -> t

  val keys : t -> key Enum.t

  val enum : t -> (key * value) Enum.t

  val of_enum : (key * value) Enum.t -> t

  val enum_by_key : t -> (key * S.t) Enum.t

  val equal : t -> t -> bool

  val compare : t -> t -> int
end;;

module Make(Key_ord : BatInterfaces.OrderedType)
    (Value_ord : BatInterfaces.OrderedType)
  : Multimap_sig with type key = Key_ord.t and type value = Value_ord.t =
struct
  module M = Map.Make(Key_ord);;
  module S = Set.Make(Value_ord);;

  type t = S.t M.t;;
  type key = Key_ord.t;;
  type value = Value_ord.t;;

  let empty = M.empty;;

  let is_empty m = M.is_empty m;;

  let num_keys m = M.cardinal m;;

  let num_values m =
    M.enum m
    |> Enum.fold (fun a (_,v) -> a + S.cardinal v) 0
  ;;

  let find_internal k m =
    match M.Exceptionless.find k m with
    | None -> S.empty
    | Some v -> v
  ;;

  let add k v m =
    let old_set = find_internal k m in
    let new_set = S.add v old_set in
    M.add k new_set m
  ;;

  let add_all k vs m =
    let old_set = find_internal k m in
    let new_set = Enum.fold (flip S.add) old_set vs in
    M.add k new_set m
  ;;

  let find k m =
    match M.Exceptionless.find k m with
    | None -> Enum.empty ()
    | Some v -> S.enum v
  ;;

  let remove k v m =
    let old_set = find_internal k m in
    let new_set = S.remove v old_set in
    if S.is_empty new_set
    then M.remove k m
    else M.add k new_set m
  ;;

  let remove_all k m =
    M.remove k m
  ;;

  let mem k v m =
    find_internal k m |> S.mem v
  ;;

  let mem_any k m =
    M.mem k m
  ;;

  let singleton k v = M.add k (S.singleton v) M.empty;;

  let keys m = M.keys m;;

  let enum m =
    M.enum m
    |> Enum.map
      (fun (k,vs) ->
         S.enum vs |> Enum.map (fun v -> (k,v))
      )
    |> Enum.concat
  ;;

  let of_enum =
    Enum.fold (fun a (k,v) -> add k v a) empty
  ;;

  let enum_by_key m =
    M.enum m
  ;;

  let compare x y = M.compare S.compare x y;;

  let equal x y = compare x y == 0;;
end;;
