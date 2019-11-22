open Core_kernel

(** We define a cost function, then find the diff which minimizes this cost
    function. Hopefully our cost function is similar to what a human perceives
    as a "good" diff. For example, we consider marking sections as the same
    to have a low cost.
*)

(** [cost_per_discrete_change] is an important tuning parameter.
    When you increase the value of this parameter, you make the algorithm
    less likely to break a change up into a sequence of smaller changes.

    In the extreme case, when this parameter goes to infinity, the algorithm
    will always produce the trivial answer [Replace (original, updated)],
*)
let cost_per_discrete_change = 1

(** The penalty for each [Enclose]. This probably doesn't need to be changed. *)
let enclose_penalty = 2

(* We want to compare two sexps for equality in O(1), so we flatten them out into a table
   whose indices are in one-to-one correspondence with all sub-expressions of the two
   sexps.  Then we compare indices rather than sexps.  The table also supports converting
   an index back into the corresponding sexp.  This technique is called Hash Consing or
   Interning.

   In our case, the table also stores the "size" of each sexp, measured as the number of
   [List] and [Atom] constructors it contains.  These sizes are used to compute the diff
   algorithm's cost function.

   We call such an index an [Interned_sexp] and the table an [Interned_sexp.Table].
*)
module Interned_sexp : sig
  module Table : sig
    type t

    val create : unit -> t
  end

  type t [@@deriving hash, compare, sexp]

  val equal : t -> t -> bool
  val of_sexp : table:Table.t -> Sexp.t -> t
  val to_sexp : table:Table.t -> t -> Sexp.t
  val size : table:Table.t -> t -> int
  val unpack_lists : table:Table.t -> t -> t -> (t list * t list) option
end = struct
  type t = int [@@deriving hash, compare, sexp]

  module Repr = struct
    module T = struct
      type nonrec t =
        | Atom of string
        | List of t list
      [@@deriving hash, compare, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  module Table = struct
    module Entry = struct
      type t =
        { repr : Repr.t
        ; size : int
        }
      [@@deriving fields]
    end

    type t =
      { mutable entries : Entry.t Option_array.t
      ; index_map : int Repr.Table.t
      ; mutable next_index : int
      }

    let create () =
      { entries = Option_array.create ~len:0
      ; index_map = Repr.Table.create ()
      ; next_index = 0
      }
    ;;

    let get_by_index t i = Option_array.get_some_exn t.entries i
    let repr t i = Entry.repr (get_by_index t i)

    let ensure_in_range t i =
      let rec loop t =
        let len = Option_array.length t.entries in
        if i >= len
        then (
          let entries =
            Option_array.init
              ((len * 2) + 1)
              ~f:(fun i -> if i < len then Option_array.get t.entries i else None)
          in
          let () = t.entries <- entries in
          loop t)
        else ()
      in
      loop t
    ;;

    let find_or_create t repr =
      Hashtbl.find_or_add t.index_map repr ~default:(fun () ->
        let index = t.next_index in
        let () = t.next_index <- index + 1 in
        let size =
          match repr with
          | Repr.Atom _ -> 1
          | Repr.List children ->
            let children_size =
              List.fold children ~init:0 ~f:(fun acc child ->
                acc + Entry.size (get_by_index t child))
            in
            children_size + 1
        in
        let entry = Entry.Fields.create ~repr ~size in
        let () = ensure_in_range t index in
        let () = Option_array.set_some t.entries index entry in
        index)
    ;;
  end

  let equal = Int.equal

  let of_sexp ~table sexp =
    let rec f sexp =
      let repr =
        match sexp with
        | Sexp.Atom x -> Repr.Atom x
        | Sexp.List xs -> Repr.List (List.map xs ~f)
      in
      Table.find_or_create table repr
    in
    f sexp
  ;;

  let size ~table t = Table.Entry.size (Table.get_by_index table t)

  let to_sexp ~table t =
    let rec f t =
      let repr = Table.repr table t in
      match repr with
      | Repr.Atom x -> Sexp.Atom x
      | Repr.List xs -> Sexp.List (List.map xs ~f)
    in
    f t
  ;;

  let unpack_lists ~table a b =
    let a = Table.repr table a in
    let b = Table.repr table b in
    match a, b with
    | Repr.Atom _, _ | _, Repr.Atom _ -> None
    | Repr.List a, Repr.List b -> Some (a, b)
  ;;
end

module AChange = struct
  type t =
    | Same of Interned_sexp.t
    | Enclose of
        { children : t list
        ; cost : int
        }
    | Replace of Interned_sexp.t * Interned_sexp.t
    | Delete of Interned_sexp.t
    | Add of Interned_sexp.t

  let cost ~table = function
    | Same _ -> 1
    | Enclose { cost; children = _ } -> cost
    | Replace (a, b) ->
      Interned_sexp.size ~table a
      + Interned_sexp.size ~table b
      + cost_per_discrete_change
    | Delete x | Add x -> Interned_sexp.size ~table x + cost_per_discrete_change
  ;;

  let min ~table a b = if cost ~table a <= cost ~table b then a else b
end

(* A [Solution.t] is a partially constructed [AChange.Enclose]. *)
module Solution = struct
  module Valid = struct
    type t =
      { cost : int
      ; changes : AChange.t list
      }
    [@@deriving fields]
  end

  type t =
    | Invalid
    | Valid of Valid.t

  let invalid = Invalid
  let empty = Valid { cost = 0; changes = [] }

  let cons a t ~cost =
    match t with
    | Invalid -> Invalid
    | Valid (t : Valid.t) ->
      let cost = t.cost + cost a in
      let changes = a :: t.changes in
      Valid { cost; changes }
  ;;

  let of_change c ~cost =
    let cost = cost c in
    let changes = [ c ] in
    Valid { cost; changes }
  ;;

  let min a b =
    match a, b with
    | Invalid, x | x, Invalid -> x
    | Valid a, Valid b -> if a.cost <= b.cost then Valid a else Valid b
  ;;
end

module Cache = struct
  module Memo_key = struct
    module T = struct
      type t = Interned_sexp.t * Interned_sexp.t [@@deriving compare, hash, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  type t =
    { interned_sexp_table : Interned_sexp.Table.t
    ; memo_cache : AChange.t Memo_key.Table.t
    }
  [@@deriving fields]

  let create () =
    { interned_sexp_table = Interned_sexp.Table.create ()
    ; memo_cache = Memo_key.Table.create ()
    }
  ;;

  let lookup t ~original ~updated ~compute =
    let key = original, updated in
    Hashtbl.find_or_add t.memo_cache key ~default:compute
  ;;
end

let rec solve ~cache ~original ~updated : AChange.t =
  let interned_sexp_table = Cache.interned_sexp_table cache in
  let cost = AChange.cost ~table:interned_sexp_table in
  Cache.lookup cache ~original ~updated ~compute:(fun () ->
    if Interned_sexp.equal original updated
    then AChange.Same original
    else (
      let simple_change = AChange.Replace (original, updated) in
      let complex_solution =
        match
          Interned_sexp.unpack_lists ~table:interned_sexp_table original updated
        with
        | None -> Solution.of_change simple_change ~cost
        | Some (original, updated) ->
          let original = Array.of_list original in
          let updated = Array.of_list updated in
          let original_len = Array.length original in
          let updated_len = Array.length updated in
          let solutions =
            Array.init (original_len + 1) ~f:(fun _index ->
              Array.create ~len:(updated_len + 1) Solution.invalid)
          in
          for i = original_len downto 0 do
            for j = updated_len downto 0 do
              let s = Solution.invalid in
              let s =
                if i < original_len
                then
                  Solution.min
                    s
                    (Solution.cons
                       ~cost
                       (AChange.Delete original.(i))
                       solutions.(i + 1).(j))
                else s
              in
              let s =
                if j < updated_len
                then
                  Solution.min
                    s
                    (Solution.cons
                       ~cost
                       (AChange.Add updated.(j))
                       solutions.(i).(j + 1))
                else s
              in
              let s =
                if i < original_len && j < updated_len
                then
                  Solution.min
                    s
                    (Solution.cons
                       ~cost
                       (solve ~cache ~original:original.(i) ~updated:updated.(j))
                       solutions.(i + 1).(j + 1))
                else s
              in
              let s =
                if i = original_len && j = updated_len
                then (
                  assert (phys_equal s Solution.invalid);
                  Solution.empty)
                else s
              in
              solutions.(i).(j) <- s
            done
          done;
          solutions.(0).(0)
      in
      match (complex_solution : Solution.t) with
      | Invalid -> simple_change
      | Valid { cost; changes } ->
        let complex_change =
          AChange.Enclose { children = changes; cost = cost + enclose_penalty }
        in
        AChange.min ~table:interned_sexp_table simple_change complex_change))
;;

let rec diff_of_achange ~table = function
  | AChange.Same x -> Diff.Same (Interned_sexp.to_sexp ~table x)
  | Replace (a, b) ->
    Replace (Interned_sexp.to_sexp ~table a, Interned_sexp.to_sexp ~table b)
  | Delete x -> Delete (Interned_sexp.to_sexp ~table x)
  | Add x -> Add (Interned_sexp.to_sexp ~table x)
  | Enclose { children; cost = _ } ->
    Enclose (List.map children ~f:(diff_of_achange ~table))
;;

let diff ~original ~updated ?(cache = Cache.create ()) () =
  let interned_sexp_table = Cache.interned_sexp_table cache in
  let original = Interned_sexp.of_sexp ~table:interned_sexp_table original in
  let updated = Interned_sexp.of_sexp ~table:interned_sexp_table updated in
  let achange = solve ~cache ~original ~updated in
  diff_of_achange ~table:interned_sexp_table achange
;;
