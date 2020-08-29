module type S = sig
  type set

  type diff = {
    (* common : set; *)
    added : set;
    removed : set;
  }

  val diff : old:set -> set -> diff

  val add_diff : diff -> set -> set

  val sub_diff : diff -> set -> set
end

module Make (Set : Set.S) : S with type set := Set.t = struct
  type set = Set.t

  type diff = {
    (* common : set; *)
    added : set;
    removed : set;
  }

  (* let get_common (s1 : set) (s2 : set) : set = Set.inter s1 s2 *)

  let get_added (s1 : set) (s2 : set) : set = Set.diff s2 s1

  let get_removed (s1 : set) (s2 : set) : set = Set.diff s1 s2

  let diff ~(old : set) (s : set) : diff =
    {
      (* common = get_common old s; *)
      added = get_added old s;
      removed = get_removed old s;
    }

  let add_diff (diff : diff) (s : set) : set =
    s
    (* remove *)
    |> (fun s -> Set.diff s diff.removed)
    (* add *)
    |> Set.union diff.added

  let sub_diff (diff : diff) (s : set) : set =
    s
    (* revert add *)
    |> (fun s -> Set.diff s diff.added)
    (* revert remove *)
    |> Set.union diff.removed
end
