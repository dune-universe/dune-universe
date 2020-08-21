module type S = sig
  type key

  type 'a t

  type 'a diff = {
    (* common : 'a t; *)
    (* updated : ('a * 'a) t; *)
    added : 'a t;
    removed : 'a t;
  }

  val diff : old:'a t -> 'a t -> 'a diff

  val add_diff : 'a diff -> 'a t -> 'a t

  val sub_diff : 'a diff -> 'a t -> 'a t

  val range : start:key option -> end_exc:key option -> 'a t -> 'a t
end

module type S_bucketed = sig
  type 'a map

  type set

  type diff_bucketed = {
    (* common : set map; *)
    added : set map;
    removed : set map;
  }

  val diff_bucketed : old:set map -> set map -> diff_bucketed

  val add_diff_bucketed : diff_bucketed -> set map -> set map

  val sub_diff_bucketed : diff_bucketed -> set map -> set map
end

module Make (M : Map.S) : S with type key := M.key and type 'a t := 'a M.t =
struct
  type 'a t = 'a M.t

  type 'a diff = {
    (* common : 'a t; *)
    (* updated : ('a * 'a) t; *)
    added : 'a t;
    removed : 'a t;
  }

  (* let get_common (m1 : 'a t) (m2 : 'a t) : 'a t =
   *   M.merge
   *     (fun _key x1 x2 ->
   *        match (x1, x2) with
   *        | None, None -> None
   *        | Some _, None -> None
   *        | None, Some _ -> None
   *        | Some x1, Some x2 -> if x1 = x2 then Some x1 else None)
   *     m1 m2 *)

  (* let get_updated (m1 : 'a t) (m2 : 'a t) : ('a * 'a) t =
   *   M.merge
   *     (fun _key x1 x2 ->
   *        match (x1, x2) with
   *        | None, None -> None
   *        | Some _, None -> None
   *        | None, Some _ -> None
   *        | Some x1, Some x2 -> if x1 <> x2 then Some (x1, x2) else None)
   *     m1 m2 *)

  let get_added (m1 : 'a t) (m2 : 'a t) : 'a t =
    M.merge
      (fun _key x1 x2 ->
         match (x1, x2) with
         | None, _ -> x2
         | Some _, None -> None
         | Some x1, Some x2 -> if x1 = x2 then None else Some x2)
      m1 m2

  let get_removed (m1 : 'a t) (m2 : 'a t) : 'a t =
    M.merge
      (fun _key x1 x2 ->
         match (x1, x2) with
         | None, _ -> None
         | Some _, None -> x1
         | Some x1, Some x2 -> if x1 = x2 then None else Some x1)
      m1 m2

  let diff ~(old : 'a t) (m : 'a t) : 'a diff =
    {
      (* common = get_common old m; *)
      (* updated = get_updated old m; *)
      added = get_added old m;
      removed = get_removed old m;
    }

  let add_diff (diff : 'a diff) (m : 'a t) : 'a t =
    m
    (* apply updates *)
    (* |> M.mapi (fun key x ->
     *     match M.find_opt key diff.updated with
     *     | None -> x
     *     | Some (x1, x2) -> if x1 = x then x2 else raise Exceptions.Invalid_diff) *)
    (* remove *)
    |> M.merge
      (fun _key to_be_removed x ->
         match (to_be_removed, x) with
         | None, _ -> x
         | _, None -> raise Exceptions.Invalid_diff
         | Some to_be_removed, Some x ->
           if x = to_be_removed then None else raise Exceptions.Invalid_diff)
      diff.removed
    (* add *)
    |> M.union (fun _key added _ -> Some added) diff.added

  let sub_diff (diff : 'a diff) (m : 'a t) : 'a t =
    m
    (* revert updates *)
    (* |> M.mapi (fun key x ->
     *     match M.find_opt key diff.updated with
     *     | None -> x
     *     | Some (x1, x2) -> if x2 = x then x1 else raise Exceptions.Invalid_diff) *)
    (* revert add *)
    |> M.merge
      (fun _key to_be_removed x ->
         match (to_be_removed, x) with
         | None, _ | _, None -> x
         | Some to_be_removed, Some x ->
           if x = to_be_removed then None else raise Exceptions.Invalid_diff)
      diff.added
    (* revert remove *)
    |> M.union (fun _key removed _ -> Some removed) diff.removed

  let range ~(start : M.key option) ~(end_exc : M.key option) (m : 'a t) : 'a t
    =
    let add' (key : M.key) (x : 'a option) (m : 'a t) =
      match x with None -> m | Some x -> M.add key x m
    in
    match (start, end_exc) with
    | None, None -> m
    | Some start, None ->
      let _, eq, after = M.split start m in
      add' start eq after
    | None, Some end_exc ->
      let before, eq, _ = M.split end_exc m in
      add' end_exc eq before
    | Some start, Some end_exc ->
      let after_or_from_start =
        let _, eq, after = M.split start m in
        add' start eq after
      in
      let before_or_on_end_exc =
        let before, eq, _ = M.split end_exc after_or_from_start in
        add' end_exc eq before
      in
      before_or_on_end_exc
end

module Make_bucketed (Map : Map.S) (Set : Set.S) :
  S_bucketed with type 'a map := 'a Map.t and type set := Set.t = struct
  type 'a map = 'a Map.t

  type set = Set.t

  type diff_bucketed = {
    (* common : set map; *)
    added : set map;
    removed : set map;
  }

  (* let get_common (m1 : set map) (m2 : set map) : set map =
   *   Map.merge
   *     (fun _key s1 s2 ->
   *        match (s1, s2) with
   *        | None, None -> None
   *        | Some _, None -> None
   *        | None, Some _ -> None
   *        | Some s1, Some s2 -> Some (Set.inter s1 s2))
   *     m1 m2 *)

  let get_added (m1 : set map) (m2 : set map) : set map =
    Map.merge
      (fun _key s1 s2 ->
         match (s1, s2) with
         | None, _ -> s2
         | Some _, None -> None
         | Some s1, Some s2 ->
           if Set.equal s1 s2 then None else Some (Set.diff s2 s1))
      m1 m2

  let get_removed (m1 : set map) (m2 : set map) : set map =
    Map.merge
      (fun _key s1 s2 ->
         match (s1, s2) with
         | None, _ -> None
         | Some _, None -> s1
         | Some s1, Some s2 ->
           if Set.equal s1 s2 then None else Some (Set.diff s1 s2))
      m1 m2

  let diff_bucketed ~(old : set map) (m : set map) : diff_bucketed =
    {
      (* common = get_common old m; *)
      added = get_added old m;
      removed = get_removed old m;
    }

  let add_diff_bucketed (diff : diff_bucketed) (m : set map) : set map =
    m
    (* remove *)
    |> Map.merge
      (fun _key to_be_removed s ->
         match (to_be_removed, s) with
         | None, _ -> s
         | _, None -> raise Exceptions.Invalid_diff
         | Some to_be_removed, Some s ->
           if Set.equal to_be_removed s then None
           else Some (Set.diff s to_be_removed))
      diff.removed
    (* add *)
    |> Map.union (fun _key s1 s2 -> Some (Set.union s1 s2)) diff.added

  let sub_diff_bucketed (diff : diff_bucketed) (m : set map) : set map =
    m
    (* revert add *)
    |> Map.merge
      (fun _key to_be_removed s ->
         match (to_be_removed, s) with
         | None, _ -> s
         | _, None -> raise Exceptions.Invalid_diff
         | Some to_be_removed, Some s ->
           if Set.equal to_be_removed s then None
           else Some (Set.diff s to_be_removed))
      diff.added
    (* revert remove *)
    |> Map.union (fun _key s1 s2 -> Some (Set.union s1 s2)) diff.removed
end
