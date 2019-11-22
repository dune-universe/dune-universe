open Core
open Core_profiler

type ('a, +'rw) t = 'a option array

let ids_range ids =
  let ids = List.map ids ~f:Probe_id.to_int_exn in
  List.iter ids ~f:(fun id -> assert (id >= 0));
  match List.max_elt ids ~compare:Int.compare with
  | None -> 0
  | Some x -> x + 1

let create ids empty =
  let len = ids_range ids in
  let t = Array.create ~len None in
  List.iter ids ~f:(fun id -> t.(Probe_id.to_int_exn id) <- Some empty);
  t

let create' other_t empty =
  Array.map other_t ~f:(function
    | None -> None
    | Some _x -> Some empty
  )

let init ids ~f =
  let len = ids_range ids in
  let t = Array.create ~len None in
  List.iter ids ~f:(fun id -> t.(Probe_id.to_int_exn id) <- Some (f id));
  t

let init_from_map id_map ~f =
  let len =
    match Map.min_elt id_map, Map.max_elt id_map with
    | (None, _)
    | (_, None) -> 0
    | (Some (min_elt, _), (Some (max_elt, _))) ->
      let min_elt = Probe_id.to_int_exn min_elt in
      let max_elt = Probe_id.to_int_exn max_elt in
      assert (min_elt >= 0);
      max_elt + 1
  in

  let t = Array.create ~len None in

  Map.iteri
    id_map
    ~f:(fun ~key:id ~data:metadata ->
      t.(Probe_id.to_int_exn id) <- Some (f id metadata)
    );

  t

let find t id =
  let id' = Probe_id.to_int_exn id in
  if id' < 0 || id' >= Array.length t then None
  else Array.unsafe_get t id'

let find_exn t id =
  match find t id with
  | None -> failwithf !"Id %{Probe_id} not found amongst" id ()
  | Some data -> data

let set_exn (type a) (t : (a, 'rw) t) id data =
  (* Check that the cell is filled / the Id is legit first: *)
  ignore (find_exn t id : a);
  t.(Probe_id.to_int_exn id) <- Some data

let iter t ~f =
  Array.iteri
    t
    ~f:(fun id data ->
      match data with
      | Some data -> f (Probe_id.of_int_exn id) data
      | None -> ()
    )

let fold t ~init ~f =
  Array.foldi
    t ~init
    ~f:(fun id accum data ->
      match data with
      | Some data -> f accum (Probe_id.of_int_exn id) data
      | None -> accum
    )

let fold_right t ~init ~f =
  let rec loop i accum =
    let accum =
      match t.(i) with
      | Some data -> f accum (Probe_id.of_int_exn i) data
      | None -> accum
    in
    if i <= 0
    then accum
    else loop (i - 1) accum
  in
  loop (Array.length t - 1) init

let to_alist t =
  fold_right t ~init:[] ~f:(fun accum id item -> (id, item) :: accum)

let map t ~f =
  let (>>|) = Option.(>>|) in
  Array.mapi t ~f:(fun id' data ->
    let id = Probe_id.of_int_exn id' in
    data >>| f id
  )

let filter_map t ~f =
  let (>>=) = Option.(>>=) in
  Array.mapi t ~f:(fun id' data ->
    let id = Probe_id.of_int_exn id' in
    data >>= f id
  )

let read_only t = t
