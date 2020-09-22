open Int64_utils
module Task_ = Task
module Sched_req_ = Sched_req

type sched_id = int

type task_store = Task_.task_data Task_id_map.t

type task_store_diff = Task_.task_data Task_id_map_utils.diff

type task_inst_store = Task_.task_inst_data Task_inst_id_map.t

type task_inst_store_diff = Task_.task_inst_data Task_inst_id_map_utils.diff

type task_seg_store = Task_.task_seg_size Task_seg_id_map.t

type task_seg_store_diff = Task_.task_seg_size Task_seg_id_map_utils.diff

(* type transit_time_slice = {
 *   start : int64;
 *   len : int64;
 * } *)

(* type transit_time_record = transit_time_slice Int64_map.t
 * 
 * type transit_time_store = transit_time_record Transit_time_map.t
 * 
 * type transit_time_store_diff = transit_time_record Transit_time_map_utils.diff *)

type sched_req_store = Sched_req_.sched_req_data Sched_req_id_map.t

type sched_req_store_diff =
  Sched_req_.sched_req_data Sched_req_id_map_utils.diff

type sched_req_record_store =
  Sched_req_.sched_req_record_data Sched_req_id_map.t

type sched_req_record_store_diff =
  Sched_req_.sched_req_record_data Sched_req_id_map_utils.diff

type task_seg_place_map = Task_seg_id_set.t Int64_map.t

type task_seg_place_map_diff =
  Int64_map_utils.Task_seg_id_bucketed.diff_bucketed

type task_related_status =
  [ `Uncompleted
  | `Completed
  | `Discarded
  ]

type sched_req_status =
  [ `Pending
  | `Discarded
  | `Recorded
  ]

type store = {
  task_uncompleted_store : task_store;
  task_completed_store : task_store;
  task_discarded_store : task_store;
  task_inst_uncompleted_store : task_inst_store;
  task_inst_completed_store : task_inst_store;
  task_inst_discarded_store : task_inst_store;
  task_seg_uncompleted_store : task_seg_store;
  task_seg_completed_store : task_seg_store;
  task_seg_discarded_store : task_seg_store;
  (* transit_time_store : transit_time_store; *)
  user_id_to_task_ids : Int64_set.t User_id_map.t;
  task_id_to_task_inst_ids : Int64_set.t Task_id_map.t;
  task_inst_id_to_task_seg_ids : Int64_int64_option_set.t Task_inst_id_map.t;
  sched_req_ids : Int64_set.t;
  sched_req_pending_store : sched_req_store;
  sched_req_discarded_store : sched_req_store;
  sched_req_record_store : sched_req_record_store;
  quota : int64 Task_inst_id_map.t;
  task_seg_id_to_progress : Task_.progress Task_seg_id_map.t;
  task_inst_id_to_progress : Task_.progress Task_inst_id_map.t;
}

type store_diff = {
  task_uncompleted_store_diff : task_store_diff;
  task_completed_store_diff : task_store_diff;
  task_discarded_store_diff : task_store_diff;
  task_inst_uncompleted_store_diff : task_inst_store_diff;
  task_inst_completed_store_diff : task_inst_store_diff;
  task_inst_discarded_store_diff : task_inst_store_diff;
  task_seg_uncompleted_store_diff : task_seg_store_diff;
  task_seg_completed_store_diff : task_seg_store_diff;
  task_seg_discarded_store_diff : task_seg_store_diff;
  (* transit_time_store_diff : transit_time_store_diff; *)
  user_id_to_task_ids_diff : User_id_map_utils.Int64_bucketed.diff_bucketed;
  task_id_to_task_inst_ids_diff :
    Task_id_map_utils.Int64_bucketed.diff_bucketed;
  task_inst_id_to_task_seg_ids_diff :
    Task_inst_id_map_utils.Int64_int64_option_bucketed.diff_bucketed;
  sched_req_ids_diff : Int64_set_utils.diff;
  sched_req_pending_store_diff : sched_req_store_diff;
  sched_req_discarded_store_diff : sched_req_store_diff;
  sched_req_record_store_diff : sched_req_record_store_diff;
  quota_diff : int64 Task_inst_id_map_utils.diff;
  task_seg_id_to_progress_diff : Task_.progress Task_seg_id_map_utils.diff;
  task_inst_id_to_progress_diff : Task_.progress Task_inst_id_map_utils.diff;
}

type agenda = {
  (* start_and_end_exc : (int64 * int64) option; *)
  indexed_by_task_seg_id : (int64 * int64) Task_seg_id_map.t;
  indexed_by_start : task_seg_place_map;
  indexed_by_end_exc : task_seg_place_map;
}

type agenda_diff = {
  indexed_by_task_seg_id_diff : (int64 * int64) Task_seg_id_map_utils.diff;
  indexed_by_start_diff : task_seg_place_map_diff;
  indexed_by_end_exc_diff : task_seg_place_map_diff;
}

type sched_data = {
  store : store;
  agenda : agenda;
}

type sched_data_diff = {
  store_diff : store_diff;
  agenda_diff : agenda_diff;
}

type sched = sched_id * sched_data

type sched_diff = sched_id * sched_id * sched_data_diff

let store_empty =
  {
    task_uncompleted_store = Task_id_map.empty;
    task_completed_store = Task_id_map.empty;
    task_discarded_store = Task_id_map.empty;
    task_inst_uncompleted_store = Task_inst_id_map.empty;
    task_inst_completed_store = Task_inst_id_map.empty;
    task_inst_discarded_store = Task_inst_id_map.empty;
    task_seg_uncompleted_store = Task_seg_id_map.empty;
    task_seg_completed_store = Task_seg_id_map.empty;
    task_seg_discarded_store = Task_seg_id_map.empty;
    (* transit_time_store = Transit_time_map.empty; *)
    user_id_to_task_ids = User_id_map.empty;
    task_id_to_task_inst_ids = Task_id_map.empty;
    task_inst_id_to_task_seg_ids = Task_inst_id_map.empty;
    sched_req_ids = Int64_set.empty;
    sched_req_pending_store = Sched_req_id_map.empty;
    sched_req_discarded_store = Sched_req_id_map.empty;
    sched_req_record_store = Sched_req_id_map.empty;
    quota = Task_inst_id_map.empty;
    task_seg_id_to_progress = Task_seg_id_map.empty;
    task_inst_id_to_progress = Task_inst_id_map.empty;
  }

let agenda_empty =
  {
    (* start_and_end_exc = None; *)
    indexed_by_task_seg_id = Task_seg_id_map.empty;
    indexed_by_start = Int64_map.empty;
    indexed_by_end_exc = Int64_map.empty;
  }

let sched_data_empty = { store = store_empty; agenda = agenda_empty }

let empty : sched = (0, sched_data_empty)

module Id = struct
  let sched_id_to_string (id : sched_id) = string_of_int id

  let incre_int64_id x = match x with None -> 0L | Some x -> Int64.succ x

  let incre_int64_int64_option_id x =
    match x with None -> (0L, None) | Some (id, _) -> (Int64.succ id, None)

  let add_task_id ((user_id, task_part) : Task_.task_id) ((sid, sd) : sched) :
    sched =
    let task_ids =
      User_id_map.find_opt user_id sd.store.user_id_to_task_ids
      |> Option.value ~default:Int64_set.empty
      |> Int64_set.add task_part
    in
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            user_id_to_task_ids =
              User_id_map.add user_id task_ids sd.store.user_id_to_task_ids;
          };
      } )

  let get_new_task_id (user_id : Task_.user_id) ((sid, sd) : sched) :
    Task_.task_id * sched =
    let task_ids =
      User_id_map.find_opt user_id sd.store.user_id_to_task_ids
      |> Option.value ~default:Int64_set.empty
    in
    let task_part = Int64_set.max_elt_opt task_ids |> incre_int64_id in
    let task_id = (user_id, task_part) in
    (task_id, add_task_id task_id (sid, sd))

  let remove_task_id ((user_id, task_part) : Task_.task_id) ((sid, sd) : sched)
    : sched =
    match User_id_map.find_opt user_id sd.store.user_id_to_task_ids with
    | None -> (sid, sd)
    | Some task_ids ->
      let task_ids = Int64_set.remove task_part task_ids in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              user_id_to_task_ids =
                User_id_map.add user_id task_ids sd.store.user_id_to_task_ids;
            };
        } )

  let add_task_inst_id
      ((user_id, task_part, task_inst_part) : Task_.task_inst_id)
      ((sid, sd) : sched) : sched =
    let task_id = (user_id, task_part) in
    let task_inst_ids =
      Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids
      |> Option.value ~default:Int64_set.empty
      |> Int64_set.add task_inst_part
    in
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            task_id_to_task_inst_ids =
              Task_id_map.add task_id task_inst_ids
                sd.store.task_id_to_task_inst_ids;
          };
      } )

  let get_new_task_inst_id ((user_id, task_part) : Task_.task_id)
      ((sid, sd) : sched) : Task_.task_inst_id * sched =
    let task_id = (user_id, task_part) in
    let task_inst_ids =
      Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids
      |> Option.value ~default:Int64_set.empty
    in
    let task_inst_part =
      Int64_set.max_elt_opt task_inst_ids |> incre_int64_id
    in
    let task_inst_id = (user_id, task_part, task_inst_part) in
    (task_inst_id, add_task_inst_id task_inst_id (sid, sd))

  let task_inst_id_seq_of_task_id ((_, sd) : sched) ((id1, id2) : Task_.task_id)
    : Task_.task_inst_id Seq.t =
    match Task_id_map.find_opt (id1, id2) sd.store.task_id_to_task_inst_ids with
    | None -> Seq.empty
    | Some s -> s |> Int64_set.to_seq |> Seq.map (fun id -> (id1, id2, id))

  let remove_task_inst_id
      ((user_id, task_part, task_inst_part) : Task_.task_inst_id)
      ((sid, sd) : sched) : sched =
    let task_id = (user_id, task_part) in
    match Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids with
    | None -> (sid, sd)
    | Some task_inst_ids ->
      let task_inst_ids = Int64_set.remove task_inst_part task_inst_ids in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_id_to_task_inst_ids =
                Task_id_map.add task_id task_inst_ids
                  sd.store.task_id_to_task_inst_ids;
            };
        } )

  let add_task_seg_id
      ((user_id, task_part, task_inst_part, task_seg_part, task_seg_sub_id) :
         Task_.task_seg_id) ((sid, sd) : sched) : sched =
    let task_inst_id = (user_id, task_part, task_inst_part) in
    let task_seg_ids =
      Task_inst_id_map.find_opt task_inst_id
        sd.store.task_inst_id_to_task_seg_ids
      |> Option.value ~default:Int64_int64_option_set.empty
    in
    let task_seg_ids =
      Int64_int64_option_set.add (task_seg_part, task_seg_sub_id) task_seg_ids
    in
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            task_inst_id_to_task_seg_ids =
              Task_inst_id_map.add task_inst_id task_seg_ids
                sd.store.task_inst_id_to_task_seg_ids;
          };
      } )

  let get_new_task_seg_id (task_inst_id : Task_.task_inst_id)
      ((sid, sd) : sched) : Task_.task_seg_id * sched =
    let task_seg_ids =
      Task_inst_id_map.find_opt task_inst_id
        sd.store.task_inst_id_to_task_seg_ids
      |> Option.value ~default:Int64_int64_option_set.empty
    in
    let task_seg_part, task_seg_sub_id =
      Int64_int64_option_set.max_elt_opt task_seg_ids
      |> incre_int64_int64_option_id
    in
    let user_id, task_part, task_inst_part = task_inst_id in
    let task_seg_id =
      (user_id, task_part, task_inst_part, task_seg_part, task_seg_sub_id)
    in
    (task_seg_id, add_task_seg_id task_seg_id (sid, sd))

  let task_seg_id_seq_of_task_inst_id ((_, sd) : sched)
      ((id1, id2, id3) as task_inst_id : Task_.task_inst_id) :
    Task_.task_seg_id Seq.t =
    match
      Task_inst_id_map.find_opt task_inst_id
        sd.store.task_inst_id_to_task_seg_ids
    with
    | None -> Seq.empty
    | Some s ->
      s
      |> Int64_int64_option_set.to_seq
      |> Seq.map (fun (id, opt) -> (id1, id2, id3, id, opt))

  let task_seg_id_seq_of_task_id (sched : sched) (task_id : Task_.task_id) :
    Task_.task_seg_id Seq.t =
    task_inst_id_seq_of_task_id sched task_id
    |> Seq.flat_map (task_seg_id_seq_of_task_inst_id sched)

  let add_task_seg_id ((id1, id2, id3, id4, id5) : Task_.task_seg_id)
      ((sid, sd) : sched) : sched =
    let task_inst_id = (id1, id2, id3) in
    let task_seg_ids =
      match
        Task_inst_id_map.find_opt task_inst_id
          sd.store.task_inst_id_to_task_seg_ids
      with
      | None ->
        Int64_int64_option_set.empty |> Int64_int64_option_set.add (id4, id5)
      | Some s ->
        ( match id5 with
          | None -> s
          | Some _ ->
            Int64_int64_option_set.filter
              (fun (id4', id5') -> not (id4' = id4 && id5' = None))
              s )
        |> Int64_int64_option_set.add (id4, id5)
    in
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            task_inst_id_to_task_seg_ids =
              Task_inst_id_map.add task_inst_id task_seg_ids
                sd.store.task_inst_id_to_task_seg_ids;
          };
      } )

  let remove_task_seg_id
      ((user_id, task_part, task_inst_part, task_seg_part, task_seg_sub_id) :
         Task_.task_seg_id) ((sid, sd) : sched) : sched =
    let task_inst_id = (user_id, task_part, task_inst_part) in
    match
      Task_inst_id_map.find_opt task_inst_id
        sd.store.task_inst_id_to_task_seg_ids
    with
    | None -> (sid, sd)
    | Some task_seg_ids ->
      let task_seg_ids =
        Int64_int64_option_set.remove
          (task_seg_part, task_seg_sub_id)
          task_seg_ids
      in
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_id_to_task_seg_ids =
                Task_inst_id_map.add task_inst_id task_seg_ids
                  sd.store.task_inst_id_to_task_seg_ids;
            };
        } )

  let get_new_sched_req_id ((sid, sd) : sched) : Sched_req_.sched_req_id * sched
    =
    let sched_req_id =
      Int64_set.max_elt_opt sd.store.sched_req_ids |> incre_int64_id
    in
    let sched_req_ids = Int64_set.add sched_req_id sd.store.sched_req_ids in
    (sched_req_id, (sid, { sd with store = { sd.store with sched_req_ids } }))

  let remove_sched_req_id (sched_req_id : Sched_req_.sched_req_id)
      ((sid, sd) : sched) : sched =
    let sched_req_ids = Int64_set.remove sched_req_id sd.store.sched_req_ids in
    (sid, { sd with store = { sd.store with sched_req_ids } })
end

module Quota = struct
  let update_quota quota ((sid, sd) : sched) : sched =
    (sid, { sd with store = { sd.store with quota } })

  let add_quota quota ((sid, sd) : sched) : sched =
    ( sid,
      {
        sd with
        store =
          {
            sd.store with
            quota =
              Task_inst_id_map.union
                (fun _ x y -> Some (x +^ y))
                sd.store.quota quota;
          };
      } )
end

module Task_seg = struct
  module To_seq = struct
    (*$ #use "src/sched.cinaps";;

      print_task_seg_to_seq ();
    *)

    let task_seg_seq_uncompleted ((_, sd) : sched) : Task_.task_seg Seq.t =
      Task_seg_id_map.to_seq sd.store.task_seg_uncompleted_store

    let task_seg_seq_completed ((_, sd) : sched) : Task_.task_seg Seq.t =
      Task_seg_id_map.to_seq sd.store.task_seg_completed_store

    let task_seg_seq_discarded ((_, sd) : sched) : Task_.task_seg Seq.t =
      Task_seg_id_map.to_seq sd.store.task_seg_discarded_store

    let task_seg_seq_all ((_, sd) : sched) : Task_.task_seg Seq.t =
      OSeq.append
        (OSeq.append
           (Task_seg_id_map.to_seq sd.store.task_seg_uncompleted_store)
           (Task_seg_id_map.to_seq sd.store.task_seg_completed_store))
        (Task_seg_id_map.to_seq sd.store.task_seg_discarded_store)

    (*$*)
  end

  module Find = struct
    (*$ #use "src/sched.cinaps";;

      print_task_seg_find ();
      print_task_seg_ids_find_by_task_inst_id ();
      print_task_seg_seq_find_by_task_inst_id ();
      print_task_seg_ids_find_by_task_id ();
      print_task_seg_seq_find_by_task_id ()
    *)

    let find_task_seg_uncompleted_opt (id : Task_.task_seg_id) ((_, sd) : sched)
      : Task_.task_seg_size option =
      Task_seg_id_map.find_opt id sd.store.task_seg_uncompleted_store

    let find_task_seg_completed_opt (id : Task_.task_seg_id) ((_, sd) : sched) :
      Task_.task_seg_size option =
      Task_seg_id_map.find_opt id sd.store.task_seg_completed_store

    let find_task_seg_discarded_opt (id : Task_.task_seg_id) ((_, sd) : sched) :
      Task_.task_seg_size option =
      Task_seg_id_map.find_opt id sd.store.task_seg_discarded_store

    let find_task_seg_any_with_status_opt (id : Task_.task_seg_id)
        (sched : sched) : (Task_.task_seg_size * task_related_status) option =
      match find_task_seg_uncompleted_opt id sched with
      | Some x -> Some (x, `Uncompleted)
      | None -> (
          match find_task_seg_completed_opt id sched with
          | Some x -> Some (x, `Completed)
          | None -> (
              match find_task_seg_discarded_opt id sched with
              | Some x -> Some (x, `Discarded)
              | None -> None ) )

    let find_task_seg_any_opt (id : Task_.task_seg_id) (sched : sched) :
      Task_.task_seg_size option =
      find_task_seg_any_with_status_opt id sched
      |> Option.map (fun (x, _status) -> x)

    let find_task_seg_ids_by_task_inst_id (id : Task_.task_inst_id)
        (sched : sched) : Task_.task_seg_id Seq.t =
      Id.task_seg_id_seq_of_task_inst_id sched id

    let find_task_seg_seq_uncompleted_by_task_inst_id (id : Task_.task_inst_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_inst_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_uncompleted_opt task_seg_id sched
          |> Option.map (fun task_seg_data -> (task_seg_id, task_seg_data)))

    let find_task_seg_seq_completed_by_task_inst_id (id : Task_.task_inst_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_inst_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_completed_opt task_seg_id sched
          |> Option.map (fun task_seg_data -> (task_seg_id, task_seg_data)))

    let find_task_seg_seq_discarded_by_task_inst_id (id : Task_.task_inst_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_inst_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_discarded_opt task_seg_id sched
          |> Option.map (fun task_seg_data -> (task_seg_id, task_seg_data)))

    let find_task_seg_seq_any_with_status_by_task_inst_id
        (id : Task_.task_inst_id) (sched : sched) :
      (Task_.task_seg * task_related_status) Seq.t =
      find_task_seg_ids_by_task_inst_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_any_with_status_opt task_seg_id sched
          |> Option.map (fun (x, status) -> ((task_seg_id, x), status)))

    let find_task_seg_seq_any_by_task_inst_id (id : Task_.task_inst_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_inst_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_any_opt task_seg_id sched
          |> Option.map (fun x -> (task_seg_id, x)))

    let find_task_seg_ids_by_task_id (id : Task_.task_id) (sched : sched) :
      Task_.task_seg_id Seq.t =
      Id.task_seg_id_seq_of_task_id sched id

    let find_task_seg_seq_uncompleted_by_task_id (id : Task_.task_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_uncompleted_opt task_seg_id sched
          |> Option.map (fun task_seg_data -> (task_seg_id, task_seg_data)))

    let find_task_seg_seq_completed_by_task_id (id : Task_.task_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_completed_opt task_seg_id sched
          |> Option.map (fun task_seg_data -> (task_seg_id, task_seg_data)))

    let find_task_seg_seq_discarded_by_task_id (id : Task_.task_id)
        (sched : sched) : Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_discarded_opt task_seg_id sched
          |> Option.map (fun task_seg_data -> (task_seg_id, task_seg_data)))

    let find_task_seg_seq_any_with_status_by_task_id (id : Task_.task_id)
        (sched : sched) : (Task_.task_seg * task_related_status) Seq.t =
      find_task_seg_ids_by_task_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_any_with_status_opt task_seg_id sched
          |> Option.map (fun (x, status) -> ((task_seg_id, x), status)))

    let find_task_seg_seq_any_by_task_id (id : Task_.task_id) (sched : sched) :
      Task_.task_seg Seq.t =
      find_task_seg_ids_by_task_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_any_opt task_seg_id sched
          |> Option.map (fun x -> (task_seg_id, x)))

    (*$*)
  end

  module Status = struct
    let get_task_seg_status (id : Task_.task_seg_id) (sched : sched) :
      task_related_status option =
      Find.find_task_seg_any_with_status_opt id sched
      |> Option.map (fun (_, status) -> status)
  end

  module Remove = struct
    (*$ #use "src/sched.cinaps";;

      print_task_seg_remove ();
      (* print_task_seg_remove_strict (); *)
      print_task_seg_remove_seq ()
    *)

    let remove_task_seg_uncompleted (id : Task_.task_seg_id) (sched : sched) :
      sched =
      let sid, sd = Id.remove_task_seg_id id sched in

      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_uncompleted_store =
                Task_seg_id_map.remove id sd.store.task_seg_uncompleted_store;
            };
        } )

    let remove_task_seg_completed (id : Task_.task_seg_id) (sched : sched) :
      sched =
      let sid, sd = Id.remove_task_seg_id id sched in

      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_completed_store =
                Task_seg_id_map.remove id sd.store.task_seg_completed_store;
            };
        } )

    let remove_task_seg_discarded (id : Task_.task_seg_id) (sched : sched) :
      sched =
      let sid, sd = Id.remove_task_seg_id id sched in

      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_discarded_store =
                Task_seg_id_map.remove id sd.store.task_seg_discarded_store;
            };
        } )

    let remove_task_seg_all (id : Task_.task_seg_id) (sched : sched) : sched =
      sched
      |> remove_task_seg_uncompleted id
      |> remove_task_seg_completed id
      |> remove_task_seg_discarded id

    let remove_task_seg_uncompleted_seq (ids : Task_.task_seg_id Seq.t)
        (sched : sched) : sched =
      Seq.fold_left
        (fun sched id -> remove_task_seg_uncompleted id sched)
        sched ids

    let remove_task_seg_completed_seq (ids : Task_.task_seg_id Seq.t)
        (sched : sched) : sched =
      Seq.fold_left
        (fun sched id -> remove_task_seg_completed id sched)
        sched ids

    let remove_task_seg_discarded_seq (ids : Task_.task_seg_id Seq.t)
        (sched : sched) : sched =
      Seq.fold_left
        (fun sched id -> remove_task_seg_discarded id sched)
        sched ids

    (*$*)
  end

  module Add = struct
    let add_task_seg ~(parent_task_inst_id : Task_.task_inst_id)
        (size : Task_.task_seg_size) ((sid, sd) : sched) :
      Task_.task_seg * sched =
      let task_seg_id, (sid, sd) =
        Id.get_new_task_seg_id parent_task_inst_id (sid, sd)
      in
      ( (task_seg_id, size),
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                task_seg_uncompleted_store =
                  Task_seg_id_map.add task_seg_id size
                    sd.store.task_seg_uncompleted_store;
              };
          } ) )

    (*$ #use "src/sched.cinaps";;

      print_task_seg_add ()
    *)

    let add_task_seg_uncompleted (id : Task_.task_seg_id)
        (size : Task_.task_seg_size) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_uncompleted_store =
                Task_seg_id_map.add id size sd.store.task_seg_uncompleted_store;
            };
        } )
      |> Id.add_task_seg_id id

    let add_task_seg_completed (id : Task_.task_seg_id)
        (size : Task_.task_seg_size) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_completed_store =
                Task_seg_id_map.add id size sd.store.task_seg_completed_store;
            };
        } )
      |> Id.add_task_seg_id id

    let add_task_seg_discarded (id : Task_.task_seg_id)
        (size : Task_.task_seg_size) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_discarded_store =
                Task_seg_id_map.add id size sd.store.task_seg_discarded_store;
            };
        } )
      |> Id.add_task_seg_id id

    (*$*)

    let add_task_seg_via_task_seg_alloc_req
        ((parent_task_inst_id, task_seg_size) : Task_.task_seg_alloc_req)
        (sched : sched) : Task_.task_seg * sched =
      let task_seg, sched =
        add_task_seg ~parent_task_inst_id task_seg_size sched
      in
      (task_seg, sched)

    let add_task_segs_via_task_seg_alloc_req_list
        (reqs : Task_.task_seg_alloc_req list) (sched : sched) :
      Task_.task_seg list * sched =
      List.fold_left
        (fun (acc, sched) req ->
           let task_seg, sched = add_task_seg_via_task_seg_alloc_req req sched in
           (task_seg :: acc, sched))
        ([], sched) reqs
      |> fun (l, t) -> (List.rev l, t)

    let add_task_seg_via_task_seg_place
        ((id, start, end_exc) : Task_.task_seg_place) (sched : sched) : sched =
      let id1, id2, id3, id4, sub_id = id in
      let task_seg_size = end_exc -^ start in
      sched
      |> (fun sched ->
          match sub_id with
          | None -> sched
          | Some _ ->
            Remove.remove_task_seg_all (id1, id2, id3, id4, None) sched)
      |> Id.add_task_seg_id id
      |> add_task_seg_uncompleted id task_seg_size

    let add_task_segs_via_task_seg_place_list
        (places : Task_.task_seg_place list) (sched : sched) : sched =
      List.fold_left
        (fun sched place -> add_task_seg_via_task_seg_place place sched)
        sched places

    let add_task_segs_via_task_seg_place_seq
        (places : Task_.task_seg_place Seq.t) (sched : sched) : sched =
      Seq.fold_left
        (fun sched place -> add_task_seg_via_task_seg_place place sched)
        sched places
  end

  module Move = struct
    let move_task_seg_internal
        ~(add_task_seg :
            Task_.task_seg_id -> Task_.task_seg_size -> sched -> sched)
        (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =
      match Find.find_task_seg_any_opt task_seg_id sched with
      | None -> sched
      | Some task_seg_size ->
        sched
        |> Remove.remove_task_seg_all task_seg_id
        |> add_task_seg task_seg_id task_seg_size

    let move_task_seg_to_completed (task_seg_id : Task_.task_seg_id)
        (sched : sched) : sched =
      move_task_seg_internal ~add_task_seg:Add.add_task_seg_completed
        task_seg_id sched

    let move_task_seg_to_uncompleted (task_seg_id : Task_.task_seg_id)
        (sched : sched) : sched =
      move_task_seg_internal ~add_task_seg:Add.add_task_seg_uncompleted
        task_seg_id sched

    let move_task_seg_to_discarded (task_seg_id : Task_.task_seg_id)
        (sched : sched) : sched =
      move_task_seg_internal ~add_task_seg:Add.add_task_seg_discarded
        task_seg_id sched
  end
end

module Task_inst = struct
  module Add = struct
    let add_task_inst ~(parent_task_id : Task_.task_id)
        (data : Task_.task_inst_data) ((sid, sd) : sched) :
      Task_.task_inst * sched =
      let task_inst_id, (sid, sd) =
        Id.get_new_task_inst_id parent_task_id (sid, sd)
      in
      let task_inst_uncompleted_store =
        Task_inst_id_map.add task_inst_id data
          sd.store.task_inst_uncompleted_store
      in
      let quota =
        match data.task_inst_type with
        | Task_.Reminder_quota_counting { quota } ->
          Task_inst_id_map.add task_inst_id quota sd.store.quota
        | Task_.Reminder | Passing -> sd.store.quota
      in
      ( (task_inst_id, data),
        ( sid,
          {
            sd with
            store = { sd.store with task_inst_uncompleted_store; quota };
          } ) )

    let add_task_inst_list ~(parent_task_id : Task_.task_id)
        (data_list : Task_.task_inst_data list) (sched : sched) :
      Task_.task_inst list * sched =
      List.fold_left
        (fun (acc, sched) data ->
           let inst, sched = add_task_inst ~parent_task_id data sched in
           (inst :: acc, sched))
        ([], sched) data_list
      |> fun (l, t) -> (List.rev l, t)

    (*$ #use "src/sched.cinaps";;

      print_task_inst_add ();
    *)

    let add_task_inst_uncompleted (id : Task_.task_inst_id)
        (data : Task_.task_inst_data) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_uncompleted_store =
                Task_inst_id_map.add id data
                  sd.store.task_inst_uncompleted_store;
            };
        } )
      |> Id.add_task_inst_id id

    let add_task_inst_completed (id : Task_.task_inst_id)
        (data : Task_.task_inst_data) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_completed_store =
                Task_inst_id_map.add id data sd.store.task_inst_completed_store;
            };
        } )
      |> Id.add_task_inst_id id

    let add_task_inst_discarded (id : Task_.task_inst_id)
        (data : Task_.task_inst_data) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_discarded_store =
                Task_inst_id_map.add id data sd.store.task_inst_discarded_store;
            };
        } )
      |> Id.add_task_inst_id id

    (*$*)
  end

  module To_seq = struct
    (*$ #use "src/sched.cinaps";;

      print_task_inst_to_seq ();
    *)

    let task_inst_seq_uncompleted ((_, sd) : sched) : Task_.task_inst Seq.t =
      Task_inst_id_map.to_seq sd.store.task_inst_uncompleted_store

    let task_inst_seq_completed ((_, sd) : sched) : Task_.task_inst Seq.t =
      Task_inst_id_map.to_seq sd.store.task_inst_completed_store

    let task_inst_seq_discarded ((_, sd) : sched) : Task_.task_inst Seq.t =
      Task_inst_id_map.to_seq sd.store.task_inst_discarded_store

    let task_inst_seq_all ((_, sd) : sched) : Task_.task_inst Seq.t =
      OSeq.append
        (OSeq.append
           (Task_inst_id_map.to_seq sd.store.task_inst_uncompleted_store)
           (Task_inst_id_map.to_seq sd.store.task_inst_completed_store))
        (Task_inst_id_map.to_seq sd.store.task_inst_discarded_store)

    (*$*)
  end

  module Find = struct
    (*$ #use "src/sched.cinaps";;

      print_task_inst_find ();
      print_task_inst_ids_find_by_task_id ();
      print_task_inst_seq_find_by_task_id ()
    *)

    let find_task_inst_uncompleted_opt (id : Task_.task_inst_id)
        ((_, sd) : sched) : Task_.task_inst_data option =
      Task_inst_id_map.find_opt id sd.store.task_inst_uncompleted_store

    let find_task_inst_completed_opt (id : Task_.task_inst_id) ((_, sd) : sched)
      : Task_.task_inst_data option =
      Task_inst_id_map.find_opt id sd.store.task_inst_completed_store

    let find_task_inst_discarded_opt (id : Task_.task_inst_id) ((_, sd) : sched)
      : Task_.task_inst_data option =
      Task_inst_id_map.find_opt id sd.store.task_inst_discarded_store

    let find_task_inst_any_with_status_opt (id : Task_.task_inst_id)
        (sched : sched) : (Task_.task_inst_data * task_related_status) option =
      match find_task_inst_uncompleted_opt id sched with
      | Some x -> Some (x, `Uncompleted)
      | None -> (
          match find_task_inst_completed_opt id sched with
          | Some x -> Some (x, `Completed)
          | None -> (
              match find_task_inst_discarded_opt id sched with
              | Some x -> Some (x, `Discarded)
              | None -> None ) )

    let find_task_inst_any_opt (id : Task_.task_inst_id) (sched : sched) :
      Task_.task_inst_data option =
      find_task_inst_any_with_status_opt id sched
      |> Option.map (fun (x, _status) -> x)

    let find_task_inst_ids_by_task_id (id : Task_.task_id) (sched : sched) :
      Task_.task_inst_id Seq.t =
      Id.task_inst_id_seq_of_task_id sched id

    let find_task_inst_seq_uncompleted_by_task_id (id : Task_.task_id)
        (sched : sched) : Task_.task_inst Seq.t =
      find_task_inst_ids_by_task_id id sched
      |> Seq.filter_map (fun task_inst_id ->
          find_task_inst_uncompleted_opt task_inst_id sched
          |> Option.map (fun task_inst_data ->
              (task_inst_id, task_inst_data)))

    let find_task_inst_seq_completed_by_task_id (id : Task_.task_id)
        (sched : sched) : Task_.task_inst Seq.t =
      find_task_inst_ids_by_task_id id sched
      |> Seq.filter_map (fun task_inst_id ->
          find_task_inst_completed_opt task_inst_id sched
          |> Option.map (fun task_inst_data ->
              (task_inst_id, task_inst_data)))

    let find_task_inst_seq_discarded_by_task_id (id : Task_.task_id)
        (sched : sched) : Task_.task_inst Seq.t =
      find_task_inst_ids_by_task_id id sched
      |> Seq.filter_map (fun task_inst_id ->
          find_task_inst_discarded_opt task_inst_id sched
          |> Option.map (fun task_inst_data ->
              (task_inst_id, task_inst_data)))

    let find_task_inst_seq_any_with_status_by_task_id (id : Task_.task_id)
        (sched : sched) : (Task_.task_inst * task_related_status) Seq.t =
      find_task_inst_ids_by_task_id id sched
      |> Seq.filter_map (fun task_inst_id ->
          find_task_inst_any_with_status_opt task_inst_id sched
          |> Option.map (fun (x, status) -> ((task_inst_id, x), status)))

    let find_task_inst_seq_any_by_task_id (id : Task_.task_id) (sched : sched) :
      Task_.task_inst Seq.t =
      find_task_inst_ids_by_task_id id sched
      |> Seq.filter_map (fun task_inst_id ->
          find_task_inst_any_opt task_inst_id sched
          |> Option.map (fun x -> (task_inst_id, x)))

    (*$*)
  end

  module Status = struct
    let get_task_inst_status (id : Task_.task_inst_id) (sched : sched) :
      task_related_status option =
      Find.find_task_inst_any_with_status_opt id sched
      |> Option.map (fun (_, status) -> status)
  end

  module Remove = struct
    (*$ #use "src/sched.cinaps";;

      print_task_inst_remove ();
      (* print_task_inst_remove_strict (); *)
      print_task_inst_remove_seq ();
    *)

    let remove_task_inst_uncompleted ?(remove_children_task_segs : bool = true)
        (id : Task_.task_inst_id) (sched : sched) : sched =
      let children_task_seg_ids = Id.task_seg_id_seq_of_task_inst_id sched id in
      sched
      |> (fun sched ->
          if remove_children_task_segs then
            Task_seg.Remove.remove_task_seg_uncompleted_seq
              children_task_seg_ids sched
          else sched)
      |> (fun (sid, sd) ->
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  task_inst_uncompleted_store =
                    Task_inst_id_map.remove id
                      sd.store.task_inst_uncompleted_store;
                  task_inst_id_to_task_seg_ids =
                    Task_inst_id_map.remove id
                      sd.store.task_inst_id_to_task_seg_ids;
                };
            } ))
      |> Id.remove_task_inst_id id

    let remove_task_inst_completed ?(remove_children_task_segs : bool = true)
        (id : Task_.task_inst_id) (sched : sched) : sched =
      let children_task_seg_ids = Id.task_seg_id_seq_of_task_inst_id sched id in
      sched
      |> (fun sched ->
          if remove_children_task_segs then
            Task_seg.Remove.remove_task_seg_completed_seq children_task_seg_ids
              sched
          else sched)
      |> (fun (sid, sd) ->
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  task_inst_completed_store =
                    Task_inst_id_map.remove id
                      sd.store.task_inst_completed_store;
                  task_inst_id_to_task_seg_ids =
                    Task_inst_id_map.remove id
                      sd.store.task_inst_id_to_task_seg_ids;
                };
            } ))
      |> Id.remove_task_inst_id id

    let remove_task_inst_discarded ?(remove_children_task_segs : bool = true)
        (id : Task_.task_inst_id) (sched : sched) : sched =
      let children_task_seg_ids = Id.task_seg_id_seq_of_task_inst_id sched id in
      sched
      |> (fun sched ->
          if remove_children_task_segs then
            Task_seg.Remove.remove_task_seg_discarded_seq children_task_seg_ids
              sched
          else sched)
      |> (fun (sid, sd) ->
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  task_inst_discarded_store =
                    Task_inst_id_map.remove id
                      sd.store.task_inst_discarded_store;
                  task_inst_id_to_task_seg_ids =
                    Task_inst_id_map.remove id
                      sd.store.task_inst_id_to_task_seg_ids;
                };
            } ))
      |> Id.remove_task_inst_id id

    let remove_task_inst_all ?(remove_children_task_segs : bool = true)
        (id : Task_.task_inst_id) (sched : sched) : sched =
      sched
      |> remove_task_inst_uncompleted ~remove_children_task_segs id
      |> remove_task_inst_completed ~remove_children_task_segs id
      |> remove_task_inst_discarded ~remove_children_task_segs id

    let remove_task_inst_uncompleted_seq
        ?(remove_children_task_segs : bool = true)
        (ids : Task_.task_inst_id Seq.t) (sched : sched) : sched =
      Seq.fold_left
        (fun sched id ->
           remove_task_inst_uncompleted ~remove_children_task_segs id sched)
        sched ids

    let remove_task_inst_completed_seq
        ?(remove_children_task_segs : bool = true)
        (ids : Task_.task_inst_id Seq.t) (sched : sched) : sched =
      Seq.fold_left
        (fun sched id ->
           remove_task_inst_completed ~remove_children_task_segs id sched)
        sched ids

    let remove_task_inst_discarded_seq
        ?(remove_children_task_segs : bool = true)
        (ids : Task_.task_inst_id Seq.t) (sched : sched) : sched =
      Seq.fold_left
        (fun sched id ->
           remove_task_inst_discarded ~remove_children_task_segs id sched)
        sched ids

    (*$*)
  end

  module Move = struct
    let move_task_inst_and_task_segs_internal
        ~(add_task_inst :
            Task_.task_inst_id -> Task_.task_inst_data -> sched -> sched)
        ~(move_task_seg_by_id : Task_.task_seg_id -> sched -> sched)
        (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =
      match Find.find_task_inst_any_opt task_inst_id sched with
      | None -> sched
      | Some task_inst_data ->
        let task_seg_ids =
          Task_seg.Find.find_task_seg_ids_by_task_inst_id task_inst_id sched
        in
        sched
        |> Remove.remove_task_inst_all ~remove_children_task_segs:false
          task_inst_id
        |> add_task_inst task_inst_id task_inst_data
        |> fun sched ->
        Seq.fold_left
          (fun sched task_seg_id -> move_task_seg_by_id task_seg_id sched)
          sched task_seg_ids

    let move_task_inst_to_uncompleted (task_inst_id : Task_.task_inst_id)
        (sched : sched) : sched =
      match Find.find_task_inst_any_opt task_inst_id sched with
      | None -> sched
      | Some task_inst_data ->
        sched
        |> Remove.remove_task_inst_all ~remove_children_task_segs:false
          task_inst_id
        |> Add.add_task_inst_uncompleted task_inst_id task_inst_data

    let move_task_inst_to_completed (task_inst_id : Task_.task_inst_id)
        (sched : sched) : sched =
      move_task_inst_and_task_segs_internal
        ~add_task_inst:Add.add_task_inst_completed
        ~move_task_seg_by_id:Task_seg.Move.move_task_seg_to_completed
        task_inst_id sched

    let move_task_inst_to_discarded (task_inst_id : Task_.task_inst_id)
        (sched : sched) : sched =
      move_task_inst_and_task_segs_internal
        ~add_task_inst:Add.add_task_inst_discarded
        ~move_task_seg_by_id:Task_seg.Move.move_task_seg_to_discarded
        task_inst_id sched
  end
end

module Task = struct
  module Add = struct
    let add_task ~(parent_user_id : Task_.user_id) (data : Task_.task_data)
        (task_inst_data_list : Task_.task_inst_data list) ((sid, sd) : sched) :
      Task_.task * Task_.task_inst list * sched =
      let parent_task_id, (sid, sd) =
        Id.get_new_task_id parent_user_id (sid, sd)
      in
      let sd =
        {
          sd with
          store =
            {
              sd.store with
              task_uncompleted_store =
                Task_id_map.add parent_task_id data
                  sd.store.task_uncompleted_store;
            };
        }
      in
      let inst_list, (sid, sd) =
        Task_inst.Add.add_task_inst_list ~parent_task_id task_inst_data_list
          (sid, sd)
      in
      ((parent_task_id, data), inst_list, (sid, sd))

    (*$ #use "src/sched.cinaps";;

      print_task_add ();
    *)

    let add_task_uncompleted (id : Task_.task_id) (data : Task_.task_data)
        ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_uncompleted_store =
                Task_id_map.add id data sd.store.task_uncompleted_store;
            };
        } )
      |> Id.add_task_id id

    let add_task_completed (id : Task_.task_id) (data : Task_.task_data)
        ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_completed_store =
                Task_id_map.add id data sd.store.task_completed_store;
            };
        } )
      |> Id.add_task_id id

    let add_task_discarded (id : Task_.task_id) (data : Task_.task_data)
        ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_discarded_store =
                Task_id_map.add id data sd.store.task_discarded_store;
            };
        } )
      |> Id.add_task_id id

    (*$*)
  end

  module To_seq = struct
    (*$ #use "src/sched.cinaps";;

      print_task_to_seq ()
    *)

    let task_seq_uncompleted ((_, sd) : sched) : Task_.task Seq.t =
      Task_id_map.to_seq sd.store.task_uncompleted_store

    let task_seq_completed ((_, sd) : sched) : Task_.task Seq.t =
      Task_id_map.to_seq sd.store.task_completed_store

    let task_seq_discarded ((_, sd) : sched) : Task_.task Seq.t =
      Task_id_map.to_seq sd.store.task_discarded_store

    let task_seq_all ((_, sd) : sched) : Task_.task Seq.t =
      OSeq.append
        (OSeq.append
           (Task_id_map.to_seq sd.store.task_uncompleted_store)
           (Task_id_map.to_seq sd.store.task_completed_store))
        (Task_id_map.to_seq sd.store.task_discarded_store)

    (*$*)
  end

  module Find = struct
    (*$ #use "src/sched.cinaps";;

      print_task_find ()
    *)

    let find_task_uncompleted_opt (id : Task_.task_id) ((_, sd) : sched) :
      Task_.task_data option =
      Task_id_map.find_opt id sd.store.task_uncompleted_store

    let find_task_completed_opt (id : Task_.task_id) ((_, sd) : sched) :
      Task_.task_data option =
      Task_id_map.find_opt id sd.store.task_completed_store

    let find_task_discarded_opt (id : Task_.task_id) ((_, sd) : sched) :
      Task_.task_data option =
      Task_id_map.find_opt id sd.store.task_discarded_store

    let find_task_any_with_status_opt (id : Task_.task_id) (sched : sched) :
      (Task_.task_data * task_related_status) option =
      match find_task_uncompleted_opt id sched with
      | Some x -> Some (x, `Uncompleted)
      | None -> (
          match find_task_completed_opt id sched with
          | Some x -> Some (x, `Completed)
          | None -> (
              match find_task_discarded_opt id sched with
              | Some x -> Some (x, `Discarded)
              | None -> None ) )

    let find_task_any_opt (id : Task_.task_id) (sched : sched) :
      Task_.task_data option =
      find_task_any_with_status_opt id sched
      |> Option.map (fun (x, _status) -> x)

    (*$*)
  end

  module Status = struct
    let get_task_status (id : Task_.task_id) (sched : sched) :
      task_related_status option =
      Find.find_task_any_with_status_opt id sched
      |> Option.map (fun (_, status) -> status)
  end

  module Remove = struct
    (*$ #use "src/sched.cinaps";;

      print_task_remove ();
      (* print_task_remove_strict (); *)
    *)

    let remove_task_uncompleted ?(remove_children_task_insts : bool = true)
        ?(remove_children_task_segs : bool = true) (id : Task_.task_id)
        (sched : sched) : sched =
      let children_task_inst_ids = Id.task_inst_id_seq_of_task_id sched id in
      sched
      |> (fun sched ->
          if remove_children_task_insts then
            Task_inst.Remove.remove_task_inst_uncompleted_seq
              ~remove_children_task_segs children_task_inst_ids sched
          else sched)
      |> (fun (sid, sd) ->
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  task_uncompleted_store =
                    Task_id_map.remove id sd.store.task_uncompleted_store;
                  task_id_to_task_inst_ids =
                    Task_id_map.remove id sd.store.task_id_to_task_inst_ids;
                };
            } ))
      |> Id.remove_task_id id

    let remove_task_completed ?(remove_children_task_insts : bool = true)
        ?(remove_children_task_segs : bool = true) (id : Task_.task_id)
        (sched : sched) : sched =
      let children_task_inst_ids = Id.task_inst_id_seq_of_task_id sched id in
      sched
      |> (fun sched ->
          if remove_children_task_insts then
            Task_inst.Remove.remove_task_inst_completed_seq
              ~remove_children_task_segs children_task_inst_ids sched
          else sched)
      |> (fun (sid, sd) ->
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  task_completed_store =
                    Task_id_map.remove id sd.store.task_completed_store;
                  task_id_to_task_inst_ids =
                    Task_id_map.remove id sd.store.task_id_to_task_inst_ids;
                };
            } ))
      |> Id.remove_task_id id

    let remove_task_discarded ?(remove_children_task_insts : bool = true)
        ?(remove_children_task_segs : bool = true) (id : Task_.task_id)
        (sched : sched) : sched =
      let children_task_inst_ids = Id.task_inst_id_seq_of_task_id sched id in
      sched
      |> (fun sched ->
          if remove_children_task_insts then
            Task_inst.Remove.remove_task_inst_discarded_seq
              ~remove_children_task_segs children_task_inst_ids sched
          else sched)
      |> (fun (sid, sd) ->
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  task_discarded_store =
                    Task_id_map.remove id sd.store.task_discarded_store;
                  task_id_to_task_inst_ids =
                    Task_id_map.remove id sd.store.task_id_to_task_inst_ids;
                };
            } ))
      |> Id.remove_task_id id

    let remove_task_all ?(remove_children_task_insts : bool = true)
        ?(remove_children_task_segs : bool = true) (id : Task_.task_id)
        (sched : sched) : sched =
      sched
      |> remove_task_uncompleted ~remove_children_task_insts
        ~remove_children_task_segs id
      |> remove_task_completed ~remove_children_task_insts
        ~remove_children_task_segs id
      |> remove_task_discarded ~remove_children_task_insts
        ~remove_children_task_segs id

    (*$*)
  end

  module Move = struct
    let move_task_and_task_inst_and_task_segs_internal
        ~(add_task : Task_.task_id -> Task_.task_data -> sched -> sched)
        ~(move_task_inst_by_id : Task_.task_inst_id -> sched -> sched)
        (task_id : Task_.task_id) (sched : sched) : sched =
      match Find.find_task_any_opt task_id sched with
      | None -> sched
      | Some task_data ->
        let task_inst_ids =
          Task_inst.Find.find_task_inst_ids_by_task_id task_id sched
        in
        sched
        |> Remove.remove_task_all ~remove_children_task_insts:false
          ~remove_children_task_segs:false task_id
        |> add_task task_id task_data
        |> fun sched ->
        Seq.fold_left
          (fun sched task_inst_id -> move_task_inst_by_id task_inst_id sched)
          sched task_inst_ids

    let move_task_to_uncompleted (task_id : Task_.task_id) (sched : sched) :
      sched =
      match Find.find_task_any_opt task_id sched with
      | None -> sched
      | Some task_data ->
        sched
        |> Remove.remove_task_all ~remove_children_task_insts:false
          ~remove_children_task_segs:false task_id
        |> Add.add_task_uncompleted task_id task_data

    let move_task_to_completed (task_id : Task_.task_id) (sched : sched) : sched
      =
      move_task_and_task_inst_and_task_segs_internal
        ~add_task:Add.add_task_completed
        ~move_task_inst_by_id:Task_inst.Move.move_task_inst_to_completed task_id
        sched

    let move_task_to_discarded (task_id : Task_.task_id) (sched : sched) : sched
      =
      move_task_and_task_inst_and_task_segs_internal
        ~add_task:Add.add_task_discarded
        ~move_task_inst_by_id:Task_inst.Move.move_task_inst_to_discarded task_id
        sched
  end
end

module Progress = struct
  module Add = struct
    (*$
      let l = [ "seg"; "inst" ] in

      List.iter
        (fun s ->
           Printf.printf
             "let add_task_%s_progress_chunk (id : Task_.task_%s_id) (chunk : \
              int64 * int64) ((sid, sd) : sched) : sched =\n"
             s s;
           Printf.printf "(sid, \n";
           Printf.printf " { sd with store = {\n";
           Printf.printf "   sd.store with task_%s_id_to_progress =\n" s;
           Printf.printf "     Task_%s_id_map.update id\n" s;
           Printf.printf "       (fun progress ->\n";
           Printf.printf "          let open Task_ in\n";
           Printf.printf "          match progress with\n";
           Printf.printf
             "          | None -> Some { chunks = Int64_int64_set.empty }\n";
           Printf.printf
             "          | Some progress -> Some { chunks = Int64_int64_set.add \
              chunk progress.chunks })\n";
           Printf.printf "     sd.store.task_%s_id_to_progress\n" s;
           Printf.printf "}})\n")
        l
    *)

    let add_task_seg_progress_chunk (id : Task_.task_seg_id)
        (chunk : int64 * int64) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_id_to_progress =
                Task_seg_id_map.update id
                  (fun progress ->
                     let open Task_ in
                     match progress with
                     | None -> Some { chunks = Int64_int64_set.empty }
                     | Some progress ->
                       Some
                         { chunks = Int64_int64_set.add chunk progress.chunks })
                  sd.store.task_seg_id_to_progress;
            };
        } )

    let add_task_inst_progress_chunk (id : Task_.task_inst_id)
        (chunk : int64 * int64) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_id_to_progress =
                Task_inst_id_map.update id
                  (fun progress ->
                     let open Task_ in
                     match progress with
                     | None -> Some { chunks = Int64_int64_set.empty }
                     | Some progress ->
                       Some
                         { chunks = Int64_int64_set.add chunk progress.chunks })
                  sd.store.task_inst_id_to_progress;
            };
        } )

    (*$*)
  end

  module Find = struct
    (*$
      let l = [ "seg"; "inst" ] in

      List.iter
        (fun s ->
           Printf.printf
             "let find_task_%s_progress (id : Task_.task_%s_id) ((_sid, sd) : \
              sched) : Task_.progress option =\n"
             s s;
           Printf.printf
             "Task_%s_id_map.find_opt id sd.store.task_%s_id_to_progress\n" s s;

           if s = "seg" then (
             Printf.printf
               "let find_task_seg_progress_seq_by_task_inst_id (id : \
                Task_.task_inst_id) (sched : sched) : Task_.progress Seq.t =\n";
             Printf.printf
               "Task_seg.Find.find_task_seg_ids_by_task_inst_id id sched\n";
             Printf.printf
               "|> Seq.filter_map (fun task_seg_id -> find_task_seg_progress \
                task_seg_id sched)\n" );

           Printf.printf
             "let find_task_%s_progress_seq_by_task_id (task_id : \
              Task_.task_id) (sched : sched) : Task_.progress Seq.t =\n"
             s;
           Printf.printf
             "Task_%s.Find.find_task_%s_ids_by_task_id task_id sched\n" s s;
           Printf.printf
             "|> Seq.filter_map (fun id -> find_task_%s_progress id sched)\n" s;

           Printf.printf
             "let find_task_%s_progress_chunk_set (id : Task_.task_%s_id) \
              ((_sid, sd) : sched) : Int64_int64_set.t =\n"
             s s;
           Printf.printf
             "match Task_%s_id_map.find_opt id sd.store.task_%s_id_to_progress \
              with\n"
             s s;
           Printf.printf "| None -> Int64_int64_set.empty\n";
           Printf.printf "| Some progress -> progress.chunks\n";

           Printf.printf
             "let find_task_%s_progress_chunk_seq (id : Task_.task_%s_id) \
              (sched : sched) : (int64 * int64) Seq.t =\n"
             s s;
           Printf.printf "find_task_%s_progress_chunk_set id sched\n" s;
           Printf.printf "|> Int64_int64_set.to_seq\n";

           if s = "seg" then (
             Printf.printf
               "let find_task_seg_progress_chunk_seq_by_task_inst_id \
                (task_inst_id : Task_.task_inst_id) (sched : sched) : (int64 * \
                int64) Seq.t =\n";
             Printf.printf
               "Task_seg.Find.find_task_seg_ids_by_task_inst_id task_inst_id \
                sched\n";
             Printf.printf
               "|> Seq.flat_map (fun id -> find_task_seg_progress_chunk_seq id \
                sched)\n" );

           Printf.printf
             "let find_task_%s_progress_chunk_seq_by_task_id (task_id : \
              Task_.task_id) (sched : sched) : (int64 * int64) Seq.t =\n"
             s;
           Printf.printf
             "Task_%s.Find.find_task_%s_ids_by_task_id task_id sched\n" s s;
           Printf.printf
             "|> Seq.flat_map (fun id -> find_task_%s_progress_chunk_seq id \
              sched)"
             s)
        l
    *)

    let find_task_seg_progress (id : Task_.task_seg_id) ((_sid, sd) : sched) :
      Task_.progress option =
      Task_seg_id_map.find_opt id sd.store.task_seg_id_to_progress

    let find_task_seg_progress_seq_by_task_inst_id (id : Task_.task_inst_id)
        (sched : sched) : Task_.progress Seq.t =
      Task_seg.Find.find_task_seg_ids_by_task_inst_id id sched
      |> Seq.filter_map (fun task_seg_id ->
          find_task_seg_progress task_seg_id sched)

    let find_task_seg_progress_seq_by_task_id (task_id : Task_.task_id)
        (sched : sched) : Task_.progress Seq.t =
      Task_seg.Find.find_task_seg_ids_by_task_id task_id sched
      |> Seq.filter_map (fun id -> find_task_seg_progress id sched)

    let find_task_seg_progress_chunk_set (id : Task_.task_seg_id)
        ((_sid, sd) : sched) : Int64_int64_set.t =
      match Task_seg_id_map.find_opt id sd.store.task_seg_id_to_progress with
      | None -> Int64_int64_set.empty
      | Some progress -> progress.chunks

    let find_task_seg_progress_chunk_seq (id : Task_.task_seg_id)
        (sched : sched) : (int64 * int64) Seq.t =
      find_task_seg_progress_chunk_set id sched |> Int64_int64_set.to_seq

    let find_task_seg_progress_chunk_seq_by_task_inst_id
        (task_inst_id : Task_.task_inst_id) (sched : sched) :
      (int64 * int64) Seq.t =
      Task_seg.Find.find_task_seg_ids_by_task_inst_id task_inst_id sched
      |> Seq.flat_map (fun id -> find_task_seg_progress_chunk_seq id sched)

    let find_task_seg_progress_chunk_seq_by_task_id (task_id : Task_.task_id)
        (sched : sched) : (int64 * int64) Seq.t =
      Task_seg.Find.find_task_seg_ids_by_task_id task_id sched
      |> Seq.flat_map (fun id -> find_task_seg_progress_chunk_seq id sched)

    let find_task_inst_progress (id : Task_.task_inst_id) ((_sid, sd) : sched) :
      Task_.progress option =
      Task_inst_id_map.find_opt id sd.store.task_inst_id_to_progress

    let find_task_inst_progress_seq_by_task_id (task_id : Task_.task_id)
        (sched : sched) : Task_.progress Seq.t =
      Task_inst.Find.find_task_inst_ids_by_task_id task_id sched
      |> Seq.filter_map (fun id -> find_task_inst_progress id sched)

    let find_task_inst_progress_chunk_set (id : Task_.task_inst_id)
        ((_sid, sd) : sched) : Int64_int64_set.t =
      match Task_inst_id_map.find_opt id sd.store.task_inst_id_to_progress with
      | None -> Int64_int64_set.empty
      | Some progress -> progress.chunks

    let find_task_inst_progress_chunk_seq (id : Task_.task_inst_id)
        (sched : sched) : (int64 * int64) Seq.t =
      find_task_inst_progress_chunk_set id sched |> Int64_int64_set.to_seq

    let find_task_inst_progress_chunk_seq_by_task_id (task_id : Task_.task_id)
        (sched : sched) : (int64 * int64) Seq.t =
      Task_inst.Find.find_task_inst_ids_by_task_id task_id sched
      |> Seq.flat_map (fun id -> find_task_inst_progress_chunk_seq id sched)

    (*$*)
  end

  module Remove = struct
    (*$
      let l = [ "seg"; "inst" ] in

      List.iter
        (fun s ->
           Printf.printf
             "let remove_task_%s_progress_chunk (id : Task_.task_%s_id) (chunk \
              : int64 * int64) ((sid, sd) : sched) : sched =\n"
             s s;
           Printf.printf "(sid, { sd with store = { sd.store with\n";
           Printf.printf "  task_%s_id_to_progress =\n" s;
           Printf.printf "  Task_%s_id_map.update id\n" s;
           Printf.printf "  (fun progress ->\n";
           Printf.printf "     let open Task_ in\n";
           Printf.printf "     match progress with\n";
           Printf.printf "     | None -> None\n";
           Printf.printf "     | Some progress ->\n";
           Printf.printf
             "       Some { chunks = Int64_int64_set.remove chunk \
              progress.chunks }\n";
           Printf.printf "  )\n";
           Printf.printf "  sd.store.task_%s_id_to_progress\n" s;
           Printf.printf "} } )\n")
        l
    *)

    let remove_task_seg_progress_chunk (id : Task_.task_seg_id)
        (chunk : int64 * int64) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_seg_id_to_progress =
                Task_seg_id_map.update id
                  (fun progress ->
                     let open Task_ in
                     match progress with
                     | None -> None
                     | Some progress ->
                       Some
                         {
                           chunks =
                             Int64_int64_set.remove chunk progress.chunks;
                         })
                  sd.store.task_seg_id_to_progress;
            };
        } )

    let remove_task_inst_progress_chunk (id : Task_.task_inst_id)
        (chunk : int64 * int64) ((sid, sd) : sched) : sched =
      ( sid,
        {
          sd with
          store =
            {
              sd.store with
              task_inst_id_to_progress =
                Task_inst_id_map.update id
                  (fun progress ->
                     let open Task_ in
                     match progress with
                     | None -> None
                     | Some progress ->
                       Some
                         {
                           chunks =
                             Int64_int64_set.remove chunk progress.chunks;
                         })
                  sd.store.task_inst_id_to_progress;
            };
        } )

    (*$*)
  end
end

module Agenda = struct
  module Add = struct
    let add_task_seg_place
        ((task_seg_id, start, end_exc) as task_seg_place : Task_.task_seg_place)
        ((sid, sd) : sched) : sched =
      let indexed_by_task_seg_id =
        Task_seg_id_map.add task_seg_id (start, end_exc)
          sd.agenda.indexed_by_task_seg_id
      in
      let indexed_by_start =
        Int64_map.update start
          (fun bucket ->
             Some
               (Task_seg_id_set.add task_seg_id
                  ( match bucket with
                    | None -> Task_seg_id_set.empty
                    | Some s -> s )))
          sd.agenda.indexed_by_start
      in
      let indexed_by_end_exc =
        Int64_map.update end_exc
          (fun bucket ->
             Some
               (Task_seg_id_set.add task_seg_id
                  ( match bucket with
                    | None -> Task_seg_id_set.empty
                    | Some s -> s )))
          sd.agenda.indexed_by_end_exc
      in
      ( sid,
        {
          sd with
          agenda =
            { indexed_by_task_seg_id; indexed_by_start; indexed_by_end_exc };
        } )
      |> Task_seg.Add.add_task_seg_via_task_seg_place task_seg_place

    let add_task_seg_place_list (task_seg_places : Task_.task_seg_place list)
        (sched : sched) : sched =
      List.fold_left
        (fun acc task_seg_place -> add_task_seg_place task_seg_place acc)
        sched task_seg_places

    let add_task_seg_place_seq (task_seg_places : Task_.task_seg_place Seq.t)
        (sched : sched) : sched =
      Seq.fold_left
        (fun acc task_seg_place -> add_task_seg_place task_seg_place acc)
        sched task_seg_places
  end

  module Range = struct
    let task_seg_id_set ~(start : int64 option) ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool)
        ~(include_task_seg_place_ending_within_time_slot : bool)
        ((_, sd) : sched) : Task_seg_id_set.t =
      let start_within_range =
        let m =
          Int64_map_utils.range ~start ~end_exc sd.agenda.indexed_by_start
        in
        Int64_map.fold
          (fun _k acc s -> Task_seg_id_set.union acc s)
          m Task_seg_id_set.empty
      in
      let end_exc_within_range =
        let m =
          let start = Option.map Int64.succ start in
          Int64_map_utils.range ~start ~end_exc sd.agenda.indexed_by_end_exc
        in
        Int64_map.fold
          (fun _k acc s -> Task_seg_id_set.union acc s)
          m Task_seg_id_set.empty
      in
      match
        ( include_task_seg_place_starting_within_time_slot,
          include_task_seg_place_ending_within_time_slot )
      with
      | true, true ->
        Task_seg_id_set.union start_within_range end_exc_within_range
      | true, false -> start_within_range
      | false, true -> end_exc_within_range
      | false, false ->
        Task_seg_id_set.inter start_within_range end_exc_within_range

    let task_seg_place_set ~(start : int64 option) ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool)
        ~(include_task_seg_place_ending_within_time_slot : bool)
        ((_, sd) as sched : sched) : Task_seg_place_set.t =
      task_seg_id_set ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched
      |> Task_seg_id_set.to_seq
      |> Seq.map (fun task_seg_id ->
          let place_start, place_end_exc =
            Task_seg_id_map.find task_seg_id sd.agenda.indexed_by_task_seg_id
          in
          (task_seg_id, place_start, place_end_exc))
      |> Task_seg_place_set.of_seq
  end

  module Internal = struct
    let task_seg_ids ~(start : int64 option) ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_id Seq.t =
      let include_task_seg_place_starting_within_time_slot =
        Option.fold ~none:false
          ~some:(fun x -> x)
          include_task_seg_place_starting_within_time_slot
      in
      let include_task_seg_place_ending_within_time_slot =
        Option.fold ~none:false
          ~some:(fun x -> x)
          include_task_seg_place_ending_within_time_slot
      in
      Range.task_seg_id_set ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched
      |> Task_seg_id_set.to_seq

    let task_seg_places ~(start : int64 option) ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      let include_task_seg_place_starting_within_time_slot =
        Option.fold ~none:false
          ~some:(fun x -> x)
          include_task_seg_place_starting_within_time_slot
      in
      let include_task_seg_place_ending_within_time_slot =
        Option.fold ~none:false
          ~some:(fun x -> x)
          include_task_seg_place_ending_within_time_slot
      in
      Range.task_seg_place_set ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched
      |> Task_seg_place_set.to_seq

    let filter_task_seg_place_seq ~(start : int64 option)
        ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (f : Task_.task_seg_place -> bool) (sched : sched) :
      Task_.task_seg_place Seq.t =
      task_seg_places ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched
      |> Seq.filter f

    let task_seg_place_is (sched : sched) (status : task_related_status)
        ((task_seg_id, _, _) : Task_.task_seg_place) : bool =
      Task_seg.Status.get_task_seg_status task_seg_id sched = Some status

    let task_seg_place_uncompleted ~(start : int64 option)
        ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      filter_task_seg_place_seq ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot
        (task_seg_place_is sched `Uncompleted)
        sched

    let task_seg_place_completed ~(start : int64 option)
        ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      filter_task_seg_place_seq ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot
        (task_seg_place_is sched `Completed)
        sched

    let task_seg_place_discarded ~(start : int64 option)
        ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      filter_task_seg_place_seq ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot
        (task_seg_place_is sched `Discarded)
        sched

    let task_seg_place_all ~(start : int64 option) ~(end_exc : int64 option)
        ~(include_task_seg_place_starting_within_time_slot : bool option)
        ~(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      task_seg_places ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched

    let time_slots_of_task_seg_places ~(start : int64 option)
        ~(end_exc : int64 option) (task_seg_places : Task_.task_seg_place Seq.t)
      : Time_slot.t Seq.t =
      task_seg_places
      |> Seq.map (fun (_, start, end_exc) -> (start, end_exc))
      |> (fun l ->
          Option.fold ~none:l
            ~some:(fun start -> Time_slots.Slice.slice ~start l)
            start)
      |> fun l ->
      Option.fold ~none:l
        ~some:(fun end_exc -> Time_slots.Slice.slice ~end_exc l)
        end_exc

    let filter_non_paralleliazble_task_seg_places sched
        (s : Task_.task_seg_place Seq.t) =
      Seq.filter
        (fun (task_seg_id, _start, _end_exc) ->
           let task_id = Task_.Id.task_id_of_task_seg_id task_seg_id in
           let task_data =
             Task.Find.find_task_uncompleted_opt task_id sched |> Option.get
           in
           not task_data.parallelizable)
        s

    let get_occupied_time_slots_with_task_seg_place_count
        ~exclude_parallelizable_task_seg_places ~start ~end_exc (sched : sched)
      : ((int64 * int64) * int) Seq.t =
      task_seg_place_uncompleted ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot:(Some true)
        ~include_task_seg_place_ending_within_time_slot:(Some true) sched
      |> (fun s ->
          if exclude_parallelizable_task_seg_places then
            filter_non_paralleliazble_task_seg_places sched s
          else s)
      |> time_slots_of_task_seg_places ~start ~end_exc
      |> Time_slots.count_overlap
  end

  module Filter = struct
    let filter_task_seg_place_seq ?(start : int64 option)
        ?(end_exc : int64 option)
        ?(include_task_seg_place_starting_within_time_slot : bool option)
        ?(include_task_seg_place_ending_within_time_slot : bool option)
        (f : Task_.task_seg_place -> bool) (sched : sched) :
      Task_.task_seg_place Seq.t =
      Internal.filter_task_seg_place_seq ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot f sched
  end

  module To_seq = struct
    let task_seg_place_is (sched : sched) (status : task_related_status)
        ((task_seg_id, _, _) : Task_.task_seg_place) : bool =
      Task_seg.Status.get_task_seg_status task_seg_id sched = Some status

    let task_seg_place_uncompleted ?(start : int64 option)
        ?(end_exc : int64 option)
        ?(include_task_seg_place_starting_within_time_slot : bool option)
        ?(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      Internal.task_seg_place_uncompleted ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched

    let task_seg_place_completed ?(start : int64 option)
        ?(end_exc : int64 option)
        ?(include_task_seg_place_starting_within_time_slot : bool option)
        ?(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      Internal.task_seg_place_completed ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched

    let task_seg_place_discarded ?(start : int64 option)
        ?(end_exc : int64 option)
        ?(include_task_seg_place_starting_within_time_slot : bool option)
        ?(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      Internal.task_seg_place_discarded ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched

    let task_seg_place_all ?(start : int64 option) ?(end_exc : int64 option)
        ?(include_task_seg_place_starting_within_time_slot : bool option)
        ?(include_task_seg_place_ending_within_time_slot : bool option)
        (sched : sched) : Task_.task_seg_place Seq.t =
      Internal.task_seg_places ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot
        ~include_task_seg_place_ending_within_time_slot sched
  end

  module Find = struct
    let find_task_seg_place_seq_by_task_id (task_id : Task_.task_id)
        (sched : sched) : Task_.task_seg_place Seq.t =
      Filter.filter_task_seg_place_seq
        (fun (task_seg_id, _, _) ->
           Task_.Id.task_seg_id_matches_task_id task_seg_id task_id)
        sched

    let find_task_seg_place_seq_by_task_inst_id
        (task_inst_id : Task_.task_inst_id) (sched : sched) :
      Task_.task_seg_place Seq.t =
      Filter.filter_task_seg_place_seq
        (fun (task_seg_id, _, _) ->
           Task_.Id.task_seg_id_matches_task_inst_id task_seg_id task_inst_id)
        sched

    let find_task_seg_place_opt_by_task_seg_id (task_seg_id : Task_.task_seg_id)
        ((_, sd) : sched) : Task_.task_seg_place option =
      Task_seg_id_map.find_opt task_seg_id sd.agenda.indexed_by_task_seg_id
      |> Option.map (fun (place_start, place_end_exc) ->
          (task_seg_id, place_start, place_end_exc))
  end

  module Remove = struct
    let remove_task_seg_place
        ((task_seg_id, start, end_exc) : Task_.task_seg_place)
        ((sid, sd) : sched) : sched =
      let task_inst_id = Task_.Id.task_inst_id_of_task_seg_id task_seg_id in
      let indexed_by_task_seg_id =
        Task_seg_id_map.remove task_seg_id sd.agenda.indexed_by_task_seg_id
      in
      let indexed_by_start =
        Int64_map.update start
          (fun bucket ->
             Option.map
               (fun bucket -> Task_seg_id_set.remove task_seg_id bucket)
               bucket)
          sd.agenda.indexed_by_start
      in
      let indexed_by_end_exc =
        Int64_map.update end_exc
          (fun bucket ->
             Option.map
               (fun bucket -> Task_seg_id_set.remove task_seg_id bucket)
               bucket)
          sd.agenda.indexed_by_end_exc
      in
      let quota =
        Task_inst_id_map.update task_inst_id
          (Option.map (fun x -> x +^ (end_exc -^ start)))
          sd.store.quota
      in
      ( sid,
        {
          store = { sd.store with quota };
          agenda =
            { indexed_by_task_seg_id; indexed_by_start; indexed_by_end_exc };
        } )

    let remove_task_seg_place_seq
        (task_seg_place_seq : Task_.task_seg_place Seq.t) (sched : sched) :
      sched =
      Seq.fold_left
        (fun sched task_seg_place -> remove_task_seg_place task_seg_place sched)
        sched task_seg_place_seq

    let remove_task_seg_place_by_task_id task_id sched =
      let s = Find.find_task_seg_place_seq_by_task_id task_id sched in
      remove_task_seg_place_seq s sched

    let remove_task_seg_place_by_task_inst_id task_inst_id sched =
      let s = Find.find_task_seg_place_seq_by_task_inst_id task_inst_id sched in
      remove_task_seg_place_seq s sched

    let remove_task_seg_place_by_task_seg_id task_seg_id
        ((_, sd) as sched : sched) =
      match
        Task_seg_id_map.find_opt task_seg_id sd.agenda.indexed_by_task_seg_id
      with
      | None -> sched
      | Some (start, end_exc) ->
        remove_task_seg_place (task_seg_id, start, end_exc) sched
  end

  module Time_slot = struct
    let get_occupied_time_slots ?(exclude_parallelizable_task_seg_places = true)
        ?start ?end_exc (sched : sched) : (int64 * int64) Seq.t =
      Internal.task_seg_place_uncompleted ~start ~end_exc
        ~include_task_seg_place_starting_within_time_slot:(Some true)
        ~include_task_seg_place_ending_within_time_slot:(Some true) sched
      |> (fun s ->
          if exclude_parallelizable_task_seg_places then
            Internal.filter_non_paralleliazble_task_seg_places sched s
          else s)
      |> Internal.time_slots_of_task_seg_places ~start ~end_exc
      |> Time_slots.Normalize.normalize ~skip_sort:true

    let get_occupied_time_slots_with_task_seg_place_count
        ?(exclude_parallelizable_task_seg_places = true) ?start ?end_exc
        (sched : sched) : ((int64 * int64) * int) Seq.t =
      Internal.get_occupied_time_slots_with_task_seg_place_count
        ~exclude_parallelizable_task_seg_places ~start ~end_exc sched

    let get_occupied_time_slots_up_to_task_seg_place_count
        ?(exclude_parallelizable_task_seg_places = true) ?start ?end_exc
        ~(up_to_task_seg_place_count_inc : int) (sched : sched) =
      Internal.get_occupied_time_slots_with_task_seg_place_count
        ~exclude_parallelizable_task_seg_places ~start ~end_exc sched
      |> Seq.filter (fun (_time_slot, count) ->
          count <= up_to_task_seg_place_count_inc)
      |> Seq.map (fun (time_slot, _) -> time_slot)

    let get_free_time_slots ?(include_parallelizable_task_seg_places = true)
        ~start ~end_exc (sched : sched) : (int64 * int64) Seq.t =
      get_occupied_time_slots
        ~exclude_parallelizable_task_seg_places:
          include_parallelizable_task_seg_places ~start ~end_exc sched
      |> Time_slots.invert ~start ~end_exc

    let get_free_or_occupied_time_slots_up_to_task_seg_place_count
        ?(include_parallelizable_task_seg_places = true) ~start ~end_exc
        ~(up_to_task_seg_place_count_inc : int) (sched : sched) =
      Internal.get_occupied_time_slots_with_task_seg_place_count
        ~exclude_parallelizable_task_seg_places:
          include_parallelizable_task_seg_places ~start:(Some start)
        ~end_exc:(Some end_exc) sched
      |> Seq.filter (fun (_time_slot, count) ->
          count <= up_to_task_seg_place_count_inc)
      |> Seq.map (fun (time_slot, _) -> time_slot)
      |> Time_slots.Union.union
        (get_free_time_slots ~include_parallelizable_task_seg_places ~start
           ~end_exc sched)

    let task_seg_place_count_in_time_slot ~start ~end_exc (sched : sched) : int
      =
      let start = Some start in
      let end_exc = Some end_exc in
      let task_seg_places_in_range =
        Range.task_seg_place_set ~start ~end_exc
          ~include_task_seg_place_starting_within_time_slot:true
          ~include_task_seg_place_ending_within_time_slot:true sched
      in
      Task_seg_place_set.cardinal task_seg_places_in_range
  end
end

module Sched_req = struct
  module Add = struct
    let add_sched_req_data (sched_req_data : Sched_req_.sched_req_data)
        (sched : sched) : (Sched_req_.sched_req * sched, unit) result =
      if Sched_req_.Check.sched_req_data_is_valid sched_req_data then
        let sched_req_id, (sid, sd) = Id.get_new_sched_req_id sched in
        Ok
          ( (sched_req_id, sched_req_data),
            ( sid,
              {
                sd with
                store =
                  {
                    sd.store with
                    sched_req_pending_store =
                      Sched_req_id_map.add sched_req_id sched_req_data
                        sd.store.sched_req_pending_store;
                  };
              } ) )
      else Error ()

    let add_sched_req_data_list
        (sched_req_data_list : Sched_req_.sched_req_data list) (sched : sched) :
      (Sched_req_.sched_req list * sched, unit) result =
      if Sched_req_.Check.sched_req_data_list_is_valid sched_req_data_list then
        List.fold_left
          (fun (sched_reqs, sched) sched_req_data ->
             let sched_req, sched =
               add_sched_req_data sched_req_data sched |> Result.get_ok
             in
             (sched_req :: sched_reqs, sched))
          ([], sched) sched_req_data_list
        |> fun (l, s) -> (List.rev l, s) |> Result.ok
      else Error ()
  end

  module Partition = struct
    type 'a partition_based_on_time_point = {
      before : 'a Sched_req_id_map.t;
      after : 'a Sched_req_id_map.t;
      crossing : 'a Sched_req_id_map.t;
    }

    type 'a partition_based_on_time_slot = {
      fully_within : 'a Sched_req_id_map.t;
      starting_within : 'a Sched_req_id_map.t;
      ending_within : 'a Sched_req_id_map.t;
      outside : 'a Sched_req_id_map.t;
    }

    let partition_based_on_time_point_internal (type a)
        ~(f_get : sched -> a Sched_req_id_map.t)
        ~(f_before_time : int64 -> Sched_req_.sched_req_id * a -> bool)
        ~(f_after_time : int64 -> Sched_req_.sched_req_id * a -> bool)
        (x : int64) (sched : sched) : a partition_based_on_time_point =
      let before, crossing_or_after =
        Sched_req_id_map.partition
          (fun id data -> f_before_time x (id, data))
          (f_get sched)
      in
      let after, crossing =
        Sched_req_id_map.partition
          (fun id data -> f_after_time x (id, data))
          crossing_or_after
      in
      { before; after; crossing }

    let partition_based_on_time_slot_internal (type a)
        ~(f_get : sched -> a Sched_req_id_map.t)
        ~(f_fully_within_time_slot :
            start:int64 -> end_exc:int64 -> Sched_req_.sched_req_id * a -> bool)
        ~(f_starting_within_time_slot :
            start:int64 -> end_exc:int64 -> Sched_req_.sched_req_id * a -> bool)
        ~(f_ending_within_time_slot :
            start:int64 -> end_exc:int64 -> Sched_req_.sched_req_id * a -> bool)
        ~start ~end_exc (sched : sched) : a partition_based_on_time_slot =
      let fully_within, leftover =
        Sched_req_id_map.partition
          (fun id data -> f_fully_within_time_slot ~start ~end_exc (id, data))
          (f_get sched)
      in
      let starting_within, leftover =
        Sched_req_id_map.partition
          (fun id data ->
             f_starting_within_time_slot ~start ~end_exc (id, data))
          leftover
      in
      let ending_within, outside =
        Sched_req_id_map.partition
          (fun id data -> f_ending_within_time_slot ~start ~end_exc (id, data))
          leftover
      in
      { fully_within; starting_within; ending_within; outside }

    module Pending = struct
      let partition_based_on_time_point (x : int64) (sched : sched) :
        Sched_req_.sched_req_data partition_based_on_time_point =
        partition_based_on_time_point_internal
          ~f_get:(fun (_sid, sd) -> sd.store.sched_req_pending_store)
          ~f_before_time:Sched_req_.sched_req_or_record_before_time
          ~f_after_time:Sched_req_.sched_req_or_record_after_time x sched

      let partition_based_on_time_slot ~start ~end_exc (sched : sched) :
        Sched_req_.sched_req_data partition_based_on_time_slot =
        partition_based_on_time_slot_internal
          ~f_get:(fun (_sid, sd) -> sd.store.sched_req_pending_store)
          ~f_fully_within_time_slot:
            Sched_req_.sched_req_or_record_fully_within_time_slot
          ~f_starting_within_time_slot:
            Sched_req_.sched_req_or_record_starting_within_time_slot
          ~f_ending_within_time_slot:
            Sched_req_.sched_req_or_record_ending_within_time_slot ~start
          ~end_exc sched
    end

    module Record = struct
      let partition_based_on_time_point (x : int64) (sched : sched) :
        Sched_req_.sched_req_record_data partition_based_on_time_point =
        partition_based_on_time_point_internal
          ~f_get:(fun (_sid, sd) -> sd.store.sched_req_record_store)
          ~f_before_time:Sched_req_.sched_req_or_record_before_time
          ~f_after_time:Sched_req_.sched_req_or_record_after_time x sched

      let partition_based_on_time_slot ~start ~end_exc (sched : sched) :
        Sched_req_.sched_req_record_data partition_based_on_time_slot =
        partition_based_on_time_slot_internal
          ~f_get:(fun (_sid, sd) -> sd.store.sched_req_record_store)
          ~f_fully_within_time_slot:
            Sched_req_.sched_req_or_record_fully_within_time_slot
          ~f_starting_within_time_slot:
            Sched_req_.sched_req_or_record_starting_within_time_slot
          ~f_ending_within_time_slot:
            Sched_req_.sched_req_or_record_ending_within_time_slot ~start
          ~end_exc sched
    end
  end

  module To_seq_internal = struct
    let seq_internal (type a) ~(f_get : sched -> a Sched_req_id_map.t)
        ~(f_partition_based_on_time_point :
            int64 -> sched -> a Partition.partition_based_on_time_point)
        ~(f_partition_based_on_time_slot :
            start:int64 ->
          end_exc:int64 ->
          sched ->
          a Partition.partition_based_on_time_slot) ~(start : int64 option)
        ~(end_exc : int64 option)
        ~(include_starting_within_time_slot : bool option)
        ~(include_ending_within_time_slot : bool option) (sched : sched) :
      (Sched_req_.sched_req_id * a) Seq.t =
      let include_task_seg_place_starting_within_time_slot =
        Option.fold ~none:false
          ~some:(fun x -> x)
          include_starting_within_time_slot
      in
      let include_task_seg_place_ending_within_time_slot =
        Option.fold ~none:false
          ~some:(fun x -> x)
          include_ending_within_time_slot
      in
      ( match (start, end_exc) with
        | None, None -> f_get sched
        | Some start, None ->
          let part = f_partition_based_on_time_point start sched in
          if include_task_seg_place_starting_within_time_slot then
            Sched_req_id_map.union (fun _ _ _ -> None) part.after part.crossing
          else part.after
        | None, Some end_exc ->
          let part = f_partition_based_on_time_point end_exc sched in
          if include_task_seg_place_ending_within_time_slot then
            Sched_req_id_map.union (fun _ _ _ -> None) part.before part.crossing
          else part.before
        | Some start, Some end_exc ->
          let part = f_partition_based_on_time_slot ~start ~end_exc sched in
          part.fully_within
          |> (fun m ->
              if include_task_seg_place_starting_within_time_slot then
                Sched_req_id_map.union
                  (fun _ _ _ -> None)
                  m part.starting_within
              else m)
          |> fun m ->
          if include_task_seg_place_ending_within_time_slot then
            Sched_req_id_map.union (fun _ _ _ -> None) m part.ending_within
          else m )
      |> Sched_req_id_map.to_seq

    module Pending = struct
      let pending_sched_req_seq ~start ~end_exc
          ~include_sched_req_starting_within_time_slot
          ~include_sched_req_ending_within_time_slot =
        seq_internal
          ~f_get:(fun (_sid, sd) -> sd.store.sched_req_pending_store)
          ~f_partition_based_on_time_point:
            Partition.Pending.partition_based_on_time_point
          ~f_partition_based_on_time_slot:
            Partition.Pending.partition_based_on_time_slot ~start ~end_exc
          ~include_starting_within_time_slot:
            include_sched_req_starting_within_time_slot
          ~include_ending_within_time_slot:
            include_sched_req_ending_within_time_slot
    end

    module Record = struct
      let sched_req_record_seq ~start ~end_exc
          ~include_sched_req_record_starting_within_time_slot
          ~include_sched_req_record_ending_within_time_slot =
        seq_internal
          ~f_get:(fun (_sid, sd) -> sd.store.sched_req_record_store)
          ~f_partition_based_on_time_point:
            Partition.Record.partition_based_on_time_point
          ~f_partition_based_on_time_slot:
            Partition.Record.partition_based_on_time_slot ~start ~end_exc
          ~include_starting_within_time_slot:
            include_sched_req_record_starting_within_time_slot
          ~include_ending_within_time_slot:
            include_sched_req_record_ending_within_time_slot
    end
  end

  module Filter_internal = struct
    module Pending = struct
      let filter_pending_sched_req_seq ~start ~end_exc
          ~include_sched_req_starting_within_time_slot
          ~include_sched_req_ending_within_time_slot
          (f : Sched_req_.sched_req -> bool) (sched : sched) :
        Sched_req_.sched_req Seq.t =
        To_seq_internal.Pending.pending_sched_req_seq sched ~start ~end_exc
          ~include_sched_req_starting_within_time_slot
          ~include_sched_req_ending_within_time_slot
        |> Seq.filter f
    end

    module Record = struct
      let filter_sched_req_record_seq ~start ~end_exc
          ~include_sched_req_record_starting_within_time_slot
          ~include_sched_req_record_ending_within_time_slot
          (f : Sched_req_.sched_req_record -> bool) (sched : sched) :
        Sched_req_.sched_req_record Seq.t =
        To_seq_internal.Record.sched_req_record_seq ~start ~end_exc
          ~include_sched_req_record_starting_within_time_slot
          ~include_sched_req_record_ending_within_time_slot sched
        |> Seq.filter f
    end
  end

  module To_seq = struct
    module Pending = struct
      let pending_sched_req_seq ?(start : int64 option)
          ?(end_exc : int64 option)
          ?(include_sched_req_starting_within_time_slot : bool option)
          ?(include_sched_req_ending_within_time_slot : bool option)
          (sched : sched) : Sched_req_.sched_req Seq.t =
        To_seq_internal.Pending.pending_sched_req_seq ~start ~end_exc
          ~include_sched_req_starting_within_time_slot
          ~include_sched_req_ending_within_time_slot sched
    end

    module Record = struct
      let sched_req_record_seq ?(start : int64 option) ?(end_exc : int64 option)
          ?(include_sched_req_record_starting_within_time_slot : bool option)
          ?(include_sched_req_record_ending_within_time_slot : bool option)
          (sched : sched) : Sched_req_.sched_req_record Seq.t =
        To_seq_internal.Record.sched_req_record_seq ~start ~end_exc
          ~include_sched_req_record_starting_within_time_slot
          ~include_sched_req_record_ending_within_time_slot sched
    end
  end

  module Filter = struct
    module Pending = struct
      let filter_pending_sched_req_seq ?(start : int64 option)
          ?(end_exc : int64 option)
          ?(include_sched_req_starting_within_time_slot : bool option)
          ?(include_sched_req_ending_within_time_slot : bool option)
          (f : Sched_req_.sched_req -> bool) (sched : sched) :
        Sched_req_.sched_req Seq.t =
        Filter_internal.Pending.filter_pending_sched_req_seq ~start ~end_exc
          ~include_sched_req_starting_within_time_slot
          ~include_sched_req_ending_within_time_slot f sched
    end

    module Record = struct
      let filter_sched_req_record_seq ?(start : int64 option)
          ?(end_exc : int64 option)
          ?(include_sched_req_record_starting_within_time_slot : bool option)
          ?(include_sched_req_record_ending_within_time_slot : bool option)
          (f : Sched_req_.sched_req_record -> bool) (sched : sched) :
        Sched_req_.sched_req_record Seq.t =
        Filter_internal.Record.filter_sched_req_record_seq ~start ~end_exc
          ~include_sched_req_record_starting_within_time_slot
          ~include_sched_req_record_ending_within_time_slot f sched
    end
  end

  module Find = struct
    module Pending = struct
      let find_pending_sched_req (id : Sched_req_.sched_req_id)
          ((_, sd) : sched) : Sched_req_.sched_req_data option =
        Sched_req_id_map.find_opt id sd.store.sched_req_pending_store

      let find_pending_sched_req_by_task_id (task_id : Task_.task_id)
          (sched : sched) : Sched_req_.sched_req Seq.t =
        Filter.Pending.filter_pending_sched_req_seq
          (fun (_, l) ->
             List.exists
               (fun x ->
                  List.exists
                    (fun (task_inst_id, _) ->
                       Task_.Id.task_inst_id_matches_task_id task_inst_id task_id)
                    (Sched_req_data_unit_skeleton.get_inner_data x))
               l)
          sched

      let find_pending_sched_req_by_task_inst_id
          (task_inst_id : Task_.task_inst_id) (sched : sched) :
        Sched_req_.sched_req Seq.t =
        Filter.Pending.filter_pending_sched_req_seq
          (fun (_, l) ->
             List.exists
               (fun x ->
                  List.exists
                    (fun (task_inst_id', _) -> task_inst_id = task_inst_id')
                    (Sched_req_data_unit_skeleton.get_inner_data x))
               l)
          sched
    end

    module Record = struct
      let find_sched_req_record (id : Sched_req_.sched_req_id) ((_, sd) : sched)
        : Sched_req_.sched_req_record_data option =
        Sched_req_id_map.find_opt id sd.store.sched_req_record_store

      let find_sched_req_record_by_task_id (task_id : Task_.task_id)
          (sched : sched) : Sched_req_.sched_req_record Seq.t =
        Filter.Record.filter_sched_req_record_seq
          (fun (_, l) ->
             List.exists
               (fun x ->
                  List.exists
                    (fun (task_seg_id, _) ->
                       Task_.Id.task_seg_id_matches_task_id task_seg_id task_id)
                    (Sched_req_data_unit_skeleton.get_inner_data x))
               l)
          sched

      let find_sched_req_record_by_task_inst_id
          (task_inst_id : Task_.task_inst_id) (sched : sched) :
        Sched_req_.sched_req_record Seq.t =
        Filter.Record.filter_sched_req_record_seq
          (fun (_, l) ->
             List.exists
               (fun x ->
                  List.exists
                    (fun (task_seg_id, _) ->
                       Task_.Id.task_seg_id_matches_task_inst_id task_seg_id
                         task_inst_id)
                    (Sched_req_data_unit_skeleton.get_inner_data x))
               l)
          sched

      let find_sched_req_record_by_task_seg_id (task_seg_id : Task_.task_seg_id)
          (sched : sched) : Sched_req_.sched_req_record Seq.t =
        Filter.Record.filter_sched_req_record_seq
          (fun (_, l) ->
             List.exists
               (fun x ->
                  List.exists
                    (fun (task_seg_id', _) -> task_seg_id = task_seg_id')
                    (Sched_req_data_unit_skeleton.get_inner_data x))
               l)
          sched

      let find_sched_req_record_by_task_seg_id_ignore_sub_id
          (task_seg_id : Task_.task_seg_id) (sched : sched) :
        Sched_req_.sched_req_record Seq.t =
        Filter.Record.filter_sched_req_record_seq
          (fun (_, l) ->
             List.exists
               (fun x ->
                  List.exists
                    (fun (task_seg_id', _) ->
                       Task_.Id.task_seg_id_matches_task_seg_id_ignore_sub_id
                         task_seg_id task_seg_id')
                    (Sched_req_data_unit_skeleton.get_inner_data x))
               l)
          sched
    end
  end

  module Status = struct
    let get_sched_req_status (id : Sched_req_.sched_req_id) ((_, sd) : sched) :
      sched_req_status option =
      match Sched_req_id_map.find_opt id sd.store.sched_req_pending_store with
      | Some _ -> Some `Pending
      | None -> (
          match
            Sched_req_id_map.find_opt id sd.store.sched_req_discarded_store
          with
          | Some _ -> Some `Discarded
          | None -> (
              match
                Sched_req_id_map.find_opt id sd.store.sched_req_record_store
              with
              | Some _ -> Some `Recorded
              | None -> None ) )
  end

  module Remove = struct
    module Pending = struct
      let remove_pending_sched_req (sched_req_id : int64) ((sid, sd) : sched) :
        sched =
        match
          Sched_req_id_map.find_opt sched_req_id
            sd.store.sched_req_pending_store
        with
        | None -> (sid, sd)
        | Some _ ->
          let sid, sd = Id.remove_sched_req_id sched_req_id (sid, sd) in
          ( sid,
            {
              sd with
              store =
                {
                  sd.store with
                  sched_req_pending_store =
                    Sched_req_id_map.remove sched_req_id
                      sd.store.sched_req_pending_store;
                };
            } )

      let remove_pending_sched_req_if_contains_matching_task_seg_alloc_req
          (f : Task_.task_seg_alloc_req -> bool) ((sid, sd) : sched) : sched =
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                sched_req_pending_store =
                  sd.store.sched_req_pending_store
                  |> Sched_req_id_map.to_seq
                  |> Seq.filter (fun (_id, sched_req_data) ->
                      not
                        (Sched_req_data_unit_skeleton
                         .list_contains_matching_inner_data f sched_req_data))
                  |> Sched_req_id_map.of_seq;
              };
          } )

      let remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req
          (f : Task_.task_seg_alloc_req -> bool) ((sid, sd) : sched) : sched =
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                sched_req_pending_store =
                  sd.store.sched_req_pending_store
                  |> Sched_req_id_map.to_seq
                  |> Seq.filter_map (fun (id, sched_req_data) ->
                      match
                        Sched_req_data_unit_skeleton
                        .remove_data_units_with_matching_inner_data f
                          sched_req_data
                      with
                      | [] -> None
                      | sched_req_data -> Some (id, sched_req_data))
                  |> Sched_req_id_map.of_seq;
              };
          } )

      (*$
        let typ_list = [ "pending_sched_req"; "pending_sched_req_data_unit" ] in

        List.iter
          (fun typ ->
             Printf.printf "let remove_%s_by_task_id\n" typ;
             Printf.printf
               "  (task_id : Task_.task_id) (sched : sched) : sched =\n";
             Printf.printf
               "  remove_%s_if_contains_matching_task_seg_alloc_req\n" typ;
             Printf.printf "  (fun (task_inst_id, _data) ->\n";
             Printf.printf
               "    Task_.Id.task_inst_id_matches_task_id task_inst_id task_id)\n";
             Printf.printf "    sched\n";

             Printf.printf "let remove_%s_by_task_inst_id\n" typ;
             Printf.printf
               "  (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =\n";
             Printf.printf
               "  remove_%s_if_contains_matching_task_seg_alloc_req\n" typ;
             Printf.printf "  (fun (task_inst_id', _data) ->\n";
             Printf.printf "    task_inst_id = task_inst_id')\n";
             Printf.printf "    sched\n";

             Printf.printf "let remove_%s_by_task_seg_id\n" typ;
             Printf.printf
               "  (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =\n";
             Printf.printf
               "  remove_%s_if_contains_matching_task_seg_alloc_req\n" typ;
             Printf.printf "  (fun (task_inst_id, _data) ->\n";
             Printf.printf
               "    Task_.Id.task_seg_id_matches_task_inst_id task_seg_id \
                task_inst_id)\n";
             Printf.printf "    sched\n")
          typ_list
      *)

      let remove_pending_sched_req_by_task_id (task_id : Task_.task_id)
          (sched : sched) : sched =
        remove_pending_sched_req_if_contains_matching_task_seg_alloc_req
          (fun (task_inst_id, _data) ->
             Task_.Id.task_inst_id_matches_task_id task_inst_id task_id)
          sched

      let remove_pending_sched_req_by_task_inst_id
          (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =
        remove_pending_sched_req_if_contains_matching_task_seg_alloc_req
          (fun (task_inst_id', _data) -> task_inst_id = task_inst_id')
          sched

      let remove_pending_sched_req_by_task_seg_id
          (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =
        remove_pending_sched_req_if_contains_matching_task_seg_alloc_req
          (fun (task_inst_id, _data) ->
             Task_.Id.task_seg_id_matches_task_inst_id task_seg_id task_inst_id)
          sched

      let remove_pending_sched_req_data_unit_by_task_id
          (task_id : Task_.task_id) (sched : sched) : sched =
        remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req
          (fun (task_inst_id, _data) ->
             Task_.Id.task_inst_id_matches_task_id task_inst_id task_id)
          sched

      let remove_pending_sched_req_data_unit_by_task_inst_id
          (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =
        remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req
          (fun (task_inst_id', _data) -> task_inst_id = task_inst_id')
          sched

      let remove_pending_sched_req_data_unit_by_task_seg_id
          (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =
        remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req
          (fun (task_inst_id, _data) ->
             Task_.Id.task_seg_id_matches_task_inst_id task_seg_id task_inst_id)
          sched

      (*$*)
    end

    module Record = struct
      let remove_sched_req_record (sched_req_id : Sched_req_.sched_req_id)
          ((sid, sd) : sched) : sched =
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                sched_req_record_store =
                  Sched_req_id_map.remove sched_req_id
                    sd.store.sched_req_record_store;
              };
          } )

      let remove_sched_req_record_if_contains_matching_task_seg
          (f : Task_.task_seg -> bool) ((sid, sd) : sched) : sched =
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                sched_req_record_store =
                  sd.store.sched_req_record_store
                  |> Sched_req_id_map.to_seq
                  |> Seq.filter (fun (_id, sched_req_record_data) ->
                      not
                        (Sched_req_data_unit_skeleton
                         .list_contains_matching_inner_data f
                           sched_req_record_data))
                  |> Sched_req_id_map.of_seq;
              };
          } )

      let remove_sched_req_record_data_unit_if_contains_matching_task_seg
          (f : Task_.task_seg -> bool) ((sid, sd) : sched) : sched =
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                sched_req_record_store =
                  sd.store.sched_req_record_store
                  |> Sched_req_id_map.to_seq
                  |> Seq.filter_map (fun (id, sched_req_record_data) ->
                      match
                        Sched_req_data_unit_skeleton
                        .remove_data_units_with_matching_inner_data f
                          sched_req_record_data
                      with
                      | [] -> None
                      | sched_req_record_data ->
                        Some (id, sched_req_record_data))
                  |> Sched_req_id_map.of_seq;
              };
          } )

      (*$
        let typ_list = [ "sched_req_record"; "sched_req_record_data_unit" ] in

        List.iter
          (fun typ ->
             Printf.printf "let remove_%s_by_task_id\n" typ;
             Printf.printf
               "  (task_id : Task_.task_id) (sched : sched) : sched =\n";
             Printf.printf "  remove_%s_if_contains_matching_task_seg\n" typ;
             Printf.printf "  (fun (task_seg_id, _data) ->\n";
             Printf.printf
               "    Task_.Id.task_seg_id_matches_task_id task_seg_id task_id)\n";
             Printf.printf "    sched\n";

             Printf.printf "let remove_%s_by_task_inst_id\n" typ;
             Printf.printf
               "  (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =\n";
             Printf.printf "  remove_%s_if_contains_matching_task_seg\n" typ;
             Printf.printf "  (fun (task_seg_id, _data) ->\n";
             Printf.printf
               "    Task_.Id.task_seg_id_matches_task_inst_id task_seg_id \
                task_inst_id)\n";
             Printf.printf "    sched\n";

             Printf.printf "let remove_%s_by_task_seg_id\n" typ;
             Printf.printf
               "  (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =\n";
             Printf.printf "  remove_%s_if_contains_matching_task_seg\n" typ;
             Printf.printf "  (fun (task_seg_id', _data) ->\n";
             Printf.printf "    task_seg_id = task_seg_id')\n";
             Printf.printf "    sched\n")
          typ_list
      *)

      let remove_sched_req_record_by_task_id (task_id : Task_.task_id)
          (sched : sched) : sched =
        remove_sched_req_record_if_contains_matching_task_seg
          (fun (task_seg_id, _data) ->
             Task_.Id.task_seg_id_matches_task_id task_seg_id task_id)
          sched

      let remove_sched_req_record_by_task_inst_id
          (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =
        remove_sched_req_record_if_contains_matching_task_seg
          (fun (task_seg_id, _data) ->
             Task_.Id.task_seg_id_matches_task_inst_id task_seg_id task_inst_id)
          sched

      let remove_sched_req_record_by_task_seg_id
          (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =
        remove_sched_req_record_if_contains_matching_task_seg
          (fun (task_seg_id', _data) -> task_seg_id = task_seg_id')
          sched

      let remove_sched_req_record_data_unit_by_task_id (task_id : Task_.task_id)
          (sched : sched) : sched =
        remove_sched_req_record_data_unit_if_contains_matching_task_seg
          (fun (task_seg_id, _data) ->
             Task_.Id.task_seg_id_matches_task_id task_seg_id task_id)
          sched

      let remove_sched_req_record_data_unit_by_task_inst_id
          (task_inst_id : Task_.task_inst_id) (sched : sched) : sched =
        remove_sched_req_record_data_unit_if_contains_matching_task_seg
          (fun (task_seg_id, _data) ->
             Task_.Id.task_seg_id_matches_task_inst_id task_seg_id task_inst_id)
          sched

      let remove_sched_req_record_data_unit_by_task_seg_id
          (task_seg_id : Task_.task_seg_id) (sched : sched) : sched =
        remove_sched_req_record_data_unit_if_contains_matching_task_seg
          (fun (task_seg_id', _data) -> task_seg_id = task_seg_id')
          sched

      (*$*)
    end
  end

  module Discard = struct
    let discard_pending_sched_req (sched_req_id : Sched_req_.sched_req_id)
        ((sid, sd) : sched) : sched =
      match
        Sched_req_id_map.find_opt sched_req_id sd.store.sched_req_pending_store
      with
      | None -> (sid, sd)
      | Some sched_req_data ->
        ( sid,
          {
            sd with
            store =
              {
                sd.store with
                sched_req_pending_store =
                  Sched_req_id_map.remove sched_req_id
                    sd.store.sched_req_pending_store;
                sched_req_discarded_store =
                  Sched_req_id_map.add sched_req_id sched_req_data
                    sd.store.sched_req_discarded_store;
              };
          } )
  end

  module Allocate_task_segs = struct
    let allocate_task_segs_for_sched_req_data
        (sched_req_data : Sched_req_.sched_req_data) (sched : sched) :
      Sched_req_.sched_req_record_data * sched =
      List.fold_left
        (fun (acc, sched) sched_req_data_unit ->
           match sched_req_data_unit with
           | Sched_req_data_unit_skeleton.Fixed { task_seg_related_data; start }
             ->
             let task_seg_related_data, sched =
               Task_seg.Add.add_task_seg_via_task_seg_alloc_req
                 task_seg_related_data sched
             in
             ( Sched_req_data_unit_skeleton.Fixed
                 { task_seg_related_data; start }
               :: acc,
               sched )
           | Shift x ->
             let task_seg_related_data_list, sched =
               Task_seg.Add.add_task_segs_via_task_seg_alloc_req_list
                 x.task_seg_related_data_list sched
             in
             (Shift { x with task_seg_related_data_list } :: acc, sched)
           | Split_and_shift x ->
             let task_seg_related_data, sched =
               Task_seg.Add.add_task_seg_via_task_seg_alloc_req
                 x.task_seg_related_data sched
             in
             (Split_and_shift { x with task_seg_related_data } :: acc, sched)
           | Split_even x ->
             let task_seg_related_data, sched =
               Task_seg.Add.add_task_seg_via_task_seg_alloc_req
                 x.task_seg_related_data sched
             in
             (Split_even { x with task_seg_related_data } :: acc, sched)
           | Time_share x ->
             let task_seg_related_data_list, sched =
               Task_seg.Add.add_task_segs_via_task_seg_alloc_req_list
                 x.task_seg_related_data_list sched
             in
             (Time_share { x with task_seg_related_data_list } :: acc, sched)
           | Push_toward x ->
             let task_seg_related_data, sched =
               Task_seg.Add.add_task_seg_via_task_seg_alloc_req
                 x.task_seg_related_data sched
             in
             (Push_toward { x with task_seg_related_data } :: acc, sched))
        ([], sched) sched_req_data
      |> fun (l, sched) -> (List.rev l, sched)

    let allocate_task_segs_for_sched_req_list
        (sched_req_list : Sched_req_.sched_req list) (sched : sched) :
      Sched_req_.sched_req_record list * sched =
      List.fold_left
        (fun (acc, sched) (sched_req_id, sched_req_data) ->
           let sched_req_record_data_unit_list, (sid, sd) =
             allocate_task_segs_for_sched_req_data sched_req_data sched
           in
           ( (sched_req_id, sched_req_record_data_unit_list) :: acc,
             ( sid,
               {
                 sd with
                 store =
                   {
                     sd.store with
                     sched_req_record_store =
                       Sched_req_id_map.add sched_req_id
                         sched_req_record_data_unit_list
                         sd.store.sched_req_record_store;
                   };
               } ) ))
        ([], sched) sched_req_list
      |> fun (l, sched) -> (List.rev l, sched)

    let allocate_task_segs_for_pending_sched_reqs ~start ~end_exc
        ~(include_sched_reqs_starting_within_time_slot : bool)
        ~(include_sched_reqs_ending_within_time_slot : bool)
        ~(up_to_sched_req_id_inc : Sched_req_.sched_req_id option)
        ((sid, sd) : sched) : Sched_req_.sched_req_record list * sched =
      let union m1 m2 = Sched_req_id_map.union (fun _ _ _ -> None) m1 m2 in
      let partition =
        Partition.Pending.partition_based_on_time_slot ~start ~end_exc (sid, sd)
      in
      let to_be_scheduled_candidates, leftover =
        match
          ( include_sched_reqs_starting_within_time_slot,
            include_sched_reqs_ending_within_time_slot )
        with
        | true, true ->
          ( partition.fully_within
            |> union partition.starting_within
            |> union partition.ending_within,
            partition.outside )
        | false, true ->
          ( partition.fully_within |> union partition.ending_within,
            partition.outside |> union partition.starting_within )
        | true, false ->
          ( partition.fully_within |> union partition.starting_within,
            partition.outside |> union partition.ending_within )
        | false, false ->
          ( partition.fully_within,
            partition.outside
            |> union partition.starting_within
            |> union partition.ending_within )
      in
      let to_be_scheduled, leftover =
        match up_to_sched_req_id_inc with
        | None -> (to_be_scheduled_candidates, leftover)
        | Some up_to_id_inc -> (
            let lt, eq, gt =
              Sched_req_id_map.split up_to_id_inc to_be_scheduled_candidates
            in
            let leftover =
              Sched_req_id_map.union (fun _ _ _ -> None) gt leftover
            in
            match eq with
            | None -> (lt, leftover)
            | Some eq -> (Sched_req_id_map.add up_to_id_inc eq lt, leftover) )
      in
      let to_be_scheduled_sched_reqs =
        to_be_scheduled |> Sched_req_id_map.to_seq |> List.of_seq
      in
      let sd =
        { sd with store = { sd.store with sched_req_pending_store = leftover } }
      in
      allocate_task_segs_for_sched_req_list to_be_scheduled_sched_reqs (sid, sd)
  end
end

module Recur = struct
  let instantiate_raw_seq ~start ~end_exc (task_data : Task_.task_data) :
    (Task_.task_inst_data * Task_.sched_req_template) Seq.t =
    match task_data.task_type with
    | Task_.One_off -> Seq.empty
    | Task_.Recurring recur ->
      let usable_time_slot_seq =
        Time_slots.invert ~start ~end_exc
          (List.to_seq recur.excluded_time_slots)
      in
      let usable_time_slot_list = List.of_seq usable_time_slot_seq in
      ( match recur.recur_type with
        | Task_.Arithemtic_seq
            ( { start = seq_start; end_exc = seq_end_exc; diff },
              { task_inst_data; sched_req_template } ) ->
          let rec aux (cur : int64) (end_exc : int64 option) (diff : int64)
              (task_inst_data : Task_.task_inst_data)
              (sched_req_template : Task_.sched_req_template) :
            (Task_.task_inst_data * Task_.sched_req_template) Seq.t =
            let continue =
              match end_exc with
              | None -> true
              | Some end_exc -> cur < end_exc
            in
            if continue then
              let sched_req_template_instance =
                Sched_req_data_unit_skeleton.shift_time_list ~offset:cur
                  sched_req_template
              in
              fun () ->
                Seq.Cons
                  ( (task_inst_data, sched_req_template_instance),
                    aux (cur +^ diff) end_exc diff task_inst_data
                      sched_req_template )
            else Seq.empty
          in
          let start =
            if start < seq_start then seq_start
            else seq_start +^ ((start -^ seq_start) /^ diff *^ diff)
          in
          let end_exc = Option.map (fun x -> min x end_exc) seq_end_exc in
          aux start end_exc diff task_inst_data sched_req_template
        | Task_.Time_pattern_match
            (pattern, { task_inst_data; sched_req_template }) ->
          Time_pattern.Single_pattern.matching_time_slots
            ~allow_search_param_override:true
            {
              search_using_tz_offset_s = None;
              typ = Time_slots usable_time_slot_list;
            }
            pattern
          |> Result.get_ok
          |> Seq.map (fun (start', _end_exc) ->
              ( task_inst_data,
                Sched_req_data_unit_skeleton.shift_time_list ~offset:start'
                  sched_req_template )) )
      |> Seq.map (fun (task_inst_data, sched_req_template) ->
          ( task_inst_data,
            sched_req_template,
            Task_.sched_req_template_bound_on_start_and_end_exc
              sched_req_template ))
      |> OSeq.take_while
        (fun (_task_inst_data, _sched_req_template, template_bound) ->
           match template_bound with
           | None -> false
           | Some (_bound_start, bound_end_exc) -> bound_end_exc <= end_exc)
      |> Seq.filter
        (fun (_task_inst_data, _sched_req_template, template_bound) ->
           match template_bound with
           | None -> true
           | Some bound ->
             Time_slots.a_is_subset_of_b ~a:(Seq.return bound)
               ~b:usable_time_slot_seq)
      |> Seq.map (fun (task_inst_data, sched_req_template, _template_bound) ->
          (task_inst_data, sched_req_template))

  let instance_recorded_already (task_id : Task_.task_id)
      (task_inst_data : Task_.task_inst_data)
      (sched_req_template : Task_.sched_req_template)
      ((_sid, sd) as sched : sched) : bool =
    let user_id, task_part = task_id in
    match Task_id_map.find_opt task_id sd.store.task_id_to_task_inst_ids with
    | None -> false
    | Some task_inst_ids ->
      Int64_set.exists
        (fun task_inst_part ->
           let task_inst_id = (user_id, task_part, task_inst_part) in
           let stored_task_inst_data =
             Task_inst.Find.find_task_inst_any_opt task_inst_id sched
             |> Option.get
           in
           task_inst_data = stored_task_inst_data
           && ( Sched_req_id_map.exists
                  (fun _sched_req_id sched_req_data ->
                     Sched_req_utils.sched_req_template_matches_sched_req_data
                       sched_req_template sched_req_data)
                  sd.store.sched_req_pending_store
                || Sched_req_id_map.exists
                  (fun _sched_req_id sched_req_data ->
                     Sched_req_utils.sched_req_template_matches_sched_req_data
                       sched_req_template sched_req_data)
                  sd.store.sched_req_discarded_store
                || Sched_req_id_map.exists
                  (fun _sched_req_id sched_req_record_data ->
                     Sched_req_utils
                     .sched_req_template_matches_sched_req_record_data
                       sched_req_template sched_req_record_data)
                  sd.store.sched_req_record_store ))
        task_inst_ids

  let instantiate ~start ~end_exc ((sid, sd) : sched) : sched =
    Task_id_map.fold
      (fun task_id task_data sched ->
         let raw_seq = instantiate_raw_seq ~start ~end_exc task_data in
         raw_seq
         |> Seq.filter (fun (task_inst_data, sched_req_template) ->
             not
               (instance_recorded_already task_id task_inst_data
                  sched_req_template sched))
         |> Seq.fold_left
           (fun sched (task_inst_data, sched_req_templates) ->
              let (task_inst_id, _), sched =
                Task_inst.Add.add_task_inst ~parent_task_id:task_id
                  task_inst_data sched
              in
              let sched_req_data =
                Sched_req_data_unit_skeleton.map_list
                  ~f_data:(fun task_seg_size -> (task_inst_id, task_seg_size))
                  ~f_time:(fun x -> x)
                  ~f_time_slot:(fun x -> x)
                  sched_req_templates
              in
              let _, sched =
                Sched_req.Add.add_sched_req_data sched_req_data sched
                |> Result.get_ok
              in
              sched)
           sched)
      sd.store.task_uncompleted_store (sid, sd)
end

module Overdue = struct
  let get_overdue_task_seg_places ~(deadline : int64) (sched : sched) :
    Task_.task_seg_place Seq.t =
    Agenda.To_seq.task_seg_place_uncompleted ~end_exc:deadline sched

  let get_overdue_task_segs ~(deadline : int64) (sched : sched) :
    Task_.task_seg Seq.t =
    get_overdue_task_seg_places ~deadline sched
    |> Seq.map (fun (task_seg_id, _, _) ->
        let task_seg_size =
          Task_seg.Find.find_task_seg_uncompleted_opt task_seg_id sched
          |> Option.get
        in
        (task_seg_id, task_seg_size))

  let add_sched_reqs_for_overdue_task_segs ~start ~end_exc (sched : sched) :
    sched =
    let leftover_task_segs = get_overdue_task_segs ~deadline:start sched in
    let sched =
      Seq.fold_left
        (fun sched (task_seg_id, _) ->
           Task_seg.Move.move_task_seg_to_discarded task_seg_id sched)
        sched leftover_task_segs
    in
    let sched_req_data_seq =
      leftover_task_segs
      |> Seq.map
        (fun (task_seg_id, task_seg_size) : Sched_req_.sched_req_data ->
           let task_inst_id =
             Task_.Id.task_inst_id_of_task_seg_id task_seg_id
           in
           [
             Sched_req_data_unit_skeleton.Shift
               {
                 task_seg_related_data_list =
                   [ (task_inst_id, task_seg_size) ];
                 time_slots = [ (start, end_exc) ];
                 incre = 1L;
               };
           ])
    in
    Seq.fold_left
      (fun sched sched_req_data ->
         let _, sched =
           Sched_req.Add.add_sched_req_data sched_req_data sched |> Result.get_ok
         in
         sched)
      sched sched_req_data_seq
end

module Serialize = struct
  (*$ #use "src/sched.cinaps";;

    Store.print_pack_related_functions ()
  *)

  let pack_task_uncompleted_store (x : task_store) : Sched_t.task list =
    x |> Task_id_map.to_seq |> Seq.map Task_.Serialize.pack_task |> List.of_seq

  let pack_task_uncompleted_store_diff (x : task_store_diff) :
    (Task_t.task_id, Task_t.task_data) Map_utils_t.diff =
    {
      added = pack_task_uncompleted_store x.added;
      removed = pack_task_uncompleted_store x.removed;
    }

  let pack_task_completed_store (x : task_store) : Sched_t.task list =
    x |> Task_id_map.to_seq |> Seq.map Task_.Serialize.pack_task |> List.of_seq

  let pack_task_completed_store_diff (x : task_store_diff) :
    (Task_t.task_id, Task_t.task_data) Map_utils_t.diff =
    {
      added = pack_task_completed_store x.added;
      removed = pack_task_completed_store x.removed;
    }

  let pack_task_discarded_store (x : task_store) : Sched_t.task list =
    x |> Task_id_map.to_seq |> Seq.map Task_.Serialize.pack_task |> List.of_seq

  let pack_task_discarded_store_diff (x : task_store_diff) :
    (Task_t.task_id, Task_t.task_data) Map_utils_t.diff =
    {
      added = pack_task_discarded_store x.added;
      removed = pack_task_discarded_store x.removed;
    }

  let pack_task_inst_uncompleted_store (x : task_inst_store) :
    Sched_t.task_inst list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map Task_.Serialize.pack_task_inst
    |> List.of_seq

  let pack_task_inst_uncompleted_store_diff (x : task_inst_store_diff) :
    (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff =
    {
      added = pack_task_inst_uncompleted_store x.added;
      removed = pack_task_inst_uncompleted_store x.removed;
    }

  let pack_task_inst_completed_store (x : task_inst_store) :
    Sched_t.task_inst list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map Task_.Serialize.pack_task_inst
    |> List.of_seq

  let pack_task_inst_completed_store_diff (x : task_inst_store_diff) :
    (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff =
    {
      added = pack_task_inst_completed_store x.added;
      removed = pack_task_inst_completed_store x.removed;
    }

  let pack_task_inst_discarded_store (x : task_inst_store) :
    Sched_t.task_inst list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map Task_.Serialize.pack_task_inst
    |> List.of_seq

  let pack_task_inst_discarded_store_diff (x : task_inst_store_diff) :
    (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff =
    {
      added = pack_task_inst_discarded_store x.added;
      removed = pack_task_inst_discarded_store x.removed;
    }

  let pack_task_seg_uncompleted_store (x : task_seg_store) :
    Sched_t.task_seg list =
    x
    |> Task_seg_id_map.to_seq
    |> Seq.map Task_.Serialize.pack_task_seg
    |> List.of_seq

  let pack_task_seg_uncompleted_store_diff (x : task_seg_store_diff) :
    (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff =
    {
      added = pack_task_seg_uncompleted_store x.added;
      removed = pack_task_seg_uncompleted_store x.removed;
    }

  let pack_task_seg_completed_store (x : task_seg_store) : Sched_t.task_seg list
    =
    x
    |> Task_seg_id_map.to_seq
    |> Seq.map Task_.Serialize.pack_task_seg
    |> List.of_seq

  let pack_task_seg_completed_store_diff (x : task_seg_store_diff) :
    (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff =
    {
      added = pack_task_seg_completed_store x.added;
      removed = pack_task_seg_completed_store x.removed;
    }

  let pack_task_seg_discarded_store (x : task_seg_store) : Sched_t.task_seg list
    =
    x
    |> Task_seg_id_map.to_seq
    |> Seq.map Task_.Serialize.pack_task_seg
    |> List.of_seq

  let pack_task_seg_discarded_store_diff (x : task_seg_store_diff) :
    (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff =
    {
      added = pack_task_seg_discarded_store x.added;
      removed = pack_task_seg_discarded_store x.removed;
    }

  let pack_sched_req_pending_store (x : sched_req_store) :
    Sched_req_t.sched_req list =
    x
    |> Sched_req_id_map.to_seq
    |> Seq.map Sched_req_.Serialize.pack_sched_req
    |> List.of_seq

  let pack_sched_req_pending_store_diff (x : sched_req_store_diff) :
    (Sched_req_t.sched_req_id, Sched_req_t.sched_req_data) Map_utils_t.diff =
    {
      added = pack_sched_req_pending_store x.added;
      removed = pack_sched_req_pending_store x.removed;
    }

  let pack_sched_req_discarded_store (x : sched_req_store) :
    Sched_req_t.sched_req list =
    x
    |> Sched_req_id_map.to_seq
    |> Seq.map Sched_req_.Serialize.pack_sched_req
    |> List.of_seq

  let pack_sched_req_discarded_store_diff (x : sched_req_store_diff) :
    (Sched_req_t.sched_req_id, Sched_req_t.sched_req_data) Map_utils_t.diff =
    {
      added = pack_sched_req_discarded_store x.added;
      removed = pack_sched_req_discarded_store x.removed;
    }

  let pack_sched_req_record_store (x : sched_req_record_store) :
    Sched_req_t.sched_req_record list =
    x
    |> Sched_req_id_map.to_seq
    |> Seq.map Sched_req_.Serialize.pack_sched_req_record
    |> List.of_seq

  let pack_sched_req_record_store_diff (x : sched_req_record_store_diff) :
    ( Sched_req_t.sched_req_id,
      Sched_req_t.sched_req_record_data )
      Map_utils_t.diff =
    {
      added = pack_sched_req_record_store x.added;
      removed = pack_sched_req_record_store x.removed;
    }

  let pack_quota (x : int64 Task_inst_id_map.t) :
    (Task_t.task_inst_id * (int32 * int32)) list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map (fun (id, quota) ->
        ( Task_.Serialize.pack_task_inst_id id,
          Misc_utils.int32_int32_of_int64 quota ))
    |> List.of_seq

  let pack_quota_diff (x : int64 Task_inst_id_map_utils.diff) :
    (Task_t.task_inst_id, int32 * int32) Map_utils_t.diff =
    { added = pack_quota x.added; removed = pack_quota x.removed }

  let pack_task_seg_id_to_progress (x : Task_.progress Task_seg_id_map.t) :
    (Task_t.task_seg_id * Task_t.progress) list =
    x
    |> Task_seg_id_map.to_seq
    |> Seq.map (fun (id, progress) ->
        ( Task_.Serialize.pack_task_seg_id id,
          Task_.Serialize.pack_progress progress ))
    |> List.of_seq

  let pack_task_seg_id_to_progress_diff
      (x : Task_.progress Task_seg_id_map_utils.diff) :
    (Task_t.task_seg_id, Task_t.progress) Map_utils_t.diff =
    {
      added = pack_task_seg_id_to_progress x.added;
      removed = pack_task_seg_id_to_progress x.removed;
    }

  let pack_task_inst_id_to_progress (x : Task_.progress Task_inst_id_map.t) :
    (Task_t.task_inst_id * Task_t.progress) list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map (fun (id, progress) ->
        ( Task_.Serialize.pack_task_inst_id id,
          Task_.Serialize.pack_progress progress ))
    |> List.of_seq

  let pack_task_inst_id_to_progress_diff
      (x : Task_.progress Task_inst_id_map_utils.diff) :
    (Task_t.task_inst_id, Task_t.progress) Map_utils_t.diff =
    {
      added = pack_task_inst_id_to_progress x.added;
      removed = pack_task_inst_id_to_progress x.removed;
    }

  let pack_indexed_by_task_seg_id (x : (int64 * int64) Task_seg_id_map.t) :
    (Task_t.task_seg_id * ((int32 * int32) * (int32 * int32))) list =
    x
    |> Task_seg_id_map.to_seq
    |> Seq.map (fun (id, (start, end_exc)) ->
        ( Task_.Serialize.pack_task_seg_id id,
          ( Misc_utils.int32_int32_of_int64 start,
            Misc_utils.int32_int32_of_int64 end_exc ) ))
    |> List.of_seq

  let pack_indexed_by_task_seg_id_diff
      (x : (int64 * int64) Task_seg_id_map_utils.diff) :
    (Task_t.task_seg_id, (int32 * int32) * (int32 * int32)) Map_utils_t.diff =
    {
      added = pack_indexed_by_task_seg_id x.added;
      removed = pack_indexed_by_task_seg_id x.removed;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    Bucket_store.print_pack_related_functions ()
  *)

  let pack_user_id_to_task_ids (x : Int64_set.t User_id_map.t) :
    (Task_t.user_id * (int32 * int32) list) list =
    x
    |> User_id_map.to_seq
    |> Seq.map (fun (id, y) ->
        (Task_.Serialize.pack_user_id id, Int64_set.Serialize.pack y))
    |> List.of_seq

  let pack_user_id_to_task_ids_diff
      (x : User_id_map_utils.Int64_bucketed.diff_bucketed) :
    (Task_t.user_id, int32 * int32) Map_utils_t.diff_bucketed =
    {
      added = pack_user_id_to_task_ids x.added;
      removed = pack_user_id_to_task_ids x.removed;
    }

  let pack_task_id_to_task_inst_ids (x : Int64_set.t Task_id_map.t) :
    (Task_t.task_id * (int32 * int32) list) list =
    x
    |> Task_id_map.to_seq
    |> Seq.map (fun (id, y) ->
        (Task_.Serialize.pack_task_id id, Int64_set.Serialize.pack y))
    |> List.of_seq

  let pack_task_id_to_task_inst_ids_diff
      (x : Task_id_map_utils.Int64_bucketed.diff_bucketed) :
    (Task_t.task_id, int32 * int32) Map_utils_t.diff_bucketed =
    {
      added = pack_task_id_to_task_inst_ids x.added;
      removed = pack_task_id_to_task_inst_ids x.removed;
    }

  let pack_task_inst_id_to_task_seg_ids
      (x : Int64_int64_option_set.t Task_inst_id_map.t) :
    (Task_t.task_inst_id * ((int32 * int32) * (int32 * int32) option) list)
      list =
    x
    |> Task_inst_id_map.to_seq
    |> Seq.map (fun (id, y) ->
        ( Task_.Serialize.pack_task_inst_id id,
          Int64_int64_option_set.Serialize.pack y ))
    |> List.of_seq

  let pack_task_inst_id_to_task_seg_ids_diff
      (x : Task_inst_id_map_utils.Int64_int64_option_bucketed.diff_bucketed) :
    ( Task_t.task_inst_id,
      (int32 * int32) * (int32 * int32) option )
      Map_utils_t.diff_bucketed =
    {
      added = pack_task_inst_id_to_task_seg_ids x.added;
      removed = pack_task_inst_id_to_task_seg_ids x.removed;
    }

  let pack_indexed_by_start (x : task_seg_place_map) :
    ((int32 * int32) * Task_t.task_seg_id list) list =
    x
    |> Int64_map.to_seq
    |> Seq.map (fun (id, y) ->
        (Misc_utils.int32_int32_of_int64 id, Task_seg_id_set.Serialize.pack y))
    |> List.of_seq

  let pack_indexed_by_start_diff (x : task_seg_place_map_diff) :
    (int32 * int32, Task_t.task_seg_id) Map_utils_t.diff_bucketed =
    {
      added = pack_indexed_by_start x.added;
      removed = pack_indexed_by_start x.removed;
    }

  let pack_indexed_by_end_exc (x : task_seg_place_map) :
    ((int32 * int32) * Task_t.task_seg_id list) list =
    x
    |> Int64_map.to_seq
    |> Seq.map (fun (id, y) ->
        (Misc_utils.int32_int32_of_int64 id, Task_seg_id_set.Serialize.pack y))
    |> List.of_seq

  let pack_indexed_by_end_exc_diff (x : task_seg_place_map_diff) :
    (int32 * int32, Task_t.task_seg_id) Map_utils_t.diff_bucketed =
    {
      added = pack_indexed_by_end_exc x.added;
      removed = pack_indexed_by_end_exc x.removed;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    Set_store.print_pack_related_functions ()
  *)

  let pack_sched_req_ids (x : Int64_set.t) : (int32 * int32) list =
    x
    |> Int64_set.to_seq
    |> Seq.map Misc_utils.int32_int32_of_int64
    |> List.of_seq

  let pack_sched_req_ids_diff (x : Int64_set_utils.diff) :
    (int32 * int32) Set_utils_t.diff =
    {
      added = pack_sched_req_ids x.added;
      removed = pack_sched_req_ids x.removed;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    print_pack_store ();
    print_pack_store_diff ()
  *)

  let pack_store (store : store) : Sched_t.store =
    {
      task_uncompleted_list =
        pack_task_uncompleted_store store.task_uncompleted_store;
      task_completed_list = pack_task_completed_store store.task_completed_store;
      task_discarded_list = pack_task_discarded_store store.task_discarded_store;
      task_inst_uncompleted_list =
        pack_task_inst_uncompleted_store store.task_inst_uncompleted_store;
      task_inst_completed_list =
        pack_task_inst_completed_store store.task_inst_completed_store;
      task_inst_discarded_list =
        pack_task_inst_discarded_store store.task_inst_discarded_store;
      task_seg_uncompleted_list =
        pack_task_seg_uncompleted_store store.task_seg_uncompleted_store;
      task_seg_completed_list =
        pack_task_seg_completed_store store.task_seg_completed_store;
      task_seg_discarded_list =
        pack_task_seg_discarded_store store.task_seg_discarded_store;
      user_id_to_task_ids = pack_user_id_to_task_ids store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        pack_task_id_to_task_inst_ids store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        pack_task_inst_id_to_task_seg_ids store.task_inst_id_to_task_seg_ids;
      sched_req_ids = pack_sched_req_ids store.sched_req_ids;
      sched_req_pending_list =
        pack_sched_req_pending_store store.sched_req_pending_store;
      sched_req_discarded_list =
        pack_sched_req_discarded_store store.sched_req_discarded_store;
      sched_req_record_list =
        pack_sched_req_record_store store.sched_req_record_store;
      quota = pack_quota store.quota;
      task_seg_id_to_progress =
        pack_task_seg_id_to_progress store.task_seg_id_to_progress;
      task_inst_id_to_progress =
        pack_task_inst_id_to_progress store.task_inst_id_to_progress;
    }

  let pack_store_diff (diff : store_diff) : Sched_t.store_diff =
    {
      task_uncompleted_list_diff =
        pack_task_uncompleted_store_diff diff.task_uncompleted_store_diff;
      task_completed_list_diff =
        pack_task_completed_store_diff diff.task_completed_store_diff;
      task_discarded_list_diff =
        pack_task_discarded_store_diff diff.task_discarded_store_diff;
      task_inst_uncompleted_list_diff =
        pack_task_inst_uncompleted_store_diff
          diff.task_inst_uncompleted_store_diff;
      task_inst_completed_list_diff =
        pack_task_inst_completed_store_diff diff.task_inst_completed_store_diff;
      task_inst_discarded_list_diff =
        pack_task_inst_discarded_store_diff diff.task_inst_discarded_store_diff;
      task_seg_uncompleted_list_diff =
        pack_task_seg_uncompleted_store_diff
          diff.task_seg_uncompleted_store_diff;
      task_seg_completed_list_diff =
        pack_task_seg_completed_store_diff diff.task_seg_completed_store_diff;
      task_seg_discarded_list_diff =
        pack_task_seg_discarded_store_diff diff.task_seg_discarded_store_diff;
      user_id_to_task_ids_diff =
        pack_user_id_to_task_ids_diff diff.user_id_to_task_ids_diff;
      task_id_to_task_inst_ids_diff =
        pack_task_id_to_task_inst_ids_diff diff.task_id_to_task_inst_ids_diff;
      task_inst_id_to_task_seg_ids_diff =
        pack_task_inst_id_to_task_seg_ids_diff
          diff.task_inst_id_to_task_seg_ids_diff;
      sched_req_ids_diff = pack_sched_req_ids_diff diff.sched_req_ids_diff;
      sched_req_pending_list_diff =
        pack_sched_req_pending_store_diff diff.sched_req_pending_store_diff;
      sched_req_discarded_list_diff =
        pack_sched_req_discarded_store_diff diff.sched_req_discarded_store_diff;
      sched_req_record_list_diff =
        pack_sched_req_record_store_diff diff.sched_req_record_store_diff;
      quota_diff = pack_quota_diff diff.quota_diff;
      task_seg_id_to_progress_diff =
        pack_task_seg_id_to_progress_diff diff.task_seg_id_to_progress_diff;
      task_inst_id_to_progress_diff =
        pack_task_inst_id_to_progress_diff diff.task_inst_id_to_progress_diff;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    print_pack_agenda ();

    print_pack_agenda_diff ()
  *)

  let pack_agenda (agenda : agenda) : Sched_t.agenda =
    {
      indexed_by_task_seg_id =
        pack_indexed_by_task_seg_id agenda.indexed_by_task_seg_id;
      indexed_by_start = pack_indexed_by_start agenda.indexed_by_start;
      indexed_by_end_exc = pack_indexed_by_end_exc agenda.indexed_by_end_exc;
    }

  let pack_agenda_diff (diff : agenda_diff) : Sched_t.agenda_diff =
    {
      indexed_by_task_seg_id_diff =
        pack_indexed_by_task_seg_id_diff diff.indexed_by_task_seg_id_diff;
      indexed_by_start_diff =
        pack_indexed_by_start_diff diff.indexed_by_start_diff;
      indexed_by_end_exc_diff =
        pack_indexed_by_end_exc_diff diff.indexed_by_end_exc_diff;
    }

  (*$*)

  let pack_sched ((sid, sd) : sched) : Sched_t.sched =
    (sid, { store = pack_store sd.store; agenda = pack_agenda sd.agenda })

  let pack_sched_diff ((sid_old, sid_new, sd_diff) : sched_diff) :
    Sched_t.sched_diff =
    ( sid_old,
      sid_new,
      {
        store_diff = pack_store_diff sd_diff.store_diff;
        agenda_diff = pack_agenda_diff sd_diff.agenda_diff;
      } )

  let json_string_of_sched (sched : sched) : string =
    sched |> pack_sched |> Sched_j.string_of_sched

  let json_string_of_sched_diff (diff : sched_diff) : string =
    diff |> pack_sched_diff |> Sched_j.string_of_sched_diff
end

module Deserialize = struct
  (*$ #use "src/sched.cinaps";;

    Store.print_unpack_related_functions ()
  *)

  let unpack_task_uncompleted_list (x : Sched_t.task list) : task_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task
    |> Task_id_map.of_seq

  let unpack_task_uncompleted_list_diff
      (x : (Task_t.task_id, Task_t.task_data) Map_utils_t.diff) :
    task_store_diff =
    {
      added = unpack_task_uncompleted_list x.added;
      removed = unpack_task_uncompleted_list x.removed;
    }

  let unpack_task_completed_list (x : Sched_t.task list) : task_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task
    |> Task_id_map.of_seq

  let unpack_task_completed_list_diff
      (x : (Task_t.task_id, Task_t.task_data) Map_utils_t.diff) :
    task_store_diff =
    {
      added = unpack_task_completed_list x.added;
      removed = unpack_task_completed_list x.removed;
    }

  let unpack_task_discarded_list (x : Sched_t.task list) : task_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task
    |> Task_id_map.of_seq

  let unpack_task_discarded_list_diff
      (x : (Task_t.task_id, Task_t.task_data) Map_utils_t.diff) :
    task_store_diff =
    {
      added = unpack_task_discarded_list x.added;
      removed = unpack_task_discarded_list x.removed;
    }

  let unpack_task_inst_uncompleted_list (x : Sched_t.task_inst list) :
    task_inst_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task_inst
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_uncompleted_list_diff
      (x : (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff) :
    task_inst_store_diff =
    {
      added = unpack_task_inst_uncompleted_list x.added;
      removed = unpack_task_inst_uncompleted_list x.removed;
    }

  let unpack_task_inst_completed_list (x : Sched_t.task_inst list) :
    task_inst_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task_inst
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_completed_list_diff
      (x : (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff) :
    task_inst_store_diff =
    {
      added = unpack_task_inst_completed_list x.added;
      removed = unpack_task_inst_completed_list x.removed;
    }

  let unpack_task_inst_discarded_list (x : Sched_t.task_inst list) :
    task_inst_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task_inst
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_discarded_list_diff
      (x : (Task_t.task_inst_id, Task_t.task_inst_data) Map_utils_t.diff) :
    task_inst_store_diff =
    {
      added = unpack_task_inst_discarded_list x.added;
      removed = unpack_task_inst_discarded_list x.removed;
    }

  let unpack_task_seg_uncompleted_list (x : Sched_t.task_seg list) :
    task_seg_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task_seg
    |> Task_seg_id_map.of_seq

  let unpack_task_seg_uncompleted_list_diff
      (x : (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff) :
    task_seg_store_diff =
    {
      added = unpack_task_seg_uncompleted_list x.added;
      removed = unpack_task_seg_uncompleted_list x.removed;
    }

  let unpack_task_seg_completed_list (x : Sched_t.task_seg list) :
    task_seg_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task_seg
    |> Task_seg_id_map.of_seq

  let unpack_task_seg_completed_list_diff
      (x : (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff) :
    task_seg_store_diff =
    {
      added = unpack_task_seg_completed_list x.added;
      removed = unpack_task_seg_completed_list x.removed;
    }

  let unpack_task_seg_discarded_list (x : Sched_t.task_seg list) :
    task_seg_store =
    x
    |> List.to_seq
    |> Seq.map Task_.Deserialize.unpack_task_seg
    |> Task_seg_id_map.of_seq

  let unpack_task_seg_discarded_list_diff
      (x : (Task_t.task_seg_id, Task_t.task_seg_size) Map_utils_t.diff) :
    task_seg_store_diff =
    {
      added = unpack_task_seg_discarded_list x.added;
      removed = unpack_task_seg_discarded_list x.removed;
    }

  let unpack_sched_req_pending_list (x : Sched_req_t.sched_req list) :
    sched_req_store =
    x
    |> List.to_seq
    |> Seq.map Sched_req_.Deserialize.unpack_sched_req
    |> Sched_req_id_map.of_seq

  let unpack_sched_req_pending_list_diff
      (x :
         (Sched_req_t.sched_req_id, Sched_req_t.sched_req_data) Map_utils_t.diff)
    : sched_req_store_diff =
    {
      added = unpack_sched_req_pending_list x.added;
      removed = unpack_sched_req_pending_list x.removed;
    }

  let unpack_sched_req_discarded_list (x : Sched_req_t.sched_req list) :
    sched_req_store =
    x
    |> List.to_seq
    |> Seq.map Sched_req_.Deserialize.unpack_sched_req
    |> Sched_req_id_map.of_seq

  let unpack_sched_req_discarded_list_diff
      (x :
         (Sched_req_t.sched_req_id, Sched_req_t.sched_req_data) Map_utils_t.diff)
    : sched_req_store_diff =
    {
      added = unpack_sched_req_discarded_list x.added;
      removed = unpack_sched_req_discarded_list x.removed;
    }

  let unpack_sched_req_record_list (x : Sched_req_t.sched_req_record list) :
    sched_req_record_store =
    x
    |> List.to_seq
    |> Seq.map Sched_req_.Deserialize.unpack_sched_req_record
    |> Sched_req_id_map.of_seq

  let unpack_sched_req_record_list_diff
      (x :
         ( Sched_req_t.sched_req_id,
           Sched_req_t.sched_req_record_data )
           Map_utils_t.diff) : sched_req_record_store_diff =
    {
      added = unpack_sched_req_record_list x.added;
      removed = unpack_sched_req_record_list x.removed;
    }

  let unpack_quota (x : (Task_t.task_inst_id * (int32 * int32)) list) :
    int64 Task_inst_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, quota) ->
        ( Task_.Deserialize.unpack_task_inst_id id,
          Misc_utils.int64_of_int32_int32 quota ))
    |> Task_inst_id_map.of_seq

  let unpack_quota_diff
      (x : (Task_t.task_inst_id, int32 * int32) Map_utils_t.diff) :
    int64 Task_inst_id_map_utils.diff =
    { added = unpack_quota x.added; removed = unpack_quota x.removed }

  let unpack_task_seg_id_to_progress
      (x : (Task_t.task_seg_id * Task_t.progress) list) :
    Task_.progress Task_seg_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, progress) ->
        ( Task_.Deserialize.unpack_task_seg_id id,
          Task_.Deserialize.unpack_progress progress ))
    |> Task_seg_id_map.of_seq

  let unpack_task_seg_id_to_progress_diff
      (x : (Task_t.task_seg_id, Task_t.progress) Map_utils_t.diff) :
    Task_.progress Task_seg_id_map_utils.diff =
    {
      added = unpack_task_seg_id_to_progress x.added;
      removed = unpack_task_seg_id_to_progress x.removed;
    }

  let unpack_task_inst_id_to_progress
      (x : (Task_t.task_inst_id * Task_t.progress) list) :
    Task_.progress Task_inst_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, progress) ->
        ( Task_.Deserialize.unpack_task_inst_id id,
          Task_.Deserialize.unpack_progress progress ))
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_id_to_progress_diff
      (x : (Task_t.task_inst_id, Task_t.progress) Map_utils_t.diff) :
    Task_.progress Task_inst_id_map_utils.diff =
    {
      added = unpack_task_inst_id_to_progress x.added;
      removed = unpack_task_inst_id_to_progress x.removed;
    }

  let unpack_indexed_by_task_seg_id
      (x : (Task_t.task_seg_id * ((int32 * int32) * (int32 * int32))) list) :
    (int64 * int64) Task_seg_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, (start, end_exc)) ->
        ( Task_.Deserialize.unpack_task_seg_id id,
          ( Misc_utils.int64_of_int32_int32 start,
            Misc_utils.int64_of_int32_int32 end_exc ) ))
    |> Task_seg_id_map.of_seq

  let unpack_indexed_by_task_seg_id_diff
      (x :
         (Task_t.task_seg_id, (int32 * int32) * (int32 * int32)) Map_utils_t.diff)
    : (int64 * int64) Task_seg_id_map_utils.diff =
    {
      added = unpack_indexed_by_task_seg_id x.added;
      removed = unpack_indexed_by_task_seg_id x.removed;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    Bucket_store.print_unpack_related_functions ()
  *)

  let unpack_user_id_to_task_ids
      (x : (Task_t.user_id * (int32 * int32) list) list) :
    Int64_set.t User_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) ->
        (Task_.Deserialize.unpack_user_id id, Int64_set.Deserialize.unpack y))
    |> User_id_map.of_seq

  let unpack_user_id_to_task_ids_diff
      (x : (Task_t.user_id, int32 * int32) Map_utils_t.diff_bucketed) :
    User_id_map_utils.Int64_bucketed.diff_bucketed =
    {
      added = unpack_user_id_to_task_ids x.added;
      removed = unpack_user_id_to_task_ids x.removed;
    }

  let unpack_task_id_to_task_inst_ids
      (x : (Task_t.task_id * (int32 * int32) list) list) :
    Int64_set.t Task_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) ->
        (Task_.Deserialize.unpack_task_id id, Int64_set.Deserialize.unpack y))
    |> Task_id_map.of_seq

  let unpack_task_id_to_task_inst_ids_diff
      (x : (Task_t.task_id, int32 * int32) Map_utils_t.diff_bucketed) :
    Task_id_map_utils.Int64_bucketed.diff_bucketed =
    {
      added = unpack_task_id_to_task_inst_ids x.added;
      removed = unpack_task_id_to_task_inst_ids x.removed;
    }

  let unpack_task_inst_id_to_task_seg_ids
      (x :
         (Task_t.task_inst_id * ((int32 * int32) * (int32 * int32) option) list)
           list) : Int64_int64_option_set.t Task_inst_id_map.t =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) ->
        ( Task_.Deserialize.unpack_task_inst_id id,
          Int64_int64_option_set.Deserialize.unpack y ))
    |> Task_inst_id_map.of_seq

  let unpack_task_inst_id_to_task_seg_ids_diff
      (x :
         ( Task_t.task_inst_id,
           (int32 * int32) * (int32 * int32) option )
           Map_utils_t.diff_bucketed) :
    Task_inst_id_map_utils.Int64_int64_option_bucketed.diff_bucketed =
    {
      added = unpack_task_inst_id_to_task_seg_ids x.added;
      removed = unpack_task_inst_id_to_task_seg_ids x.removed;
    }

  let unpack_indexed_by_start
      (x : ((int32 * int32) * Task_t.task_seg_id list) list) :
    task_seg_place_map =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) ->
        ( Misc_utils.int64_of_int32_int32 id,
          Task_seg_id_set.Deserialize.unpack y ))
    |> Int64_map.of_seq

  let unpack_indexed_by_start_diff
      (x : (int32 * int32, Task_t.task_seg_id) Map_utils_t.diff_bucketed) :
    task_seg_place_map_diff =
    {
      added = unpack_indexed_by_start x.added;
      removed = unpack_indexed_by_start x.removed;
    }

  let unpack_indexed_by_end_exc
      (x : ((int32 * int32) * Task_t.task_seg_id list) list) :
    task_seg_place_map =
    x
    |> List.to_seq
    |> Seq.map (fun (id, y) ->
        ( Misc_utils.int64_of_int32_int32 id,
          Task_seg_id_set.Deserialize.unpack y ))
    |> Int64_map.of_seq

  let unpack_indexed_by_end_exc_diff
      (x : (int32 * int32, Task_t.task_seg_id) Map_utils_t.diff_bucketed) :
    task_seg_place_map_diff =
    {
      added = unpack_indexed_by_end_exc x.added;
      removed = unpack_indexed_by_end_exc x.removed;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    Set_store.print_unpack_related_functions ()
  *)

  let unpack_sched_req_ids (x : (int32 * int32) list) : Int64_set.t =
    x
    |> List.to_seq
    |> Seq.map Misc_utils.int64_of_int32_int32
    |> Int64_set.of_seq

  let unpack_sched_req_ids_diff (x : (int32 * int32) Set_utils_t.diff) :
    Int64_set_utils.diff =
    {
      added = unpack_sched_req_ids x.added;
      removed = unpack_sched_req_ids x.removed;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    print_unpack_store ();
    print_unpack_store_diff ();
  *)

  let unpack_store (store : Sched_t.store) : store =
    {
      task_uncompleted_store =
        unpack_task_uncompleted_list store.task_uncompleted_list;
      task_completed_store =
        unpack_task_completed_list store.task_completed_list;
      task_discarded_store =
        unpack_task_discarded_list store.task_discarded_list;
      task_inst_uncompleted_store =
        unpack_task_inst_uncompleted_list store.task_inst_uncompleted_list;
      task_inst_completed_store =
        unpack_task_inst_completed_list store.task_inst_completed_list;
      task_inst_discarded_store =
        unpack_task_inst_discarded_list store.task_inst_discarded_list;
      task_seg_uncompleted_store =
        unpack_task_seg_uncompleted_list store.task_seg_uncompleted_list;
      task_seg_completed_store =
        unpack_task_seg_completed_list store.task_seg_completed_list;
      task_seg_discarded_store =
        unpack_task_seg_discarded_list store.task_seg_discarded_list;
      user_id_to_task_ids = unpack_user_id_to_task_ids store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        unpack_task_id_to_task_inst_ids store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        unpack_task_inst_id_to_task_seg_ids store.task_inst_id_to_task_seg_ids;
      sched_req_ids = unpack_sched_req_ids store.sched_req_ids;
      sched_req_pending_store =
        unpack_sched_req_pending_list store.sched_req_pending_list;
      sched_req_discarded_store =
        unpack_sched_req_discarded_list store.sched_req_discarded_list;
      sched_req_record_store =
        unpack_sched_req_record_list store.sched_req_record_list;
      quota = unpack_quota store.quota;
      task_seg_id_to_progress =
        unpack_task_seg_id_to_progress store.task_seg_id_to_progress;
      task_inst_id_to_progress =
        unpack_task_inst_id_to_progress store.task_inst_id_to_progress;
    }

  let unpack_store_diff (diff : Sched_t.store_diff) : store_diff =
    {
      task_uncompleted_store_diff =
        unpack_task_uncompleted_list_diff diff.task_uncompleted_list_diff;
      task_completed_store_diff =
        unpack_task_completed_list_diff diff.task_completed_list_diff;
      task_discarded_store_diff =
        unpack_task_discarded_list_diff diff.task_discarded_list_diff;
      task_inst_uncompleted_store_diff =
        unpack_task_inst_uncompleted_list_diff
          diff.task_inst_uncompleted_list_diff;
      task_inst_completed_store_diff =
        unpack_task_inst_completed_list_diff diff.task_inst_completed_list_diff;
      task_inst_discarded_store_diff =
        unpack_task_inst_discarded_list_diff diff.task_inst_discarded_list_diff;
      task_seg_uncompleted_store_diff =
        unpack_task_seg_uncompleted_list_diff
          diff.task_seg_uncompleted_list_diff;
      task_seg_completed_store_diff =
        unpack_task_seg_completed_list_diff diff.task_seg_completed_list_diff;
      task_seg_discarded_store_diff =
        unpack_task_seg_discarded_list_diff diff.task_seg_discarded_list_diff;
      user_id_to_task_ids_diff =
        unpack_user_id_to_task_ids_diff diff.user_id_to_task_ids_diff;
      task_id_to_task_inst_ids_diff =
        unpack_task_id_to_task_inst_ids_diff diff.task_id_to_task_inst_ids_diff;
      task_inst_id_to_task_seg_ids_diff =
        unpack_task_inst_id_to_task_seg_ids_diff
          diff.task_inst_id_to_task_seg_ids_diff;
      sched_req_ids_diff = unpack_sched_req_ids_diff diff.sched_req_ids_diff;
      sched_req_pending_store_diff =
        unpack_sched_req_pending_list_diff diff.sched_req_pending_list_diff;
      sched_req_discarded_store_diff =
        unpack_sched_req_discarded_list_diff diff.sched_req_discarded_list_diff;
      sched_req_record_store_diff =
        unpack_sched_req_record_list_diff diff.sched_req_record_list_diff;
      quota_diff = unpack_quota_diff diff.quota_diff;
      task_seg_id_to_progress_diff =
        unpack_task_seg_id_to_progress_diff diff.task_seg_id_to_progress_diff;
      task_inst_id_to_progress_diff =
        unpack_task_inst_id_to_progress_diff diff.task_inst_id_to_progress_diff;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    print_unpack_agenda ();
    print_unpack_agenda_diff ();
  *)

  let unpack_agenda (agenda : Sched_t.agenda) : agenda =
    {
      indexed_by_task_seg_id =
        unpack_indexed_by_task_seg_id agenda.indexed_by_task_seg_id;
      indexed_by_start = unpack_indexed_by_start agenda.indexed_by_start;
      indexed_by_end_exc = unpack_indexed_by_end_exc agenda.indexed_by_end_exc;
    }

  let unpack_agenda_diff (diff : Sched_t.agenda_diff) : agenda_diff =
    {
      indexed_by_task_seg_id_diff =
        unpack_indexed_by_task_seg_id_diff diff.indexed_by_task_seg_id_diff;
      indexed_by_start_diff =
        unpack_indexed_by_start_diff diff.indexed_by_start_diff;
      indexed_by_end_exc_diff =
        unpack_indexed_by_end_exc_diff diff.indexed_by_end_exc_diff;
    }

  (*$*)

  let unpack_sched ((sid, sd) : Sched_t.sched) : sched =
    (sid, { store = unpack_store sd.store; agenda = unpack_agenda sd.agenda })

  let unpack_sched_diff ((sid_old, sid_new, sd_diff) : Sched_t.sched_diff) :
    sched_diff =
    ( sid_old,
      sid_new,
      {
        store_diff = unpack_store_diff sd_diff.store_diff;
        agenda_diff = unpack_agenda_diff sd_diff.agenda_diff;
      } )

  let sched_of_json_string string : sched =
    string |> Sched_j.sched_of_string |> unpack_sched

  let sched_diff_of_json_string string : sched_diff =
    string |> Sched_j.sched_diff_of_string |> unpack_sched_diff
end

module Equal = struct
  (*$ #use "src/sched.cinaps";;

    print_store_equal ();
    print_agenda_equal ();
  *)

  let store_equal (store1 : store) (store2 : store) : bool =
    Task_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_uncompleted_store store2.task_uncompleted_store
    && Task_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_completed_store store2.task_completed_store
    && Task_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_discarded_store store2.task_discarded_store
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_inst_uncompleted_store store2.task_inst_uncompleted_store
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_inst_completed_store store2.task_inst_completed_store
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_inst_discarded_store store2.task_inst_discarded_store
    && Task_seg_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_seg_uncompleted_store store2.task_seg_uncompleted_store
    && Task_seg_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_seg_completed_store store2.task_seg_completed_store
    && Task_seg_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_seg_discarded_store store2.task_seg_discarded_store
    && User_id_map.equal Int64_set.equal store1.user_id_to_task_ids
      store2.user_id_to_task_ids
    && Task_id_map.equal Int64_set.equal store1.task_id_to_task_inst_ids
      store2.task_id_to_task_inst_ids
    && Task_inst_id_map.equal Int64_int64_option_set.equal
      store1.task_inst_id_to_task_seg_ids store2.task_inst_id_to_task_seg_ids
    && Int64_set.equal store1.sched_req_ids store2.sched_req_ids
    && Sched_req_id_map.equal
      (fun x y -> compare x y = 0)
      store1.sched_req_pending_store store2.sched_req_pending_store
    && Sched_req_id_map.equal
      (fun x y -> compare x y = 0)
      store1.sched_req_discarded_store store2.sched_req_discarded_store
    && Sched_req_id_map.equal
      (fun x y -> compare x y = 0)
      store1.sched_req_record_store store2.sched_req_record_store
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.quota store2.quota
    && Task_seg_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_seg_id_to_progress store2.task_seg_id_to_progress
    && Task_inst_id_map.equal
      (fun x y -> compare x y = 0)
      store1.task_inst_id_to_progress store2.task_inst_id_to_progress

  let agenda_equal (agenda1 : agenda) (agenda2 : agenda) : bool =
    Task_seg_id_map.equal ( = ) agenda1.indexed_by_task_seg_id
      agenda2.indexed_by_task_seg_id
    && Int64_map.equal Task_seg_id_set.equal agenda1.indexed_by_start
      agenda2.indexed_by_start
    && Int64_map.equal Task_seg_id_set.equal agenda1.indexed_by_end_exc
      agenda2.indexed_by_end_exc

  (*$*)

  let sched_data_equal (sd1 : sched_data) (sd2 : sched_data) =
    store_equal sd1.store sd2.store && agenda_equal sd1.agenda sd2.agenda

  let sched_equal ((sid1, sd1) : sched) ((sid2, sd2) : sched) =
    sid1 = sid2 && sched_data_equal sd1 sd2
end

module Diff = struct
  (*$ #use "src/sched.cinaps";;

    print_diff_store ();
    print_add_diff_store ();
    print_sub_diff_store ();
  *)

  let diff_store (store1 : store) (store2 : store) : store_diff =
    {
      task_uncompleted_store_diff =
        Task_id_map_utils.diff ~old:store1.task_uncompleted_store
          store2.task_uncompleted_store;
      task_completed_store_diff =
        Task_id_map_utils.diff ~old:store1.task_completed_store
          store2.task_completed_store;
      task_discarded_store_diff =
        Task_id_map_utils.diff ~old:store1.task_discarded_store
          store2.task_discarded_store;
      task_inst_uncompleted_store_diff =
        Task_inst_id_map_utils.diff ~old:store1.task_inst_uncompleted_store
          store2.task_inst_uncompleted_store;
      task_inst_completed_store_diff =
        Task_inst_id_map_utils.diff ~old:store1.task_inst_completed_store
          store2.task_inst_completed_store;
      task_inst_discarded_store_diff =
        Task_inst_id_map_utils.diff ~old:store1.task_inst_discarded_store
          store2.task_inst_discarded_store;
      task_seg_uncompleted_store_diff =
        Task_seg_id_map_utils.diff ~old:store1.task_seg_uncompleted_store
          store2.task_seg_uncompleted_store;
      task_seg_completed_store_diff =
        Task_seg_id_map_utils.diff ~old:store1.task_seg_completed_store
          store2.task_seg_completed_store;
      task_seg_discarded_store_diff =
        Task_seg_id_map_utils.diff ~old:store1.task_seg_discarded_store
          store2.task_seg_discarded_store;
      user_id_to_task_ids_diff =
        User_id_map_utils.Int64_bucketed.diff_bucketed
          ~old:store1.user_id_to_task_ids store2.user_id_to_task_ids;
      task_id_to_task_inst_ids_diff =
        Task_id_map_utils.Int64_bucketed.diff_bucketed
          ~old:store1.task_id_to_task_inst_ids store2.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids_diff =
        Task_inst_id_map_utils.Int64_int64_option_bucketed.diff_bucketed
          ~old:store1.task_inst_id_to_task_seg_ids
          store2.task_inst_id_to_task_seg_ids;
      sched_req_ids_diff =
        Int64_set_utils.diff ~old:store1.sched_req_ids store2.sched_req_ids;
      sched_req_pending_store_diff =
        Sched_req_id_map_utils.diff ~old:store1.sched_req_pending_store
          store2.sched_req_pending_store;
      sched_req_discarded_store_diff =
        Sched_req_id_map_utils.diff ~old:store1.sched_req_discarded_store
          store2.sched_req_discarded_store;
      sched_req_record_store_diff =
        Sched_req_id_map_utils.diff ~old:store1.sched_req_record_store
          store2.sched_req_record_store;
      quota_diff = Task_inst_id_map_utils.diff ~old:store1.quota store2.quota;
      task_seg_id_to_progress_diff =
        Task_seg_id_map_utils.diff ~old:store1.task_seg_id_to_progress
          store2.task_seg_id_to_progress;
      task_inst_id_to_progress_diff =
        Task_inst_id_map_utils.diff ~old:store1.task_inst_id_to_progress
          store2.task_inst_id_to_progress;
    }

  let add_diff_store (diff : store_diff) (store : store) : store =
    {
      task_uncompleted_store =
        Task_id_map_utils.add_diff diff.task_uncompleted_store_diff
          store.task_uncompleted_store;
      task_completed_store =
        Task_id_map_utils.add_diff diff.task_completed_store_diff
          store.task_completed_store;
      task_discarded_store =
        Task_id_map_utils.add_diff diff.task_discarded_store_diff
          store.task_discarded_store;
      task_inst_uncompleted_store =
        Task_inst_id_map_utils.add_diff diff.task_inst_uncompleted_store_diff
          store.task_inst_uncompleted_store;
      task_inst_completed_store =
        Task_inst_id_map_utils.add_diff diff.task_inst_completed_store_diff
          store.task_inst_completed_store;
      task_inst_discarded_store =
        Task_inst_id_map_utils.add_diff diff.task_inst_discarded_store_diff
          store.task_inst_discarded_store;
      task_seg_uncompleted_store =
        Task_seg_id_map_utils.add_diff diff.task_seg_uncompleted_store_diff
          store.task_seg_uncompleted_store;
      task_seg_completed_store =
        Task_seg_id_map_utils.add_diff diff.task_seg_completed_store_diff
          store.task_seg_completed_store;
      task_seg_discarded_store =
        Task_seg_id_map_utils.add_diff diff.task_seg_discarded_store_diff
          store.task_seg_discarded_store;
      user_id_to_task_ids =
        User_id_map_utils.Int64_bucketed.add_diff_bucketed
          diff.user_id_to_task_ids_diff store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        Task_id_map_utils.Int64_bucketed.add_diff_bucketed
          diff.task_id_to_task_inst_ids_diff store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        Task_inst_id_map_utils.Int64_int64_option_bucketed.add_diff_bucketed
          diff.task_inst_id_to_task_seg_ids_diff
          store.task_inst_id_to_task_seg_ids;
      sched_req_ids =
        Int64_set_utils.add_diff diff.sched_req_ids_diff store.sched_req_ids;
      sched_req_pending_store =
        Sched_req_id_map_utils.add_diff diff.sched_req_pending_store_diff
          store.sched_req_pending_store;
      sched_req_discarded_store =
        Sched_req_id_map_utils.add_diff diff.sched_req_discarded_store_diff
          store.sched_req_discarded_store;
      sched_req_record_store =
        Sched_req_id_map_utils.add_diff diff.sched_req_record_store_diff
          store.sched_req_record_store;
      quota = Task_inst_id_map_utils.add_diff diff.quota_diff store.quota;
      task_seg_id_to_progress =
        Task_seg_id_map_utils.add_diff diff.task_seg_id_to_progress_diff
          store.task_seg_id_to_progress;
      task_inst_id_to_progress =
        Task_inst_id_map_utils.add_diff diff.task_inst_id_to_progress_diff
          store.task_inst_id_to_progress;
    }

  let sub_diff_store (diff : store_diff) (store : store) : store =
    {
      task_uncompleted_store =
        Task_id_map_utils.sub_diff diff.task_uncompleted_store_diff
          store.task_uncompleted_store;
      task_completed_store =
        Task_id_map_utils.sub_diff diff.task_completed_store_diff
          store.task_completed_store;
      task_discarded_store =
        Task_id_map_utils.sub_diff diff.task_discarded_store_diff
          store.task_discarded_store;
      task_inst_uncompleted_store =
        Task_inst_id_map_utils.sub_diff diff.task_inst_uncompleted_store_diff
          store.task_inst_uncompleted_store;
      task_inst_completed_store =
        Task_inst_id_map_utils.sub_diff diff.task_inst_completed_store_diff
          store.task_inst_completed_store;
      task_inst_discarded_store =
        Task_inst_id_map_utils.sub_diff diff.task_inst_discarded_store_diff
          store.task_inst_discarded_store;
      task_seg_uncompleted_store =
        Task_seg_id_map_utils.sub_diff diff.task_seg_uncompleted_store_diff
          store.task_seg_uncompleted_store;
      task_seg_completed_store =
        Task_seg_id_map_utils.sub_diff diff.task_seg_completed_store_diff
          store.task_seg_completed_store;
      task_seg_discarded_store =
        Task_seg_id_map_utils.sub_diff diff.task_seg_discarded_store_diff
          store.task_seg_discarded_store;
      user_id_to_task_ids =
        User_id_map_utils.Int64_bucketed.sub_diff_bucketed
          diff.user_id_to_task_ids_diff store.user_id_to_task_ids;
      task_id_to_task_inst_ids =
        Task_id_map_utils.Int64_bucketed.sub_diff_bucketed
          diff.task_id_to_task_inst_ids_diff store.task_id_to_task_inst_ids;
      task_inst_id_to_task_seg_ids =
        Task_inst_id_map_utils.Int64_int64_option_bucketed.sub_diff_bucketed
          diff.task_inst_id_to_task_seg_ids_diff
          store.task_inst_id_to_task_seg_ids;
      sched_req_ids =
        Int64_set_utils.sub_diff diff.sched_req_ids_diff store.sched_req_ids;
      sched_req_pending_store =
        Sched_req_id_map_utils.sub_diff diff.sched_req_pending_store_diff
          store.sched_req_pending_store;
      sched_req_discarded_store =
        Sched_req_id_map_utils.sub_diff diff.sched_req_discarded_store_diff
          store.sched_req_discarded_store;
      sched_req_record_store =
        Sched_req_id_map_utils.sub_diff diff.sched_req_record_store_diff
          store.sched_req_record_store;
      quota = Task_inst_id_map_utils.sub_diff diff.quota_diff store.quota;
      task_seg_id_to_progress =
        Task_seg_id_map_utils.sub_diff diff.task_seg_id_to_progress_diff
          store.task_seg_id_to_progress;
      task_inst_id_to_progress =
        Task_inst_id_map_utils.sub_diff diff.task_inst_id_to_progress_diff
          store.task_inst_id_to_progress;
    }

  (*$*)

  (*$ #use "src/sched.cinaps";;

    print_diff_agenda ();
    print_add_diff_agenda ();
    print_sub_diff_agenda ();
  *)

  let diff_agenda (agenda1 : agenda) (agenda2 : agenda) : agenda_diff =
    {
      indexed_by_task_seg_id_diff =
        Task_seg_id_map_utils.diff ~old:agenda1.indexed_by_task_seg_id
          agenda2.indexed_by_task_seg_id;
      indexed_by_start_diff =
        Int64_map_utils.Task_seg_id_bucketed.diff_bucketed
          ~old:agenda1.indexed_by_start agenda2.indexed_by_start;
      indexed_by_end_exc_diff =
        Int64_map_utils.Task_seg_id_bucketed.diff_bucketed
          ~old:agenda1.indexed_by_end_exc agenda2.indexed_by_end_exc;
    }

  let add_diff_agenda (diff : agenda_diff) (agenda : agenda) : agenda =
    {
      indexed_by_task_seg_id =
        Task_seg_id_map_utils.add_diff diff.indexed_by_task_seg_id_diff
          agenda.indexed_by_task_seg_id;
      indexed_by_start =
        Int64_map_utils.Task_seg_id_bucketed.add_diff_bucketed
          diff.indexed_by_start_diff agenda.indexed_by_start;
      indexed_by_end_exc =
        Int64_map_utils.Task_seg_id_bucketed.add_diff_bucketed
          diff.indexed_by_end_exc_diff agenda.indexed_by_end_exc;
    }

  let sub_diff_agenda (diff : agenda_diff) (agenda : agenda) : agenda =
    {
      indexed_by_task_seg_id =
        Task_seg_id_map_utils.sub_diff diff.indexed_by_task_seg_id_diff
          agenda.indexed_by_task_seg_id;
      indexed_by_start =
        Int64_map_utils.Task_seg_id_bucketed.sub_diff_bucketed
          diff.indexed_by_start_diff agenda.indexed_by_start;
      indexed_by_end_exc =
        Int64_map_utils.Task_seg_id_bucketed.sub_diff_bucketed
          diff.indexed_by_end_exc_diff agenda.indexed_by_end_exc;
    }

  (*$*)

  let diff_sched_data ~(old : sched_data) (sd : sched_data) : sched_data_diff =
    {
      store_diff = diff_store old.store sd.store;
      agenda_diff = diff_agenda old.agenda sd.agenda;
    }

  let diff_sched ~(old : sched) ((sid, sd) : sched) : sched_diff =
    let sid_old, sd_old = old in
    (sid_old, sid, diff_sched_data ~old:sd_old sd)

  let add_diff_sched_data (diff : sched_data_diff) (sd : sched_data) :
    sched_data =
    {
      store = add_diff_store diff.store_diff sd.store;
      agenda = add_diff_agenda diff.agenda_diff sd.agenda;
    }

  let sub_diff_sched_data (diff : sched_data_diff) (sd : sched_data) :
    sched_data =
    {
      store = sub_diff_store diff.store_diff sd.store;
      agenda = sub_diff_agenda diff.agenda_diff sd.agenda;
    }

  let add_diff_sched ((sid_old, sid_new, sd_diff) : sched_diff)
      ((sid, sd) : sched) : sched =
    if sid_old <> sid then raise Exceptions.Invalid_diff
    else (sid_new, add_diff_sched_data sd_diff sd)

  let sub_diff_sched ((sid_old, sid_new, sd_diff) : sched_diff)
      ((sid, sd) : sched) : sched =
    if sid_new <> sid then raise Exceptions.Invalid_diff
    else (sid_old, sub_diff_sched_data sd_diff sd)
end

module To_string = struct
  let string_of_task_related_status (status : task_related_status) : string =
    match status with
    | `Uncompleted -> "Uncompleted"
    | `Completed -> "Completed"
    | `Discarded -> "Discarded"

  let debug_string_of_sched ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      (sid, sd) =
    Debug_print.bprintf ~indent_level buffer "schedule id : %s\n"
      (Id.sched_id_to_string sid);
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "pending scheduling requests :\n";
    Sched_req_id_map.iter
      (fun id data ->
         Sched_req_.To_string.debug_string_of_sched_req
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.sched_req_pending_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "scheduling request record :\n";
    Sched_req_id_map.iter
      (fun id data ->
         Sched_req_.To_string.debug_string_of_sched_req_record
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.sched_req_record_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "tasks uncompleted :\n";
    Task_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task ~indent_level:(indent_level + 2)
           ~buffer (id, data)
         |> ignore)
      sd.store.task_uncompleted_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "tasks completed :\n";
    Task_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task ~indent_level:(indent_level + 2)
           ~buffer (id, data)
         |> ignore)
      sd.store.task_completed_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "tasks discarded :\n";
    Task_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task ~indent_level:(indent_level + 2)
           ~buffer (id, data)
         |> ignore)
      sd.store.task_discarded_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task insts uncompleted :\n";
    Task_inst_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task_inst
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.task_inst_uncompleted_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task insts completed :\n";
    Task_inst_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task_inst
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.task_inst_completed_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task insts discarded :\n";
    Task_inst_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task_inst
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.task_inst_discarded_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task segs uncompleted :\n";
    Task_seg_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task_seg
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.task_seg_uncompleted_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task segs completed :\n";
    Task_seg_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task_seg
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.task_seg_completed_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task segs discarded :\n";
    Task_seg_id_map.iter
      (fun id data ->
         Task_.To_string.debug_string_of_task_seg
           ~indent_level:(indent_level + 2) ~buffer (id, data)
         |> ignore)
      sd.store.task_seg_discarded_store;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "agenda :\n";
    Seq.iter
      (fun (id, start, end_exc) ->
         Debug_print.bprintf ~indent_level:(indent_level + 2) buffer
           "%s - %s | %s\n"
           ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
               ~display_using_tz_offset_s:None start
             |> Result.get_ok )
           ( Time.To_string.yyyymmdd_hhmmss_string_of_unix_second
               ~display_using_tz_offset_s:None end_exc
             |> Result.get_ok )
           (Task_.Id.string_of_task_seg_id id))
      (Agenda.To_seq.task_seg_place_uncompleted (sid, sd));
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "leftover quota :\n";
    Task_inst_id_map.iter
      (fun id quota ->
         Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s : %Ld\n"
           (Task_.Id.string_of_task_inst_id id)
           quota)
      sd.store.quota;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task seg progress :\n";
    Task_seg_id_map.iter
      (fun id progress ->
         Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "id : %s\n"
           (Task_.Id.string_of_task_seg_id id);
         Task_.To_string.debug_string_of_progress
           ~indent_level:(indent_level + 3) ~buffer progress
         |> ignore)
      sd.store.task_seg_id_to_progress;
    Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
      "task inst progress :\n";
    Task_inst_id_map.iter
      (fun id progress ->
         Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "id : %s\n"
           (Task_.Id.string_of_task_inst_id id);
         Task_.To_string.debug_string_of_progress
           ~indent_level:(indent_level + 3) ~buffer progress
         |> ignore)
      sd.store.task_inst_id_to_progress;
    Buffer.contents buffer
end

module Print = struct
  let debug_print_sched ?(indent_level = 0) sched =
    print_string (To_string.debug_string_of_sched ~indent_level sched)
end
