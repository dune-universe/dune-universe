module Task_ = Task
module Sched_req_ = Sched_req

type sched_id = int

type task_store = Task_.task_data Task_id_map.t

type task_store_diff = Task_.task_data Task_id_map_utils.diff

type task_inst_store = Task_.task_inst_data Task_inst_id_map.t

type task_inst_store_diff = Task_.task_inst_data Task_inst_id_map_utils.diff

type task_seg_store = Task_.task_seg_size Task_seg_id_map.t

type task_seg_store_diff = Task_.task_seg_size Task_seg_id_map_utils.diff

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

val sched_data_empty : sched_data

val empty : sched

module Quota : sig
  val update_quota : int64 Task_inst_id_map.t -> sched -> sched

  val add_quota : int64 Task_inst_id_map.t -> sched -> sched
end

module Task : sig
  module Status : sig
    val get_task_status : Task_.task_id -> sched -> task_related_status option
  end

  module Add : sig
    val add_task :
      parent_user_id:Task_.user_id ->
      Task_.task_data ->
      Task_.task_inst_data list ->
      sched ->
      Task_.task * Task_.task_inst list * sched
  end

  module To_seq : sig
    val task_seq_uncompleted : sched -> Task_.task Seq.t

    val task_seq_completed : sched -> Task_.task Seq.t

    val task_seq_discarded : sched -> Task_.task Seq.t

    val task_seq_all : sched -> Task_.task Seq.t
  end

  module Find : sig
    val find_task_uncompleted_opt :
      Task_.task_id -> sched -> Task_.task_data option

    val find_task_completed_opt :
      Task_.task_id -> sched -> Task_.task_data option

    val find_task_discarded_opt :
      Task_.task_id -> sched -> Task_.task_data option

    val find_task_any_opt : Task_.task_id -> sched -> Task_.task_data option

    val find_task_any_with_status_opt :
      Task_.task_id -> sched -> (Task_.task_data * task_related_status) option
  end

  module Remove : sig
    val remove_task_uncompleted :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_.task_id ->
      sched ->
      sched

    val remove_task_completed :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_.task_id ->
      sched ->
      sched

    val remove_task_discarded :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_.task_id ->
      sched ->
      sched

    val remove_task_all :
      ?remove_children_task_insts:bool ->
      ?remove_children_task_segs:bool ->
      Task_.task_id ->
      sched ->
      sched
  end

  module Move : sig
    val move_task_to_completed : Task_.task_id -> sched -> sched

    val move_task_to_uncompleted : Task_.task_id -> sched -> sched

    val move_task_to_discarded : Task_.task_id -> sched -> sched
  end
end

module Task_inst : sig
  module Status : sig
    val get_task_inst_status :
      Task_.task_inst_id -> sched -> task_related_status option
  end

  module Add : sig
    val add_task_inst :
      parent_task_id:Task_.task_id ->
      Task_.task_inst_data ->
      sched ->
      Task_.task_inst * sched

    val add_task_inst_list :
      parent_task_id:Task_.task_id ->
      Task_.task_inst_data list ->
      sched ->
      Task_.task_inst list * sched
  end

  module To_seq : sig
    val task_inst_seq_uncompleted : sched -> Task_.task_inst Seq.t

    val task_inst_seq_completed : sched -> Task_.task_inst Seq.t

    val task_inst_seq_discarded : sched -> Task_.task_inst Seq.t

    val task_inst_seq_all : sched -> Task_.task_inst Seq.t
  end

  module Find : sig
    val find_task_inst_uncompleted_opt :
      Task_.task_inst_id -> sched -> Task_.task_inst_data option

    val find_task_inst_completed_opt :
      Task_.task_inst_id -> sched -> Task_.task_inst_data option

    val find_task_inst_discarded_opt :
      Task_.task_inst_id -> sched -> Task_.task_inst_data option

    val find_task_inst_any_opt :
      Task_.task_inst_id -> sched -> Task_.task_inst_data option

    val find_task_inst_any_with_status_opt :
      Task_.task_inst_id ->
      sched ->
      (Task_.task_inst_data * task_related_status) option

    val find_task_inst_ids_by_task_id :
      Task_.task_id -> sched -> Task_.task_inst_id Seq.t

    val find_task_inst_seq_uncompleted_by_task_id :
      Task_.task_id -> sched -> Task_.task_inst Seq.t

    val find_task_inst_seq_completed_by_task_id :
      Task_.task_id -> sched -> Task_.task_inst Seq.t

    val find_task_inst_seq_discarded_by_task_id :
      Task_.task_id -> sched -> Task_.task_inst Seq.t

    val find_task_inst_seq_any_by_task_id :
      Task_.task_id -> sched -> Task_.task_inst Seq.t

    val find_task_inst_seq_any_with_status_by_task_id :
      Task_.task_id -> sched -> (Task_.task_inst * task_related_status) Seq.t
  end

  module Remove : sig
    val remove_task_inst_uncompleted :
      ?remove_children_task_segs:bool -> Task_.task_inst_id -> sched -> sched

    val remove_task_inst_completed :
      ?remove_children_task_segs:bool -> Task_.task_inst_id -> sched -> sched

    val remove_task_inst_discarded :
      ?remove_children_task_segs:bool -> Task_.task_inst_id -> sched -> sched

    val remove_task_inst_all :
      ?remove_children_task_segs:bool -> Task_.task_inst_id -> sched -> sched

    (* val remove_task_inst_uncompleted_strict :
     *   ?remove_children_task_segs:bool ->
     *   Task_.task_inst_id ->
     *   sched ->
     *   (sched, unit) result
     * 
     * val remove_task_inst_completed_strict :
     *   ?remove_children_task_segs:bool ->
     *   Task_.task_inst_id ->
     *   sched ->
     *   (sched, unit) result
     * 
     * val remove_task_inst_discarded_strict :
     *   ?remove_children_task_segs:bool ->
     *   Task_.task_inst_id ->
     *   sched ->
     *   (sched, unit) result *)

    val remove_task_inst_uncompleted_seq :
      ?remove_children_task_segs:bool ->
      Task_.task_inst_id Seq.t ->
      sched ->
      sched

    val remove_task_inst_completed_seq :
      ?remove_children_task_segs:bool ->
      Task_.task_inst_id Seq.t ->
      sched ->
      sched

    val remove_task_inst_discarded_seq :
      ?remove_children_task_segs:bool ->
      Task_.task_inst_id Seq.t ->
      sched ->
      sched
  end

  module Move : sig
    val move_task_inst_to_completed : Task_.task_inst_id -> sched -> sched

    val move_task_inst_to_uncompleted : Task_.task_inst_id -> sched -> sched

    val move_task_inst_to_discarded : Task_.task_inst_id -> sched -> sched
  end
end

module Task_seg : sig
  module Status : sig
    val get_task_seg_status :
      Task_.task_seg_id -> sched -> task_related_status option
  end

  module Add : sig
    val add_task_seg :
      parent_task_inst_id:Task_.task_inst_id ->
      Task_.task_seg_size ->
      sched ->
      Task_.task_seg * sched

    val add_task_seg_via_task_seg_alloc_req :
      Task_.task_seg_alloc_req -> sched -> Task_.task_seg * sched

    val add_task_segs_via_task_seg_alloc_req_list :
      Task_.task_seg_alloc_req list -> sched -> Task_.task_seg list * sched

    val add_task_seg_via_task_seg_place : Task_.task_seg_place -> sched -> sched

    val add_task_segs_via_task_seg_place_list :
      Task_.task_seg_place list -> sched -> sched

    val add_task_segs_via_task_seg_place_seq :
      Task_.task_seg_place Seq.t -> sched -> sched
  end

  module To_seq : sig
    val task_seg_seq_uncompleted : sched -> Task_.task_seg Seq.t

    val task_seg_seq_completed : sched -> Task_.task_seg Seq.t

    val task_seg_seq_discarded : sched -> Task_.task_seg Seq.t

    val task_seg_seq_all : sched -> Task_.task_seg Seq.t
  end

  module Find : sig
    val find_task_seg_uncompleted_opt :
      Task_.task_seg_id -> sched -> Task_.task_seg_size option

    val find_task_seg_completed_opt :
      Task_.task_seg_id -> sched -> Task_.task_seg_size option

    val find_task_seg_discarded_opt :
      Task_.task_seg_id -> sched -> Task_.task_seg_size option

    val find_task_seg_any_opt :
      Task_.task_seg_id -> sched -> Task_.task_seg_size option

    val find_task_seg_any_with_status_opt :
      Task_.task_seg_id ->
      sched ->
      (Task_.task_seg_size * task_related_status) option

    val find_task_seg_ids_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.task_seg_id Seq.t

    val find_task_seg_seq_uncompleted_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_completed_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_discarded_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_any_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_any_with_status_by_task_inst_id :
      Task_.task_inst_id ->
      sched ->
      (Task_.task_seg * task_related_status) Seq.t

    val find_task_seg_ids_by_task_id :
      Task_.task_id -> sched -> Task_.task_seg_id Seq.t

    val find_task_seg_seq_uncompleted_by_task_id :
      Task_.task_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_completed_by_task_id :
      Task_.task_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_discarded_by_task_id :
      Task_.task_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_any_by_task_id :
      Task_.task_id -> sched -> Task_.task_seg Seq.t

    val find_task_seg_seq_any_with_status_by_task_id :
      Task_.task_id -> sched -> (Task_.task_seg * task_related_status) Seq.t
  end

  module Remove : sig
    val remove_task_seg_uncompleted : Task_.task_seg_id -> sched -> sched

    val remove_task_seg_completed : Task_.task_seg_id -> sched -> sched

    val remove_task_seg_discarded : Task_.task_seg_id -> sched -> sched

    val remove_task_seg_all : Task_.task_seg_id -> sched -> sched

    val remove_task_seg_uncompleted_seq :
      Task_.task_seg_id Seq.t -> sched -> sched

    val remove_task_seg_completed_seq :
      Task_.task_seg_id Seq.t -> sched -> sched

    val remove_task_seg_discarded_seq :
      Task_.task_seg_id Seq.t -> sched -> sched
  end

  module Move : sig
    val move_task_seg_to_completed : Task_.task_seg_id -> sched -> sched

    val move_task_seg_to_uncompleted : Task_.task_seg_id -> sched -> sched

    val move_task_seg_to_discarded : Task_.task_seg_id -> sched -> sched
  end
end

module Progress : sig
  module Add : sig
    val add_task_seg_progress_chunk :
      Task_.task_seg_id -> int64 * int64 -> sched -> sched

    val add_task_seg_progress_chunk :
      Task_.task_seg_id -> int64 * int64 -> sched -> sched

    val add_task_inst_progress_chunk :
      Task_.task_inst_id -> int64 * int64 -> sched -> sched
  end

  module Find : sig
    val find_task_seg_progress :
      Task_.task_seg_id -> sched -> Task_.progress option

    val find_task_seg_progress_chunk_set :
      Task_.task_seg_id -> sched -> Int64_int64_set.t

    val find_task_seg_progress_chunk_seq :
      Task_.task_seg_id -> sched -> (int64 * int64) Seq.t

    val find_task_seg_progress :
      Task_.task_seg_id -> sched -> Task_.progress option

    val find_task_seg_progress_seq_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.progress Seq.t

    val find_task_seg_progress_seq_by_task_id :
      Task_.task_id -> sched -> Task_.progress Seq.t

    val find_task_seg_progress_chunk_set :
      Task_.task_seg_id -> sched -> Int64_int64_set.t

    val find_task_seg_progress_chunk_seq :
      Task_.task_seg_id -> sched -> (int64 * int64) Seq.t

    val find_task_seg_progress_chunk_seq_by_task_inst_id :
      Task_.task_inst_id -> sched -> (int64 * int64) Seq.t

    val find_task_seg_progress_chunk_seq_by_task_id :
      Task_.task_id -> sched -> (int64 * int64) Seq.t

    val find_task_inst_progress :
      Task_.task_inst_id -> sched -> Task_.progress option

    val find_task_inst_progress_seq_by_task_id :
      Task_.task_id -> sched -> Task_.progress Seq.t

    val find_task_inst_progress_chunk_set :
      Task_.task_inst_id -> sched -> Int64_int64_set.t

    val find_task_inst_progress_chunk_seq :
      Task_.task_inst_id -> sched -> (int64 * int64) Seq.t

    val find_task_inst_progress_chunk_seq_by_task_id :
      Task_.task_id -> sched -> (int64 * int64) Seq.t
  end

  module Remove : sig
    val remove_task_seg_progress_chunk :
      Task_.task_seg_id -> int64 * int64 -> sched -> sched

    val remove_task_seg_progress_chunk :
      Task_.task_seg_id -> int64 * int64 -> sched -> sched

    val remove_task_inst_progress_chunk :
      Task_.task_inst_id -> int64 * int64 -> sched -> sched
  end
end

module Agenda : sig
  module Add : sig
    val add_task_seg_place : Task_.task_seg_place -> sched -> sched

    val add_task_seg_place_list : Task_.task_seg_place list -> sched -> sched

    val add_task_seg_place_seq : Task_.task_seg_place Seq.t -> sched -> sched
  end

  module Range : sig
    val task_seg_id_set :
      start:int64 option ->
      end_exc:int64 option ->
      include_task_seg_place_starting_within_time_slot:bool ->
      include_task_seg_place_ending_within_time_slot:bool ->
      sched ->
      Task_seg_id_set.t

    val task_seg_place_set :
      start:int64 option ->
      end_exc:int64 option ->
      include_task_seg_place_starting_within_time_slot:bool ->
      include_task_seg_place_ending_within_time_slot:bool ->
      sched ->
      Task_seg_place_set.t
  end

  module Filter : sig
    val filter_task_seg_place_seq :
      ?start:int64 ->
      ?end_exc:int64 ->
      ?include_task_seg_place_starting_within_time_slot:bool ->
      ?include_task_seg_place_ending_within_time_slot:bool ->
      (Task_.task_seg_place -> bool) ->
      sched ->
      Task_.task_seg_place Seq.t
  end

  module To_seq : sig
    val task_seg_place_uncompleted :
      ?start:int64 ->
      ?end_exc:int64 ->
      ?include_task_seg_place_starting_within_time_slot:bool ->
      ?include_task_seg_place_ending_within_time_slot:bool ->
      sched ->
      Task_.task_seg_place Seq.t

    val task_seg_place_completed :
      ?start:int64 ->
      ?end_exc:int64 ->
      ?include_task_seg_place_starting_within_time_slot:bool ->
      ?include_task_seg_place_ending_within_time_slot:bool ->
      sched ->
      Task_.task_seg_place Seq.t

    val task_seg_place_discarded :
      ?start:int64 ->
      ?end_exc:int64 ->
      ?include_task_seg_place_starting_within_time_slot:bool ->
      ?include_task_seg_place_ending_within_time_slot:bool ->
      sched ->
      Task_.task_seg_place Seq.t

    val task_seg_place_all :
      ?start:int64 ->
      ?end_exc:int64 ->
      ?include_task_seg_place_starting_within_time_slot:bool ->
      ?include_task_seg_place_ending_within_time_slot:bool ->
      sched ->
      Task_.task_seg_place Seq.t
  end

  module Find : sig
    val find_task_seg_place_seq_by_task_id :
      Task_.task_id -> sched -> Task_.task_seg_place Seq.t

    val find_task_seg_place_seq_by_task_inst_id :
      Task_.task_inst_id -> sched -> Task_.task_seg_place Seq.t

    val find_task_seg_place_opt_by_task_seg_id :
      Task_.task_seg_id -> sched -> Task_.task_seg_place option
  end

  module Remove : sig
    val remove_task_seg_place : Task_.task_seg_place -> sched -> sched

    val remove_task_seg_place_seq : Task_.task_seg_place Seq.t -> sched -> sched

    val remove_task_seg_place_by_task_id : Task_.task_id -> sched -> sched

    val remove_task_seg_place_by_task_inst_id :
      Task_.task_inst_id -> sched -> sched

    val remove_task_seg_place_by_task_seg_id :
      Task_.task_seg_id -> sched -> sched
  end

  module Time_slot : sig
    val get_occupied_time_slots :
      ?exclude_parallelizable_task_seg_places:bool ->
      ?start:int64 ->
      ?end_exc:int64 ->
      sched ->
      (int64 * int64) Seq.t

    val get_occupied_time_slots_with_task_seg_place_count :
      ?exclude_parallelizable_task_seg_places:bool ->
      ?start:int64 ->
      ?end_exc:int64 ->
      sched ->
      ((int64 * int64) * int) Seq.t

    val get_occupied_time_slots_up_to_task_seg_place_count :
      ?exclude_parallelizable_task_seg_places:bool ->
      ?start:int64 ->
      ?end_exc:int64 ->
      up_to_task_seg_place_count_inc:int ->
      sched ->
      (int64 * int64) Seq.t

    val get_free_time_slots :
      ?include_parallelizable_task_seg_places:bool ->
      start:int64 ->
      end_exc:int64 ->
      sched ->
      (int64 * int64) Seq.t

    val get_free_or_occupied_time_slots_up_to_task_seg_place_count :
      ?include_parallelizable_task_seg_places:bool ->
      start:int64 ->
      end_exc:int64 ->
      up_to_task_seg_place_count_inc:int ->
      sched ->
      (int64 * int64) Seq.t

    val task_seg_place_count_in_time_slot :
      start:int64 -> end_exc:int64 -> sched -> int
  end
end

module Sched_req : sig
  module Status : sig
    val get_sched_req_status :
      Sched_req_.sched_req_id -> sched -> sched_req_status option
  end

  module Add : sig
    val add_sched_req_data :
      Sched_req_.sched_req_data ->
      sched ->
      (Sched_req_.sched_req * sched, unit) result

    val add_sched_req_data_list :
      Sched_req_.sched_req_data list ->
      sched ->
      (Sched_req_.sched_req list * sched, unit) result
  end

  module Partition : sig
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

    module Pending : sig
      val partition_based_on_time_point :
        int64 ->
        sched ->
        Sched_req_.sched_req_data partition_based_on_time_point

      val partition_based_on_time_slot :
        start:int64 ->
        end_exc:int64 ->
        sched ->
        Sched_req_.sched_req_data partition_based_on_time_slot
    end

    module Record : sig
      val partition_based_on_time_point :
        int64 ->
        sched ->
        Sched_req_.sched_req_record_data partition_based_on_time_point

      val partition_based_on_time_slot :
        start:int64 ->
        end_exc:int64 ->
        sched ->
        Sched_req_.sched_req_record_data partition_based_on_time_slot
    end
  end

  module To_seq : sig
    module Pending : sig
      val pending_sched_req_seq :
        ?start:int64 ->
        ?end_exc:int64 ->
        ?include_sched_req_starting_within_time_slot:bool ->
        ?include_sched_req_ending_within_time_slot:bool ->
        sched ->
        Sched_req_.sched_req Seq.t
    end

    module Record : sig
      val sched_req_record_seq :
        ?start:int64 ->
        ?end_exc:int64 ->
        ?include_sched_req_record_starting_within_time_slot:bool ->
        ?include_sched_req_record_ending_within_time_slot:bool ->
        sched ->
        Sched_req_.sched_req_record Seq.t
    end
  end

  module Filter : sig
    module Pending : sig
      val filter_pending_sched_req_seq :
        ?start:int64 ->
        ?end_exc:int64 ->
        ?include_sched_req_starting_within_time_slot:bool ->
        ?include_sched_req_ending_within_time_slot:bool ->
        (Sched_req_.sched_req -> bool) ->
        sched ->
        Sched_req_.sched_req Seq.t
    end

    module Record : sig
      val filter_sched_req_record_seq :
        ?start:int64 ->
        ?end_exc:int64 ->
        ?include_sched_req_record_starting_within_time_slot:bool ->
        ?include_sched_req_record_ending_within_time_slot:bool ->
        (Sched_req_.sched_req_record -> bool) ->
        sched ->
        Sched_req_.sched_req_record Seq.t
    end
  end

  module Find : sig
    module Pending : sig
      val find_pending_sched_req :
        Sched_req_.sched_req_id -> sched -> Sched_req_.sched_req_data option

      val find_pending_sched_req_by_task_id :
        Task_.task_id -> sched -> Sched_req_.sched_req Seq.t

      val find_pending_sched_req_by_task_inst_id :
        Task_.task_inst_id -> sched -> Sched_req_.sched_req Seq.t
    end

    module Record : sig
      val find_sched_req_record :
        Sched_req_.sched_req_id ->
        sched ->
        Sched_req_.sched_req_record_data option

      val find_sched_req_record_by_task_id :
        Task_.task_id -> sched -> Sched_req_.sched_req_record Seq.t

      val find_sched_req_record_by_task_inst_id :
        Task_.task_inst_id -> sched -> Sched_req_.sched_req_record Seq.t

      val find_sched_req_record_by_task_seg_id :
        Task_.task_seg_id -> sched -> Sched_req_.sched_req_record Seq.t
    end
  end

  module Remove : sig
    module Pending : sig
      val remove_pending_sched_req : Sched_req_.sched_req_id -> sched -> sched

      val remove_pending_sched_req_if_contains_matching_task_seg_alloc_req :
        (Task_.task_seg_alloc_req -> bool) -> sched -> sched

      val remove_pending_sched_req_data_unit_if_contains_matching_task_seg_alloc_req :
        (Task_.task_seg_alloc_req -> bool) -> sched -> sched

      val remove_pending_sched_req_by_task_id : Task_.task_id -> sched -> sched

      val remove_pending_sched_req_by_task_inst_id :
        Task_.task_inst_id -> sched -> sched

      val remove_pending_sched_req_by_task_seg_id :
        Task_.task_seg_id -> sched -> sched

      val remove_pending_sched_req_data_unit_by_task_id :
        Task_.task_id -> sched -> sched

      val remove_pending_sched_req_data_unit_by_task_inst_id :
        Task_.task_inst_id -> sched -> sched

      val remove_pending_sched_req_data_unit_by_task_seg_id :
        Task_.task_seg_id -> sched -> sched
    end

    module Record : sig
      val remove_sched_req_record : Sched_req_.sched_req_id -> sched -> sched

      val remove_sched_req_record_if_contains_matching_task_seg :
        (Task_.task_seg -> bool) -> sched -> sched

      val remove_sched_req_record_data_unit_if_contains_matching_task_seg :
        (Task_.task_seg -> bool) -> sched -> sched

      val remove_sched_req_record_by_task_id : Task_.task_id -> sched -> sched

      val remove_sched_req_record_by_task_inst_id :
        Task_.task_inst_id -> sched -> sched

      val remove_sched_req_record_by_task_seg_id :
        Task_.task_seg_id -> sched -> sched

      val remove_sched_req_record_data_unit_by_task_id :
        Task_.task_id -> sched -> sched

      val remove_sched_req_record_data_unit_by_task_inst_id :
        Task_.task_inst_id -> sched -> sched

      val remove_sched_req_record_data_unit_by_task_seg_id :
        Task_.task_seg_id -> sched -> sched
    end
  end

  module Discard : sig
    val discard_pending_sched_req : Sched_req_.sched_req_id -> sched -> sched
  end

  module Allocate_task_segs : sig
    val allocate_task_segs_for_pending_sched_reqs :
      start:int64 ->
      end_exc:int64 ->
      include_sched_reqs_starting_within_time_slot:bool ->
      include_sched_reqs_ending_within_time_slot:bool ->
      up_to_sched_req_id_inc:Sched_req_.sched_req_id option ->
      sched ->
      Sched_req_.sched_req_record list * sched
  end
end

module Recur : sig
  val instantiate : start:int64 -> end_exc:int64 -> sched -> sched
end

module Overdue : sig
  val get_overdue_task_seg_places :
    deadline:int64 -> sched -> Task_.task_seg_place Seq.t

  val get_overdue_task_segs : deadline:int64 -> sched -> Task_.task_seg Seq.t

  val add_sched_reqs_for_overdue_task_segs :
    start:int64 -> end_exc:int64 -> sched -> sched
end

module Serialize : sig
  val pack_task_uncompleted_store : task_store -> Sched_t.task list

  val pack_task_completed_store : task_store -> Sched_t.task list

  val pack_task_discarded_store : task_store -> Sched_t.task list

  val pack_task_inst_uncompleted_store :
    task_inst_store -> Sched_t.task_inst list

  val pack_task_inst_completed_store : task_inst_store -> Sched_t.task_inst list

  val pack_task_inst_discarded_store : task_inst_store -> Sched_t.task_inst list

  val pack_task_seg_uncompleted_store : task_seg_store -> Sched_t.task_seg list

  val pack_task_seg_completed_store : task_seg_store -> Sched_t.task_seg list

  val pack_task_seg_discarded_store : task_seg_store -> Sched_t.task_seg list

  val pack_sched_req_pending_store :
    sched_req_store -> Sched_req_t.sched_req list

  val pack_sched_req_record_store :
    sched_req_record_store -> Sched_req_t.sched_req_record list

  val pack_quota :
    int64 Task_inst_id_map.t -> (Task_t.task_inst_id * (int32 * int32)) list

  val pack_user_id_to_task_ids :
    Int64_set.t User_id_map.t -> (Task_t.user_id * (int32 * int32) list) list

  val pack_task_id_to_task_inst_ids :
    Int64_set.t Task_id_map.t -> (Task_t.task_id * (int32 * int32) list) list

  val pack_task_inst_id_to_task_seg_ids :
    Int64_int64_option_set.t Task_inst_id_map.t ->
    (Task_t.task_inst_id * ((int32 * int32) * (int32 * int32) option) list) list

  val pack_task_seg_id_to_progress :
    Task_.progress Task_seg_id_map.t ->
    (Task_t.task_seg_id * Task_t.progress) list

  val pack_task_inst_id_to_progress :
    Task_.progress Task_inst_id_map.t ->
    (Task_t.task_inst_id * Task_t.progress) list

  val pack_indexed_by_task_seg_id :
    (int64 * int64) Task_seg_id_map.t ->
    (Task_t.task_seg_id * ((int32 * int32) * (int32 * int32))) list

  val pack_indexed_by_start :
    task_seg_place_map -> ((int32 * int32) * Task_t.task_seg_id list) list

  val pack_indexed_by_end_exc :
    task_seg_place_map -> ((int32 * int32) * Task_t.task_seg_id list) list

  val pack_sched_req_ids : Int64_set.t -> (int32 * int32) list

  val pack_sched : sched -> Sched_t.sched

  val pack_sched_diff : sched_diff -> Sched_t.sched_diff

  val json_string_of_sched : sched -> string

  val json_string_of_sched_diff : sched_diff -> string
end

module Deserialize : sig
  val unpack_task_uncompleted_list : Sched_t.task list -> task_store

  val unpack_task_completed_list : Sched_t.task list -> task_store

  val unpack_task_discarded_list : Sched_t.task list -> task_store

  val unpack_task_inst_uncompleted_list :
    Sched_t.task_inst list -> task_inst_store

  val unpack_task_inst_completed_list :
    Sched_t.task_inst list -> task_inst_store

  val unpack_task_inst_discarded_list :
    Sched_t.task_inst list -> task_inst_store

  val unpack_task_seg_uncompleted_list : Sched_t.task_seg list -> task_seg_store

  val unpack_task_seg_completed_list : Sched_t.task_seg list -> task_seg_store

  val unpack_task_seg_discarded_list : Sched_t.task_seg list -> task_seg_store

  val unpack_sched_req_pending_list :
    Sched_req_t.sched_req list -> sched_req_store

  val unpack_sched_req_record_list :
    Sched_req_t.sched_req_record list -> sched_req_record_store

  val unpack_quota :
    (Task_t.task_inst_id * (int32 * int32)) list -> int64 Task_inst_id_map.t

  val unpack_user_id_to_task_ids :
    (Task_t.user_id * (int32 * int32) list) list -> Int64_set.t User_id_map.t

  val unpack_task_id_to_task_inst_ids :
    (Task_t.task_id * (int32 * int32) list) list -> Int64_set.t Task_id_map.t

  val unpack_task_inst_id_to_task_seg_ids :
    (Task_t.task_inst_id * ((int32 * int32) * (int32 * int32) option) list) list ->
    Int64_int64_option_set.t Task_inst_id_map.t

  val unpack_task_seg_id_to_progress :
    (Task_t.task_seg_id * Task_t.progress) list ->
    Task_.progress Task_seg_id_map.t

  val unpack_task_inst_id_to_progress :
    (Task_t.task_inst_id * Task_t.progress) list ->
    Task_.progress Task_inst_id_map.t

  val unpack_indexed_by_task_seg_id :
    (Task_t.task_seg_id * ((int32 * int32) * (int32 * int32))) list ->
    (int64 * int64) Task_seg_id_map.t

  val unpack_indexed_by_end_exc :
    ((int32 * int32) * Task_t.task_seg_id list) list -> task_seg_place_map

  val unpack_indexed_by_start :
    ((int32 * int32) * Task_t.task_seg_id list) list -> task_seg_place_map

  val unpack_sched_req_ids : (int32 * int32) list -> Int64_set.t

  val unpack_sched : Sched_t.sched -> sched

  val unpack_sched_diff : Sched_t.sched_diff -> sched_diff

  val sched_of_json_string : string -> sched

  val sched_diff_of_json_string : string -> sched_diff
end

module Equal : sig
  val sched_data_equal : sched_data -> sched_data -> bool

  val sched_equal : sched -> sched -> bool
end

module Diff : sig
  val diff_sched_data : old:sched_data -> sched_data -> sched_data_diff

  val diff_sched : old:sched -> sched -> sched_diff

  val add_diff_sched_data : sched_data_diff -> sched_data -> sched_data

  val add_diff_sched : sched_diff -> sched -> sched

  val sub_diff_sched_data : sched_data_diff -> sched_data -> sched_data

  val sub_diff_sched : sched_diff -> sched -> sched
end

module To_string : sig
  val string_of_task_related_status : task_related_status -> string

  val debug_string_of_sched :
    ?indent_level:int -> ?buffer:Buffer.t -> sched -> string
end

module Print : sig
  val debug_print_sched : ?indent_level:int -> sched -> unit
end
