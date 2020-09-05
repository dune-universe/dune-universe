module Task_ = Task

type t

type action_record =
  | Updated_head of Sched.sched_id
  | Added_new_head of Sched.sched_id
  | Did_nothing

val make_empty : unit -> t

val of_sched_list : Sched.sched list -> t

module Read : sig
  val get_head : t -> Sched.sched
end

module In_place_head : sig
  module Task : sig
    module Add : sig
      val add_task :
        parent_user_id:int64 ->
        Task_.task_data ->
        Task_.task_inst_data list ->
        t ->
        Task_.task * Task_.task_inst list * action_record
    end

    module Move : sig
      val move_task_to_completed : Task_.task_id -> t -> action_record

      val move_task_to_uncompleted : Task_.task_id -> t -> action_record

      val move_task_to_discarded : Task_.task_id -> t -> action_record
    end
  end

  module Task_inst : sig
    module Add : sig
      val add_task_inst :
        parent_task_id:Task_.task_id ->
        Task_.task_inst_data ->
        t ->
        Task_.task_inst * action_record
    end

    module Move : sig
      val move_task_inst_to_completed : Task_.task_inst_id -> t -> action_record

      val move_task_inst_to_uncompleted :
        Task_.task_inst_id -> t -> action_record

      val move_task_inst_to_discarded : Task_.task_inst_id -> t -> action_record
    end
  end

  module Task_seg : sig
    module Move : sig
      val move_task_seg_to_completed : Task_.task_seg_id -> t -> action_record

      val move_task_seg_to_uncompleted : Task_.task_seg_id -> t -> action_record

      val move_task_seg_to_discarded : Task_.task_seg_id -> t -> action_record
    end
  end

  module Sched_req : sig
    module Add : sig
      val add_sched_req :
        Sched_req.sched_req_data ->
        t ->
        (Sched_req.sched_req, unit) result * action_record
    end
  end

  module Recur : sig
    val instantiate : start:int64 -> end_exc:int64 -> t -> action_record
  end

  module Progress : sig
    module Add : sig
      val add_task_seg_progress_chunk :
        Task_.task_seg_id -> int64 * int64 -> t -> action_record

      val add_task_inst_progress_chunk :
        Task_.task_inst_id -> int64 * int64 -> t -> action_record
    end
  end
end

module Maybe_append_to_head : sig
  val remove_task : Task_.task_id -> t -> action_record

  val remove_task_inst : Task_.task_inst_id -> t -> action_record

  val remove_task_seg_progress_chunk :
    Task_.task_seg_id -> int64 * int64 -> t -> action_record

  val remove_task_inst_progress_chunk :
    Task_.task_inst_id -> int64 * int64 -> t -> action_record

  val remove_pending_sched_req : Sched_req.sched_req_id -> t -> action_record

  val sched :
    start:int64 ->
    end_exc:int64 ->
    include_sched_reqs_starting_within_time_slot:bool ->
    include_sched_reqs_ending_within_time_slot:bool ->
    up_to_sched_req_id_inc:Sched_req.sched_req_id option ->
    t ->
    (unit, unit) result * action_record
end

module Append_to_head : sig
  val snapshot : t -> action_record
end

module Equal : sig
  val equal : t -> t -> bool
end

module Serialize : sig
  val base_and_diffs_of_list :
    Sched.sched list -> (Sched.sched * Sched.sched_diff list) option

  val to_base_and_diffs : t -> (Sched.sched * Sched.sched_diff list) option

  val write_to_dir : dir:string -> t -> (unit, string) result
end

module Deserialize : sig
  val list_of_base_and_diffs :
    Sched.sched -> Sched.sched_diff list -> Sched.sched list

  val of_base_and_diffs : Sched.sched -> Sched.sched_diff list -> t

  val read_from_dir : dir:string -> (t, string) result
end

module To_string : sig
  val debug_string_of_sched_ver_history :
    ?indent_level:int -> ?buffer:Buffer.t -> t -> string

  val debug_string_of_action_record :
    ?indent_level:int -> ?buffer:Buffer.t -> action_record -> string
end

module Print : sig
  val debug_print_sched_ver_history : ?indent_level:int -> t -> unit

  val debug_print_action_record : ?indent_level:int -> action_record -> unit
end
