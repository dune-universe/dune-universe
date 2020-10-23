open Int64_utils

type sched_req_id = int64

type sched_req = sched_req_id * sched_req_data

and sched_req_data_unit =
  (Task.task_seg_alloc_req, int64, Time_slot.t) Sched_req_data_unit_skeleton.t

and sched_req_data = sched_req_data_unit list

type sched_req_record = sched_req_id * sched_req_record_data

and sched_req_record_data_unit =
  (Task.task_seg, int64, Time_slot.t) Sched_req_data_unit_skeleton.t

and sched_req_record_data = sched_req_record_data_unit list

let flexibility_score_of_sched_req_record
    ((_id, req_record_data_unit_list) : sched_req_record) : float =
  match List.hd req_record_data_unit_list with
  | Sched_req_data_unit_skeleton.Fixed _ -> 0.0
  | Shift x ->
    let task_seg_alloc_req_sum_len =
      Task.task_seg_alloc_req_sum_length x.task_seg_related_data_list
      |> Int64.to_float
    in
    let time_slot_sum_len =
      Time_slots.Sum.sum_length_list x.time_slots |> Int64.to_float
    in
    1. -. (task_seg_alloc_req_sum_len /. time_slot_sum_len)
  | Split_and_shift x ->
    let _, size = x.task_seg_related_data in
    let time_slot_sum_len =
      Time_slots.Sum.sum_length_list x.time_slots |> Int64.to_float
    in
    1. -. (Int64.to_float size /. time_slot_sum_len)
  | Split_even x ->
    let _, size = x.task_seg_related_data in
    let time_slot_sum_len =
      Time_slots.inter (x.time_slots |> List.to_seq) (x.buckets |> List.to_seq)
      |> List.of_seq
      |> Time_slots.Sum.sum_length_list
      |> Int64.to_float
    in
    1. -. (Int64.to_float size /. time_slot_sum_len)
  | Time_share x ->
    let task_seg_alloc_req_sum_len =
      Task.task_seg_alloc_req_sum_length x.task_seg_related_data_list
      |> Int64.to_float
    in
    let time_slot_sum_len =
      Time_slots.Sum.sum_length_list x.time_slots |> Int64.to_float
    in
    1. -. (task_seg_alloc_req_sum_len /. time_slot_sum_len)
  | Push_toward x ->
    let _, size = x.task_seg_related_data in
    let time_slot_sum_len =
      Time_slots.Sum.sum_length_list x.time_slots |> Int64.to_float
    in
    1. -. (Int64.to_float size /. time_slot_sum_len)

let sort_sched_req_record_list_by_flexibility_score
    (reqs : sched_req_record list) : sched_req_record list =
  List.sort
    (fun x y ->
       compare
         (flexibility_score_of_sched_req_record x)
         (flexibility_score_of_sched_req_record y))
    reqs

let start_and_end_exc_bound_of_sched_req_or_record
    ((_id, req_record_data_unit_list) :
       sched_req_id
       * ('a, int64, Time_slot.t) Sched_req_data_unit_skeleton.t list) :
  (int64 * int64) option =
  List.fold_left
    (fun acc req_record_data_unit ->
       let cur =
         match req_record_data_unit with
         | Sched_req_data_unit_skeleton.Fixed
             { task_seg_related_data = _, task_seg_size; start } ->
           Some (start, start +^ task_seg_size)
         | Shift { time_slots; _ }
         | Split_and_shift { time_slots }
         | Split_even { time_slots; _ }
         | Time_share { time_slots; _ }
         | Push_toward { time_slots; _ } ->
           Time_slots.Bound.min_start_and_max_end_exc_list time_slots
       in
       match acc with
       | None -> cur
       | Some (start, end_exc) -> (
           match cur with
           | None -> acc
           | Some (cur_start, cur_end_exc) ->
             Some (min start cur_start, max end_exc cur_end_exc) ))
    None req_record_data_unit_list

let sched_req_or_record_before_time (x : int64)
    (sched_req_or_record :
       sched_req_id
       * ('a, int64, Time_slot.t) Sched_req_data_unit_skeleton.t list) : bool =
  match start_and_end_exc_bound_of_sched_req_or_record sched_req_or_record with
  | None -> false
  | Some (_, end_exc) -> end_exc < x

let sched_req_or_record_after_time (x : int64)
    (sched_req_or_record :
       sched_req_id
       * ('a, int64, Time_slot.t) Sched_req_data_unit_skeleton.t list) : bool =
  match start_and_end_exc_bound_of_sched_req_or_record sched_req_or_record with
  | None -> false
  | Some (start, _) -> x < start

let sched_req_or_record_fully_within_time_slot ~start ~end_exc
    (sched_req_or_record :
       sched_req_id
       * ('a, int64, Time_slot.t) Sched_req_data_unit_skeleton.t list) : bool =
  match start_and_end_exc_bound_of_sched_req_or_record sched_req_or_record with
  | None -> false
  | Some (start', end_exc') -> start <= start' && end_exc' <= end_exc

let sched_req_or_record_starting_within_time_slot ~start ~end_exc
    (sched_req_or_record :
       sched_req_id
       * ('a, int64, Time_slot.t) Sched_req_data_unit_skeleton.t list) : bool =
  match start_and_end_exc_bound_of_sched_req_or_record sched_req_or_record with
  | None -> false
  | Some (start', _) -> start <= start' && start' < end_exc

let sched_req_or_record_ending_within_time_slot ~start ~end_exc
    (sched_req_or_record :
       sched_req_id
       * ('a, int64, Time_slot.t) Sched_req_data_unit_skeleton.t list) : bool =
  match start_and_end_exc_bound_of_sched_req_or_record sched_req_or_record with
  | None -> false
  | Some (_, end_exc') -> start <= end_exc' && end_exc' < end_exc

module Check = struct
  let sched_req_data_is_valid (data : sched_req_data) : bool =
    List.for_all
      (fun x ->
         Sched_req_data_unit_skeleton.Check.check
           ~f_data:Task.Check.task_seg_alloc_req_is_valid
           ~f_time:Time.Check.unix_second_is_valid
           ~f_time_slot:Time_slot.Check.is_valid x)
      data

  let sched_req_data_list_is_valid (l : sched_req_data list) : bool =
    List.for_all sched_req_data_is_valid l

  let sched_req_is_valid ((id, data) : sched_req) : bool =
    id >= 0L && sched_req_data_is_valid data

  let sched_req_record_data_is_valid (data : sched_req_record_data) : bool =
    List.for_all
      (fun x ->
         Sched_req_data_unit_skeleton.Check.check
           ~f_data:Task.Check.task_seg_is_valid
           ~f_time:Time.Check.unix_second_is_valid
           ~f_time_slot:Time_slot.Check.is_valid x)
      data

  let sched_req_record_data_list_is_valid (l : sched_req_record_data list) :
    bool =
    List.for_all sched_req_record_data_is_valid l

  let sched_req_record_is_valid ((id, data) : sched_req_record) : bool =
    id >= 0L && sched_req_record_data_is_valid data
end

module Serialize = struct
  let rec pack_sched_req (id, data) : Sched_req_t.sched_req =
    (Misc_utils.int32_int32_of_int64 id, List.map pack_sched_req_data_unit data)

  and pack_sched_req_data_unit (sched_req_data_unit : sched_req_data_unit) :
    Sched_req_t.sched_req_data_unit =
    Sched_req_data_unit_skeleton.Serialize.pack
      ~pack_data:Task.Serialize.pack_task_seg_alloc_req
      ~pack_time:Misc_utils.int32_int32_of_int64
      ~pack_time_slot:Time_slot.Serialize.pack_time_slot sched_req_data_unit

  let rec pack_sched_req_record (id, data_list) : Sched_req_t.sched_req_record =
    ( Misc_utils.int32_int32_of_int64 id,
      List.map pack_sched_req_record_data_unit data_list )

  and pack_sched_req_record_data_unit
      (sched_req_record_data : sched_req_record_data_unit) :
    Sched_req_t.sched_req_record_data_unit =
    Sched_req_data_unit_skeleton.Serialize.pack
      ~pack_data:Task.Serialize.pack_task_seg
      ~pack_time:Misc_utils.int32_int32_of_int64
      ~pack_time_slot:Time_slot.Serialize.pack_time_slot sched_req_record_data
end

module Deserialize = struct
  let rec unpack_sched_req (id, data) : sched_req =
    ( Misc_utils.int64_of_int32_int32 id,
      List.map unpack_sched_req_data_unit data )

  and unpack_sched_req_data_unit
      (sched_req_data_unit : Sched_req_t.sched_req_data_unit) :
    sched_req_data_unit =
    Sched_req_data_unit_skeleton.Deserialize.unpack
      ~unpack_data:Task.Deserialize.unpack_task_seg_alloc_req
      ~unpack_time:Misc_utils.int64_of_int32_int32
      ~unpack_time_slot:Time_slot.Deserialize.unpack_time_slot
      sched_req_data_unit

  let rec unpack_sched_req_record (id, data) : sched_req_record =
    ( Misc_utils.int64_of_int32_int32 id,
      List.map unpack_sched_req_record_data_unit data )

  and unpack_sched_req_record_data_unit
      (sched_req_record_data_unit : Sched_req_t.sched_req_record_data_unit) :
    sched_req_record_data_unit =
    Sched_req_data_unit_skeleton.Deserialize.unpack
      ~unpack_data:Task.Deserialize.unpack_task_seg
      ~unpack_time:Misc_utils.int64_of_int32_int32
      ~unpack_time_slot:Time_slot.Deserialize.unpack_time_slot
      sched_req_record_data_unit
end

module To_string = struct
  let debug_string_of_sched_req_data_unit ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) req_data =
    Sched_req_data_unit_skeleton.To_string
    .debug_string_of_sched_req_data_unit_skeleton ~indent_level ~buffer
      ~string_of_data:(fun (id, len) ->
          Printf.sprintf "task_id : %s, len : %Ld\n"
            (Task.Id.string_of_task_inst_id id)
            len)
      ~string_of_time:Int64.to_string ~string_of_time_slot:Time_slot.to_string
      req_data

  let debug_string_of_sched_req_data ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) req_data =
    List.iter
      (fun data_unit ->
         debug_string_of_sched_req_data_unit ~indent_level ~buffer data_unit
         |> ignore)
      req_data;
    Buffer.contents buffer

  let debug_string_of_sched_req ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, req_data) =
    Debug_print.bprintf ~indent_level buffer "schedule request id : %Ld\n" id;
    debug_string_of_sched_req_data ~indent_level:(indent_level + 1) ~buffer
      req_data
    |> ignore;
    Buffer.contents buffer

  let debug_string_of_sched_req_record_data_unit ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) req_data =
    Sched_req_data_unit_skeleton.To_string
    .debug_string_of_sched_req_data_unit_skeleton ~indent_level ~buffer
      ~string_of_data:(fun (id, len) ->
          Printf.sprintf "task_seg_id : %s, len : %Ld\n"
            (Task.Id.string_of_task_seg_id id)
            len)
      ~string_of_time:Int64.to_string ~string_of_time_slot:Time_slot.to_string
      req_data

  let debug_string_of_sched_req_record_data ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) req_record_data_list =
    List.iter
      (fun req_record_data ->
         debug_string_of_sched_req_record_data_unit ~indent_level ~buffer
           req_record_data
         |> ignore)
      req_record_data_list;
    Buffer.contents buffer

  let debug_string_of_sched_req_record ?(indent_level = 0)
      ?(buffer = Buffer.create 4096) (id, req_data_list) =
    Debug_print.bprintf ~indent_level buffer
      "schedule request record id : %Ld\n" id;
    debug_string_of_sched_req_record_data ~indent_level:(indent_level + 1)
      ~buffer req_data_list
    |> ignore;
    Buffer.contents buffer
end

module Print = struct
  let debug_print_sched_req_data_unit ?(indent_level = 0) sched_req_data_unit =
    print_string
      (To_string.debug_string_of_sched_req_data_unit ~indent_level
         sched_req_data_unit)

  let debug_print_sched_req_data ?(indent_level = 0) sched_req_data =
    print_string
      (To_string.debug_string_of_sched_req_data ~indent_level sched_req_data)

  let debug_print_sched_req ?(indent_level = 0) sched_req =
    print_string (To_string.debug_string_of_sched_req ~indent_level sched_req)

  let debug_print_sched_req_record_data_unit ?(indent_level = 0)
      sched_req_data_unit =
    print_string
      (To_string.debug_string_of_sched_req_record_data_unit ~indent_level
         sched_req_data_unit)

  let debug_print_sched_req_record_data ?(indent_level = 0) sched_req_data =
    print_string
      (To_string.debug_string_of_sched_req_record_data ~indent_level
         sched_req_data)

  let debug_print_sched_req_record ?(indent_level = 0) sched_req =
    print_string
      (To_string.debug_string_of_sched_req_record ~indent_level sched_req)
end
