open Int64_utils

type ('task_seg_related_data, 'time) fixed = {
  task_seg_related_data : 'task_seg_related_data;
  start : 'time;
}

type ('task_seg_related_data, 'time_slot) shift = {
  task_seg_related_data_list : 'task_seg_related_data list;
  time_slots : 'time_slot list;
  incre : int64;
}

type split_count =
  | Max_split of int64
  | Exact_split of int64

type ('task_seg_related_data, 'time_slot) split_and_shift = {
  task_seg_related_data : 'task_seg_related_data;
  time_slots : 'time_slot list;
  incre : int64;
  split_count : split_count;
  min_seg_size : int64;
  max_seg_size : int64 option;
}

type ('task_seg_related_data, 'time_slot) split_even = {
  task_seg_related_data : 'task_seg_related_data;
  time_slots : 'time_slot list;
  buckets : 'time_slot list;
  incre : int64;
}

type ('task_seg_related_data, 'time_slot) time_share = {
  task_seg_related_data_list : 'task_seg_related_data list;
  time_slots : 'time_slot list;
  interval_size : int64;
}

type ('task_seg_related_data, 'time, 'time_slot) push_toward = {
  task_seg_related_data : 'task_seg_related_data;
  target : 'time;
  time_slots : 'time_slot list;
  incre : int64;
}

type ('task_seg_related_data, 'time, 'time_slot) t =
  | Fixed of ('task_seg_related_data, 'time) fixed
  | Shift of ('task_seg_related_data, 'time_slot) shift
  | Split_and_shift of ('task_seg_related_data, 'time_slot) split_and_shift
  | Split_even of ('task_seg_related_data, 'time_slot) split_even
  | Time_share of ('task_seg_related_data, 'time_slot) time_share
  | Push_toward of ('task_seg_related_data, 'time, 'time_slot) push_toward

let shift_time ~offset (t : ('a, int64, Time_slot.t) t) :
  ('a, int64, Time_slot.t) t =
  match t with
  | Fixed { task_seg_related_data; start } ->
    Fixed { task_seg_related_data; start = start +^ offset }
  | Shift x ->
    Shift { x with time_slots = Time_slots.shift_list ~offset x.time_slots }
  | Split_and_shift x ->
    Split_and_shift
      { x with time_slots = Time_slots.shift_list ~offset x.time_slots }
  | Split_even x ->
    Split_even
      {
        x with
        time_slots = Time_slots.shift_list ~offset x.time_slots;
        buckets = Time_slots.shift_list ~offset x.buckets;
      }
  | Time_share x ->
    Time_share
      { x with time_slots = Time_slots.shift_list ~offset x.time_slots }
  | Push_toward x ->
    Push_toward
      {
        x with
        target = x.target +^ offset;
        time_slots = Time_slots.shift_list ~offset x.time_slots;
      }

let shift_time_list ~offset (ts : ('a, int64, Time_slot.t) t list) :
  ('a, int64, Time_slot.t) t list =
  List.map (shift_time ~offset) ts

let map (type a b c d e f) ~(f_data : a -> d) ~(f_time : b -> e)
    ~(f_time_slot : c -> f) (t : (a, b, c) t) : (d, e, f) t =
  match t with
  | Fixed { task_seg_related_data; start } ->
    Fixed
      {
        task_seg_related_data = f_data task_seg_related_data;
        start = f_time start;
      }
  | Shift x ->
    Shift
      {
        x with
        task_seg_related_data_list =
          List.map f_data x.task_seg_related_data_list;
        time_slots = List.map f_time_slot x.time_slots;
      }
  | Split_and_shift x ->
    Split_and_shift
      {
        x with
        task_seg_related_data = f_data x.task_seg_related_data;
        time_slots = List.map f_time_slot x.time_slots;
      }
  | Split_even x ->
    Split_even
      {
        x with
        task_seg_related_data = f_data x.task_seg_related_data;
        time_slots = List.map f_time_slot x.time_slots;
        buckets = List.map f_time_slot x.buckets;
      }
  | Time_share x ->
    Time_share
      {
        x with
        task_seg_related_data_list =
          List.map f_data x.task_seg_related_data_list;
        time_slots = List.map f_time_slot x.time_slots;
      }
  | Push_toward x ->
    Push_toward
      {
        x with
        task_seg_related_data = f_data x.task_seg_related_data;
        target = f_time x.target;
        time_slots = List.map f_time_slot x.time_slots;
      }

let map_list ~f_data ~f_time ~f_time_slot ts =
  List.map (map ~f_data ~f_time ~f_time_slot) ts

module Check = struct
  let check (type a b c) ~(f_data : a -> bool) ~(f_time : b -> bool)
      ~(f_time_slot : c -> bool) (t : (a, b, c) t) : bool =
    match t with
    | Fixed { task_seg_related_data; start } ->
      f_data task_seg_related_data && f_time start
    | Shift { task_seg_related_data_list; time_slots; incre } ->
      List.for_all f_data task_seg_related_data_list
      && List.for_all f_time_slot time_slots
      && incre > 0L
    | Split_and_shift
        {
          task_seg_related_data;
          time_slots;
          incre;
          split_count;
          min_seg_size;
          max_seg_size;
        } -> (
        f_data task_seg_related_data
        && List.for_all f_time_slot time_slots
        && incre > 0L
        && ( match split_count with
            | Max_split x -> x >= 0L
            | Exact_split x -> x >= 0L )
        && min_seg_size > 0L
        && match max_seg_size with None -> true | Some x -> x >= min_seg_size )
    | Split_even { task_seg_related_data; time_slots; buckets; incre } ->
      f_data task_seg_related_data
      && List.for_all f_time_slot time_slots
      && List.for_all f_time_slot buckets
      && incre > 0L
    | Time_share { task_seg_related_data_list; time_slots; interval_size } ->
      List.for_all f_data task_seg_related_data_list
      && List.for_all f_time_slot time_slots
      && interval_size > 0L
    | Push_toward { task_seg_related_data; target; time_slots; incre } ->
      f_data task_seg_related_data
      && f_time target
      && List.for_all f_time_slot time_slots
      && incre > 0L
end

let get_inner_data (type a b c) (t : (a, b, c) t) : a list =
  match t with
  | Fixed { task_seg_related_data; _ } -> [ task_seg_related_data ]
  | Shift { task_seg_related_data_list; _ } -> task_seg_related_data_list
  | Split_and_shift { task_seg_related_data; _ } -> [ task_seg_related_data ]
  | Split_even { task_seg_related_data } -> [ task_seg_related_data ]
  | Time_share { task_seg_related_data_list; _ } -> task_seg_related_data_list
  | Push_toward { task_seg_related_data; _ } -> [ task_seg_related_data ]

let contains_matching_inner_data (type a b c) (f : a -> bool) (t : (a, b, c) t)
  : bool =
  List.exists f (get_inner_data t)

let list_contains_matching_inner_data (type a b c) (f : a -> bool)
    (ts : (a, b, c) t list) : bool =
  List.exists (fun t -> contains_matching_inner_data f t) ts

let remove_data_units_with_matching_inner_data (type a b c) (f : a -> bool)
    (ts : (a, b, c) t list) : (a, b, c) t list =
  List.filter (fun t -> not (contains_matching_inner_data f t)) ts

module To_string = struct
  let debug_string_of_sched_req_data_unit_skeleton (type a b c)
      ?(indent_level = 0) ?(buffer = Buffer.create 4096)
      ~(string_of_data : a -> string) ~(string_of_time : b -> string)
      ~(string_of_time_slot : c -> string) (t : (a, b, c) t) =
    ( match t with
      | Fixed { task_seg_related_data; start } ->
        Debug_print.bprintf ~indent_level buffer "fixed\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "start = %s\n" (string_of_time start)
      | Shift x ->
        Debug_print.bprintf ~indent_level buffer "shift\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "data\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_data x))
          x.task_seg_related_data_list;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_time_slot x))
          x.time_slots
      | Split_and_shift x ->
        Debug_print.bprintf ~indent_level buffer "split and shift\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data x.task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_time_slot x))
          x.time_slots
      | Split_even x ->
        Debug_print.bprintf ~indent_level buffer "split even\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data x.task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_time_slot x))
          x.time_slots;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "buckets\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_time_slot x))
          x.buckets
      | Time_share x ->
        Debug_print.bprintf ~indent_level buffer "time share\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "data\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_data x))
          x.task_seg_related_data_list;
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_time_slot x))
          x.time_slots
      | Push_toward x ->
        Debug_print.bprintf ~indent_level buffer "push toward\n";
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "data = %s\n"
          (string_of_data x.task_seg_related_data);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer "target\n";
        Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
          (string_of_time x.target);
        Debug_print.bprintf ~indent_level:(indent_level + 1) buffer
          "time slots\n";
        List.iter
          (fun x ->
             Debug_print.bprintf ~indent_level:(indent_level + 2) buffer "%s\n"
               (string_of_time_slot x))
          x.time_slots );
    Buffer.contents buffer
end

module Serialize = struct
  let pack (type a b c d e f) ~(pack_data : a -> d) ~(pack_time : b -> e)
      ~(pack_time_slot : c -> f) (t : (a, b, c) t) :
    (d, e, f) Sched_req_data_unit_skeleton_t.sched_req_data_unit_skeleton =
    match t with
    | Fixed { task_seg_related_data; start } ->
      `Fixed
        {
          task_seg_related_data = pack_data task_seg_related_data;
          start = pack_time start;
        }
    | Shift x ->
      `Shift
        {
          task_seg_related_data_list =
            List.map pack_data x.task_seg_related_data_list;
          incre = Misc_utils.int32_int32_of_int64 x.incre;
          time_slots = List.map pack_time_slot x.time_slots;
        }
    | Split_and_shift x ->
      `Split_and_shift
        {
          task_seg_related_data = pack_data x.task_seg_related_data;
          incre = Misc_utils.int32_int32_of_int64 x.incre;
          split_count =
            ( match x.split_count with
              | Max_split x -> `Max_split (Misc_utils.int32_int32_of_int64 x)
              | Exact_split x ->
                `Exact_split (Misc_utils.int32_int32_of_int64 x) );
          min_seg_size = Misc_utils.int32_int32_of_int64 x.min_seg_size;
          max_seg_size =
            Option.map Misc_utils.int32_int32_of_int64 x.max_seg_size;
          time_slots = List.map pack_time_slot x.time_slots;
        }
    | Split_even x ->
      `Split_even
        {
          task_seg_related_data = pack_data x.task_seg_related_data;
          time_slots = List.map pack_time_slot x.time_slots;
          buckets = List.map pack_time_slot x.buckets;
          incre = Misc_utils.int32_int32_of_int64 x.incre;
        }
    | Time_share x ->
      `Time_share
        {
          task_seg_related_data_list =
            List.map pack_data x.task_seg_related_data_list;
          interval_size = Misc_utils.int32_int32_of_int64 x.interval_size;
          time_slots = List.map pack_time_slot x.time_slots;
        }
    | Push_toward x ->
      `Push_toward
        {
          task_seg_related_data = pack_data x.task_seg_related_data;
          target = pack_time x.target;
          time_slots = List.map pack_time_slot x.time_slots;
          incre = Misc_utils.int32_int32_of_int64 x.incre;
        }
end

module Deserialize = struct
  let unpack (type a b c d e f) ~(unpack_data : d -> a) ~(unpack_time : e -> b)
      ~(unpack_time_slot : f -> c)
      (x :
         (d, e, f) Sched_req_data_unit_skeleton_t.sched_req_data_unit_skeleton) :
    (a, b, c) t =
    match x with
    | `Fixed { task_seg_related_data; start } ->
      Fixed
        {
          task_seg_related_data = unpack_data task_seg_related_data;
          start = unpack_time start;
        }
    | `Shift x ->
      Shift
        {
          task_seg_related_data_list =
            List.map unpack_data x.task_seg_related_data_list;
          time_slots = List.map unpack_time_slot x.time_slots;
          incre = Misc_utils.int64_of_int32_int32 x.incre;
        }
    | `Split_and_shift x ->
      Split_and_shift
        {
          task_seg_related_data = unpack_data x.task_seg_related_data;
          time_slots = List.map unpack_time_slot x.time_slots;
          incre = Misc_utils.int64_of_int32_int32 x.incre;
          split_count =
            ( match x.split_count with
              | `Max_split x -> Max_split (Misc_utils.int64_of_int32_int32 x)
              | `Exact_split x ->
                Exact_split (Misc_utils.int64_of_int32_int32 x) );
          min_seg_size = Misc_utils.int64_of_int32_int32 x.min_seg_size;
          max_seg_size =
            Option.map Misc_utils.int64_of_int32_int32 x.max_seg_size;
        }
    | `Split_even x ->
      Split_even
        {
          task_seg_related_data = unpack_data x.task_seg_related_data;
          time_slots = List.map unpack_time_slot x.time_slots;
          buckets = List.map unpack_time_slot x.buckets;
          incre = Misc_utils.int64_of_int32_int32 x.incre;
        }
    | `Time_share x ->
      Time_share
        {
          task_seg_related_data_list =
            List.map unpack_data x.task_seg_related_data_list;
          time_slots = List.map unpack_time_slot x.time_slots;
          interval_size = Misc_utils.int64_of_int32_int32 x.interval_size;
        }
    | `Push_toward x ->
      Push_toward
        {
          task_seg_related_data = unpack_data x.task_seg_related_data;
          target = unpack_time x.target;
          time_slots = List.map unpack_time_slot x.time_slots;
          incre = Misc_utils.int64_of_int32_int32 x.incre;
        }
end
