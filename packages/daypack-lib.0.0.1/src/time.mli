type tz_offset_s = int

val tz_offset_s_utc : tz_offset_s

type weekday =
  [ `Sun
  | `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  ]

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

type weekday_range = weekday Range.range

type month_day_range = int Range.range

type day_range =
  | Weekday_range of weekday_range
  | Month_day_range of month_day_range

val first_mday : int

val tm_year_offset : int

val minute_to_second_multiplier : int64

val hour_to_second_multiplier : int64

val day_to_second_multiplier : int64

module Date_time : sig
  type t = {
    year : int;
    month : month;
    day : int;
    hour : int;
    minute : int;
    second : int;
    tz_offset_s : int;
  }

  val of_ptime_date_time : Ptime.date * Ptime.time -> (t, unit) result

  val to_ptime_date_time : t -> Ptime.date * Ptime.time

  val to_unix_second : t -> (int64, unit) result

  val of_unix_second :
    tz_offset_s_of_date_time:tz_offset_s option -> int64 -> (t, unit) result

  val min : t

  val max : t

  val compare : t -> t -> int

  val set_to_first_sec : t -> t

  val set_to_last_sec : t -> t

  val set_to_first_min_sec : t -> t

  val set_to_last_min_sec : t -> t

  val set_to_first_hour_min_sec : t -> t

  val set_to_last_hour_min_sec : t -> t

  val set_to_first_day_hour_min_sec : t -> t

  val set_to_last_day_hour_min_sec : t -> t

  val set_to_first_month_day_hour_min_sec : t -> t

  val set_to_last_month_day_hour_min_sec : t -> t
end

module Check : sig
  val unix_second_is_valid : int64 -> bool

  val second_is_valid : second:int -> bool

  val minute_second_is_valid : minute:int -> second:int -> bool

  val hour_minute_second_is_valid : hour:int -> minute:int -> second:int -> bool

  val date_time_is_valid : Date_time.t -> bool
end

val next_hour_minute : hour:int -> minute:int -> (int * int, unit) result

val next_weekday : weekday -> weekday

val tm_int_of_weekday : weekday -> int

val weekday_of_tm_int : int -> (weekday, unit) result

val tm_int_of_month : month -> int

val month_of_tm_int : int -> (month, unit) result

val human_int_of_month : month -> int

val month_of_human_int : int -> (month, unit) result

val compare_month : month -> month -> int

val month_lt : month -> month -> bool

val month_le : month -> month -> bool

val month_gt : month -> month -> bool

val month_ge : month -> month -> bool

val compare_weekday : weekday -> weekday -> int

val weekday_lt : weekday -> weekday -> bool

val weekday_le : weekday -> weekday -> bool

val weekday_gt : weekday -> weekday -> bool

val weekday_ge : weekday -> weekday -> bool

val zero_tm_sec : Unix.tm -> Unix.tm

(* val tm_of_unix_second : time_zone_of_tm:time_zone -> int64 -> Unix.tm

   val unix_second_of_tm : time_zone_of_tm:time_zone -> Unix.tm -> int64

   val normalize_tm : Unix.tm -> Unix.tm

   val tm_change_time_zone :
   from_time_zone:time_zone -> to_time_zone:time_zone -> Unix.tm -> Unix.tm *)

val is_leap_year : year:int -> bool

val day_count_of_year : year:int -> int

val day_count_of_month : year:int -> month:month -> int

val weekday_of_month_day :
  year:int -> month:month -> mday:int -> (weekday, unit) result

(* val local_tm_to_utc_tm : Unix.tm -> Unix.tm *)

module Second_ranges : Ranges.S with type t := int

module Minute_ranges : Ranges.S with type t := int

module Hour_ranges : Ranges.S with type t := int

module Weekday_tm_int_ranges : Ranges.S with type t := int

module Weekday_ranges : Ranges.S with type t := weekday

module Month_day_ranges : Ranges.S with type t := int

module Month_tm_int_ranges : Ranges.S with type t := int

module Month_ranges : Ranges.S with type t := month

module Year_ranges : Ranges.S with type t := int

module Current : sig
  val cur_unix_second : unit -> int64

  val cur_date_time :
    tz_offset_s_of_date_time:tz_offset_s option -> (Date_time.t, unit) result

  val cur_tm_local : unit -> Unix.tm

  val cur_tm_utc : unit -> Unix.tm
end

module Of_string : sig
  val weekday_of_string : string -> (weekday, unit) result

  val month_of_string : string -> (month, unit) result
end

module Add : sig
  val add_days_unix_second : days:int -> int64 -> int64
end

module Serialize : sig
  val pack_weekday : weekday -> Time_t.weekday

  val pack_month : month -> Time_t.month
end

module Deserialize : sig
  val unpack_weekday : Time_t.weekday -> weekday
end

module To_string : sig
  val string_of_weekday : weekday -> string

  val string_of_month : month -> string

  (* val yyyymondd_hhmmss_string_of_tm : Unix.tm -> (string, unit) result *)
  val yyyymondd_hhmmss_string_of_date_time : Date_time.t -> string

  val yyyymondd_hhmmss_string_of_unix_second :
    display_using_tz_offset_s:tz_offset_s option ->
    int64 ->
    (string, unit) result

  (* val yyyymmdd_hhmmss_string_of_tm : Unix.tm -> (string, unit) result *)
  val yyyymmdd_hhmmss_string_of_date_time : Date_time.t -> string

  val yyyymmdd_hhmmss_string_of_unix_second :
    display_using_tz_offset_s:tz_offset_s option ->
    int64 ->
    (string, unit) result

  (* val yyyymondd_hhmm_string_of_tm : Unix.tm -> (string, unit) result *)
  val yyyymondd_hhmm_string_of_date_time : Date_time.t -> string

  val yyyymondd_hhmm_string_of_unix_second :
    display_using_tz_offset_s:tz_offset_s option ->
    int64 ->
    (string, unit) result

  (* val yyyymmdd_hhmm_string_of_tm : Unix.tm -> (string, unit) result *)
  val yyyymmdd_hhmm_string_of_date_time : Date_time.t -> string

  val yyyymmdd_hhmm_string_of_unix_second :
    display_using_tz_offset_s:tz_offset_s option ->
    int64 ->
    (string, unit) result
end

module Print : sig
  val debug_print_time :
    ?indent_level:int ->
    display_using_tz_offset_s:tz_offset_s option ->
    int64 ->
    unit
end

module Date_time_set : Set.S with type elt = Date_time.t
