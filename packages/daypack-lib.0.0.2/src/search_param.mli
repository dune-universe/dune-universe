type t =
  | Time_slots of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      time_slots : Time_slot.t list;
    }
  | Years_ahead_start_unix_second of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : int64;
      search_years_ahead : int;
    }
  | Years_ahead_start_date_time of {
      search_using_tz_offset_s : Time.tz_offset_s option;
      start : Time.Date_time.t;
      search_years_ahead : int;
    }

type error =
  | Invalid_start
  | Invalid_time_slots
  | Invalid_search_years_ahead
  | Too_far_into_future

val search_using_tz_offset_s_of_search_param : t -> Time.tz_offset_s option

val push_search_param_to_later_start : start:int64 -> t -> (t, unit) result

val start_date_time_and_search_years_ahead_of_search_param :
  t -> (Time.Date_time.t * int) option

module Check : sig
  val check_search_param : t -> (unit, error) result
end
