type branch_unary_op =
  | Next_n_batches of int
  | Every_batch

type second_expr = int

type minute_second_expr = {
  minute : int;
  second : int;
}

type hms_expr = {
  hour : int;
  minute : int;
  second : int;
}

type sign_expr =
  | Pos
  | Neg

type second_range_expr = second_expr Range.range

type minute_second_range_expr = minute_second_expr Range.range

type hms_range_expr = hms_expr Range.range

type day_expr =
  | Weekday of Time.weekday
  | Month_day of int

type day_range_expr =
  | Weekday_range of Time.weekday_range
  | Month_day_range of Time.month_day_range

type month_expr = Time.month

type year_expr = int

type month_weekday_mode =
  | First_n of int
  | Last_n of int

type time_point_expr =
  | Tpe_name of string
  | Tpe_unix_seconds of int64 list
  | Second of second_expr
  | Minute_second of minute_second_expr
  | Hms of hms_expr
  | Day_hms of {
      day : day_expr;
      hms : hms_expr;
    }
  | Month_day_hms of {
      month : month_expr;
      month_day : int;
      hms : hms_expr;
    }
  | Year_month_day_hms of {
      year : year_expr;
      month : month_expr;
      month_day : int;
      hms : hms_expr;
    }

(* type branching_time_point_expr =
 *   | Btp_unary_op of branch_unary_op * branching_time_point_expr
 *   | Btp_month_days_and_hmss of {
 *       month_days : int Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_weekdays_and_hmss of {
 *       weekdays : Time.weekday Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_months_and_month_days_and_hmss of {
 *       months : month_expr Range.range list;
 *       month_days : int Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_months_and_weekdays_and_hmss of {
 *       months : month_expr Range.range list;
 *       weekdays : Time.weekday Range.range list;
 *       hmss : hms_expr list;
 *     }
 *   | Btp_months_and_weekday_and_hmss of {
 *       months : month_expr Range.range list;
 *       weekday : Time.weekday;
 *       hmss : hms_expr list;
 *       month_weekday_mode : month_weekday_mode option;
 *     }
 *   | Btp_years_and_months_and_month_days_and_hmss of {
 *       years : int Range.range list;
 *       months : month_expr Range.range list;
 *       month_days : int Range.range list;
 *       hmss : hms_expr list;
 *     } *)

type time_slot_expr =
  | Tse_name of string
  | Explicit_time_slot of (time_point_expr * time_point_expr)

type branching_time_slot_expr =
  | Bts_unary_op of branch_unary_op * branching_time_slot_expr
  | Bts_hms_ranges of hms_range_expr list
  | Bts_month_days_and_hms_ranges of {
      month_days : int Range.range list;
      hms_ranges : hms_range_expr list;
    }
  | Bts_weekdays_and_hms_ranges of {
      weekdays : Time.weekday Range.range list;
      hms_ranges : hms_range_expr list;
    }
  | Bts_months_and_month_days_and_hms_ranges of {
      months : month_expr Range.range list;
      month_days : int Range.range list;
      hms_ranges : hms_range_expr list;
    }
  | Bts_months_and_weekdays_and_hms_ranges of {
      months : month_expr Range.range list;
      weekdays : Time.weekday Range.range list;
      hms_ranges : hms_range_expr list;
    }
  | Bts_months_and_weekday_and_hms_ranges of {
      months : month_expr Range.range list;
      weekday : Time.weekday;
      hms_ranges : hms_range_expr list;
      month_weekday_mode : month_weekday_mode option;
    }
  | Bts_years_and_months_and_month_days_and_hms_ranges of {
      years : int Range.range list;
      months : month_expr Range.range list;
      month_days : int Range.range list;
      hms_ranges : hms_range_expr list;
    }

type unary_op =
  | Not
  | Every
  | Next_n_points of int
  | Next_n_slots of int
  | Tz_offset of sign_expr * hms_expr

type binary_op =
  | Union
  | Inter

type t =
  | Time_point_expr of time_point_expr
  | Time_slot_expr of time_slot_expr
  (* | Branching_time_point_expr of branching_time_point_expr *)
  | Branching_time_slot_expr of branching_time_slot_expr
  | Time_pattern of Time_pattern.time_pattern
  | Time_unary_op of unary_op * t
  | Time_binary_op of binary_op * t * t
  | Time_round_robin_select of t list
