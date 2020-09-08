let tz_offset_s = Ptime_clock.current_tz_offset_s () |> Option.get

let cur_unix_second = Daypack_lib.Time.Current.cur_unix_second ()

let default_search_years_ahead = 5

let default_time_slot_count = 10
