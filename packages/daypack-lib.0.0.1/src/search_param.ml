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

let search_using_tz_offset_s_of_search_param (param : t) :
  Time.tz_offset_s option =
  match param with
  | Time_slots { search_using_tz_offset_s; _ } -> search_using_tz_offset_s
  | Years_ahead_start_unix_second { search_using_tz_offset_s; _ } ->
    search_using_tz_offset_s
  | Years_ahead_start_date_time { search_using_tz_offset_s; _ } ->
    search_using_tz_offset_s

let push_search_param_to_later_start ~(start : int64) (search_param : t) :
  (t, unit) result =
  match search_param with
  | Time_slots { search_using_tz_offset_s; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
      | None -> Ok search_param
      | Some (start', end_exc') ->
        let start = max start' start in
        let time_slots =
          time_slots
          |> List.to_seq
          |> Time_slots.inter (Seq.return (start, end_exc'))
          |> List.of_seq
        in
        Ok (Time_slots { search_using_tz_offset_s; time_slots }) )
  | Years_ahead_start_unix_second
      { search_using_tz_offset_s; start = start'; search_years_ahead } ->
    let start = max start' start in
    Ok
      (Years_ahead_start_unix_second
         { search_using_tz_offset_s; start; search_years_ahead })
  | Years_ahead_start_date_time
      { search_using_tz_offset_s; start = start'; search_years_ahead } -> (
      match Time.Date_time.to_unix_second start' with
      | Error () -> Error ()
      | Ok start' ->
        let start = max start' start in
        Time.Date_time.of_unix_second
          ~tz_offset_s_of_date_time:search_using_tz_offset_s start
        |> Result.map (fun start ->
            Years_ahead_start_date_time
              { search_using_tz_offset_s; start; search_years_ahead }) )

let start_date_time_and_search_years_ahead_of_search_param (search_param : t) :
  (Time.Date_time.t * int) option =
  match search_param with
  | Time_slots { search_using_tz_offset_s; time_slots } -> (
      match Time_slots.Bound.min_start_and_max_end_exc_list time_slots with
      | None -> None
      | Some (start, end_exc) ->
        let start =
          Time.Date_time.of_unix_second
            ~tz_offset_s_of_date_time:search_using_tz_offset_s start
          |> Result.get_ok
        in
        let end_exc =
          Time.Date_time.of_unix_second
            ~tz_offset_s_of_date_time:search_using_tz_offset_s end_exc
          |> Result.get_ok
        in
        let search_years_ahead = end_exc.year - start.year + 1 in
        Some (start, search_years_ahead) )
  | Years_ahead_start_unix_second
      { search_using_tz_offset_s; start; search_years_ahead } ->
    let start =
      Time.Date_time.of_unix_second
        ~tz_offset_s_of_date_time:search_using_tz_offset_s start
      |> Result.get_ok
    in
    Some (start, search_years_ahead)
  | Years_ahead_start_date_time
      { search_using_tz_offset_s = _; start; search_years_ahead } ->
    Some (start, search_years_ahead)

module Check = struct
  let check_search_param (x : t) : (unit, error) result =
    match x with
    | Time_slots { search_using_tz_offset_s = _; time_slots } ->
      if
        List.for_all
          (fun (x, y) ->
             Time_slot.Check.is_valid (x, y)
             && Time.Date_time.of_unix_second ~tz_offset_s_of_date_time:None x
                |> Result.is_ok
             && Time.Date_time.of_unix_second ~tz_offset_s_of_date_time:None y
                |> Result.is_ok)
          time_slots
      then Ok ()
      else Error Invalid_time_slots
    | Years_ahead_start_unix_second
        { search_using_tz_offset_s; start; search_years_ahead } -> (
        match
          Time.Date_time.of_unix_second
            ~tz_offset_s_of_date_time:search_using_tz_offset_s start
        with
        | Error () -> Error Invalid_start
        | Ok start ->
          if search_years_ahead <= 0 then Error Invalid_search_years_ahead
          else if start.year + search_years_ahead > Time.Date_time.max.year
          then Error Too_far_into_future
          else Ok () )
    | Years_ahead_start_date_time
        { search_using_tz_offset_s = _; start; search_years_ahead } ->
      if Time.Check.date_time_is_valid start then
        if search_years_ahead <= 0 then Error Invalid_search_years_ahead
        else if start.year + search_years_ahead > Time.Date_time.max.year then
          Error Too_far_into_future
        else Ok ()
      else Error Invalid_start
end
