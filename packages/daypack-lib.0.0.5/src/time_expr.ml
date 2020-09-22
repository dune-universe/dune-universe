type error =
  | Invalid_time_point_expr
  | Invalid_time_slot_expr

type f_resolve_tse_name = string -> Time_expr_ast.time_slot_expr option

type f_resolve_tpe_name = string -> Time_expr_ast.time_point_expr option

type lang_fragment =
  [ `Time_point_expr
  | `Time_slot_expr
  | `Branching_time_point_expr
  | `Branching_time_slot_expr
  | `Time_pattern
  ]

let all_lang_fragments =
  [
    `Time_point_expr;
    `Time_slot_expr;
    `Branching_time_point_expr;
    `Branching_time_slot_expr;
    `Time_pattern;
  ]

let default_f_resolve_tse_name (_ : string) :
  Time_expr_ast.time_slot_expr option =
  None

let default_f_resolve_tpe_name (_ : string) :
  Time_expr_ast.time_point_expr option =
  None

module To_string = struct
  let debug_string_of_hms_ranges
      ({ hour; minute; second } : Time_expr_ast.hms_expr) : string =
    Printf.sprintf "%02d:%02d:%02d" hour minute second
end

exception Invalid_time_expr of string

let max_resolve_depth = 1000

module Resolve = struct
  let resolve_time_point_expr ~(f_resolve_tpe_name : f_resolve_tpe_name)
      (e : Time_expr_ast.time_point_expr) :
    (Time_expr_ast.time_point_expr, string) result =
    let rec aux f_resolve_tpe_name remaining_resolve_depth name e =
      if remaining_resolve_depth <= 0 then Error "Maximum resolve depth reached"
      else
        match e with
        | Time_expr_ast.Tpe_name s -> (
            if name = Some s then
              Error
                (Printf.sprintf "Name resolution loop detected for name: %s" s)
            else
              let name = match name with None -> Some s | Some x -> Some x in
              match f_resolve_tpe_name s with
              | None ->
                Error (Printf.sprintf "Name resolution failed for name: %s" s)
              | Some e ->
                aux f_resolve_tpe_name (pred remaining_resolve_depth) name e )
        | e -> Ok e
    in
    aux f_resolve_tpe_name max_resolve_depth None e

  let resolve_time_slot_expr ~(f_resolve_tse_name : f_resolve_tse_name)
      ~(f_resolve_tpe_name : f_resolve_tpe_name)
      (e : Time_expr_ast.time_slot_expr) :
    (Time_expr_ast.time_slot_expr, string) result =
    let rec aux f_resolve_tse_name f_resolve_tpe_name remaining_resolve_depth
        name e =
      if remaining_resolve_depth <= 0 then Error "Maximum resolve depth reached"
      else
        match e with
        | Time_expr_ast.Tse_name s -> (
            if name = Some s then
              Error
                (Printf.sprintf "Name resolution loop detected for name: %s" s)
            else
              let name = match name with None -> Some s | Some x -> Some x in
              match f_resolve_tse_name s with
              | None ->
                Error (Printf.sprintf "Name resolution failed for name: %s" s)
              | Some e ->
                aux f_resolve_tse_name f_resolve_tpe_name
                  (pred remaining_resolve_depth)
                  name e )
        | Explicit_time_slot (start, end_exc) -> (
            try
              Ok
                (Time_expr_ast.Explicit_time_slot
                   ( match resolve_time_point_expr ~f_resolve_tpe_name start with
                     | Error msg -> raise (Invalid_time_expr msg)
                     | Ok start -> (
                         match
                           resolve_time_point_expr ~f_resolve_tpe_name end_exc
                         with
                         | Error msg -> raise (Invalid_time_expr msg)
                         | Ok end_exc -> (start, end_exc) ) ))
            with Invalid_time_expr msg -> Error msg )
    in
    aux f_resolve_tse_name f_resolve_tpe_name max_resolve_depth None e
end

module Check = struct
  let day_expr_is_valid (e : Time_expr_ast.day_expr) : bool =
    match e with
    | Time_expr_ast.Weekday _ -> true
    | Month_day x -> 1 <= x && x <= 31

  let hms_ranges_are_valid (l : Time_expr_ast.hms_range_expr list) =
    let open Time_expr_ast in
    List.for_all
      (fun x ->
         match x with
         | `Range_inc (x, y) | `Range_exc (x, y) ->
           Time.Check.hour_minute_second_is_valid ~hour:x.hour ~minute:x.minute
             ~second:x.second
           && Time.Check.hour_minute_second_is_valid ~hour:y.hour
             ~minute:y.minute ~second:y.second)
      l

  let check_time_point_expr (e : Time_expr_ast.time_point_expr) :
    (unit, unit) result =
    let open Time_expr_ast in
    match e with
    | Tpe_name _ -> Ok ()
    | Tpe_unix_seconds l -> (
        let invalid_unix_seconds =
          List.filter
            (fun x ->
               Result.is_error
                 (Time.Date_time.of_unix_second ~tz_offset_s_of_date_time:None x))
            l
        in
        match invalid_unix_seconds with [] -> Ok () | _ -> Error () )
    | Second second ->
      if Time.Check.second_is_valid ~second then Ok () else Error ()
    | Minute_second { minute; second } ->
      if Time.Check.minute_second_is_valid ~minute ~second then Ok ()
      else Error ()
    | Hms { hour; minute; second } ->
      if Time.Check.hour_minute_second_is_valid ~hour ~minute ~second then
        Ok ()
      else Error ()
    | Day_hms { day; hms = { hour; minute; second } } ->
      if
        day_expr_is_valid day
        && Time.Check.hour_minute_second_is_valid ~hour ~minute ~second
      then Ok ()
      else Error ()
    | Month_day_hms { month = _; month_day; hms = { hour; minute; second } } ->
      if
        1 <= month_day
        && month_day <= 31
        && Time.Check.hour_minute_second_is_valid ~hour ~minute ~second
      then Ok ()
      else Error ()
    | Year_month_day_hms
        { year; month = _; month_day; hms = { hour; minute; second } } ->
      if
        0 <= year
        && year <= 9999
        && 1 <= month_day
        && month_day <= 31
        && Time.Check.hour_minute_second_is_valid ~hour ~minute ~second
      then Ok ()
      else Error ()

  let check_time_slot_expr (e : Time_expr_ast.time_slot_expr) :
    (unit, unit) result =
    let open Time_expr_ast in
    let aux e =
      match e with
      | Tse_name _ -> Ok ()
      | Explicit_time_slot (x, y) ->
        if
          Result.is_ok (check_time_point_expr x)
          && Result.is_ok (check_time_point_expr y)
        then Ok ()
        else Error ()
    in
    aux e

  let check_branching_time_slot_expr
      (e : Time_expr_ast.branching_time_slot_expr) : (unit, unit) result =
    let open Time_expr_ast in
    let rec aux e =
      match e with
      | Bts_unary_op (op, e) -> aux e
      | Bts_month_days_and_hms_ranges { month_days; hms_ranges } ->
        if
          Time.Month_day_ranges.Check.list_is_valid month_days
          && hms_ranges_are_valid hms_ranges
        then Ok ()
        else Error ()
      | Bts_weekdays_and_hms_ranges { weekdays = _; hms_ranges } ->
        if hms_ranges_are_valid hms_ranges then Ok () else Error ()
      | Bts_months_and_month_days_and_hms_ranges
          { months = _; month_days; hms_ranges } ->
        if
          Time.Month_day_ranges.Check.list_is_valid month_days
          && hms_ranges_are_valid hms_ranges
        then Ok ()
        else Error ()
      | Bts_months_and_weekdays_and_hms_ranges
          { months; weekdays = _; hms_ranges } ->
        if
          Time.Month_ranges.Check.list_is_valid months
          && hms_ranges_are_valid hms_ranges
        then Ok ()
        else Error ()
      | Bts_months_and_weekday_and_hms_ranges
          { months; weekday = _; hms_ranges; month_weekday_mode = _ } ->
        if
          Time.Month_ranges.Check.list_is_valid months
          && hms_ranges_are_valid hms_ranges
        then Ok ()
        else Error ()
      | Bts_years_and_months_and_month_days_and_hms_ranges
          { years = _; months; month_days; hms_ranges } ->
        if
          Time.Month_ranges.Check.list_is_valid months
          && Time.Month_day_ranges.Check.list_is_valid month_days
          && hms_ranges_are_valid hms_ranges
        then Ok ()
        else Error ()
    in
    aux e
end

let check_time_expr (e : Time_expr_ast.t) : (unit, unit) result =
  let open Time_expr_ast in
  let rec aux e =
    match e with
    | Time_point_expr e' -> Check.check_time_point_expr e'
    | Time_slot_expr e' -> Check.check_time_slot_expr e'
    | Branching_time_point_expr e' ->
      failwith "Unimplemented" (* Check.check_branching_time_point_expr e' *)
    | Branching_time_slot_expr e' ->
      failwith "Unimplemented" (* Check.check_branching_time_slot_expr e' *)
    | Time_pattern p ->
      Time_pattern.Check.check_time_pattern p
      |> Result.map_error (fun _ -> ())
    | Time_unary_op (_op, e') -> aux e'
    | Time_binary_op (_op, e1, e2) -> (
        match aux e1 with Error () -> Error () | Ok () -> aux e2 )
    | Time_round_robin_select l ->
      l
      |> List.map aux
      |> Misc_utils.get_ok_error_list
      |> Result.map (fun _ -> ())
  in
  aux e

module Of_string = struct
  open CCParse
  open Parser_components

  let not_str = string "not"

  let next_slot_str = string "next-slot"

  let next_point_str = string "next-point"

  let next_str = string "next"

  let point_str = string "point"

  let slot_str = string "slot"

  let points_str = string "points"

  let slots_str = string "slots"

  let batch_str = string "batch"

  let batches_str = string "batches"

  let of_str = string "of"

  let to_str = string "to"

  let first_str = string "first"

  let last_str = string "last"

  (* let bound =
   *   let open Time_expr_ast in
   *   option Every
   *     ( try_ (string "next-slot" *> return (Next_n_slot 1))
   *       <|> try_ (string "next-point" *> return Every)
   *       <|> (string "every" *> return Every)
   *     ) *)

  let branch_unary_op =
    let open Time_expr_ast in
    try_ (string "next-batch" *> return (Next_n_batches 1))
    <|> try_ (string "every-batch" *> return Every_batch)
    <|> try_
      ( next_str *> hyphen *> nat_zero
        <* hyphen
        <* (try_ batches_str <|> batch_str)
        >>= fun n -> return (Next_n_batches n) )

  let ident_string =
    ident_string ~reserved_words:[ "to"; "first"; "last"; "next"; "every" ]

  let range_inc_expr (p : 'a t) : 'a Range.range t =
    try_
      ( p
        >>= fun x ->
        skip_space *> to_str *> skip_space *> p
        >>= fun y -> return (`Range_inc (x, y)) )
    <|> (p >>= fun x -> return (`Range_inc (x, x)))

  let range_exc_expr (p : 'a t) : 'a Range.range t =
    try_
      ( p
        >>= fun x ->
        skip_space *> to_str *> skip_space *> p
        >>= fun y -> return (`Range_exc (x, y)) )
    <|> (p >>= fun x -> return (`Range_inc (x, x)))

  let symbols = "()[]&|>"

  let ranges_expr (p : 'a Range.range t) : 'a Range.range list t =
    sep_by_comma1 p

  module Second = struct
    let second_expr : Time_expr_ast.second_expr t =
      try_ (string "::") *> get_pos
      >>= fun pos ->
      try_ nat_zero
      >>= (fun second ->
          if second >= 60 then
            failf "Invalid second: %d, pos: %s" second (string_of_pos pos)
          else return second)
          <|> ( non_space_string
                >>= fun s ->
                if s = "" then
                  failf "Missing second after ::, pos: %s" (string_of_pos pos)
                else failf "Invalid second: %s, pos: %s" s (string_of_pos pos) )
  end

  module Minute_second = struct
    let minute_second_expr : Time_expr_ast.minute_second_expr t =
      try_ (char ':') *> get_cnum
      >>= fun cnum ->
      nat_zero
      >>= fun minute ->
      if minute >= 60 then failf "Invalid minute: %d, pos: %d" minute cnum
      else
        get_cnum
        >>= fun cnum ->
        option 0 (char ':' *> nat_zero)
        >>= fun second ->
        if second >= 60 then failf "Invalid second: %d, pos: %d" second cnum
        else return Time_expr_ast.{ minute; second }
  end

  module Hms = struct
    let hms_mode =
      option `Hour_in_24_hours
        ( try_ (string "am" *> return `Hour_in_AM)
          <|> string "pm" *> return `Hour_in_PM )

    let handle_time_with_mode ~hour_pos ~(hour : int) ~(minute : int)
        ~(second : int) mode =
      let pos = string_of_pos hour_pos in
      match mode with
      | `Hour_in_24_hours ->
        if hour >= 24 then failf "Invalid hour: %d, pos: %s" hour pos
        else return Time_expr_ast.{ hour; minute; second }
      | `Hour_in_AM ->
        if 1 <= hour && hour <= 12 then
          let hour = if hour = 12 then 0 else hour in
          return Time_expr_ast.{ hour; minute; second }
        else failf "Invalid hour: %d, pos: %s" hour pos
      | `Hour_in_PM ->
        if 1 <= hour && hour <= 12 then
          let hour = if hour = 12 then 0 else hour in
          return Time_expr_ast.{ hour = hour + 12; minute; second }
        else failf "Invalid hour: %d, pos %s" hour pos

    let hms : Time_expr_ast.hms_expr t =
      get_pos
      >>= fun hour_pos ->
      try_ (nat_zero <* char ':')
      >>= (fun hour ->
          get_pos
          >>= fun minute_pos ->
          nat_zero
          >>= fun minute ->
          if minute >= 60 then
            failf "Invalid minute: %d, pos: %s" minute
              (string_of_pos minute_pos)
          else
            get_pos
            >>= fun second_pos ->
            option 0 (char ':' *> nat_zero)
            >>= fun second ->
            if second >= 60 then
              fail
                (Printf.sprintf "Invalid second: %d, pos: %s" second
                   (string_of_pos second_pos))
            else
              skip_space *> hms_mode
              >>= fun mode ->
              handle_time_with_mode ~hour_pos ~hour ~minute ~second mode)
          <|> ( nat_zero
                >>= fun hour ->
                skip_space *> hms_mode
                >>= fun mode ->
                handle_time_with_mode ~hour_pos ~hour ~minute:0 ~second:0 mode )

    let hms_range : Time_expr_ast.hms_range_expr t = range_exc_expr hms

    let hms_ranges : Time_expr_ast.hms_range_expr list t =
      sep_by_comma1 hms_range

    let hmss : Time_expr_ast.hms_expr list t = sep_by_comma1 hms
  end

  module Month_day = struct
    let month_day_expr : int t =
      get_pos
      >>= fun pos ->
      nat_zero
      >>= fun x ->
      if 1 <= x && x <= 31 then return x
      else
        fail
          (Printf.sprintf "Invalid month day: %d, pos: %s" x
             (string_of_pos pos))

    let month_day_range_expr : int Range.range t = range_inc_expr month_day_expr

    let month_day_ranges_expr : int Range.range list t =
      ranges_expr month_day_range_expr
  end

  module Weekday = struct
    let weekday_expr : Time.weekday t =
      alpha_string
      >>= fun x ->
      match Time.Of_string.weekday_of_string x with
      | Ok x -> return x
      | Error _ -> fail "Failed to interpret weekday string"

    let weekday_range_expr : Time.weekday Range.range t =
      range_inc_expr weekday_expr

    let weekday_ranges_expr : Time.weekday Range.range list t =
      ranges_expr weekday_range_expr
  end

  module Day = struct
    let day_expr : Time_expr_ast.day_expr t =
      try_
        ( Month_day.month_day_expr
          >>= fun x -> return (Time_expr_ast.Month_day x) )
      <|> (Weekday.weekday_expr >>= fun x -> return (Time_expr_ast.Weekday x))
  end

  module Month = struct
    let human_int_month_expr : Time_expr_ast.month_expr t =
      nat_zero
      >>= fun x ->
      match Time.month_of_human_int x with
      | Ok m -> return m
      | Error () -> fail (Printf.sprintf "Invalid month: %d" x)

    let human_int_month_range_expr = range_inc_expr human_int_month_expr

    let human_int_month_ranges_expr = sep_by_comma1 human_int_month_range_expr

    let direct_pick_month_expr : Time_expr_ast.month_expr t =
      alpha_string
      >>= fun x ->
      match Time.Of_string.month_of_string x with
      | Ok x -> return x
      | Error _ ->
        fail (Printf.sprintf "Failed to interpret month string: %s" x)

    let direct_pick_month_range_expr = range_inc_expr direct_pick_month_expr

    let direct_pick_month_ranges_expr =
      sep_by_comma1 direct_pick_month_range_expr

    let month_expr = try_ human_int_month_expr <|> direct_pick_month_expr

    let month_range_expr = range_inc_expr month_expr

    let month_ranges_expr = sep_by_comma1 month_range_expr
  end

  module Year = struct
    let year_expr : int t = nat_zero

    let year_range_expr = range_inc_expr year_expr

    let year_ranges_expr = sep_by_comma1 year_range_expr
  end

  module Time_point_expr = struct
    let tp_name =
      try_ (string "at:") *> ident_string
      >>= fun s -> return (Time_expr_ast.Tpe_name s)

    let tp_ymd_hms =
      try_
        ( nat_zero
          >>= fun year ->
          hyphen *> Month.human_int_month_expr
          >>= fun month ->
          hyphen *> Month_day.month_day_expr
          >>= fun month_day -> return (year, month, month_day) )
      >>= fun (year, month, month_day) ->
      skip_space *> Hms.hms
      >>= fun hms ->
      return (Time_expr_ast.Year_month_day_hms { year; month; month_day; hms })

    let tp_ymond_hms =
      try_
        ( nat_zero
          >>= fun year ->
          skip_space *> Month.direct_pick_month_expr
          >>= fun month -> return (year, month) )
      >>= (fun (year, month) ->
          skip_space *> Month_day.month_day_expr
          >>= fun month_day -> return (year, month, month_day))
      >>= fun (year, month, month_day) ->
      skip_space *> Hms.hms
      >>= fun hms ->
      return (Time_expr_ast.Year_month_day_hms { year; month; month_day; hms })

    let tp_md_hms =
      try_
        ( Month.month_expr
          >>= fun month ->
          hyphen *> Month_day.month_day_expr
          >>= fun month_day -> return (month, month_day) )
      >>= fun (month, month_day) ->
      skip_space *> Hms.hms
      >>= fun hms ->
      return (Time_expr_ast.Month_day_hms { month; month_day; hms })

    let tp_mond_hms =
      try_
        ( Month.direct_pick_month_expr
          >>= fun month ->
          skip_space *> Month_day.month_day_expr
          >>= fun month_day -> return (month, month_day) )
      >>= fun (month, month_day) ->
      skip_space *> Hms.hms
      >>= fun hms ->
      return (Time_expr_ast.Month_day_hms { month; month_day; hms })

    let tp_d_hms =
      try_
        ( Day.day_expr
          >>= fun day -> skip_space *> Hms.hms >>= fun hms -> return (day, hms) )
      >>= fun (day, hms) -> return (Time_expr_ast.Day_hms { day; hms })

    let tp_hms = Hms.hms >>= fun hms -> return (Time_expr_ast.Hms hms)

    let tp_minute_second =
      Minute_second.minute_second_expr
      >>= fun minute_second ->
      return (Time_expr_ast.Minute_second minute_second)

    let tp_second =
      Second.second_expr >>= fun second -> return (Time_expr_ast.Second second)

    let time_point_expr : Time_expr_ast.time_point_expr t =
      tp_name
      <|> tp_ymd_hms
      <|> tp_ymond_hms
      <|> tp_md_hms
      <|> tp_mond_hms
      <|> tp_d_hms
      <|> tp_second
      <|> tp_minute_second
      <|> tp_hms
  end

  module Branching_time_point_expr = struct
    let btp_days_hmss =
      try_
        ( Month_day.month_day_ranges_expr
          >>= fun month_days ->
          skip_space *> dot *> skip_space *> Hms.hmss
          >>= fun hmss ->
          return (Time_expr_ast.Btp_month_days_and_hmss { month_days; hmss }) )
      <|> ( Weekday.weekday_ranges_expr
            >>= fun weekdays ->
            skip_space *> dot *> skip_space *> Hms.hmss
            >>= fun hmss ->
            return (Time_expr_ast.Btp_weekdays_and_hmss { weekdays; hmss }) )

    let btp_hmss_days =
      try_
        ( Hms.hmss
          >>= fun hmss ->
          skip_space *> of_str *> skip_space *> Month_day.month_day_ranges_expr
          >>= fun month_days ->
          return (Time_expr_ast.Btp_month_days_and_hmss { month_days; hmss }) )
      <|> ( Hms.hmss
            >>= fun hmss ->
            skip_space *> of_str *> skip_space *> Weekday.weekday_ranges_expr
            >>= fun weekdays ->
            return (Time_expr_ast.Btp_weekdays_and_hmss { weekdays; hmss }) )

    let btp_months_mdays_hmss =
      Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> dot *> skip_space *> Hms.hmss
      >>= fun hmss ->
      return
        (Time_expr_ast.Btp_months_and_month_days_and_hmss
           { months; month_days; hmss })

    let btp_hmss_mdays_months =
      Hms.hmss
      >>= fun hmss ->
      skip_space *> of_str *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> of_str *> skip_space *> Month.month_ranges_expr
      >>= fun months ->
      return
        (Time_expr_ast.Btp_months_and_month_days_and_hmss
           { months; month_days; hmss })

    let btp_months_wdays_hmss =
      Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> Weekday.weekday_ranges_expr
      >>= fun weekdays ->
      skip_space *> dot *> skip_space *> Hms.hmss
      >>= fun hmss ->
      return
        (Time_expr_ast.Btp_months_and_weekdays_and_hmss
           { months; weekdays; hmss })

    let month_weekday_mode_expr =
      try_
        ( first_str *> skip_space *> nat_zero
          >>= fun n -> return (Some (Time_expr_ast.First_n n)) )
      <|> ( last_str *> skip_space *> nat_zero
            >>= fun n -> return (Some (Time_expr_ast.Last_n n)) )

    let btp_months_wday_hmss =
      Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> month_weekday_mode_expr
      >>= fun month_weekday_mode ->
      skip_space *> Weekday.weekday_expr
      >>= fun weekday ->
      skip_space *> dot *> skip_space *> Hms.hmss
      >>= fun hmss ->
      return
        (Time_expr_ast.Btp_months_and_weekday_and_hmss
           { months; weekday; hmss; month_weekday_mode })

    let btp_years_months_mdays_hmss =
      Year.year_ranges_expr
      >>= fun years ->
      skip_space *> dot *> skip_space *> Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> dot *> skip_space *> Hms.hmss
      >>= fun hmss ->
      return
        (Time_expr_ast.Btp_years_and_months_and_month_days_and_hmss
           { years; months; month_days; hmss })

    let btp_hmss_mdays_months_years =
      Hms.hmss
      >>= fun hmss ->
      skip_space *> of_str *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> of_str *> skip_space *> Month.month_ranges_expr
      >>= fun months ->
      skip_space *> of_str *> skip_space *> Year.year_ranges_expr
      >>= fun years ->
      return
        (Time_expr_ast.Btp_years_and_months_and_month_days_and_hmss
           { years; months; month_days; hmss })

    let branching_time_point_expr_atom :
      Time_expr_ast.branching_time_point_expr t =
      try_ btp_days_hmss
      <|> try_ btp_months_mdays_hmss
      <|> try_ btp_months_wdays_hmss
      <|> try_ btp_months_wday_hmss
      <|> try_ btp_years_months_mdays_hmss
      <|> try_ btp_hmss_mdays_months_years
      <|> try_ btp_hmss_mdays_months
      <|> try_ btp_hmss_days

    let branching_time_point_expr : Time_expr_ast.branching_time_point_expr t =
      try_
        ( branch_unary_op
          >>= fun op ->
          skip_space *> branching_time_point_expr_atom
          >>= fun e -> return (Time_expr_ast.Btp_unary_op (op, e)) )
      <|> branching_time_point_expr_atom
  end

  module Time_slot_expr = struct
    let ts_name =
      try_ (string "during:") *> ident_string
      >>= fun s -> return (Time_expr_ast.Tse_name s)

    let ts_explicit_time_slot =
      try_
        ( Time_point_expr.time_point_expr
          >>= fun start -> skip_space *> to_str *> return start )
      >>= fun start ->
      skip_space *> get_pos
      >>= fun pos ->
      try_ Time_point_expr.time_point_expr
      >>= (fun end_exc ->
          return (Time_expr_ast.Explicit_time_slot (start, end_exc)))
          <|> failf "Expected time point expression at %s" (string_of_pos pos)

    let time_slot_expr : Time_expr_ast.time_slot_expr t =
      ts_name <|> ts_explicit_time_slot
  end

  module Branching_time_slot_expr = struct
    let bts_days_hms_ranges =
      try_
        ( Month_day.month_day_ranges_expr
          >>= fun month_days ->
          skip_space *> dot *> skip_space *> Hms.hms_ranges
          >>= fun hms_ranges ->
          return
            (Time_expr_ast.Bts_month_days_and_hms_ranges
               { month_days; hms_ranges }) )
      <|> ( Weekday.weekday_ranges_expr
            >>= fun weekdays ->
            skip_space *> dot *> skip_space *> Hms.hms_ranges
            >>= fun hms_ranges ->
            return
              (Time_expr_ast.Bts_weekdays_and_hms_ranges { weekdays; hms_ranges })
          )

    let bts_hms_ranges_days =
      try_
        ( Hms.hms_ranges
          >>= fun hms_ranges ->
          skip_space *> of_str *> skip_space *> Month_day.month_day_ranges_expr
          >>= fun month_days ->
          return
            (Time_expr_ast.Bts_month_days_and_hms_ranges
               { month_days; hms_ranges }) )
      <|> ( Hms.hms_ranges
            >>= fun hms_ranges ->
            skip_space *> of_str *> skip_space *> Weekday.weekday_ranges_expr
            >>= fun weekdays ->
            return
              (Time_expr_ast.Bts_weekdays_and_hms_ranges { weekdays; hms_ranges })
          )

    let bts_months_mdays_hms_ranges =
      Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> dot *> skip_space *> Hms.hms_ranges
      >>= fun hms_ranges ->
      return
        (Time_expr_ast.Bts_months_and_month_days_and_hms_ranges
           { months; month_days; hms_ranges })

    let bts_hms_ranges_mdays_months =
      Hms.hms_ranges
      >>= fun hms_ranges ->
      skip_space *> of_str *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> of_str *> skip_space *> Month.month_ranges_expr
      >>= fun months ->
      return
        (Time_expr_ast.Bts_months_and_month_days_and_hms_ranges
           { months; month_days; hms_ranges })

    let bts_months_wdays_hms_ranges =
      Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> Weekday.weekday_ranges_expr
      >>= fun weekdays ->
      skip_space *> dot *> skip_space *> Hms.hms_ranges
      >>= fun hms_ranges ->
      return
        (Time_expr_ast.Bts_months_and_weekdays_and_hms_ranges
           { months; weekdays; hms_ranges })

    let month_weekday_mode_expr =
      try_
        ( first_str *> skip_space *> nat_zero
          >>= fun n -> return (Some (Time_expr_ast.First_n n)) )
      <|> ( last_str *> skip_space *> nat_zero
            >>= fun n -> return (Some (Time_expr_ast.Last_n n)) )

    let bts_months_wday_hms_ranges =
      Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> month_weekday_mode_expr
      >>= fun month_weekday_mode ->
      skip_space *> Weekday.weekday_expr
      >>= fun weekday ->
      skip_space *> dot *> skip_space *> Hms.hms_ranges
      >>= fun hms_ranges ->
      return
        (Time_expr_ast.Bts_months_and_weekday_and_hms_ranges
           { months; weekday; hms_ranges; month_weekday_mode })

    let bts_years_months_mdays_hms_ranges =
      Year.year_ranges_expr
      >>= fun years ->
      skip_space *> dot *> skip_space *> Month.month_ranges_expr
      >>= fun months ->
      skip_space *> dot *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> dot *> skip_space *> Hms.hms_ranges
      >>= fun hms_ranges ->
      return
        (Time_expr_ast.Bts_years_and_months_and_month_days_and_hms_ranges
           { years; months; month_days; hms_ranges })

    let bts_hms_ranges_mdays_months_years =
      Hms.hms_ranges
      >>= fun hms_ranges ->
      skip_space *> of_str *> skip_space *> Month_day.month_day_ranges_expr
      >>= fun month_days ->
      skip_space *> of_str *> skip_space *> Month.month_ranges_expr
      >>= fun months ->
      skip_space *> of_str *> skip_space *> Year.year_ranges_expr
      >>= fun years ->
      return
        (Time_expr_ast.Bts_years_and_months_and_month_days_and_hms_ranges
           { years; months; month_days; hms_ranges })

    let branching_time_slot_expr_atom : Time_expr_ast.branching_time_slot_expr t
      =
      try_ bts_days_hms_ranges
      <|> try_ bts_months_mdays_hms_ranges
      <|> try_ bts_months_wdays_hms_ranges
      <|> try_ bts_months_wday_hms_ranges
      <|> try_ bts_years_months_mdays_hms_ranges
      <|> try_ bts_hms_ranges_mdays_months_years
      <|> try_ bts_hms_ranges_mdays_months
      <|> try_ bts_hms_ranges_days

    let branching_time_slot_expr : Time_expr_ast.branching_time_slot_expr t =
      try_
        ( branch_unary_op
          >>= fun op ->
          skip_space *> branching_time_slot_expr_atom
          >>= fun e -> return (Time_expr_ast.Bts_unary_op (op, e)) )
      <|> branching_time_slot_expr_atom
  end

  let inter : (Time_expr_ast.t -> Time_expr_ast.t -> Time_expr_ast.t) t =
    string "&&"
    *> return (fun a b -> Time_expr_ast.Time_binary_op (Inter, a, b))

  let union : (Time_expr_ast.t -> Time_expr_ast.t -> Time_expr_ast.t) t =
    string "||"
    *> return (fun a b -> Time_expr_ast.Time_binary_op (Union, a, b))

  let round_robin_select :
    (Time_expr_ast.t -> Time_expr_ast.t -> Time_expr_ast.t) t =
    string ">>"
    *> return (fun a b -> Time_expr_ast.Time_round_robin_select [ a; b ])

  let flatten_round_robin_select (e : Time_expr_ast.t) : Time_expr_ast.t =
    let open Time_expr_ast in
    let rec aux e =
      match e with
      | Time_point_expr _ -> e
      | Time_slot_expr _ -> e
      | Branching_time_point_expr _ -> e
      | Branching_time_slot_expr _ -> e
      | Time_pattern _ -> e
      | Time_unary_op (op, e) -> Time_unary_op (op, aux e)
      | Time_binary_op (op, e1, e2) -> Time_binary_op (op, aux e1, aux e2)
      | Time_round_robin_select l ->
        l
        |> List.to_seq
        |> Seq.map aux
        |> Seq.flat_map (fun e ->
            match e with
            | Time_round_robin_select l -> List.to_seq l
            | _ -> Seq.return e)
        |> List.of_seq
        |> fun l -> Time_round_robin_select l
    in
    aux e

  let time_expr ~(enabled_fragments : lang_fragment list) : Time_expr_ast.t t =
    let open Time_expr_ast in
    let atom_parsers =
      [
        ( if List.mem `Time_pattern enabled_fragments then
            Some
              ( try_ Time_pattern.Parsers.time_pattern_expr
                >>= fun e -> return (Time_expr_ast.Time_pattern e) )
          else None );
        ( if List.mem `Branching_time_point_expr enabled_fragments then
            Some
              ( Branching_time_point_expr.branching_time_point_expr
                >>= fun e -> return (Time_expr_ast.Branching_time_point_expr e) )
          else None );
        ( if List.mem `Branching_time_slot_expr enabled_fragments then
            Some
              ( Branching_time_slot_expr.branching_time_slot_expr
                >>= fun e -> return (Time_expr_ast.Branching_time_slot_expr e) )
          else None );
        ( if List.mem `Time_slot_expr enabled_fragments then
            Some
              ( Time_slot_expr.time_slot_expr
                >>= fun e -> return (Time_expr_ast.Time_slot_expr e) )
          else None );
        ( if List.mem `Time_point_expr enabled_fragments then
            Some
              ( Time_point_expr.time_point_expr
                >>= fun e -> return (Time_expr_ast.Time_point_expr e) )
          else None );
      ]
      |> List.filter_map (fun x -> x)
    in
    let rec make_atom l =
      match l with
      | [] ->
        get_pos
        >>= fun pos ->
        failf "Failed to parse expression, pos: %s" (string_of_pos pos)
      | x :: xs -> x <|> make_atom xs
    in
    let atom = skip_space *> make_atom atom_parsers <* skip_space in
    fix (fun expr ->
        (* let atom =
         *   skip_space
         *   *> ( try_ Time_pattern.Parsers.time_pattern_expr
         *        >>= (fun e -> return (Time_expr_ast.Time_pattern e))
         *            <|> ( Branching_time_point_expr.branching_time_point_expr
         *                  >>= fun e -> return (Time_expr_ast.Branching_time_point_expr e)
         *                )
         *            <|> ( Branching_time_slot_expr.branching_time_slot_expr
         *                  >>= fun e -> return (Time_expr_ast.Branching_time_slot_expr e)
         *                )
         *            <|> ( Time_slot_expr.time_slot_expr
         *                  >>= fun e -> return (Time_expr_ast.Time_slot_expr e) )
         *            <|> ( Time_point_expr.time_point_expr
         *                  >>= fun e -> return (Time_expr_ast.Time_point_expr e) ) )
         *   <* skip_space
         * in *)
        let group =
          try_ (char '(') *> skip_space *> expr
          <* skip_space
          <* char ')'
          <|> atom
        in
        let unary_op =
          try_ not_str *> return Not
          <|> try_ next_slot_str *> return (Next_n_slots 1)
          <|> try_ next_point_str *> return (Next_n_points 1)
          <|> ( try_
                  ( next_str *> hyphen *> nat_zero
                    <* hyphen
                    <* (try_ slots_str <|> slot_str) )
                >>= fun n -> return (Next_n_slots n) )
          <|> ( try_
                  ( next_str *> hyphen *> nat_zero
                    <* hyphen
                    <* (try_ points_str <|> point_str) )
                >>= fun n -> return (Next_n_points n) )
        in
        let inter_part =
          try_ unary_op
          >>= (fun op ->
              skip_space *> expr >>= fun e -> return (Time_unary_op (op, e)))
              <|> group
        in
        let ordered_select_part = chainl1 inter_part round_robin_select in
        let union_part = chainl1 ordered_select_part inter in
        chainl1 union_part union)
    >>= fun e -> return (flatten_round_robin_select e)

  let of_string ?(enabled_fragments = all_lang_fragments) (s : string) :
    (Time_expr_ast.t, string) result =
    match enabled_fragments with
    | [] -> Error "No language fragments are enabled"
    | _ -> parse_string (time_expr ~enabled_fragments <* skip_space <* eoi) s
end

let of_string = Of_string.of_string

module To_time_pattern_lossy = struct
  module Second = struct
    let update_time_pattern_using_second_expr (e : Time_expr_ast.second_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      if Time.Check.second_is_valid ~second:e then { base with seconds = [ e ] }
      else raise (Invalid_time_expr (Printf.sprintf "Invalid second: ::%d" e))

    let time_range_pattern_of_second_range_expr_and_base_time_pattern
        (e : Time_expr_ast.second_range_expr) (base : Time_pattern.time_pattern)
      : Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_second_expr x base,
            update_time_pattern_using_second_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_second_expr x base,
            update_time_pattern_using_second_expr y base )

    let time_range_patterns_of_second_ranges_and_base_time_pattern
        (l : Time_expr_ast.second_range_expr list)
        (base : Time_pattern.time_pattern) :
      Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_second_range_expr_and_base_time_pattern e
            base)
  end

  module Minute_second = struct
    let update_time_pattern_using_minute_second_expr
        (e : Time_expr_ast.minute_second_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      if Time.Check.minute_second_is_valid ~minute:e.minute ~second:e.second
      then { base with minutes = [ e.minute ] }
      else
        raise
          (Invalid_time_expr
             (Printf.sprintf "Invalid minute second: :%d:%d" e.minute e.second))

    let time_range_pattern_of_minute_second_range_expr_and_base_time_pattern
        (e : Time_expr_ast.minute_second_range_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_minute_second_expr x base,
            update_time_pattern_using_minute_second_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_minute_second_expr x base,
            update_time_pattern_using_minute_second_expr y base )

    let time_range_patterns_of_hour_minute_second_ranges_and_base_time_pattern
        (l : Time_expr_ast.minute_second_range_expr list)
        (base : Time_pattern.time_pattern) :
      Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_minute_second_range_expr_and_base_time_pattern
            e base)
  end

  module Hms = struct
    let update_time_pattern_using_hms_expr (e : Time_expr_ast.hms_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      if
        Time.Check.hour_minute_second_is_valid ~hour:e.hour ~minute:e.minute
          ~second:e.second
      then { base with hours = [ e.hour ]; minutes = [ e.minute ] }
      else
        raise
          (Invalid_time_expr
             (Printf.sprintf "Invalid hour minute: %d:%d" e.hour e.minute))

    let time_range_pattern_of_hms_range_expr_and_base_time_pattern
        (e : Time_expr_ast.hms_range_expr) (base : Time_pattern.time_pattern) :
      Time_pattern.time_range_pattern =
      match e with
      | `Range_inc (x, y) ->
        `Range_inc
          ( update_time_pattern_using_hms_expr x base,
            update_time_pattern_using_hms_expr y base )
      | `Range_exc (x, y) ->
        `Range_exc
          ( update_time_pattern_using_hms_expr x base,
            update_time_pattern_using_hms_expr y base )

    let time_range_patterns_of_hms_ranges_and_base_time_pattern
        (l : Time_expr_ast.hms_range_expr list)
        (base : Time_pattern.time_pattern) :
      Time_pattern.time_range_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e ->
          time_range_pattern_of_hms_range_expr_and_base_time_pattern e base)

    let time_patterns_of_hmss_and_base_time_pattern
        (l : Time_expr_ast.hms_expr list) (base : Time_pattern.time_pattern) :
      Time_pattern.time_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_hms_expr e base)
  end

  module Month_day = struct
    let update_time_pattern_using_month_day_expr (x : int)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      if 1 <= x && x <= 31 then { base with month_days = [ x ] }
      else
        raise (Invalid_time_expr (Printf.sprintf "Invalid day of month: %d" x))

    let time_pattern_of_month_day_expr x =
      update_time_pattern_using_month_day_expr x Time_pattern.empty

    let time_patterns_of_month_days_and_base_time_pattern (l : int list)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_month_day_expr e base)
  end

  module Weekday = struct
    let update_time_pattern_using_weekday_expr (x : Time.weekday)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      { base with weekdays = [ x ] }

    let time_pattern_of_weekday_expr x =
      update_time_pattern_using_weekday_expr x Time_pattern.empty

    let time_patterns_of_weekdays_and_base_time_pattern (l : Time.weekday list)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_weekday_expr e base)
  end

  module Day = struct
    let update_time_pattern_using_day_expr (e : Time_expr_ast.day_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      match e with
      | Month_day e -> Month_day.update_time_pattern_using_month_day_expr e base
      | Weekday e -> Weekday.update_time_pattern_using_weekday_expr e base
  end

  module Month = struct
    let update_time_pattern_using_month_expr (e : Time_expr_ast.month_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      { base with months = [ e ] }

    let time_pattern_of_month_expr x =
      update_time_pattern_using_month_expr x Time_pattern.empty

    let time_patterns_of_months_and_base_time_pattern (l : Time.month list)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern Seq.t =
      List.to_seq l
      |> Seq.map (fun e -> update_time_pattern_using_month_expr e base)
  end

  module Year = struct
    let update_time_pattern_using_year_expr (e : Time_expr_ast.year_expr)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      { base with years = [ e ] }

    let time_pattern_of_year_expr x =
      update_time_pattern_using_year_expr x Time_pattern.empty
  end

  module Unix_times = struct
    let update_time_pattern_using_unix_seconds (unix_seconds : int64 list)
        (base : Time_pattern.time_pattern) : Time_pattern.time_pattern =
      { base with unix_seconds }

    let time_pattern_of_unix_seconds l =
      update_time_pattern_using_unix_seconds l Time_pattern.empty
  end

  let time_pattern_of_time_point_expr
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (e : Time_expr_ast.time_point_expr) :
    (Time_pattern.time_pattern, string) result =
    try
      match Resolve.resolve_time_point_expr ~f_resolve_tpe_name e with
      | Error msg -> Error msg
      | Ok e ->
        Ok
          ( match e with
            | Tpe_name _ -> failwith "Unexpected case"
            | Tpe_unix_seconds l -> Unix_times.time_pattern_of_unix_seconds l
            | Year_month_day_hms { year; month; month_day; hms } ->
              Time_pattern.empty
              |> Year.update_time_pattern_using_year_expr year
              |> Month.update_time_pattern_using_month_expr month
              |> Month_day.update_time_pattern_using_month_day_expr month_day
              |> Hms.update_time_pattern_using_hms_expr hms
            | Month_day_hms { month; month_day; hms } ->
              Time_pattern.empty
              |> Month.update_time_pattern_using_month_expr month
              |> Month_day.update_time_pattern_using_month_day_expr month_day
              |> Hms.update_time_pattern_using_hms_expr hms
            | Day_hms { day; hms } ->
              Time_pattern.empty
              |> Day.update_time_pattern_using_day_expr day
              |> Hms.update_time_pattern_using_hms_expr hms
            | Hms hms ->
              Hms.update_time_pattern_using_hms_expr hms Time_pattern.empty
            | Minute_second second ->
              Minute_second.update_time_pattern_using_minute_second_expr
                second Time_pattern.empty
            | Second second ->
              Second.update_time_pattern_using_second_expr second
                Time_pattern.empty )
    with Invalid_time_expr msg -> Error msg

  let time_pattern_of_time_point_expr
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (e : Time_expr_ast.time_point_expr) :
    (Time_pattern.time_pattern, string) result =
    time_pattern_of_time_point_expr ~f_resolve_tpe_name e

  let time_range_patterns_of_time_slot_expr
      ?(f_resolve_tse_name = default_f_resolve_tse_name)
      ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
      (e : Time_expr_ast.time_slot_expr) :
    (Time_pattern.time_range_pattern list, string) result =
    let open Time_expr_ast in
    let aux e =
      match e with
      | Tse_name _ -> failwith "Unexpected case"
      | Explicit_time_slot (start, end_exc) -> (
          match time_pattern_of_time_point_expr ~f_resolve_tpe_name start with
          | Error msg -> raise (Invalid_time_expr msg)
          | Ok start -> (
              match
                time_pattern_of_time_point_expr ~f_resolve_tpe_name end_exc
              with
              | Error msg -> raise (Invalid_time_expr msg)
              | Ok end_exc -> [ `Range_exc (start, end_exc) ] ) )
    in
    try
      match
        Resolve.resolve_time_slot_expr ~f_resolve_tse_name ~f_resolve_tpe_name e
      with
      | Error msg -> Error msg
      | Ok e -> Ok (aux e)
    with Invalid_time_expr msg -> Error msg

  let time_patterns_of_branching_time_point_expr
      (e : Time_expr_ast.branching_time_point_expr) :
    (Time_pattern.time_pattern list, string) result =
    let open Time_expr_ast in
    let rec aux e =
      match e with
      | Btp_unary_op (_, e) -> aux e
      | Btp_month_days_and_hmss { month_days; hmss } ->
        (* check_hmss hmss; *)
        month_days
        |> List.to_seq
        |> Time.Month_day_ranges.Flatten.flatten
        |> Seq.map Month_day.time_pattern_of_month_day_expr
        |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
        |> List.of_seq
      | Btp_weekdays_and_hmss { weekdays; hmss } ->
        weekdays
        |> List.to_seq
        |> Time.Weekday_ranges.Flatten.flatten
        |> Seq.map Weekday.time_pattern_of_weekday_expr
        |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
        |> List.of_seq
      | Btp_months_and_month_days_and_hmss { months; month_days; hmss } ->
        let month_days =
          Time.Month_tm_int_ranges.Flatten.flatten_list month_days
        in
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map Month.time_pattern_of_month_expr
        |> Seq.flat_map
          (Month_day.time_patterns_of_month_days_and_base_time_pattern
             month_days)
        |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
        |> List.of_seq
      | Btp_months_and_weekdays_and_hmss { months; weekdays; hmss } ->
        let weekdays = Time.Weekday_ranges.Flatten.flatten_list weekdays in
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map Month.time_pattern_of_month_expr
        |> Seq.flat_map
          (Weekday.time_patterns_of_weekdays_and_base_time_pattern
             weekdays)
        |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
        |> List.of_seq
      | Btp_months_and_weekday_and_hmss
          { months; weekday; hmss; month_weekday_mode = _ } ->
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map Month.time_pattern_of_month_expr
        |> Seq.map (Weekday.update_time_pattern_using_weekday_expr weekday)
        |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
        |> List.of_seq
      | Btp_years_and_months_and_month_days_and_hmss
          { years; months; month_days; hmss } ->
        let months = Time.Month_ranges.Flatten.flatten_list months in
        let month_days =
          Time.Month_day_ranges.Flatten.flatten_list month_days
        in
        years
        |> List.to_seq
        |> Time.Year_ranges.Flatten.flatten
        |> Seq.map Year.time_pattern_of_year_expr
        |> Seq.flat_map
          (Month.time_patterns_of_months_and_base_time_pattern months)
        |> Seq.flat_map
          (Month_day.time_patterns_of_month_days_and_base_time_pattern
             month_days)
        |> Seq.flat_map (Hms.time_patterns_of_hmss_and_base_time_pattern hmss)
        |> List.of_seq
    in
    try Ok (aux e) with Invalid_time_expr msg -> Error msg

  let time_range_patterns_of_branching_time_slot_expr
      (e : Time_expr_ast.branching_time_slot_expr) :
    (Time_pattern.time_range_pattern list, string) result =
    let open Time_expr_ast in
    let rec aux e =
      match e with
      | Bts_unary_op (_, e) -> aux e
      | Bts_month_days_and_hms_ranges { month_days; hms_ranges } ->
        (* check_hms_ranges hms_ranges; *)
        month_days
        |> List.to_seq
        |> Time.Month_day_ranges.Flatten.flatten
        |> Seq.map Month_day.time_pattern_of_month_day_expr
        |> Seq.flat_map
          (Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern
             hms_ranges)
        |> List.of_seq
      | Bts_weekdays_and_hms_ranges { weekdays; hms_ranges } ->
        weekdays
        |> List.to_seq
        |> Time.Weekday_ranges.Flatten.flatten
        |> Seq.map Weekday.time_pattern_of_weekday_expr
        |> Seq.flat_map
          (Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern
             hms_ranges)
        |> List.of_seq
      | Bts_months_and_month_days_and_hms_ranges
          { months; month_days; hms_ranges } ->
        let month_days =
          Time.Month_tm_int_ranges.Flatten.flatten_list month_days
        in
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map Month.time_pattern_of_month_expr
        |> Seq.flat_map
          (Month_day.time_patterns_of_month_days_and_base_time_pattern
             month_days)
        |> Seq.flat_map
          (Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern
             hms_ranges)
        |> List.of_seq
      | Bts_months_and_weekdays_and_hms_ranges { months; weekdays; hms_ranges }
        ->
        let weekdays = Time.Weekday_ranges.Flatten.flatten_list weekdays in
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map Month.time_pattern_of_month_expr
        |> Seq.flat_map
          (Weekday.time_patterns_of_weekdays_and_base_time_pattern
             weekdays)
        |> Seq.flat_map
          (Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern
             hms_ranges)
        |> List.of_seq
      | Bts_months_and_weekday_and_hms_ranges
          { months; weekday; hms_ranges; month_weekday_mode = _ } ->
        months
        |> List.to_seq
        |> Time.Month_ranges.Flatten.flatten
        |> Seq.map Month.time_pattern_of_month_expr
        |> Seq.map (Weekday.update_time_pattern_using_weekday_expr weekday)
        |> Seq.flat_map
          (Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern
             hms_ranges)
        |> List.of_seq
      | Bts_years_and_months_and_month_days_and_hms_ranges
          { years; months; month_days; hms_ranges } ->
        let months = Time.Month_ranges.Flatten.flatten_list months in
        let month_days =
          Time.Month_day_ranges.Flatten.flatten_list month_days
        in
        years
        |> List.to_seq
        |> Time.Year_ranges.Flatten.flatten
        |> Seq.map Year.time_pattern_of_year_expr
        |> Seq.flat_map
          (Month.time_patterns_of_months_and_base_time_pattern months)
        |> Seq.flat_map
          (Month_day.time_patterns_of_month_days_and_base_time_pattern
             month_days)
        |> Seq.flat_map
          (Hms.time_range_patterns_of_hms_ranges_and_base_time_pattern
             hms_ranges)
        |> List.of_seq
    in
    try Ok (aux e) with Invalid_time_expr msg -> Error msg
end

module Time_point_expr = struct
  let matching_unix_seconds ~f_resolve_tpe_name (search_param : Search_param.t)
      (e : Time_expr_ast.time_point_expr) : (int64 Seq.t, string) result =
    match
      To_time_pattern_lossy.time_pattern_of_time_point_expr ~f_resolve_tpe_name
        e
    with
    | Error msg -> Error msg
    | Ok pat -> (
        match
          Time_pattern.Single_pattern.matching_time_slots
            ~allow_search_param_override:true search_param pat
        with
        | Error e -> Error (Time_pattern.To_string.string_of_error e)
        | Ok s -> s |> Seq.map fst |> Result.ok )

  (* (
   *   match
   *     Resolve.resolve_time_point_expr ~f_resolve_tpe_name e
   *   with
   *   | Error msg -> Error msg
   *   | Ok e -> (
   *       let selector =
   *         match e with
   *         | Tpe_name _ -> failwith "Unexpected case"
   *         | Tpe_unix_seconds l -> OSeq.take (List.length l)
   *         | Year_month_day_hour_minute_second _
   *         | Month_day_hour_minute_second _ | Day_hour_minute_second _
   *         | Hour_minute_second _ | Minute_second _ | Second _ -> (
   *             match Option.value ~default:bound force_bound with
   *             | `Next -> OSeq.take 1
   *             | `Every -> fun x -> x )
   *       in
   *       match
   *         Time_pattern.Single_pattern.matching_time_slots
   *           ~allow_search_param_override:true search_param pat
   *       with
   *       | Error e -> Error (Time_pattern.To_string.string_of_error e)
   *       | Ok s -> s |> Seq.map (fun (x, _) -> x)
   *                 (\* |> selector *\)
   *                 |> Result.ok )
   * ) *)
end

module Time_slot_expr = struct
  let get_first_or_last_n_matches_of_same_month_date_time_pair_seq
      ~(first_or_last : [ `First | `Last ]) ~(n : int)
      (s : (Time.Date_time.t * Time.Date_time.t) Seq.t) :
    (Time.Date_time.t * Time.Date_time.t) Seq.t =
    let flush_acc first_or_last (n : int)
        (acc : (Time.Date_time.t * Time.Date_time.t) list) :
      (Time.Date_time.t * Time.Date_time.t) Seq.t =
      ( match first_or_last with
        | `First -> acc |> List.rev |> Misc_utils.take_first_n_list n
        | `Last -> acc |> List.rev |> Misc_utils.take_last_n_list n )
      |> List.to_seq
    in
    let rec aux first_or_last (n : int)
        (acc : (Time.Date_time.t * Time.Date_time.t) list)
        (s : (Time.Date_time.t * Time.Date_time.t) Seq.t) :
      (Time.Date_time.t * Time.Date_time.t) Seq.t =
      match s () with
      | Seq.Nil -> flush_acc first_or_last n acc
      | Seq.Cons ((start, end_exc), rest) -> (
          match acc with
          | [] -> aux first_or_last n [ (start, end_exc) ] rest
          | (x, _) :: _ ->
            if x.month = start.month then
              aux first_or_last n ((start, end_exc) :: acc) rest
            else
              OSeq.append
                (flush_acc first_or_last n acc)
                (aux first_or_last n [ (start, end_exc) ] rest) )
    in
    aux first_or_last n [] s

  let get_first_or_last_n_matches_of_same_month
      ~(first_or_last : [ `First | `Last ]) ~(n : int)
      (search_param : Search_param.t) (s : Time_slot.t Seq.t) :
    Time_slot.t Seq.t =
    let tz_offset_s_of_date_time = search_param.search_using_tz_offset_s in
    s
    |> Seq.map (fun (x, y) ->
        ( Time.Date_time.of_unix_second ~tz_offset_s_of_date_time x
          |> Result.get_ok,
          Time.Date_time.of_unix_second ~tz_offset_s_of_date_time y
          |> Result.get_ok ))
    |> get_first_or_last_n_matches_of_same_month_date_time_pair_seq
      ~first_or_last ~n
    |> Seq.map (fun (x, y) ->
        ( Time.Date_time.to_unix_second x |> Result.get_ok,
          Time.Date_time.to_unix_second y |> Result.get_ok ))

  let matching_time_slots ~f_resolve_tpe_name ~f_resolve_tse_name
      (search_param : Search_param.t) (e : Time_expr_ast.time_slot_expr) :
    (Time_slot.t Seq.t, string) result =
    match
      Resolve.resolve_time_slot_expr ~f_resolve_tse_name ~f_resolve_tpe_name e
    with
    | Error msg -> Error msg
    | Ok e -> (
        (* let list_selector =
         *   match e with
         *   | Tse_name _ -> failwith "Unexpected case"
         *   | Explicit_time_slot _ -> (
         *       match Option.value ~default:bound force_bound with
         *       | `Next -> OSeq.take 1
         *       | `Every -> fun x -> x )
         *   | Month_days_and_hour_minute_second_ranges _
         *   | Weekdays_and_hour_minute_second_ranges _
         *   | Months_and_month_days_and_hour_minute_second_ranges _
         *   | Months_and_weekdays_and_hour_minute_second_ranges _
         *   | Years_and_months_and_month_days_and_hour_minute_second_ranges _ -> (
         *       match Option.value ~default:bound force_bound with
         *       | `Next -> OSeq.take 1
         *       | `Every -> fun x -> x )
         *   | Months_and_weekday_and_hour_minute_second_ranges _ -> (
         *       match Option.value ~default:bound force_bound with
         *       | `Next -> OSeq.take 4
         *       | `Every -> fun x -> x )
         * in
         * let flat_selector =
         *   match e with
         *   | Tse_name _ -> failwith "Unexpected case"
         *   | Explicit_time_slot _ | Month_days_and_hour_minute_second_ranges _
         *   | Weekdays_and_hour_minute_second_ranges _
         *   | Months_and_month_days_and_hour_minute_second_ranges _
         *   | Months_and_weekdays_and_hour_minute_second_ranges _
         *   | Years_and_months_and_month_days_and_hour_minute_second_ranges _ ->
         *     fun x -> x
         *   | Months_and_weekday_and_hour_minute_second_ranges
         *       { month_weekday_mode; _ } -> (
         *       match month_weekday_mode with
         *       | None -> fun x -> x
         *       | Some (First_n n) ->
         *         get_first_or_last_n_matches_of_same_month
         *           ~first_or_last:`First ~n search_param
         *       | Some (Last_n n) ->
         *         get_first_or_last_n_matches_of_same_month ~first_or_last:`Last
         *           ~n search_param )
         * in *)
        match
          To_time_pattern_lossy.time_range_patterns_of_time_slot_expr
            ~f_resolve_tse_name ~f_resolve_tpe_name e
        with
        | Error msg -> Error msg
        | Ok l -> (
            match
              Time_pattern.Range_pattern
              .matching_time_slots_round_robin_non_decreasing
                ~allow_search_param_override:true search_param l
            with
            | Error e -> Error (Time_pattern.To_string.string_of_error e)
            | Ok s ->
              s
              (* |> list_selector *)
              |> Seq.flat_map List.to_seq
              (* |> flat_selector *)
              |> Result.ok ) )
end

module Branching_time_point_expr = struct
  let matching_unix_seconds (search_param : Search_param.t)
      (e : Time_expr_ast.branching_time_point_expr) :
    (int64 Seq.t, string) result =
    let rec aux (e : Time_expr_ast.branching_time_point_expr) :
      (Time_slot.t list Seq.t, string) result =
      let open Time_expr_ast in
      match e with
      | Btp_unary_op (op, e) ->
        aux e
        |> Result.map (fun s ->
            match op with
            | Next_n_batches n -> OSeq.take n s
            | Every_batch -> s)
      | _ -> (
          match
            To_time_pattern_lossy.time_patterns_of_branching_time_point_expr e
          with
          | Error msg -> Error msg
          | Ok l ->
            Time_pattern.Single_pattern
            .matching_time_slots_round_robin_non_decreasing
              ~allow_search_param_override:true search_param l
            |> Result.map_error Time_pattern.To_string.string_of_error )
    in
    aux e |> Result.map (fun s -> s |> Seq.flat_map List.to_seq |> Seq.map fst)
end

module Branching_time_slot_expr = struct
  let matching_time_slots (search_param : Search_param.t)
      (e : Time_expr_ast.branching_time_slot_expr) :
    (Time_slot.t Seq.t, string) result =
    let rec aux (e : Time_expr_ast.branching_time_slot_expr) :
      (Time_slot.t list Seq.t, string) result =
      let open Time_expr_ast in
      match e with
      | Bts_unary_op (op, e) ->
        aux e
        |> Result.map (fun s ->
            match op with
            | Next_n_batches n -> OSeq.take n s
            | Every_batch -> s)
      | _ -> (
          match
            To_time_pattern_lossy
            .time_range_patterns_of_branching_time_slot_expr e
          with
          | Error msg -> Error msg
          | Ok l ->
            Time_pattern.Range_pattern
            .matching_time_slots_round_robin_non_decreasing
              ~allow_search_param_override:true search_param l
            |> Result.map_error Time_pattern.To_string.string_of_error )
    in
    aux e |> Result.map (fun s -> s |> Seq.flat_map List.to_seq)
end

let matching_time_slots ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
    ?(f_resolve_tse_name = default_f_resolve_tse_name)
    (search_param : Search_param.t) (e : Time_expr_ast.t) :
  (Time_slot.t Seq.t, string) result =
  let rec aux e =
    let open Time_expr_ast in
    match e with
    | Time_point_expr e ->
      Time_point_expr.matching_unix_seconds ~f_resolve_tpe_name search_param e
      |> Result.map (Seq.map (fun x -> (x, Int64.succ x)))
    | Time_slot_expr e ->
      Time_slot_expr.matching_time_slots ~f_resolve_tpe_name
        ~f_resolve_tse_name search_param e
    | Branching_time_point_expr e ->
      Branching_time_point_expr.matching_unix_seconds search_param e
      |> Result.map (Seq.map (fun x -> (x, Int64.succ x)))
    | Branching_time_slot_expr e ->
      Branching_time_slot_expr.matching_time_slots search_param e
    | Time_pattern pat ->
      Time_pattern.Single_pattern.matching_time_slots
        ~allow_search_param_override:true search_param pat
      |> Result.map_error Time_pattern.To_string.string_of_error
    | Time_unary_op (op, e) -> (
        match aux e with
        | Error x -> Error x
        | Ok s' -> (
            match op with
            | Not -> (
                match
                  Time_pattern.Single_pattern.matching_time_slots
                    ~allow_search_param_override:true search_param
                    Time_pattern.empty
                with
                | Error x -> Error (Time_pattern.To_string.string_of_error x)
                | Ok s -> Ok (Time_slots.relative_complement ~not_mem_of:s' s) )
            | Every -> Ok s'
            | Next_n_slots n -> s' |> OSeq.take n |> Result.ok
            | Next_n_points n ->
              s'
              |> Time_slots.chunk ~skip_check:true ~chunk_size:1L
              |> OSeq.take n
              |> Time_slots.Normalize.normalize ~skip_filter_invalid:true
                ~skip_filter_empty:true ~skip_sort:true
              |> Result.ok ) )
    | Time_binary_op (op, e1, e2) -> (
        match aux e1 with
        | Error x -> Error x
        | Ok s1 -> (
            match aux e2 with
            | Error e -> Error e
            | Ok s2 ->
              Ok
                ( match op with
                  | Union -> Time_slots.Union.union ~skip_check:true s1 s2
                  | Inter -> Time_slots.inter ~skip_check:true s1 s2 ) ) )
    | Time_round_robin_select l -> (
        l
        |> List.map aux
        |> Misc_utils.get_ok_error_list
        |> fun x ->
        match x with
        | Ok l ->
          l
          |> List.to_seq
          |> Time_slots.Round_robin.merge_multi_seq_round_robin_non_decreasing
          |> Result.ok
        | Error x -> Error x )
  in
  aux e
  |> Result.map
    (Time_slots.Normalize.normalize ~skip_filter_invalid:true
       ~skip_filter_empty:true ~skip_sort:true)

let next_match_time_slot ?(f_resolve_tpe_name = default_f_resolve_tpe_name)
    ?(f_resolve_tse_name = default_f_resolve_tse_name)
    (search_param : Search_param.t) (e : Time_expr_ast.t) :
  (Time_slot.t option, string) result =
  match
    matching_time_slots ~f_resolve_tse_name ~f_resolve_tpe_name search_param e
  with
  | Error msg -> Error msg
  | Ok seq -> (
      match seq () with Seq.Nil -> Ok None | Seq.Cons (x, _) -> Ok (Some x) )
