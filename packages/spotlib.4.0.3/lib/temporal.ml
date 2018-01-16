(** Date *)

(*
camlp4o -I ~/.opam/system/lib/pa_ounit/ pa_ounit_syntax.cma -pa-ounit-lib spotlib  temporal.ml
*)
  
open Base

module Year = struct
  (* leap year of Gregorian calendar, which is introduced
     by Pope Gregory XIII, after whom the calendar was named, 
     by a decree signed on 24 February 1582.

     The next day of 1582/10/4(Thu) in Julian calendar
     is 1582/10/15(Fri) in Gregorian calendar
  *)
  let is_leap n = 
    if n mod 400 = 0 then true
    else if n mod 100 = 0 then false
    else if n mod 4 = 0 then true
    else false

  let days_of_year n = if is_leap n then 366 else 365
end

module Weekday = struct

  let to_string = function
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> assert false

end

module Date = struct

  type t = Temporal_lexer.date = {
    year : int;  (* 1998 *)
    month : int; (* 1-12 *)
    day : int
  }

  type _t = t

  include Mtypes.Make_comparable(struct
    type t = _t
    let compare = compare
  end)

  let to_string t = Printf.sprintf "%04d-%02d-%02d" t.year t.month t.day
  
  exception Parse_error
  
  let of_string_exn s = 
    try 
      let lexbuf = Lexing.from_string s in
      let res = Temporal_lexer.parse_date lexbuf in
      (* There may be some garbage in lexbuf *)
      if String.length s <> Lexing.lexeme_end lexbuf then raise Parse_error;
      res
    with _ -> raise Parse_error
  
  let of_string s = Vresult.catch_exn & fun () -> of_string_exn s
  
  let random_with_invalid () =
    { year = Random.int 200 + 1900;
      month = Random.int 12 + 1;
      day = Random.int 31 + 1 }

  let days_of_month_in_non_leap_year = 
    (*  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12 *)
    [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

  let days_of_month_in_leap_year = 
    (*  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12 *)
    [| 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

  let days_of_month y = 
    if Year.is_leap y then days_of_month_in_leap_year 
    else days_of_month_in_non_leap_year

  let is_valid t = 
    if t.year < 1900 || t.year > 2100 then false
    else if t.month < 1 || t.month > 12 then false
    else
      let dom =(days_of_month t.year).(t.month-1) in
      if t.day < 1 || t.day > dom then false
      else true
 
  let %TEST 
      is_valid = is_valid (Vresult.from_Ok & of_string "2000-04-01")
  and is_valid = not & is_valid (Vresult.from_Ok & of_string "2000-04-31")
  and is_valid = is_valid (Vresult.from_Ok & of_string "2000-02-29")
  
  let random () =
    let year = Random.int 200 + 1900 in
    let month = Random.int 12 + 1 in
    let day = 
      let days = (days_of_month year).(month-1) in
      (* To check more bondary conditions, we have more 1 and 28,29,30,31 *)
      let d8 = Random.int 8 in
      match d8 with
      | 0 -> 1
      | 1 -> days
      | 2 -> days - 1
      | _ -> Random.int days + 1
    in
    { year; month; day }

      
  let date_1970_01_01 = { year = 1970; month = 1; day = 1 }
  let date_2038_01_01 = { year = 2038; month = 1; day = 1 }

  let rec random_2038 () =
    let d = random () in
    if d < date_1970_01_01 || d >= date_2038_01_01 then random_2038 ()
    else d

  let make_day_shifts days_of_month =
    let a = Array.make 12 0 in
    let rec make sum = function
      | 12 -> ()
      | n -> 
          a.(n) <- sum;
          make (sum + days_of_month.(n)) (n+1)
    in
    make 0 0;
    a

  let day_shifts_in_non_leap_year = make_day_shifts days_of_month_in_non_leap_year
  let day_shifts_in_leap_year = make_day_shifts days_of_month_in_leap_year

  (* day of the year 1--365 or 366. Named from tm_yday *)
  let yday t =
    let day_shifts = 
      if Year.is_leap t.year then day_shifts_in_leap_year
      else day_shifts_in_non_leap_year
    in
    day_shifts.(t.month-1) + t.day

  let diff t1 t2 =
    let diff' tgt tlt =
      let rec sum y = 
        if y = tgt.year then yday tgt
        else Year.days_of_year y + sum (y+1)
      in
      sum (tlt.year + 1) + Year.days_of_year tlt.year - yday tlt
    in
    match Pervasives.compare t1.year t2.year with
    | 0 -> yday t1 - yday t2
    | 1 -> diff' t1 t2
    | -1 -> - (diff' t2 t1)
    | _ -> assert false

  let wday t = 
    (* 1970/01/01 is Thurdsday 
       Sunday : 0
       Mon : 1
       Tue : 2
       Wed : 3
       Thu : 4
       Fri : 5
       Sat : 6
    *)
    (diff t date_1970_01_01 + 4) mod 7

  let %TEST to_string =
    "2000-04-01" = to_string { year = 2000; month = 4; day = 1 }

  and of_string =
    of_string "2000-04-01" = Ok { year = 2000; month = 4; day = 1 }

  and of_string =
    match of_string "2000-04-01-" with
    | Ok _ -> false
    | Error _ -> true

  and diff =
    diff (Vresult.from_Ok & of_string "2013-06-01")
      (Vresult.from_Ok & of_string "2012-01-31") 
    = 366 - 31 + 31 + 28 + 31 + 30 + 31 + 1

end

module Time = struct
  type t = Temporal_lexer.time = {
    hour : int; (* 00-24, 24 is only for 24:00:00 *)
    min  : int; (* 00-59 *)
    sec  : int; (* 00-60, 60 is for leap second *)
  }
    
  let to_string t = Printf.sprintf "%02d:%02d:%02d" t.hour t.min t.sec

  let is_valid t = 
    if t = { hour = 24; min = 0; sec = 0 } then true
    else 
      t.hour >= 0 && t.hour <= 23
      && t.min >= 0 && t.min <= 59
      && t.sec >= 0 && t.sec <= 60
  
  exception Parse_error
  
  let of_string_exn s = 
    try 
      let lexbuf = Lexing.from_string s in
      let res = Temporal_lexer.parse_time lexbuf in
      (* There may be some garbage in lexbuf *)
      if String.length s <> Lexing.lexeme_end lexbuf then raise Parse_error;
      if not & is_valid res then raise Parse_error;
      res
    with _ -> raise Parse_error
  
  let of_string s = Vresult.catch_exn & fun () -> of_string_exn s

  let random () = 
    if Random.int 30 = 0 then { hour = 24; min = 0; sec = 0 }
    else 
      { hour = Random.int 24;
        min = Random.int 60;
        sec = Random.int 61
      }

  let seconds_of_a_day = 86400. (* 24 * 60 * 60 *)

  let dsec t = 
    let open Overload.Float in
    float t.hour * 3600. + float t.min * 60. + float t.sec
end

module TZone = struct
  type t = [ `UTC | `Plus of int * int | `Minus of int * int ]

  let to_string = function
    | `UTC -> "Z"
    | `Plus (h, m) -> Printf.sprintf "+%02d:%02d" h m
    | `Minus (h, m) -> Printf.sprintf "-%02d:%02d" h m

  let is_valid = function
    | `UTC -> true
    | `Minus (0,0) -> false
    | `Plus (h, m) | `Minus (h, m) -> 0 <= h && h < 24 && 0 <= m && m < 60

  let in_secs = 
    let open Overload.Float in
    function
      | `UTC -> 0.0
      | `Plus (h, m) -> float h * 3600. + float m * 60.
      | `Minus (h, m) -> float h * (-3600.) + float m * (-60.)

  let rec random () = 
    let r = Random.int 25 in
    if r = 24 then `UTC
    else 
      let sign = Random.int 2 = 0 in
      let min = match Random.int 20 with
        | 0 -> 15
        | 1 -> 30
        | 2 -> 45
        | _ -> 0
      in
      if sign then `Plus (r, min)
      else if r = 0 && min = 0 then random ()  (* -00:00 is invalid *)
      else `Minus (r, min)
end

module Unix0 = Unix

module Unix = struct
  open Unix
  open Date

  let to_tm t = 
    assert (t.year >= 1970);
    assert (t.year < 2038);
    let tm = 
      { tm_sec   = 0;
        tm_min   = 0;
        tm_hour  = 0;
        tm_mday  = t.day;
        tm_mon   = t.month - 1;
        tm_year  = t.year - 1900;
        tm_wday  = 0; (* unspecified *)
        tm_yday  = 0; (* unspecified *)
        tm_isdst = false }
    in
    let f, tm' = Unix.mktime tm in
    let invalid = 
      not (tm'.tm_mday = tm.tm_mday
          && tm'.tm_mon  = tm.tm_mon
            && tm'.tm_year = tm.tm_year)
    in
    (f, tm', if invalid then `Invalid else `Ok) 
  
  let tm_of_date t = Option.catch_exn (fun () -> to_tm t) 

  let date_of_tm tm = 
    { year  = tm.tm_year + 1900;
      month = tm.tm_mon + 1;
      day   = tm.tm_mday }

  (* As UTC time *)
  let _string_of_tm tm = 
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec

  let %TEST date_unix_ =
    let open Unix in
    let t = Vresult.from_Ok & of_string "2000-04-01" in
    let _, tm, inv = Option.from_Some & tm_of_date t in
    assert ((tm.tm_year, tm.tm_mon, tm.tm_mday, inv) = (2000 - 1900, 4 - 1, 1, `Ok))

end

module Datetime = struct
  type t = {
    date : Date.t;
    time : Time.t;
    zone : TZone.t
  }

  let to_string t = Printf.sprintf "%sT%s%s"
    (Date.to_string t.date)
    (Time.to_string t.time)
    (TZone.to_string t.zone)

  let is_valid t = 
    Date.is_valid t.date 
    && Time.is_valid t.time
    && TZone.is_valid t.zone

  let random () = { date = Date.random ();
                    time = Time.random ();
                    zone = TZone.random () }

  let random_2038 () = { date = Date.random_2038 ();
                         time = Time.random ();
                         zone = TZone.random () }

  exception Parse_error

  let of_string_exn s = 
    try 
      let lexbuf = Lexing.from_string s in
      let date = Temporal_lexer.parse_date lexbuf in
      let () = Temporal_lexer.parse_t lexbuf in
      let time = Temporal_lexer.parse_time lexbuf in
      let zone = Temporal_lexer.parse_tzone lexbuf in
      (* There may be some garbage in lexbuf *)
      if String.length s <> Lexing.lexeme_end lexbuf then raise Parse_error;
      let res = { date; time; zone } in
      if not & is_valid res then raise Parse_error;
      res
    with _ -> raise Parse_error
  
  let of_string s = Vresult.catch_exn & fun () -> of_string_exn s

  let %TEST parse_ =
    for _i = 0 to 100000 do
      let t = random () in
      let s = to_string t in
      if of_string s <> Ok t then begin
        prerr_endline s;
        assert false
      end
    done

  let epoch t =
    (* Seconds from 1970-01-01T00:00:00Z
       No leap second need to be counted.
    *)
    let open Overload.Float in
    let diff_days = float & Date.diff t.date Date.date_1970_01_01 in
    diff_days * Time.seconds_of_a_day
    + Time.dsec t.time
    + TZone.in_secs t.zone

  let of_utc_tm tm = 
    { date = Unix.date_of_tm tm;
      time = { Time.hour = tm.Unix0.tm_hour;
               min  = tm.Unix0.tm_min;
               sec  = tm.Unix0.tm_sec };
      zone = `UTC } 

  let %TEST epoch_GMT_ =
    for _i = 0 to 100000 do
      let dt = 
        let rec random () = 
          let dt = random_2038 () in
          (* Avoid EOD, since its is computed back to D+1:00:00:00 *)
          if dt.time = { Time.hour = 24; min = 0; sec = 0 } then random ()
          (* Avoid leap second, since its is computed back to +1min *)
          else if dt.time.Time.sec = 60 then random ()
          else dt
        in
        random ()
      in
      let dt = { dt with zone = `UTC } in
      let secs = epoch dt in
      let tm = Unix0.gmtime secs in
      let dt' = { date = Unix.date_of_tm tm;
                  time = { Time.hour = tm.Unix0.tm_hour;
                           min  = tm.Unix0.tm_min;
                           sec  = tm.Unix0.tm_sec };
                  zone = `UTC } 
      in
      if dt <> dt' then begin
        Format.eprintf "%s <> %s@." (to_string dt) (to_string dt');
        assert false
      end
    done
end
