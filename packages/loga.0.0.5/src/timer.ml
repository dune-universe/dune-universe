(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  year : int;
  (* 1-origin *)
  month : int;
  mday : int;
  hours : int;
  minutes : int;
  seconds : int;
  tz_hours : int;
  tz_minutes : int;
}

type gen_t = unit -> t

let current_gen () : t =
  let (tz, _) = Unix.mktime (Unix.gmtime 0.0) in
  let {
    Unix.tm_year = year;
    Unix.tm_mon = month;
    Unix.tm_mday = mday;
    Unix.tm_hour = hours;
    Unix.tm_min = minutes;
    Unix.tm_sec = seconds;
    _;
  } =
    Unix.localtime (Unix.gettimeofday ())
  in
  let year = year + 1900 in
  let month = month + 1 in
  let tz_hours = -int_of_float tz / 3600 in
  let tz_minutes = -int_of_float tz / 60 mod 60 in
  { year; month; mday; hours; minutes; seconds; tz_hours; tz_minutes }

let zero_gen () : t =
  let year = 1900 in
  let month = 1 in
  let mday = 1 in
  let hours = 0 in
  let minutes = 0 in
  let seconds = 0 in
  let tz_hours = 0 in
  let tz_minutes = 0 in
  { year; month; mday; hours; minutes; seconds; tz_hours; tz_minutes }
