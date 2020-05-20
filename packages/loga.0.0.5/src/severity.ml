(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
  | Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Info
  | Debug

let to_int s =
  match s with
  | Emergency -> 0
  | Alert -> 1
  | Critical -> 2
  | Error -> 3
  | Warning -> 4
  | Notice -> 5
  | Info -> 6
  | Debug -> 7

let string_of k =
  match k with
  | Emergency -> "EMERGENCY"
  | Alert -> "ALERT"
  | Critical -> "CRITICAL"
  | Error -> "ERROR"
  | Warning -> "WARNING"
  | Notice -> "NOTICE"
  | Info -> "INFO"
  | Debug -> "DEBUG"

let more_severe_than_or_equal a b = to_int a <= to_int b
