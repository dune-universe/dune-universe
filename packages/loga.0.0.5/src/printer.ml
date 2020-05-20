(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type 'a cont = Format.formatter -> 'a

module Builtin = struct
  let endline_printer k _ctx formatter _fmt = Format.kfprintf k formatter "\n%!"

  let format_printer k _ctx formatter fmt : 'a = Format.kfprintf k formatter fmt

  let position_printer k ctx formatter _fmt : 'a =
    let module_name = Context.module_name ctx in
    let line = Context.line ctx in
    Format.kfprintf k formatter "(%s:%d) " module_name line

  let severity_printer k ctx formatter _fmt : 'a =
    let severity = Context.severity ctx in
    Format.kfprintf k formatter "[%9s] " (Severity.string_of severity)

  let datetime_printer gen k _ctx formatter _fmt : 'a =
    let time = gen () in
    Format.kfprintf k formatter "%04d-%02d-%02dT%02d:%02d:%02d%0+3d:%02d "
      time.Timer.year time.Timer.month time.Timer.mday time.Timer.hours
      time.Timer.minutes time.Timer.seconds time.Timer.tz_hours
      time.Timer.tz_minutes

  let ( >> ) first_printer second_printer =
    let printer k ctx formatter fmt : 'a =
      let k0 formatter = second_printer k ctx formatter fmt in
      first_printer k0 ctx formatter fmt
    in
    printer
end
