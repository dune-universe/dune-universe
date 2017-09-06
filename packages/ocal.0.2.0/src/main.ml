(*
 * Copyright (c) 2016-2017 Richard Mortier <mort@cantab.net>
 *
 * Licensed under the ISC Licence; see LICENSE.md in the root of this
 * distribution or the full text at https://opensource.org/licenses/isc-license
 *
 *)

open Cmdliner
open CalendarLib
open Astring

let () = Time_Zone.(change Local)

let today =
  let date =
    let parse date =
      try
        `Ok (Printer.Date.from_fstring "%d%b%Y" date)
      with
      | Invalid_argument _ ->
        `Error ("invalid date string: " ^ date)
    in
    parse, fun ppf p -> Format.fprintf ppf "%s" (Printer.Date.to_string p)
  in
  let doc = "Set today's date." in
  Arg.(value & opt date (Date.today ())
       & info ["t"; "today"] ~docv:"ddMmmyyyy" ~doc)

let ncols =
  let doc = "Format across $(docv) columns." in
  Arg.(value & opt int 3 & info ["c"; "columns"] ~docv:"N" ~doc)

let sep =
  let doc = "Format using $(docv) as month separator." in
  Arg.(value & opt string "    " & info ["s"; "separator"] ~docv:"sep" ~doc)

let weeks_of_year =
  let doc = "Display weeks of year." in
  Arg.(value & flag & info ["w"; "weeks"] ~doc)

let first_dow =
  let aux =
    let parse day =
      try
        `Ok (day |> String.Ascii.capitalize |> Day.of_string)
      with
      | Invalid_argument s -> `Error s
    in
    parse, fun ppf p -> Format.fprintf ppf "%s" (Printer.short_name_of_day p)
  in
  let doc = "Format with $(docv) as first day-of-week." in
  Arg.(value & opt aux (Date.Mon) & info ["f"; "first_dow"] ~docv:"ddd" ~doc)

let plain =
  let doc = "Turn off highlighting." in
  Arg.(value & flag & info ["p"; "plain"] ~doc)

let range =
  let doc = "$(docv) are specified as:\n\
            \  $(i,mmm) (month $(i,mmm));\n\
            \  $(i,yyyy) (year $(i,yyyy));\n\
            \  $(i,mmmyyyy) (month $(i,mmm), year $(i,yyyy));\n\
            \  $(i,d1-d2) (all months in the range $(i,d1-d2)).\
            "
  in
  let thismonth = Some (Printer.Date.sprint "%b%Y" (Date.today ())) in
  Arg.(required & pos 0 (some string) thismonth & info [] ~docv:"DATES" ~doc)

let cmd =
  let doc = "pretty print calendar months" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) -- pretty prints specified monthly calendars.";

    `S "SEE ALSO";
    `P "cal(1), ncal(1), calendar(3), strftime(3)";

    `S "HISTORY";
    `P "\
An alternative to\
\ https://github.com/mor1/python-scripts/blob/master/cal.py because I got\
\ tired of the startup time. The Python version was written because I got\
\ tired of the CLI.";

    `S "AUTHORS";
    `P "Richard Mortier";

    `S "BUGS";
    `P "Report bugs at https://github.com/mor1/ocal/issues";
  ]
  in
  Term.(const Ocal.cal
        $ plain $ weeks_of_year $ today $ ncols $ sep $ first_dow $ range),
  Term.info Config.command ~version:Config.version ~doc ~man

(* go! *)

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
