(*
 * Copyright (c) 2016 Richard Mortier <mort@cantab.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cmdliner
open Astring
open CalendarLib

module String = struct
  include String
  let space = fun _ -> ' '

  let lpad, rpad =
    let pad len = if len < 0 then "" else v ~len space in
    (fun n s -> (pad n) ^ s),
    (fun n s -> s ^ (pad n))
end

module List = struct
  include List

  let chunk n list =
    list
    |> fold_left (fun (i,acc) e ->
        match (i, n) with
        | (i, n) when i = 0 || n = 1 -> 1, [e] :: acc
        | (i, n) when i < n ->
          let n' = (i+1) mod n in
          let acc' = ((e :: hd acc) :: (tl acc)) in
          n', acc'
        | (_i, _n) (* i >= n *) -> failwith "never reached"
      ) (0, [])
    |> snd
    |> List.(rev_map rev)

  let seq a b =
    let rec aux a b =
      if a > b then [] else a :: aux (a+1) b
    in
    if a < b then aux a b else List.rev (aux b a)

  let split n list =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l ->
        if i = 0 then List.rev acc, l else aux (i-1) (h :: acc) t
    in
    aux n [] list

end

module Day : sig
  val of_string: string -> Date.day
  val to_string: Date.day -> string
  val find: Date.day -> int
  val week: Date.day -> Date.day array
end = struct
  let of_string = function
    | "Mon" -> Date.Mon
    | "Tue" -> Date.Tue
    | "Wed" -> Date.Wed
    | "Thu" -> Date.Thu
    | "Fri" -> Date.Fri
    | "Sat" -> Date.Sat
    | "Sun" -> Date.Sun
    | _ as s -> invalid_arg ("invalid day: " ^ s)

  let to_string d = d |> Printer.short_name_of_day |> String.with_range ~len:2

  let _days = Date.([| Mon; Tue; Wed; Thu; Fri; Sat; Sun;
                       Mon; Tue; Wed; Thu; Fri; Sat; Sun
                    |])
  let find x =
    let rec aux a x n = if a.(n) = x then n else aux a x (n+1) in
    aux _days x 0

  let week firstday =
    let i = find firstday in
    Array.sub _days i 7
end

let months range =
  let parse ?(rh=false) s =
    let s = String.Ascii.capitalize s in
    Printer.Date.(
      (* monthyear *)
      try from_fstring "%d%b%Y" ("01"^s)
      with Invalid_argument _ -> begin
          (* year *)
          try from_fstring "%d%b%Y" ("01"^(if rh then "Dec" else "Jan")^s)
          with Invalid_argument _ -> begin
              (* month *)
              let thisyear = string_of_int Date.(year (today ())) in
              from_fstring "%d%b%Y" ("01"^s^thisyear)
            end
        end
    )
  in

  let expand st nd =
    let st = parse st in
    let nd = parse ~rh:true nd in

    let rec aux d nd acc =
      match Date.compare d nd with
      | n  when n > 0 (* >  *) -> List.rev acc
      | _n            (* <= *) ->
        let d' = (Date.next d `Month) in
        aux d' nd (d::acc)
    in
    aux st nd []
  in

  match String.cuts ~sep:"-" range with
  | [st; nd] -> expand st nd
  | [st]     -> expand st st
  | _        -> invalid_arg ("invalid date range: " ^ range)

module F = struct
  open Printf
  let bold fmt      = sprintf ("\x1b[0;1m"^^fmt^^"\x1b[0m")
  let hilight fmt   = sprintf ("\x1b[0;1;7m"^^fmt^^"\x1b[0m")
  let underline fmt = sprintf ("\x1b[0;4m"^^fmt^^"\x1b[0m")

  let center ~w s =
    let pad = w - String.length s in
    s |> String.lpad (pad/2) |> String.rpad ((pad+1)/2)
end

let cal plain today ncols sep firstday range =
  months range
  |> List.map (fun date ->
      (* generate list of lines per month *)
      let open Printf in
      let month, year = Date.(month date, year date) in

      (* header: "Month Year", centred, bold *)
      let monthyear = sprintf "%s %d" (Printer.name_of_month month) year
                      |> F.center ~w:20
                      |> (fun s -> if not plain then F.bold "%s" s else s)
      in

      (* weekdays: "Mo Tu ... Su", underlined *)
      let weekdays = Day.week firstday
                     |> Array.map Day.to_string
                     |> Array.to_list
                     |> String.concat ~sep:" "
                     |> (fun s -> if not plain then F.underline "%s" s else s)
      in

      (* days of the week: first row lpadded, last row rpadded *)
      let days =
        List.seq 1 (Date.days_in_month date) (* generate list *)
        |> (fun days ->
            (* divide list: partial first and last weeks, plus full weeks *)
            let start_full_week =
              Date.nth_weekday_of_month year month firstday 1
              |> Date.day_of_month
            in
            let h, t = List.split (start_full_week - 1) days in
            h :: List.chunk 7 t
            |> List.filter (function [] -> false | _ -> true)
          )
        |> (let opt_hilight d =
              (* highlight today's date *)
              if (not plain
                  && Date.year today = year
                  && Date.month today = month
                  && Date.day_of_month today = d
                 )
              then
                F.hilight "%2d" d
              else
                sprintf "%2d" d
            in
            (* stringify each week, padding as needed  *)
            List.mapi (fun i line ->
                let pad = (20 - (List.length line * 3) + 1) mod 20 in
                line
                |> List.map (fun d -> sprintf "%s" (opt_hilight d))
                |> String.concat ~sep:" "
                |> (fun line -> String.(if i = 0 then lpad else rpad) pad line)
              )
           )
      in
      monthyear :: weekdays :: days
    )
  |> List.chunk ncols
  |> (fun rows ->
      List.iteri (fun _r row ->
          let nlines =
            List.(map length row |> fold_left (fun acc e -> max acc e) 0)
          in
          for i = 0 to nlines-1 do
            for j = 0 to ncols-1 do
              let month =
                try Some (List.nth row j) with Failure "nth" -> None
              in
              let line =
                match month with
                | Some month -> (
                    try Some (List.nth month i) with Failure "nth" -> None
                  )
                | None -> None
              in
              let sep = if j = 0 then "" else sep in
              match month, line with
              | None, _ -> ()
              | Some _month, None ->
                Printf.printf "%s%s" sep (String.(v ~len:20 space));
              | Some _month, Some line ->
                Printf.printf "%s%s" sep line
            done;
            Printf.printf "\n"
          done;
        ) rows;
        Printf.printf "%!"
    )

(* command line parsing *)

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

let firstday =
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
  Arg.(value & opt aux (Date.Mon) & info ["f"; "firstday"] ~docv:"ddd" ~doc)

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
    `P
      "An alternative to\
      \ https://github.com/mor1/python-scripts/blob/master/cal.py because I got\
      \ tired of the startup time. The Python version was written because I got\
      \ tired of the CLI.";

    `S "AUTHORS";
    `P "Richard Mortier <mort@cantab.net>.";

    `S "BUGS";
    `P "Report bugs at https://github.com/mor1/ocal/issues.";
  ]
  in
  Term.(const cal $ plain $ today $ ncols $ sep $ firstday $ range),
  Term.info Config.command ~version:Config.version ~doc ~man

(* go! *)

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
