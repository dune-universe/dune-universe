(* This file is part of ocaml-crontab.
 *
 * Copyright (C) 2019 Yann Régis-Gianas
 *
 * ocaml-crontab is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Cron is a task scheduler standardized by POSIX. *)

(* Excerpt from POSIX
   [https://pubs.opengroup.org/onlinepubs/9699919799/utilities/crontab.html]

   Each of these patterns can be either an <asterisk> (meaning all
   valid values), an element, or a list of elements separated by
   <comma> characters. An element shall be either a number or two
   numbers separated by a <hyphen-minus> (meaning an inclusive
   range). The specification of days can be made by two fields (day of
   the month and day of the week). If month, day of month, and day of
   week are all <asterisk> characters, every day shall be matched. If
   either the month or day of month is specified as an element or
   list, but the day of week is an <asterisk>, the month and day of
   month fields shall specify the days that match. If both month and
   day of month are specified as an <asterisk>, but day of week is an
   element or list, then only the specified days of the week
   match. Finally, if either the month or day of month is specified as
   an element or list, and the day of week is also specified as an
   element or list, then any day matching either the month and day of
   month, or the day of week, shall be matched.  *)

type field =
  | All
  | List of element list
and element =
  | Single of int
  | Range of int * int

type entry = {
    minute            : field;
    hour              : field;
    day_of_the_month  : field;
    month_of_the_year : field;
    day_of_the_week   : field;
    command           : string;
}

type t = entry list

let entries t = t

let make t = t

exception InvalidElement of string * int

type validator = int -> unit

let single validate v =
  validate v;
  Single v

let range validate start stop =
  validate start;
  validate stop;
  Range (start, stop)

let valid_range what start stop k =
  if not ((k >= start) && (k <= stop)) then
    raise (InvalidElement (what, k))

let valid_minute            = valid_range "minute" 0 59
let valid_hour              = valid_range "hour" 0 23
let valid_day_of_the_month  = valid_range "day of the month" 1 31
let valid_month_of_the_year = valid_range "month of the year" 1 12
let valid_day_of_the_week   = valid_range "day of the week" 0 6

let make_entry
      ?(minute            = All)
      ?(hour              = All)
      ?(day_of_the_month  = All)
      ?(month_of_the_year = All)
      ?(day_of_the_week   = All)
      command =
  { minute; hour; day_of_the_week; day_of_the_month; month_of_the_year;
    command }

let minute e = e.minute
let hour e = e.hour
let day_of_the_month e = e.day_of_the_month
let month_of_the_year e = e.month_of_the_year
let day_of_the_week e = e.day_of_the_week

exception ParseError of int * string

let blanks = Str.regexp " +"

let error ?(lineno=0) msg =
  raise (ParseError (lineno, msg))

let invalid_field f =
  error (Printf.sprintf "`%s' is an invalid field." f)

let parse_element valid e =
  match Str.(split (regexp "-") e) with
  | [d] ->
     (try single valid (int_of_string d) with _ -> invalid_field e)
  | [start; stop] ->
     (try
       let start = int_of_string start
       and stop = int_of_string stop in
       range valid start stop
     with _ -> invalid_field e)
  | _ ->
     invalid_field e

let parse_field valid f =
  match Str.(split (regexp ",") f) with
  | [] -> assert false (* By split. *)
  | ["*"] -> All
  | elements -> List (List.map (parse_element valid) elements)

let entry_of_string s =
  match Str.(split blanks s) with
    | minute :: hour :: day_of_the_month :: month_of_the_year :: day_of_the_week
      :: command ->
       let command =
         String.concat " " command
       and minute =
         parse_field valid_minute minute
       and hour =
         parse_field valid_hour hour
       and day_of_the_month =
         parse_field valid_day_of_the_month day_of_the_month
       and month_of_the_year =
         parse_field valid_month_of_the_year month_of_the_year
       and day_of_the_week =
         parse_field valid_day_of_the_week day_of_the_week
       in
       make_entry
         ~minute ~hour ~day_of_the_month ~month_of_the_year ~day_of_the_week
         command
    | _ ->
       Printf.eprintf "%s\n" s;
       error "Invalid number of fields: there must be 6, separated by blanks."

let string_of_element = function
  | Single k -> string_of_int k
  | Range (start, stop) -> Printf.sprintf "%d-%d" start stop

let string_of_field = function
  | All -> "*"
  | List es -> String.concat "," (List.map string_of_element es)

let string_of_entry e =
  let field a e = string_of_field (a e) in
  String.concat " " [
      field minute e; field hour e; field day_of_the_month e;
      field month_of_the_year e; field day_of_the_week e;
      e.command
    ]

let crontab_of_string input =
  let maybe_entry_of_string lineno s =
    let blank_line s = (String.length s = 0) in
    let comment s = (s.[0] = '#') in
    if blank_line s || comment s then [] else [
        try entry_of_string s
        with ParseError (_, msg) -> raise (ParseError (lineno, msg))
      ]
  in
  let lines = Str.(split (regexp "\n") input) in
  List.(flatten (mapi maybe_entry_of_string lines))

let string_of_crontab t =
  String.concat "\n" (List.map string_of_entry t) ^ "\n"

exception CrontabError of Unix.process_status

let crontab_command user =
  Printf.sprintf "crontab %s"
  (match user with None -> "" | Some u -> "-u " ^ u)

let crontab_result = function
  | (lines, Unix.WEXITED 0) -> lines
  | (_, Unix.WEXITED 1) -> ""
  | (_, status) -> raise (CrontabError status)

let crontab ?user mode options =
  let command = Printf.sprintf "%s %s" (crontab_command user) options in
  let (cout, cin) as cs = Unix.open_process command in
  begin match mode with
    | `Read ->
       X.read_all (Buffer.create 13) cout
    | `Write lines ->
       output_string cin lines;
       ""
  end
  |> fun out -> (out, Unix.close_process cs)
  |> crontab_result

let crontab_install ?user crontable =
  crontab ?user (`Write (string_of_crontab crontable)) "-" |> ignore

let crontab_get ?user () =
  crontab ?user `Read "-l" |> crontab_of_string

let crontab_remove ?user () =
  crontab ?user `Read "-r" |> ignore

let crontab_insert_entry ?user entry =
  let table = crontab_get ?user () in
  if not (List.mem entry table) then
    crontab_install ?user (entry :: table)

let crontab_remove_entry ?user entry =
  let table = crontab_get ?user () in
  if not (List.mem entry table) then
    raise Not_found
  else
    let table, _ = List.partition (( <> ) entry) table in
    crontab_install ?user table

let version = Version.current
