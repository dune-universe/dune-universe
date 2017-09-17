(** This module uses regular expressions to scan calendar dates. A more
    conventionmal choice would have been to use a regular expression
    library like Re.

    Copyright (c) 2015, Christian Lindig <lindig@gmail.com>
    All rights reserved.
*)

{
    exception Error of string
    let error fmt = Printf.kprintf (fun msg -> raise (Error msg)) fmt

    let get     = Lexing.lexeme
    let int_of str =
        try int_of_string str
        with Failure _ -> error "not an integer: %s" str

    (** current date *)
    let now =
        let tm = Unix.time () |> Unix.localtime in
            (tm.Unix.tm_year+1900, tm.Unix.tm_mon+1, tm.Unix.tm_mday)
}

let digit   = ['0'-'9']
let day2    = ['0'-'3']  digit
let month2  = ['0'-'1']  digit
let day     = ['0'-'3']? digit
let month   = ['0'-'1']? digit

let year    = digit digit digit digit

rule date = parse

    (year   as yy) '-'
    (month2 as mm) '-'
    (day2   as dd) eof      { (int_of yy, int_of mm, int_of dd) }

|   (day    as dd) '.'
    (month  as mm) '.'
    (year   as yy) eof      { (int_of yy, int_of mm, int_of dd) }

|   "now"   eof             { now }
|   "today" eof             { now }
|   "epoch" eof             { (1, 1, 1) }

{
let from_string str =
    try
        Lexing.from_string str |> date
    with
        Failure _ -> error "not a yyyy-mm-dd date: %s" str
}
