(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3 - commit 0806f5ae3774c30cc8ee18aaabe9d106d7816457
  ---------------------------------------------------------------------------*)

(* an ocaml version of Openssl's conf library

   Version {e 0.8.3} — {{: https://github.com/awuersch/sslconf}homepage} *)

(* Intro

   Welcome to the implementation of Sslconf!

   0. Origin

   Sslconf recodes Openssl's NCONF "new config" parser in OCaml.
   The recoding should be conforming but simpler, cleaner, and unit tested.

   Openssl code is at https://github.com/openssl/openssl/blob/master/ .
   The NCONF parser header is in "include/openssl" at "conf.h".
   NCONF parser code is in "crypto/conf" at

   - "conf_def.c"
   - "conf_api.c", and
   - "conf_lib.c".

   1. Line handling

   This describes [continued_line], [getline], [Buf.grow], and [Bio.gets].

   NCONF opens, reads, and parses a text file.

   Lines in the text file are terminated by newlines, either

   - Unix-style line feeds (LF), or
   - Windows style carrage return line feeds (CRLF).

   A text file read scans for a newline.
   The scan is limited up to a maximum block size (512 bytes).
   Maximum block size is 512 bytes.

   Reads go in a buffer.
   The maximum size of the buffer is bounded by a bit size length.
   Default bit size length is 31 bits.
   Unit tests can reduce buffer bit size length in order to trigger errors.

   The buffer is resized:

   - if a line is longer than the maximum block size; or
   - if the character immediately before the newline is a continuation
     character ['\\'], and the continuation character is not immediately
     preceded by another continuation character ['\\'].

   A line, for NCONF, is the sequence of lines, each separated by a
   continuation character.

   The newline at the end of a line is stripped out.
   Each newline preceded by a continuation characters is also stripped out,
   and its continuation character is stripped out too.

   If a line ends at the end of a file and no newline is present, then
   nothing is stripped out, unless a continuation character is present,
   in which case it alone is stripped out.

   For error reporting, this implementation saves the offsets of
   continued lines in the structure of a line.  Using these offsets,
   when an error happens, error reporting reports:

   - the line number of the error in the text file, and
   - the position of the error relative to the line in the text file.

   The structure of a line is defined in [continued_line].

   Two error types are defined relative to line handling:

   - "Open": if the text file cannot be opened.
   - "Extend": if the buffer cannot be resized.

   2. Line parsing.

   Parsing [Sslconf.parse_line] returns two hash tables:

   - a hash table mapping section names to stacks of name-value pairs; and
   - a hash table mapping "<section>::<name>" keys to values.

   If parsing fails, an error is returned.
   Returns and errors use OCaml's result structure.

   2.1. Lines to the Parser

   The parser recognizes three kinds of lines:

   - an empty, blank or commented out line.
   - a line declaring a new section.
   - a line assigning a value to a name.

   See interface documentation for more detail on line types.
   Comments here are relevant to the implementation.

   2.1.1. Section lines.

   [parse_line] recognizes the beginning of a section line.
   It delegates section parsing to

   - a scanner loop using [span_with_escapes], and
   - a value evaluator [parse_value].

   Then it recognizes end of section.

   2.1.2. Name/Value Assignments.

   At the most general level, an NCONF name-value assignment
   is made up of:

   - a name, followed by
   - an equal sign (['=']), followed by
   - a value.

   If the equal sign is not present, and the line is not one
   of the other two types, the parser will quit in error, and
   will report that no equal sign was found.

   [parse_line] does this general work.

   - It delegates name recognition to [parse_name].
   - It recognizes the equal sign.
   - It delegates value reading and evaluation to [parse_value].
   - Then it returns a result.

   2.1.2.1. Names.

   A qualified name is made up of:

   - a section, followed by
   - two colon characters, i.e., ["::"], followed by
   - a name.

   If only one colon character is seen, the parser will quit
   with an error, that two colon characters were not seen.

   A name, and the section of a qualified name, share the same
   syntax rules. These rules are the same as the section in a
   section line, with two changes:

   - unescaped white space in a name is not allowed.
   - the resulting name is NOT further processed, so escape
     characters remain in the name.

   White space before and after a name, up to the equal sign,
   is ignored (dropped).

   2.1.2.2. Values.

   The parser quits with an error if an empty value is seen.
   This can occur if only white space follows the equal sign,
   or if only white space after the equal sign is ended by a
   hash character (comment to end of line).

   A small set of characters introduce or delimit special processing.

   - a dollar sign (['$'])
     with optional wrap characters (["()"] or ["{}"])
     introduces name substitution.
   - quotes (['\''] or ['"']) suppress processing in wrapped subparts.
   - an escape character (['\\']) admits the next character unconditionally,
     and suppresses processing from it.

   In the case of name substitution, a special subclass is
   names with section "ENV".  These substitute from the process
   environment.

   The parser strips out special characters from value results,
   unless their special meaning was cancelled.

   [parse_value] recognizes values in name-value assignments.
   It recognizes special characters and delegates their processing.

   - name substitution is handled by [subst].
   - Unix and Cygwin quote processing is handled by [parse_quote].
   - Win32 double quote processing is handled by [parse_dquote].
   - escape char processing is handled directly by [parse_value].

   Section line sections apply [parse_value] escape char processing.

   2.1.2.2.1. Name Substitution.

   [subst] handles name substitution.

   Name substitution replaces names with values. Names used must have
   previously assigned values, or they must have section ["ENV"], and
   their name must exist in the process environment.

   If a start wrap character (['('] or ['{']) immediately follows the
   dollar sign, then the name to substitute must be immediately followed
   by the end wrap character ([')'] or ['}'], or the parser will fail
   and report an error that no end wrap character was seen.

   Names which fail to have a previously assigned value, or which are
   qualified names with section ["ENV"], whose name is not in the process
   environment, will cause the parser to fail with an error that a name
   could not be resolved.

   Parsing of a name used in name substitution is like that of a name
   used in name-value assignments, but with one further restriction:

   - punctuation is not allowed.

   A name used in name substitution may have only

   - alphanumeric characters,
   - underscores, or
   - two-character escaped sequences.

   2.1.2.2.2. Quote Wrapping in Unix and Cygwin.

   [parse_quote] handles quote wrapped strings in Unix and Cygwin.

   A single quote string is ended when a single quote is found, unless the
   single quote is preceded by an escape character (['\\']).

   Escape chars allow the next char to always be in the substring.

   The parser strips out escape characters from the substring it adds to
   the value, unless the escape character is itself escaped.

   The parser strips out wrapping single quotes.

   2.1.2.2.3. Quote Wrapping in Win32 (double-quotes only).

   [parse_dquote] handles double quote wrapped strings in Windows.

   A double quote string is ended when a double quote is found, unless the
   double quote is immediately preceded by a double quote.

   If two double quotes appear in the string, the parser replaces them with
   one double quote in the substring it adds to the value.

   Escape characters have no interpretation in double-quote strings.  They
   appear as themselves, unmodified.

   The parser strips out wrapping double quotes.

   3. Implementation Notes.

   3.1. The Astring Package.

   This implementation primarily uses Daniel Buenzli's Astring package.
   Astring defines a substring structure over base strings.  A substring
   structure is an interval range (a start position and an end position)
   in a base string. Substring representation minimizes string copies
   and replaces them, wherever possible, with substring creations and
   modifications.

   Strings can be converted to substrings, and substrings back to strings. 
   Scanners can traverse substrings and apply character predicates, returning
   matched and unmatched ("remainder") sections. Head characters and tail
   substrings can be taken from substrings. Substring lists can be concatenated
   to create new substrings. The interval range of a substring can be altered
   ("extended") without creating a new base string.

   3.2. OUnit2, Bisect, and Sexplib Packages.

   For unit testing, we use OUnit2, Bisect, and Sexplib.

   Bisect assures us that unit tests have test coverage.

   Sexplib allows canonical and comparable strings to be generated
   from results. We may change the choice of Sexplib here, to use
   an XML generator and comparator. If so, it should be easier to
   convert results to web representations for display.

   A testcase array ("test/testcase.ml") drives unit testing.
   It defines expected results as an option structure, so None
   is an option that returns success.

   Unit testing rewrites the testcase array to a side output file
   ("test/data/cases.out"). This file has all cases and their results,
   by writing out expected results as a Some value.  If all tests
   succeed, one can replace the testcase array with the side output
   file.
*)

open Rresult

module Buf = Buf

module Bio = Bio

type section_hashtbl = (string, (string * string) Stack.t) Hashtbl.t

type section_name_hashtbl = (string * string, string) Hashtbl.t

type trec = { ht_sect : section_hashtbl; ht : section_name_hashtbl; }

type t = trec
(* sslconf type *)

let create () =
  let ht_sect = Hashtbl.create ~random:false 10 in
  let ht = Hashtbl.create ~random:false 10 in
  let stack = Stack.create () in
  let section = "default" in
  Hashtbl.add ht_sect section stack;
  { ht_sect; ht }

(* call here only with a section+name hash table ! *)
let find_option_section_name ht section_name = 
  try Some (Hashtbl.find ht section_name) with Not_found -> None

(* get value for name in default section *)
let get_default_value ht name =
  find_option_section_name ht ("default", name)

(* get value for name in process environment *)
let get_env_value name =
  try Some (Sys.getenv name) with Not_found -> None

(* call here only with an optional section+name hash table ! *)
let get_value ?ht ?section name =
  match ht with
  | Some ht -> (
      match section with
      | Some section -> (
          match find_option_section_name ht (section, name) with
          | Some value -> Some value
          | None ->
            if section = "ENV"
            then
              match get_env_value name with
              | Some value -> Some value
              | None -> get_default_value ht name
            else
              get_default_value ht name
        )
      | None -> get_default_value ht name
    )
  | None -> get_env_value name

(* call here only with an optional section+name hash table ! *)
let conf_get_value ?conf ?section name =
  match conf with
  | Some conf -> get_value ~ht:conf.ht ?section name
  | None -> get_value ?section name

(* call here only with a section hash table ! *)
let get_section ht section = 
  try Some (Hashtbl.find ht section) with Not_found -> None

let conf_get_section conf section =
  get_section conf.ht_sect section

(* maximum string length returned by {!Buf.gets}. (Openssl CONFBUFSIZE) *)
let max_length = 510 (* CONFBUFSIZE = 512 for C strings ending in '\0' *)

(* i/o buffer size in bits - we allow lower than default for test support *)
let default_nbits = 31
and nbits = ref None

(* a line inside parse_line can contain multiple external lines. *)
type continued_line = string * int list

(* returned by [getline] *)
type line =
    Line of continued_line
  | EOF

(** [getline ic] converts reads into a line for the parser.
  A result structure is returned.  If ok, an {!line} is returned.
  If error, the following is returned:
   * a message string;
   * the [nbits] of the buffer;
   * the length requested for a next read; and
   * the buffer memory max size limit.

  If an error is returned, it should be the error result of a call
  to extend the buffer. The memory allocation request would be 4/3
  times the requested length.

  [getline] handles:
   * I/O getting,
   * buffering,
   * buffer extending, and
   * continuation character handling.

  Returns a continued line and a list of offsets.
  The list shows where (at which offsets) next lines were appended.

  Logic here follows openssl code, except see note in code.

  Detail:

  A line for the parser can hold multiple lines from the channel.

  One hopes a piece (a block or a subblock) is a line from a channel.
  Almost always, it will be -- but it could be less.
  "Pieces" are assembled by char reads, until
   * a newline is found;
   * EOF is detected; or
   * a max length is read.

  Characters read are put in a buffer.
  The buffer is pre-extended so it can always hold a next piece.
  Buffer memory allocation is bounded by a max {!nbits} bit length.

  A piece may end with a line continuation.  A {e line continuation}
  is an escape character at the end of a piece (or just before,
  if the end is a newline). The special meaning of this character is
  nullified if an escape char is found just before it.

  {v This is \
a line with \
two line continuations. v}

   {v This is \
a line with two line continuations,\
ending with an escape char. \\ v}

  Ends of pieces, i.e., line continuations and newlines, are stripped.

  {v This was a line with two line continuations. v}

  {v This was a line with two line continuations, ending with an escape char. \\ v}

  The buffer concatenates
   * a first piece read;}
   * further pieces, if line continuations were found; and}
   * further pieces, if a piece is max length and has no newline.}}
  Relative offsets of pieces which follow any newlines read
  are saved as a list of integers.  This list is used by the parser for
  line and column number error reporting.
 *)
let getline ic =

  (* [loop ic buf starts start]
     - ic : input channel
     - buf : line buffer ([!nbits] max length)
     - starts : offsets in line buffer of lines from file
     - start : offset in line buffer of next line

     return continued_line * start_offsets
   *)
  let rec loop ic buf starts start =
    match Buf.extend buf (start + max_length) with
    | Ok buf ->
      let data = Buf.data buf in
      let n = Bio.gets ic data start max_length in
      (*
         - debug output
      if n = max_length && start = 0
      then
        let last_ch = Bytes.get data (start + n - 1) in
        if last_ch != '\n'
        then
          let next_last_ch = Bytes.get data (start + n - 2) in
          Printf.printf
            "max_length = %d; last_ch = '%c'; next_last_ch = '%c'\n"
            max_length last_ch next_last_ch
        else ()
      else ();
       *)
      if start = 0 && n = 0
      then Ok EOF (* empty buffer and no newline seen === end of file *)
      else
        (* Strip continuation character and newline (LF or CRLF).
           Here, we are simpler than NCONF code.
           In NCONF code, newline stripping is recursive.
           I guess a line with \\\n\\\n\\\n could end up as one line.
           That can't happen for us.
           Our "get" ([Bio.gets]) reads zero or one newline.
        *)
        let line_length =
          if n = 0
          then n
          else if Bytes.get data (start + n - 1) = '\n'
          then
            let strip_n =
              if n > 1 && Bytes.get data (start + n - 2) = '\r'
              then 2
              else 1
            in n - strip_n
          else n
        in
        (* end of line was seen *)
        let eol_seen = line_length != n in
        (* a long line is a full read with no newline at its end *)
        let long_line = n = max_length && not eol_seen in
        (* seek start of next line *)
        let start = start + line_length in
          (*
             Q: is an unescaped escape char at end of line?
             Then it's a continuation character.
             But what if the line is long, so we're not at end of line?
             NCONF code still detects a continuation character.
             In context of NCONF parsing, this isn't a harmful thing.
             So I'll allow it ... but it seems like a bug ...
           *)
        let line_cont_seen =
          start != 0
          && Bytes.get data (start - 1) = '\\'
          && (start = 1 || not (Bytes.get data (start - 2) = '\\')) in
        let start = if line_cont_seen then start - 1 else start in
        (* if line continuation was seen, drop continuation char *)
        if long_line || line_cont_seen
        then
          let starts = if eol_seen then start :: starts else starts in
          (* if saw end of line, add start offset to line offsets *)
          loop ic buf starts start
        else
          let line = Bytes.sub_string data 0 start
          and starts = List.rev starts in
          Ok (Line (line, starts))
    | Error tuple -> Error tuple
  in
  let nbits = match !nbits with None -> default_nbits | Some i -> i in
  let buf = Buf.empty nbits in
  loop ic buf [] 0

open Astring

(*
   use cases: parse_dquote (2 times), parse_quote (3 times).
   side effect: a new base string is created.
 *)
let finish_parse_loop acc data rem =
  let open String.Sub in
  let acc = List.rev (data :: acc) in
  concat
    acc (* new base string *)
, rem

(* used in Win32 *)
(* allow everything, double quotes escape double quotes, strip wrappers. *)
let parse_dquote s = (*BISECT-IGNORE-BEGIN*)
  let rec loop acc s =
    let open String.Sub in
    let is_data = function '"' -> false | _ -> true in
    (* all but quotes *)
    let data, rem = span ~sat:is_data s in
    match head rem with
    | Some '"' -> (
        let rem = tail rem in
        match head rem with
        | Some '"' -> (* double quotes escape double quotes *)
          let data = extend ~max:1 data in
          loop (data :: acc) (tail rem)
        | Some _ | None ->
          finish_parse_loop acc data rem
      )
    | Some _ -> failwith "parse_dquote: pattern-match failure" (*BISECT-IGNORE*)
    | None -> finish_parse_loop acc data rem (* accept if eol *)
  in
  loop [] s (*BISECT-IGNORE-END*)

(* used in Unix and Cygwin *)
(* allow everything, use '\\' to escape any next character. *)
let parse_quote q s =
  let is_data c = c != q && c != '\\' in
  let rec loop acc s =
    let open String.Sub in
    (* all but quotes and escape chars *)
    let data, rem = span ~sat:is_data s in
    match head rem with
    | Some c when c = q -> finish_parse_loop acc data (tail rem)
    | Some '\\' -> (
        let rem = tail rem in (* skip past escape char *)
        match head rem with
        | Some c -> loop ((of_char c) :: data :: acc) (tail rem)
        | None -> finish_parse_loop acc data rem (*BISECT-IGNORE*)
        (* None case excluded by call context
           -- always a char next, or char is continuation char at eol.
        *)
      )
    | Some _ -> failwith "parse_quote: pattern-match failure" (*BISECT-IGNORE*)
    | None -> finish_parse_loop acc data rem (* accept if eol *)
  in
  loop [] s

(*
   use cases: parse_name (2 times), parse_line (1 time).
   span, include escaped characters, don't exclude escape chars.
 *)
let span_with_escapes sat rem =
  let open String.Sub in
  let rec loop acc rem =
    let data, rem = span ~sat rem in
    match head rem with
    | Some '\\' -> (
        let rem = tail rem in
        match head rem with
        | Some _ ->
          let data = extend ~max:2 data in
          loop (data :: acc) (tail rem)
        | None ->                             (*BISECT-IGNORE*)
          let data = extend ~max:1 data in   (*BISECT-IGNORE*)
          finish_parse_loop acc data rem     (*BISECT-IGNORE*)
          (* case excluded by call context
             -- always a char next, or char is continuation char at eol.
          *)
      )
    | Some _ | None ->
      finish_parse_loop acc data rem
  in
  loop [] rem

(*
   use cases: subst (1 time, is_an), parse_line (1 time, is_anp).
   parse a qualified or unqualified name, with different predicates
 *)
let parse_name sat rem =
  let open String.Sub in
  let part, rem = span_with_escapes sat rem in
  match head rem with
  | Some ':' -> (
      let rem = tail rem in
      match head rem with
      | Some ':' ->
        let section = to_string part in
        let name, rem = span_with_escapes sat (tail rem) in
        Ok (Some section, name, rem)
      | Some _ | None ->
        Error (start_pos rem, "parse_name", "two-colon token not found")
    )
  | Some _ | None -> Ok (None, part, rem)

(* substitute [Sslconf.get_value] result *)
let subst ht section rem =
  let open String.Sub in
  (* look for a wrapper *)
  let wrap, rem =
    match head rem with
    | Some '{' -> Some '}', tail rem
    | Some '(' -> Some ')', tail rem
    | _ -> None, rem
  in
  let is_an c = Char.Ascii.is_alphanum c || c = '_' in
  match parse_name is_an rem with (* no punctuation! no whitespace! *)
  | Ok (psection, name, rem) -> (
      let result =
        match wrap with
        | Some c -> (
            match head rem with
            | Some ch ->
              if ch = c
              then Ok (tail rem) (* don't include closing brace *)
              else Error "missing closing wrap char"
            | None -> Error "missing closing wrap char"
          )
        | None -> Ok rem
      in
      match result with
      | Ok rem -> (
          let section = match psection with
          | Some s -> Some s
          | None -> section
          in
          match get_value ~ht ?section (to_string name) with
          | Some s -> Ok (String.sub s, rem)
          | None -> Error (start_pos rem, "subst", "variable has no value")
        )
      | Error s -> Error (start_pos rem, "subst", s)
    )
  | Error tuple -> Error tuple

(* translate if special whitespace char *)
let translate_char = function
| 'r' -> '\r' (* carriage return *)
| 'n' -> '\n' (* newline *)
| 'b' -> '\b' (* backspace *)
| 't' -> '\t' (* tab *)
| c -> c

(*
   see [str_copy] in Openssl [conf_def.c].
   use cases: parse_line (2 times).
   note: 1st use case (section line) may have escape chars,
         but no other special chars are in play.
   note: section is optional, so is None in 1st use case.
 *)
let parse_value ht section value =
  (* not escape char, double quote, single quote, dollar sign, hash char *)
  let is_data = function '\\' | '"' | '\'' | '$' | '#' -> false | _ -> true in
  let rec loop acc s =
    let open String.Sub in
    let data, rem = span ~sat:is_data s in
    match head rem with
    | Some ('\'' | '"' as quote) -> (
        let f =
          if Sys.os_type = "Win32" (* Unix bisect can't go here *)
          then (if quote = '"' then Some parse_dquote else None) (*BISECT-IGNORE*)
          else Some (parse_quote quote) in
        match f with
        | Some f ->
            let quoted, rem = f (tail rem) in
            loop (quoted :: data :: acc) rem
        | None -> (*BISECT-IGNORE*)
            loop ((of_char quote) :: acc) (tail rem)
            (* bisect under Unix will not go here *)
      )
    | Some '\\' -> (
        let rem = tail rem in
        let acc = data :: acc in
        match head rem with
        | Some c -> loop ((of_char (translate_char c)) :: acc) (tail rem)
        | None -> loop acc rem (*BISECT-IGNORE*)
        (* None case excluded by call context
           -- always a char next, or char is continuation char at eol.
        *)
      )
    | Some '$' -> (
        (* variable substitution *)
        match subst ht section (tail rem) with
        | Ok (value, rem) -> loop (value :: data :: acc) rem
        | Error tuple -> Error tuple
      )
    | Some '#' | None ->
      let acc = List.rev (data :: acc) in
      Ok (drop ~rev:true ~sat:Char.Ascii.is_white (concat acc)) (* trim trailing space *)
    | Some _ -> failwith "parse_value: pattern-match failure" (*BISECT-IGNORE*)
  in
  loop [] value

(* note - no special characters are here at all *)
let is_punct = function
  '!' | '.' | '%' | '&' | '*' | '+' |
  ';' | '?' | '@' | '^' | '~' | '|' | '-' -> true
| _ -> false

(* see openssl crypto/conf/conf_def.c; section changes if ['['..']'] seen.

   [parse_line hts ht section line] parses a line.
   [section] is a default section.
   A result structure is returned.
   Ok returns a state to pass to the next [parse_line] call:
   {ul {- a default section;}
       {- a {!section_hashtbl}; and}
       {- a {!section_name_hashtbl}.}}
   Error returns:
   {ul {- a position offset in the string;}
       {- a procedure name; and}
       {- an error message.}}
   The position offset and procedure name show where an error was found.
 *)
let parse_line ht_section ht section line =
  let is_anp c = Char.Ascii.is_alphanum c || c = '_' || is_punct c in
  let is_anp_or_ws c = Char.Ascii.is_white c || is_anp c in
  let line = String.sub line in
  let open String.Sub in
  let line = drop ~sat:Char.Ascii.is_white line in
  match head line with
  | Some '#' | None -> Ok (section, ht_section, ht) (* comment *)
  | Some '[' -> (
      let start = drop ~sat:Char.Ascii.is_white (tail line) in
      let name, rem = span_with_escapes is_anp_or_ws start in
      match head rem with
      | Some ']' -> (
          match parse_value ht None name with
          | Ok value -> (
              let section = to_string value in
              let _ =
                try ignore (Hashtbl.find ht_section section)
                with Not_found ->
                  let stack = Stack.create () in
                  Hashtbl.add ht_section section stack
              in
              Ok (section, ht_section, ht)
            )
          | Error tuple -> Error tuple (*BISECT-IGNORE*)
          (* [caller context] parse_value can't return Error here *)
        )
      | Some _ | None ->
        Error (start_pos rem, "parse_line", "closing square bracket not found")
    )
  | Some c -> (
      if Sys.os_type = "Win32" && c == ';' (*BISECT-IGNORE*)
      then Ok (section, ht_section, ht) (*BISECT-IGNORE*)
           (* Unix bisect can't go here *)
      else match parse_name is_anp line with
      | Ok (psection, name, rem) -> (
          let section = match psection with Some s -> s | None -> section
          and rem = drop ~sat:Char.Ascii.is_white rem in
          match head rem with
          | Some '=' -> (
              let rem = tail rem in
              let value = drop ~sat:Char.Ascii.is_white rem in
              match parse_value ht (Some section) value with
              | Ok value -> (
                  let name = to_string name and value = to_string value in
                  let v = (name, value) in
                  begin
                    match (get_section ht_section section) with
                    | Some stack -> Stack.push v stack
                    | None -> (
                        let stack = Stack.create () in
                        Stack.push v stack;
                        Hashtbl.add ht_section section stack
                      );
                  end;
                  Hashtbl.add ht (section, name) value;
                  Ok (section, ht_section, ht)
                )
              | Error tuple -> Error tuple
            )
          | Some _ | None ->
            Error (start_pos rem, "parse_line", "equal sign not found")
        )
      | Error tuple -> Error tuple
    )

(*
   used only in parse_file.
   identify which original line and column contains a pos.
 *)
let find_pos starts pos =
  let rec loop i a starts pos =
    match starts with
    | b :: starts ->
      if pos >= b
      then loop (i + 1) b starts pos
      else i, pos - a
    | [] -> i, pos - a
  in
  loop 0 0 starts pos

type error =
  | Open of string
  | Extend of string * int * int * int
  | Parse of string * int * int * string * string

(* return a user-friendly string summarizing an error. *)
let string_of_error error =
  match error with
  | Open s -> s
  | Extend (s, nbits, len, limit) ->
    Printf.sprintf "%s: nbits=%d,len=%d,limit=%d" s nbits len limit
  | Parse (filename, lineno, pos, fname, msg) ->
    Printf.sprintf "%s|line %d col %d|func %s|%s" filename lineno pos fname msg

(* error report precision wrangling, mostly *)
let conf_load_file conf filename =
  match (try Ok (open_in filename) with Sys_error s -> Error s) with
  | Error s -> Error (Open s)
  | Ok ic ->
    let rec loop lineno section ht_sect ht ic =
      (* line handler *)
      match getline ic with
      | Ok (Line (line, ranges)) -> (
          (* line parser *)
          match parse_line ht_sect ht section line with
          | Ok (section, ht_sect, ht) ->
            (* update line number for error reporting *)
            let lineno = lineno + (List.length ranges) + 1 in
            loop lineno section ht_sect ht ic
          | Error (pos, fname, msg) ->
            let i, pos = find_pos ranges pos in
            Error (Parse (filename, lineno + i, pos, fname, msg))
        )
      | Ok EOF -> Ok ()
      | Error (s, nbits, len, limit) -> Error (Extend (s, nbits, len, limit))
    in
    let res = loop 1 "default" conf.ht_sect conf.ht ic in
    close_in ic;
    res

open Sexplib.Std

(* conversion to / from sexp *)

type string_pair = string * string [@@deriving sexp]
type spl = string_pair list [@@deriving sexp]
type hkv = string_pair * string [@@deriving sexp]
type hkvl = hkv list [@@deriving sexp]
type hskv = string * spl [@@deriving sexp]
type hskvl = hskv list [@@deriving sexp]
type hs_h = hskvl * hkvl [@@deriving sexp]

let hfold h = let f k v l = (k, v) :: l in Hashtbl.fold f h []
and sfold stk = let f l e = e :: l in Stack.fold f [] stk
let hsfold h = let f k v l = (k, sfold v) :: l in Hashtbl.fold f h []
let htfold (hs, h) = hsfold hs, hfold h

let hunfold l =
  let h = Hashtbl.create ~random:false 10 in
  let f (k, v) = Hashtbl.add h k v in
  List.iter f l;
  h
and sunfold kvl =
  let stack = Stack.create () in
  let f kv = Stack.push kv stack in
  List.iter f kvl;
  stack
let hsunfold l =
  let h = Hashtbl.create ~random:false 10 in
  let f (k, kvl) = Hashtbl.add h k (sunfold kvl) in
  List.iter f l;
  h
let htunfold (hsl, hl) = hsunfold hsl, hunfold hl

type stack = string_pair Stack.t

let sexp_of_stack stack = sfold stack |> sexp_of_spl
let stack_of_sexp sexp = spl_of_sexp sexp |> sunfold

let sexp_of_conf conf = htfold (conf.ht_sect, conf.ht) |> sexp_of_hs_h
let conf_of_sexp sexp =
  let hs_h = hs_h_of_sexp sexp in
  match htunfold hs_h with (ht_sect, ht) -> { ht_sect; ht; }

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
