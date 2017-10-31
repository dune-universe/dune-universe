(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

(* write result to "cases.out" file, to replace "testcase.ml" if desired *)
type caserec = {
  nbits:int option; (* use nbits to limit buffer size, to check Buf module *)
  desc:string option; (* test description *)
  input:string option; (* written to a temp file, then parsed *)
  expect:string option; (* if None, return true *)
}

(* used to call [conf_get_section] *)
type stackrec = {
  section:string;
}

(* used to call [conf_get_string] *)
type valuerec = {
  usehashtable:bool; (* if hashtable not used, look at environment *)
  optsection:string option; (* if section not given, look at "default" *)
  name:string;
}

type case =
  | Parse of caserec (* parse a config, using [conf_load_file] *)
  | ParseLoad of caserec (* parse and load its sexp *)
  | Stack of caserec * stackrec (* parse, get section *)
  | StackLoad of caserec * stackrec (* parse, get section, load its sexp *)
  | Value of caserec * valuerec (* parse, get value from section and string *)

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
