(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

open OUnit2
open Printf
open Rresult
open Testcase

let append_result, close_oc =
  let oc = open_out "cases.out" in
  let close_oc () =
    output_string oc "]\n";
    close_out oc
  in
  let string_of_caserec case output =
    let r = match case with
    | Parse r
    | ParseLoad r
    | Stack (r,_)
    | StackLoad (r,_)
    | Value (r,_) -> r in
    "\n    {\n" ^
    (match r.nbits with
     | None -> "      nbits = None;\n"
     | Some n -> sprintf "      nbits = Some %d;\n" n) ^
    (match r.desc with
     | None -> "      desc = None;\n"
     | Some s -> sprintf "      desc = Some %S;\n" s) ^
    (match r.input with
     | None -> "      input = None;\n"
     | Some s -> sprintf "      input = Some %S;\n" s) ^
    (sprintf "      expect = Some %S;\n" output) ^
    "    }" in
  let string_of_stackrec r =
    "    {\n" ^
    "      section = " ^ (sprintf "%S" r.section) ^
    ";\n    }\n" in
  let string_of_valuerec r =
    "    {\n" ^
    "      usehashtable = " ^ (if r.usehashtable then "true" else "false") ^ ";\n" ^
    "      optsection = " ^
    (match r.optsection with
     | Some s -> "Some " ^ (sprintf "%S" s)
     | None -> "None"
    ) ^ ";\n" ^
    "      name = " ^ (sprintf "%S" r.name) ^
    ";\n    }\n" in
  let append_result name case output =
    output_string oc ("  (* " ^ name ^ " *)\n");
    let caserec = string_of_caserec case output in
    match case with
    | Parse _ -> output_string oc ("  Parse" ^ caserec ^ ";\n")
    | ParseLoad _ -> output_string oc ("  ParseLoad" ^ caserec ^ ";\n")
    | Stack (_, r) ->
        let stack = string_of_stackrec r in
        output_string oc ("  Stack (" ^ caserec ^ ",\n" ^ stack ^ "  );\n")
    | StackLoad (_, r) ->
        let stack = string_of_stackrec r in
        output_string oc ("  StackLoad (" ^ caserec ^ ",\n" ^ stack ^ "  );\n")
    | Value (_, r) ->
        let value = string_of_valuerec r in
        output_string oc ("  Value (" ^ caserec ^ ",\n" ^ value ^ "  );\n") in
  output_string oc "include Casetype\n\n";
  output_string oc "let cases = [\n"; flush oc;
  append_result, close_oc

let string_of_sexp = Sexplib.Sexp.to_string_mach

let pp_err err = Sslconf.string_of_error err

let print_err err = sprintf "%S" (pp_err err)

let build_test_name n = "test" ^ (string_of_int n)

let build_test_fun n case ctx =
  (* set up temporary files for test cases *)
  let flags = [Open_creat; Open_wronly; Open_text] in
  let filename, open_out = bracket_tmpfile ~mode:flags ctx in
  (* three kinds of test cases *)
  let r = match case with
  | Parse r
  | ParseLoad r
  | Stack (r,_)
  | StackLoad (r,_)
  | Value (r,_) -> r in
  let input = match r.input with Some s -> s | None -> "" in
  (* write test case file *)
  output_string open_out input;
  close_out open_out;
  (* getline buffer -- 31 bits default; use smaller size to force errors *)
  (match r.nbits with None -> () | Some i -> Sslconf.nbits := Some i);
  let filename = match r.input with Some _ -> filename | None -> "xx" in
  let conf = Sslconf.create () in
  let res = Sslconf.conf_load_file conf filename in
  (* reset nbits to default for next case *)
  Sslconf.nbits := None;
  let res =
    (* rewrite result to replace temp file name with a generic "_" name *)
    match res with
    | Ok _ -> res
    | Error err -> (
        match err with
        | Sslconf.Open _ | Sslconf.Extend (_, _, _, _) -> res
        | Sslconf.Parse (_, i2, i3, s4, s5) ->
            Error (Sslconf.Parse ("_", i2, i3, s4, s5))
            (* the temp file name is the first component of Sslconf.Parse *)
      ) in
  let real =
    (* this real result will set the expectation in "cases/cases.out" *)
    match case with
    | Parse _ -> (
        (* this is a test case for loading *)
        match res with
        | Ok () -> Sslconf.sexp_of_conf conf |> string_of_sexp
        | Error err -> print_err err
      )
    | ParseLoad _ -> (
        (* this is a test case for loading *)
        match res with
        | Ok () ->
            let open Sslconf in
            sexp_of_conf conf |>
            conf_of_sexp |>
            sexp_of_conf |>
            string_of_sexp
        | Error err -> print_err err
      )
    | Stack (_, r) -> (
        (* this is a test case for conf_get_section *)
        match res with
        | Ok () -> (
            (* the file is loaded; now we ask for a section *)
            match Sslconf.conf_get_section conf r.section with
            | Some stack ->
                let sexp = Sslconf.sexp_of_stack stack in
                string_of_sexp sexp
            | None -> sprintf "stack for section %S not found" r.section
          )
        | Error err -> print_err err
      )
    | StackLoad (_, r) -> (
        (* this is a test case for conf_get_section *)
        match res with
        | Ok () -> (
            (* the file is loaded; now we ask for a section *)
            match Sslconf.conf_get_section conf r.section with
            | Some stack ->
                let open Sslconf in
                sexp_of_stack stack |>
                stack_of_sexp |>
                sexp_of_stack |>
                string_of_sexp
            | None -> sprintf "stack for section %S not found" r.section
          )
        | Error err -> print_err err
      )
    | Value (_, r) ->
        (* this is a test case for conf_get_value *)
        match res with
        | Ok () -> (
            (* determine how to call conf_get_value *)
            let conf = if r.usehashtable then Some conf else None in
            let section = r.optsection in
            (* the file is loaded; now we ask for a name *)
            match Sslconf.conf_get_value ?conf ?section r.name with
            | Some s -> s
            | None -> sprintf "value for name %S not found" r.name
          )
        | Error err -> print_err err
  in
  let name = build_test_name n in
  (* append done test case to "cases/cases.out"; set expect to real result *)
  append_result name case real;
  (* compare result with expectation *)
  let expected = match r.expect with None -> real | Some s -> s in
  let self_s s = s in
  let msg = match r.desc with None -> name | Some s -> name ^ ": " ^ s in
  assert_equal ~ctxt:ctx ~printer:self_s ~msg:msg expected real

let build_test n case =
  let label = build_test_name n in
  label >:: build_test_fun n case

let tests = List.mapi build_test cases

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
