(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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
 *
 *)

module ErrnoMap = struct

  let roundtrip () =
    let defns = Errno.Host.to_defns Errno_unix.host in
    print_endline "after defns_of_host";
    let s = Errno.string_of_defns defns in
    print_endline "after string_of_defns";
    let rdefns = Errno.defns_of_string s in
    print_endline "after defns_of_string";
    let rs = Errno.string_of_defns rdefns in
    print_endline "after string_of_defns2";
    Alcotest.(check string) "roundtrip local defns" s rs

  let tests = [
    "roundtrip", `Quick, roundtrip;
  ]

end

let tests = [
  "errno-map", ErrnoMap.tests;
]

;;
Alcotest.run "unix-errno" tests
