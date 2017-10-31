(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3
  ---------------------------------------------------------------------------*)

(*
   [gets ic buf pos_offset max] gets a line from an input channel [ic].

   chars are read from [ic].
   if a newline (['\n']) is found, the line ends.
   else, if [max] chars are read, the line ends.

   data goes into buffer [buf], starting at offset [pos].

   if less than [max] is returned, and no newline is at the end,
   then end of file was reached.
 *)
let gets ic b pos max =
  let rec loop n =
    if n == max
    then n
    else
      try
        let c = input_char ic in
        Bytes.set b (pos + n) c;
        let n = n + 1
        in
        (* according to Pervasives documentation,
           if ic is in text mode
           (on OSs that *have* text mode, i.e., Windows),
           then "\r\n" will be translated to "\n"
        *)
        if c == '\n'
        then n
        else loop n
      with
        End_of_file -> n
  in
  loop 0

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
