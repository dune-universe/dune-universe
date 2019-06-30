(*
 * Copyright 2019 Julien Sagot <julien.sagot@geneanet.org>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *)

let soundex s =
  let b = Bytes.create 4 in
  Bytes.fill b 0 4 '0' ;
  if s = "" then b
  else begin
    let len = String.length s in
    let rec loop i j =
      if i = len || j = 4 then b
      else if j = 0 then begin
        Bytes.unsafe_set b j (String.unsafe_get s i) ;
        loop (i + 1) (j + 1)
      end
      else match String.unsafe_get s i with
        | 'B' | 'F' | 'P' | 'V' ->
          set_loop i j '1'
        | 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' ->
          set_loop i j '2'
        | 'D' | 'T' ->
          set_loop i j '3'
        | 'L' ->
          set_loop i j '4'
        | 'M' | 'N' ->
          set_loop i j '5'
        | 'R' ->
          set_loop i j '6'
        | _ -> loop (i + 1) j
    and set_loop i j x =
      if Bytes.unsafe_get b (j - 1) = x
      && match String.unsafe_get s (i - 1) with
      | 'H' | 'W' -> true
      | c -> String.unsafe_get s i = c
      then loop (i + 1) j
      else begin
        Bytes.unsafe_set b j x ;
        loop (i + 1) (j + 1)
      end
    in
    loop 0 0
  end
