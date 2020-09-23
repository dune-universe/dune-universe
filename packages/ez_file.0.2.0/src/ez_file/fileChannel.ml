(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

type out_file = out_channel

let output_line chan string =
  output_string chan (string ^ FileOS.line_separator)

let copy_file ic oc =
  let s = EzBytes.alloc FileOS.default_buffer_size in
  let rec copy s ic oc =
    let n = input ic s 0 FileOS.default_buffer_size in
    if n = 0 then () else (output oc s 0 n; copy s ic oc)
  in copy s ic oc;
  EzBytes.free s

let iter_blocks f ic =
  let s = EzBytes.alloc FileOS.default_buffer_size in
  let rec iter f ic s =
    let nread = input ic s 0 FileOS.default_buffer_size in
    if nread > 0 then begin
      f s nread;
      iter f ic s
    end
  in
  iter f ic s;
  EzBytes.free s


let read_file ic =
  let s = EzBytes.alloc FileOS.default_buffer_size in
  let b = Buffer.create 1000 in
  let rec iter ic b s =
    let nread = input ic s 0 FileOS.default_buffer_size in
    if nread > 0 then begin
      Buffer.add_subbytes b s 0 nread;
      iter ic b s
    end
  in
  iter ic b s;
  EzBytes.free s;
  Buffer.contents b
let string_of_file = read_file

let write_file = output_string
let file_of_string = write_file

let read_subfile ic pos len =
  seek_in ic pos;
  if len = 0 then begin
    ""
  end else
    let s = Bytes.create len in
    let rec iter pos len =
      if len > 0 then
        let nread = input ic s pos len in
        if nread > 0 then
          iter (pos+nread) (len-nread)
        else raise End_of_file
    in
    iter 0 len;
    Bytes.to_string s

let string_of_subfile = read_subfile


let read_lines_to_revlist ic =
  let lines = ref [] in
  begin try
      while true do
        lines := input_line ic :: !lines
      done
    with End_of_file -> ()
  end;
  !lines

let read_lines ic =
  let lines = Array.of_list (read_lines_to_revlist ic) in
  EzArray.rev lines;
  lines

let read_lines_to_list ic = List.rev (read_lines_to_revlist ic)

let lines_of_file = read_lines

let write_lines oc lines =
  Array.iter (output_line oc) lines

let write_lines_of_list oc lines =
  List.iter (output_line oc) lines

let file_of_lines = write_lines

let iter_lines f ic =
  try
    while true do
      let line = input_line ic in
      f line
    done
  with
  | End_of_file -> ()

let iteri_lines f ic =
  let n = ref 0 in
  try
    while true do
      let line = input_line ic in
      f !n line;
      incr n;
    done
  with
  | End_of_file -> ()

let read_sublines_to_revlist ic off len =
  if len = 0 then []
  else
    let lines = ref [] in
    let aux i elt =
      if i >= off && i < off + len then
        lines := elt :: !lines
      else
      if i = off + len then raise Exit;
    in
    (try iteri_lines aux ic with Exit -> ());
    !lines

let read_sublines_to_list ic off len =
  List.rev (read_sublines_to_revlist ic off len)

let read_sublines ic off len =
  let lines = Array.of_list (read_sublines_to_revlist ic off len) in
  EzArray.rev lines;
  lines
