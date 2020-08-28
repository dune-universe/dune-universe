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

(* Reentrant buffers :
   If you call a function that needs a buffer, you might want to use this
   module to reuse such buffers, instead of reallocating them everytime.

   This module is not thread-safe. Reentrance is only provided for a function
   that uses a buffer, and might call another function using a similar
   buffer.

   Buffer sizes should be between 4kB and 1MB.
*)

open EzCompat

let nsizes = 10

let max_queue_size = ref 100
let buffers = Array.init nsizes (fun _ -> Queue.create ())
let lengths = Array.make nsizes 0

let invalid_size size =
  Printf.kprintf failwith
    "ReentrantBuffer.get: size %d is not a power of two" size

let get_power size =
  let rec find_power pos size =
    if size = 1 then pos else
      let size2 = size lsr 1 in
      if size2 lsl 1 <> size then invalid_size size;
      find_power (pos+1) size2
  in
  if (size lsr 10) lsl 10 <> size then invalid_size size;
  find_power 0 (size lsr 10)

let _ =
  assert (get_power 1024 = 0);
  assert (get_power 2048 = 1);
  ()

let alloc size =
  let pos = get_power size in
  let nbuffers = lengths.(pos) in
  if nbuffers > 0 then
    let b = Queue.take buffers.(pos) in
    lengths.(pos) <- nbuffers -1 ;
    b
  else
    Bytes.create size

let free s =
  let size = Bytes.length s in
  let pos = get_power size in
  let nbuffers = lengths.(pos) in
  if nbuffers < !max_queue_size then begin
    lengths.(pos) <- nbuffers + 1;
    Queue.add s buffers.(pos)
  end

let set_max_queue_size size =
  if !max_queue_size > size then begin
    for pos = 0 to nsizes - 1 do
      let nbuffers = lengths.(pos) in
      if nbuffers > size then begin
        for _i = size+1 to nbuffers do
          ignore ( Queue.take buffers.(pos) )
        done;
        lengths.(pos) <- size
      end
    done
  end ;
  max_queue_size := size
