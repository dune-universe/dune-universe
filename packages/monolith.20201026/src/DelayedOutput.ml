(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

let b =
  Buffer.create 1024

let () =
  GlobalState.register (fun () ->
    let snapshot = Buffer.contents b in
    fun () ->
      Buffer.clear b;
      Buffer.add_string b snapshot
  )

let[@inline] dprintf format =
  Printf.bprintf b format

let dump dst =
  Buffer.add_buffer dst b
