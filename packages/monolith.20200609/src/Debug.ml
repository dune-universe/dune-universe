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

module[@inline] Make (X : sig
  val debug : bool
end) = struct

  let indentation =
    ref 0

  let[@inline] section k =
    if X.debug then begin
      indentation := !indentation + 2;
      match k() with
      | y ->
          indentation := !indentation - 2;
          y
      | exception e ->
          indentation := !indentation - 2;
          raise e
    end
    else
      k()

  let[@inline] log format =
    if X.debug then begin
      for _ = 1 to !indentation do
        Printf.fprintf stderr " "
      done;
      Printf.fprintf stderr format
    end
    else
      Printf.ifprintf stderr format

end
