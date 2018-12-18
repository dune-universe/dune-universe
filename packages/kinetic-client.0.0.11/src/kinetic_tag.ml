(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
*)

open Kinetic_util

module Tag = struct
  type t =
    | Invalid of Bytes.t
    | Sha1 of Bytes.t
    | Crc32 of int32

  let show = function
    | Invalid h -> Printf.sprintf "Invalid %s" (to_hex h)
    | Sha1 h -> Printf.sprintf "Sha1 %s" (to_hex h)
    | Crc32 h ->Printf.sprintf "Crc32 %lx" h
end
