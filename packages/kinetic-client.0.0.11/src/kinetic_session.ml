(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
 *)

open Kinetic_config

module Session = struct

    type t = {
        secret: string;
        cluster_version: int64;
        identity: int64;
        connection_id: int64;
        mutable sequence: int64;
        mutable batch_id: int32;

        config : Config.t;
        mutable trace: bool;
        mutable in_batch :bool;
      }

    let incr_sequence t = t.sequence <- Int64.succ t.sequence
    let set_sequence t i64 = t.sequence <- i64

    let batch_on t = t.in_batch <- true
    let batch_off t = t.in_batch <- false
end
