open Apero
open Time
open Time_64bit
open Timestamp
open Clock

module HLC : sig

  module type S = sig

    module Time: Time
    module Timestamp: Timestamp.S

    type t

    val create : ?csize:int -> ?delta:Time.t -> Uuid.t -> t
    (** [create ?csize ?delta id] creates a new HLC using [id] as source identifier.
        [csize] specifies the size of the counter part used within the 64-bit time represnetation (default: 8 bits).
        [delta] specifies the maximum time drift accepted wrt. local time for an incoming timestamp (default: 100ms). *)

    val new_timestamp:  t -> Timestamp.t Lwt.t
    (** [new_timestamp ()] updates the HLC with the local time and returns a new [Timestamp]
        which is greater than any previously returned timestamp *)

    val update_with_timestamp: Timestamp.t -> t -> (unit, Apero.error) Result.t Lwt.t
    (** [update_with_timestamp t] checks if the timestamp [t] (that should come from an incoming message)
        doesn't exceeds the local time above the specified {! Config.delta }.
        If not, the HLC is updated with this timestamps and will further create timestamps that are 
        greater thant [t] and than any previously returned timestamp. *)
  end

  module Make (Clk: Clock with type Time.t = Time_64bit.t): S

end
