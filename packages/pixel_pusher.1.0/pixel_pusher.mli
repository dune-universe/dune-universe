(** This is the interface to the Pixel Pusher subsystem.

  Pixel Pushers are devices that sit on IP networks that can be sent
  commands over UDP to control LEDs and other kinds of lighting hardware.

  Each Pixel Pusher can have up to eight LED "strips" connected, and each strip
  can have up to N "pixels".

  This module listens for Pixel Pushers to announce themselves, remembers
  them, and provides an interface for telling a pixel on a strip
  to turn to a color.

  This library depends on the Async library, but it can be linked into non-Async
  applications.  The library uses Async to live and handle background tasks, such
  as listening for beacons from Pixel Pushers and sending packets.

  If you do this, be sure in your main thread you frequently yield the CPU, such as
  by calling UNIX sleep at least as often as your expected FPS rate.  Also see the
  [send_updates_from_non_async_thread] function below.
*)

open Core
open Async

module Color = Pp_color

module Controller_report : sig
  type t =
      { controller_id : int
      ; group_id : int
      ; update_period : Time.Span.t
      ; last_beacon : Time.t }
end

module Strip : sig
  type t =
      { strip_number: int
      ; strip_length : int
      ; controller_id : int
      ; group_id : int
      ; matrix : Color.t Array.t }
  val set_pixel : t -> color:Color.t -> index:int -> unit
end


type non_async_token

(* [start ()] begins watching for Pixel Pusher presence UDP broadcasts.
   You can ignore the send_updates return value if you're using pure Async. *)
val start : unit -> non_async_token Deferred.t

(* [get_controllers ()] returns list of all currently seen controllers.
   Query often to get updates, as controllers come online and disappear. *)
val get_controllers : unit -> Controller_report.t list

(* [get_strips ()] returns all strips seen by the subsystem. *)
val get_strips : unit -> Strip.t list

(* Convenience function wrapping get_strips.
   Strips are indexed by (controller_id, strip_id) *)
val get_strips_as_map : unit -> (int * int, Strip.t) Map.Poly.t

(* [send_updates ()] instructs the subsystem to release any pending updates.
   Do this every time you've finished creating your "frame".  Use this
   from pure-Async applications. *)
val send_updates : unit -> unit Deferred.t

(* Same as above, but for calling from non-Async contexts. *)
val send_updates_from_non_async_thread: non_async_token -> unit
