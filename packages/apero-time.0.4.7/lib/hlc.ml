open Apero
open Time
open Time_64bit
open Timestamp
open Clock

module HLC = struct

  module type S = sig

    module Time: Time
    module Timestamp: Timestamp.S

    type t

    val create : ?csize:int -> ?delta:Time.t -> Uuid.t -> t

    val new_timestamp:  t -> Timestamp.t Lwt.t
    (** [new_timestamp ()] updates the HLC with the local time and returns a new [Timestamp]
        which is greater than any previously returned timestamp *)

    val update_with_timestamp: Timestamp.t -> t -> (unit, error) Result.t Lwt.t
    (** [update_with_timestamp t] checks if the timestamp [t] (that should come from an incoming message)
        doesn't exceeds the local time above the specified {! Config.delta }.
        If not, the HLC is updated with this timestamps and will further create timestamps that are 
        greater thant [t] and than any previously returned timestamp. *)
  end

  module Make (Clk: Clock with type Time.t = Time_64bit.t) = struct

    module Time = Clk.Time
    module Timestamp = Timestamp.Make(Time)

    type t = { id: Uuid.t
             ; cmask: Time.t
             ; lmask: Time.t
             ; delta: Time.t
             ; last_time: Time.t Guard.t }

    let create ?(csize=8) ?(delta=Time_64bit.of_seconds 0.1) id =
      let cmask = let open Int64 in sub (shift_left 1L csize) 1L in
      let lmask = Int64.lognot @@ cmask in
      let last_time = Guard.create 0L in
      { id; cmask; lmask; delta; last_time }

    let get_l time self = Int64.logand time self.lmask
    let get_c time self = Int64.logand time self.cmask

    let max t1 t2 = let open Time.Infix in if t1 > t2 then t1 else t2

    let max3 t1 t2 t3 = max t1 t2 |> max t3

    let new_timestamp self =
      let open Int64 in
      let pt = get_l (Clk.now ()) self in
      Guard.guarded self.last_time @@
      fun time ->
        let l' = get_l time self in
        let l = max l' pt in
        let c = if (Int64.equal l l') then succ (get_c time self) else 0L in
        let new_time = logor l c in
      Guard.return (Timestamp.create self.id new_time) new_time

    let update_with_timestamp timestamp self =
      let open Int64 in
      let now = Clk.now() in
      let msg_time = Timestamp.get_time timestamp in
      if (sub msg_time now) > self.delta then
        let source = Timestamp.get_source timestamp in
        let error_msg = Printf.sprintf "[HLC] incoming timestamp from %s exceeding delta %Ld is rejected: %Ld vs. now: %Ld"
          (Uuid.to_string source) self.delta msg_time now in
        let _ = Logs.warn (fun m -> m "%s" error_msg) in
        Lwt.return @@ Result.fail (`OutOfRange (`Msg error_msg))
      else
        let pt = get_l now self in
        let lm = get_l msg_time self in
        let cm = get_c msg_time self in
        Guard.guarded self.last_time @@
        fun time ->
          let l' = get_l time self in
          let l = max3 l' msg_time pt in
          let c =
            if (Int64.equal l l') && (Int64.equal l msg_time) then succ (max (get_c time self) cm)
            else if (Int64.equal l l') then succ (get_c time self)
            else if (Int64.equal l lm) then succ cm
            else 0L
          in
          let new_time = logor l c in
        Guard.return (Result.return ()) new_time

  end
end
