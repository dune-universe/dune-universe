type 'a t = ('a -> unit) -> unit

type 'a input = 'a t

type 'a output = 'a t

type 'a connection = ('a input, unit Sync.output) Connection.t

type 'a connection' = ('a input, unit output) Connection.t

let make f = f

let listen cb stream = stream cb

let pure value cb = cb value

let map f stream cb = stream (fun value -> cb (f value))

let filter f stream cb = stream (fun value -> if f value then cb value)

let scan f init stream =
  let acc = ref init in
  fun cb ->
    stream (fun value ->
        acc := f !acc value;
        cb !acc)

module type INTERVAL = sig
  type interval_id

  val set_interval : (unit -> unit) -> int -> interval_id

  val clear_interval : interval_id -> unit
end

module Interval (I : INTERVAL) = struct
  let forever ~ms =
    let i = ref 0 in
    fun cb ->
      I.set_interval
        (fun () ->
          i := !i + 1;
          cb !i)
        ms
      |> ignore

  let make ~ms =
    let i = ref 0 and interval = ref None in
    let stream cb =
      interval :=
        Some
          (I.set_interval
             (fun () ->
               i := !i + 1;
               cb !i)
             ms)
    in
    {
      Connection.stream;
      close =
        Sync.make_output (fun () ->
            match !interval with
            | Some interval' -> I.clear_interval interval'
            | _ -> ());
    }
end
