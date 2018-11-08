module Make(C: sig
  val channel: OutChannel.t
  val flush: bool
end) = struct
  let channel = C.channel

  let print ?(flush=C.flush) format =
    OutChannel.print ~flush channel format

  let output x =
    OutChannel.output channel x

  let flush () =
    OutChannel.flush channel
end
