module Make(C: sig val channel: OutChannel.t end) = struct
  let channel = C.channel

  let print format =
    OutChannel.print channel format

  let output x =
    OutChannel.output channel x

  let flush () =
    OutChannel.flush channel
end
