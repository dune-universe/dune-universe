open Time

module type Clock = sig
   module Time : Time

   val now: unit -> Time.t

end
