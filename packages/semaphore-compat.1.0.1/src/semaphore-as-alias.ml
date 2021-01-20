include (
  Semaphore :
    sig
      module Counting :
        Semaphore_intf.COUNTING with type t = Semaphore.Counting.t

      module Binary : Semaphore_intf.BINARY with type t = Semaphore.Binary.t
    end )
