module type S = sig
  include Distributor.S
  include Serverset.S
end


module Make (SS: Serverset.S) (D: Distributor.S with type state = SS.t) = struct
  include SS
  include D  
end

                                                                            
                  
module Distributor = Distributor
module Node = Node
module Serverset = Serverset
module Chash = Chash
module Util = Util
                

open Serverset
       
module P2C = Make(Serverset.LoadedNodes)(Distributor.P2C)


module CHash(C: Distributor.Checksum) =
  Make(Nodes)(Distributor.CHash(C) )


module CHashLeastLoaded(C: Distributor.Checksum)(F: Distributor.Fanout) =
  Make(LoadedNodes)(Distributor.CHashLeastLoaded(C)(F) )
                              
module RoundRobin = Make(RRQueue)(Distributor.RoundRobin)


module P2C_PKG(C1: Distributor.Checksum) (C2: Distributor.Checksum) =
  Make(LoadedNodes)(Distributor.P2C_PKG(C1)(C2))
                   

                 
