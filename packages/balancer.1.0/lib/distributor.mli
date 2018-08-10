open Serverset

module type S = sig

  type param
  type state

  type peer
         
         
  val pick: state -> param -> peer Lwt.t 
  val use: state -> param -> (Node.t -> 'a Lwt.t) -> 'a Lwt.t
                                                        
end


module type Fanout = sig
  val fanout: int
end

module type Checksum = sig
  val sum64: Cstruct.t -> int64 
end 


module P2C: S with type param = unit and type state = LoadedNodes.t and type peer = LoadedNode.t


                                                                         
module CHash:
functor(C: Checksum) -> S with type param = Cstruct.t and type state = Nodes.t and type peer = Node.t 

                                                                                               
module CHashLeastLoaded:
functor (C: Checksum) (F: Fanout) -> S with type param = Cstruct.t and type state = LoadedNodes.t and type peer = LoadedNode.t

                                                                                          
module RoundRobin: S with type param = unit and type state = RRQueue.t and type peer = Node.t

module P2C_PKG:
functor (C1: Checksum) (C2: Checksum) -> S with type param = Cstruct.t and type state = LoadedNodes.t and type peer = LoadedNode.t
