open Async

type 'a t = 'a Deferred.t

include Netsnmp.Netsnmp_monad.IO with type 'a t := 'a t
