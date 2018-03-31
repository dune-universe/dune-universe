module IO : Redis.S.IO with type 'a t = 'a Lwt.t and type 'a stream = 'a Lwt_stream.t

module Client : Redis.S.Client with module IO = IO

module Cache (Params : Redis.S.Cache_params) : Redis.S.Cache
  with module IO = IO
  with module Client = Client

module Mutex : Redis.S.Mutex
  with module IO = IO
  with module Client = Client

module ClusterClient : Redis.S.Client with module IO = IO

module ClusterCache (Params : Redis.S.Cache_params) : Redis.S.Cache
  with module IO = IO
  with module Client = ClusterClient

module ClusterMutex : Redis.S.Mutex
  with module IO = IO
  with module Client = ClusterClient
