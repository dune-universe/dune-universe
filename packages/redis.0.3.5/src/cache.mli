(**
   Cache module
*)
module Make(IO : S.IO)(Client : S.Client with module IO = IO)(Params : S.Cache_params) : S.Cache
  with module IO = IO
  with module Client = Client
  with module Params = Params
