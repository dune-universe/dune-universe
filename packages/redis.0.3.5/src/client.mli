(**
   Redis client
*)
module Make(IO : S.IO) : S.Client with module IO = IO

(**
   Redis cluster client
*)
module MakeCluster(IO : S.IO) : S.Client with module IO = IO
