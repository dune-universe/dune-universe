open Stdint

let gettime () : float =
  Unix.time ()
;;

let gettime_uint64 () : uint64 =
  Uint64.of_float (gettime ())
;;
