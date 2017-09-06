open Stdint

type fuint16

val (&)       : fuint16     -> fuint16 -> fuint16

val (or)      : fuint16     -> fuint16 -> fuint16

val (^)       : fuint16     -> fuint16 -> fuint16

val (<<)      : fuint16     -> int     -> fuint16

val (>>)      : fuint16     -> int     -> fuint16

val add1      : fuint16 ref -> unit

val sub1      : fuint16 ref -> unit

val of_int    : int         -> fuint16

val to_int    : fuint16     -> int

val of_char   : char        -> fuint16

val to_uint16 : fuint16     -> uint16

val of_uint16 : uint16      -> fuint16
