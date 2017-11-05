open Stdint

exception File_access_error

val getmtime        : filename:string -> float

val getmtime_uint64 : filename:string -> uint64

val getsize         : filename:string -> int64

val getsize_uint64  : filename:string -> uint64
