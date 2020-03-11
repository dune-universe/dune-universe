open Fmlib_native
open Albalib

module Alba2 = Alba_console.Make (Native_io.IO)

let _ = Alba2.run ()
