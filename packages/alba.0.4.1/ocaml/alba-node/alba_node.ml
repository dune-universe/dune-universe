open Fmlib_node
open Albalib


module Alba = Alba_console.Make (Node_io.IO)

let _ = Alba.run ()
