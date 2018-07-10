open Ctypes

module Defn = struct
  type t = unit ptr
  let t : t typ = ptr void
end
type t = unit ptr
let t : t typ = ptr void
let t_opt : t option typ = ptr_opt void

let err exn =
  view
    ~read:(fun i -> if i = 0 then () else raise exn)
    ~write:(fun () -> 0)
    int

type envelope
let envelope : envelope structure typ = structure "envelope"
let envelope_minx = field envelope "MinX" double
let envelope_maxx = field envelope "MaxX" double
let envelope_miny = field envelope "MinY" double
let envelope_maxy = field envelope "MaxY" double
let () = seal envelope
