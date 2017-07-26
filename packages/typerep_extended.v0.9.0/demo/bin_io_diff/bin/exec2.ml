open Core
open Typerep_extended.Std

module A01 = struct
  type t = int [@@deriving typerep]
end
let () = Protocol_table.register (module A01)

module A02 = struct
  type t = {
    x : string;
    y : [ `A | `B | `C ];
  } [@@deriving typerep]
end
let () = Protocol_table.register (module A02)

module A03 = struct
  type t =
  | A
  | B of int
  | C
  | D of float
  | C2
  [@@deriving typerep]
end
let () = Protocol_table.register (module A03)

let () = Exn.handle_uncaught ~exit:true (fun () ->
  Command.run Protocol_table.simple_command
)
