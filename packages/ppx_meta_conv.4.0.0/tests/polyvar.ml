module M : sig 
  type t = [ `Bar | `Foo ] [@@deriving conv {ocaml}]
end = struct
  type t = [ `Bar | `Foo ] [@@deriving conv {ocaml}]
end
