module M : sig
  type 'a t = Foo of 'a s
  and 'a s = Boo of 'a t | Goo [@@deriving conv {ocaml}]
end = struct
  type 'a t = Foo of 'a s
  and 'a s = Boo of 'a t | Goo [@@deriving conv {ocaml}]
end
