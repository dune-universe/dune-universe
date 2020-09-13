(*
STORAGE=Lang_module4.Z.Foo (Int 2)
*)
open SCaml

module Z = struct
  type t = Foo of int
end

module Y = struct
  let [@entry] default () _ =
    [], Z.Foo (Int 3)
end
