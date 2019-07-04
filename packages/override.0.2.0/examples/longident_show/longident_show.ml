(* Example adapted from https://github.com/ocaml-ppx/ppx_import/ *)

module%override Longident = struct
  type t = _  [@@deriving show]
end

let test () =
  assert (
    Longident.show (Longident.parse "Foo.Bar.baz") =
    {|Longident.Ldot (Longident.Ldot ((Longident.Lident "Foo"), "Bar"), "baz")|}
 )


let () = test ()
