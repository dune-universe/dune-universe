module S : sig
  type t =
    | Foo
    | Bar
  [@@deriving enum]

  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]

  type enum_with_custom_value =
    | Foo
    | Bar
    | Baz
  [@@deriving enum]
end = struct
  type t =
    | Foo
    | Bar
  [@@deriving enum]

  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]

  type enum_with_custom_value =
    | Foo [@value "foo-1"]
    | Bar [@enum.value "bar-1"]
    | Baz [@ppx_enum.enum.value "baz-1"]
  [@@deriving enum]
end
