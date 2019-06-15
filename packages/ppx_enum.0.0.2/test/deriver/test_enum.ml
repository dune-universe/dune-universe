module S : sig
  type t =
    | Foo
    | Bar
  [@@deriving str_enum]

  type simple_enum =
    | Foo
    | Bar
  [@@deriving str_enum]

  type enum_with_custom_value =
    | Foo
    | Bar
    | Baz
  [@@deriving str_enum]
end = struct
  type t =
    | Foo
    | Bar
  [@@deriving str_enum]

  type simple_enum =
    | Foo
    | Bar
  [@@deriving str_enum]

  type enum_with_custom_value =
    | Foo [@value "foo-1"]
    | Bar [@str_enum.value "bar-1"]
    | Baz [@ppx_enum.str_enum.value "baz-1"]
  [@@deriving str_enum]
end
