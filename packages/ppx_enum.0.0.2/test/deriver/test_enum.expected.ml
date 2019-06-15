module S :
  sig
    type t =
      | Foo 
      | Bar [@@deriving str_enum]
    val to_string : t -> string
    val from_string : string -> (t, string) result
    val from_string_exn : string -> t
    type simple_enum =
      | Foo 
      | Bar [@@deriving str_enum]
    val simple_enum_to_string : simple_enum -> string
    val simple_enum_from_string : string -> (simple_enum, string) result
    val simple_enum_from_string_exn : string -> simple_enum
    type enum_with_custom_value =
      | Foo 
      | Bar 
      | Baz [@@deriving str_enum]
    val enum_with_custom_value_to_string : enum_with_custom_value -> string
    val enum_with_custom_value_from_string :
      string -> (enum_with_custom_value, string) result
    val enum_with_custom_value_from_string_exn :
      string -> enum_with_custom_value
  end =
  struct
    type t =
      | Foo 
      | Bar [@@deriving str_enum]
    let to_string = function | Foo -> "Foo" | Bar -> "Bar"
    let from_string =
      function
      | "Foo" -> Ok Foo
      | "Bar" -> Ok Bar
      | s ->
          Error
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "from_string" s)
    let from_string_exn =
      function
      | "Foo" -> Foo
      | "Bar" -> Bar
      | s ->
          invalid_arg
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "from_string_exn" s)
    type simple_enum =
      | Foo 
      | Bar [@@deriving str_enum]
    let simple_enum_to_string = function | Foo -> "Foo" | Bar -> "Bar"
    let simple_enum_from_string =
      function
      | "Foo" -> Ok Foo
      | "Bar" -> Ok Bar
      | s ->
          Error
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "simple_enum_from_string" s)
    let simple_enum_from_string_exn =
      function
      | "Foo" -> Foo
      | "Bar" -> Bar
      | s ->
          invalid_arg
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "simple_enum_from_string_exn" s)
    type enum_with_custom_value =
      | Foo [@value "foo-1"]
      | Bar [@str_enum.value "bar-1"]
      | Baz [@ppx_enum.str_enum.value "baz-1"][@@deriving str_enum]
    let enum_with_custom_value_to_string =
      function | Foo -> "foo-1" | Bar -> "bar-1" | Baz -> "baz-1"
    let enum_with_custom_value_from_string =
      function
      | "foo-1" -> Ok Foo
      | "bar-1" -> Ok Bar
      | "baz-1" -> Ok Baz
      | s ->
          Error
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "enum_with_custom_value_from_string" s)
    let enum_with_custom_value_from_string_exn =
      function
      | "foo-1" -> Foo
      | "bar-1" -> Bar
      | "baz-1" -> Baz
      | s ->
          invalid_arg
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "enum_with_custom_value_from_string_exn" s)
  end 
