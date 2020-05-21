[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]

type t =
  | C89
  | C94
  | Gnu89
  | C99
  | Gnu99
  | C11
  | Gnu11
  | C17
  | Gnu17
  | C2x
  | Gnu2x
  | Cxx98
  | Gnucxx98
  | Cxx11
  | Gnucxx11
  | Cxx14
  | Gnucxx14
  | Cxx17
  | Gnucxx17
  | Cxx20
  | Gnucxx20
  | Opencl10
  | Opencl11
  | Opencl12
  | Opencl20
  | Openclcpp
  | Cuda
  | Hip
      [@@deriving refl]

exception Unavailable of t

let to_clang : t -> Clang__bindings.clang_ext_langstandards = function
  | C89 -> C89
  | C94 -> C94
  | Gnu89 -> Gnu89
  | C99 -> C99
  | Gnu99 -> Gnu99
  | C11 -> C11
  | Gnu11 -> Gnu11
  | C17 ->
      [%meta if Clangml_config.version.major >= 6 then [%expr
        C17]
      else [%expr
        raise (Unavailable C17)]]
  | Gnu17 ->
      [%meta if Clangml_config.version.major >= 6 then [%expr
        Gnu17]
      else [%expr
        raise (Unavailable Gnu17)]]
  | C2x ->
      [%meta if Clangml_config.version.major >= 9 then [%expr
        C2x]
      else [%expr
        raise (Unavailable C2x)]]
  | Gnu2x ->
      [%meta if Clangml_config.version.major >= 9 then [%expr
        Gnu2x]
      else [%expr
        raise (Unavailable Gnu2x)]]
  | Cxx98 -> Cxx98
  | Gnucxx98 -> Gnucxx98
  | Cxx11 -> Cxx11
  | Gnucxx11 -> Gnucxx11
  | Cxx14 ->
      [%meta if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Cxx14]
      else [%expr
        Cxx1y]]
  | Gnucxx14 ->
      [%meta if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Gnucxx14]
      else [%expr
        Gnucxx1y]]
  | Cxx17 ->
      [%meta if Clangml_config.version.major >= 5 then [%expr
        Cxx17]
      else if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Cxx1z]
      else [%expr
        raise (Unavailable Cxx17)]]
  | Gnucxx17 ->
      [%meta if Clangml_config.version.major >= 5 then [%expr
        Gnucxx17]
      else if
        Clangml_config.version >= { major = 3; minor = 5; subminor = 0 }
      then [%expr
        Gnucxx1z]
      else [%expr
        raise (Unavailable Gnucxx17)]]
  | Cxx20 ->
      [%meta if Clangml_config.version.major >= 10 then [%expr
        Cxx20]
      else if Clangml_config.version.major >= 5 then [%expr
        Cxx2a]
      else [%expr
        raise (Unavailable Cxx20)]]
  | Gnucxx20 ->
      [%meta if Clangml_config.version.major >= 10 then [%expr
        Gnucxx20]
      else if Clangml_config.version.major >= 5 then [%expr
        Gnucxx2a]
      else [%expr
        raise (Unavailable Gnucxx20)]]
  | Opencl10 ->
      [%meta if Clangml_config.version.major >= 5 then [%expr
        Opencl10]
      else [%expr
        raise (Unavailable Opencl10)]]
  | Opencl11 -> Opencl11
  | Opencl12 -> Opencl12
  | Opencl20 ->
      [%meta if
        Clangml_config.version >= { major = 3; minor = 6; subminor = 0 }
      then [%expr
        Opencl20]
      else [%expr
        raise (Unavailable Opencl20)]]
  | Openclcpp ->
      [%meta if Clangml_config.version.major >= 7 then [%expr
        Openclcpp]
      else [%expr
        raise (Unavailable Openclcpp)]]
  | Cuda -> Cuda
  | Hip ->
      [%meta if Clangml_config.version.major >= 7 then [%expr
        Hip]
      else [%expr
        raise (Unavailable Hip)]]
