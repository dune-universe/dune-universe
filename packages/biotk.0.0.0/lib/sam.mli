open Core_kernel

type header = Biocaml_unix.Sam.header
type alignment = Biocaml_unix.Sam.alignment

val fold :
  string ->
  init:'a ->
  f:(header -> 'a -> alignment -> 'a) ->
  'a Or_error.t
