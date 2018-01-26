include Typpx.Make.F(struct
  let tool_name = "ppx_type_of"
  let args = []
  let firstUntypedTransformation = Typpx.Default.untyped_identity
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)

